// ─────────────────────────────────────────────────────────────────────────────
// detect.rs — turn the model's raw int8 grid into scores and detections
//
// Pure Rust with no ESP-IDF dependencies, so it can be tested on your PC:
//     rustc --edition 2021 --test src/detect.rs -o /tmp/detect && /tmp/detect
//
// The model outputs one int8 value per grid cell per channel. Two layouts are
// supported, matching the notebook's two generations of FOMO head:
//   • background + K classes, trained with softmax/cross-entropy
//   • K classes, no background channel, trained as sigmoid heatmaps
//     (CenterNet focal loss)
// ─────────────────────────────────────────────────────────────────────────────

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Activation {
    /// Independent per-class probability: p = 1 / (1 + e^-x)
    Sigmoid,
    /// Probabilities across channels of one cell sum to 1
    Softmax,
    /// The model already ends in a sigmoid/softmax op; values are probabilities
    Identity,
}

impl Activation {
    pub fn parse(s: &str) -> Option<Self> {
        match s {
            "sigmoid" => Some(Self::Sigmoid),
            "softmax" => Some(Self::Softmax),
            "none" | "identity" => Some(Self::Identity),
            _ => None,
        }
    }

    pub fn name(self) -> &'static str {
        match self {
            Self::Sigmoid => "sigmoid",
            Self::Softmax => "softmax",
            Self::Identity => "none",
        }
    }
}

/// Shape and quantisation of the output tensor.
#[derive(Debug, Clone, Copy)]
pub struct OutputTensor {
    pub grid_w: usize,
    pub grid_h: usize,
    pub channels: usize,
    pub scale: f32,
    pub zero_point: i32,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Detection {
    /// Index into the metadata's class_names (0 is background, never returned)
    pub class_id: usize,
    pub grid_x: usize,
    pub grid_y: usize,
    /// Centroid in 0..1 image coordinates, refined to sub-cell precision by
    /// weighting the 3×3 neighbourhood by score.
    pub x: f32,
    pub y: f32,
    pub score: f32,
}

pub struct Decoder {
    out: OutputTensor,
    activation: Activation,
    /// Channel holding class 1. 1 if channel 0 is background, else 0.
    first_fg_channel: usize,
    num_classes: usize,
    /// int8 → probability × 255, for activations that work per value.
    lut: [u8; 256],
}

impl Decoder {
    /// `class_names` is the metadata list, background first.
    /// `activation` overrides the guess made from the tensor.
    pub fn new(
        out: OutputTensor,
        class_names: &[&str],
        activation: Option<Activation>,
    ) -> Result<Self, String> {
        let has_bg_name = class_names.first() == Some(&"background");
        let num_classes = class_names.len() - usize::from(has_bg_name);

        let first_fg_channel = if out.channels == num_classes + 1 {
            1
        } else if out.channels == num_classes {
            0
        } else {
            return Err(format!(
                "model outputs {} channels but model_metadata.json lists {} classes \
                 (+ background). Re-export the metadata with the model.",
                out.channels, num_classes
            ));
        };

        // TFLite quantises sigmoid/softmax outputs with a fixed scale of 1/256
        // and zero point -128, so a tensor like that already holds
        // probabilities. Otherwise these are logits: a background channel
        // means the model was trained with softmax, no background channel
        // means per-class sigmoid heatmaps.
        let is_probability = (out.scale - 1.0 / 256.0).abs() < 1e-6 && out.zero_point == -128;
        let activation = activation.unwrap_or(if is_probability {
            Activation::Identity
        } else if first_fg_channel == 1 {
            Activation::Softmax
        } else {
            Activation::Sigmoid
        });

        let mut lut = [0u8; 256];
        for (i, slot) in lut.iter_mut().enumerate() {
            let q = i as i32 - 128; // slot i holds int8 value i - 128
            let x = (q - out.zero_point) as f32 * out.scale;
            let p = match activation {
                Activation::Sigmoid => 1.0 / (1.0 + (-x).exp()),
                _ => x.clamp(0.0, 1.0),
            };
            *slot = (p * 255.0).round() as u8;
        }

        Ok(Self {
            out,
            activation,
            first_fg_channel,
            num_classes,
            lut,
        })
    }

    pub fn activation(&self) -> Activation {
        self.activation
    }

    pub fn score_len(&self) -> usize {
        self.out.grid_w * self.out.grid_h * self.num_classes
    }

    /// Per-cell, per-class probability × 255, laid out [y][x][class].
    /// `raw` is the NHWC output tensor; `scores` must be `score_len()` long.
    pub fn scores(&self, raw: &[i8], scores: &mut [u8]) {
        let (c, k, fg) = (self.out.channels, self.num_classes, self.first_fg_channel);
        let cells = self.out.grid_w * self.out.grid_h;
        assert_eq!(raw.len(), cells * c);
        assert_eq!(scores.len(), cells * k);

        for cell in 0..cells {
            let logits = &raw[cell * c..(cell + 1) * c];
            let dst = &mut scores[cell * k..(cell + 1) * k];

            if self.activation == Activation::Softmax {
                let deq = |q: i8| (q as i32 - self.out.zero_point) as f32 * self.out.scale;
                // Subtract the max before exp() so it can't overflow.
                let max = logits.iter().map(|&q| deq(q)).fold(f32::MIN, f32::max);
                let sum: f32 = logits.iter().map(|&q| (deq(q) - max).exp()).sum();
                for (j, d) in dst.iter_mut().enumerate() {
                    let p = (deq(logits[fg + j]) - max).exp() / sum;
                    *d = (p * 255.0).round() as u8;
                }
            } else {
                for (j, d) in dst.iter_mut().enumerate() {
                    *d = self.lut[(logits[fg + j] as i32 + 128) as usize];
                }
            }
        }
    }

    /// A cell is a detection when its score clears `threshold` and it is the
    /// highest of its 3×3 neighbourhood for that class: one peak per object,
    /// even though a ball lights up several cells. This is the same
    /// "max-pool NMS" the notebook's decode_peaks() does.
    pub fn peaks(&self, scores: &[u8], threshold: f32, max: usize) -> Vec<Detection> {
        let (w, h, k) = (self.out.grid_w, self.out.grid_h, self.num_classes);
        let at = |x: usize, y: usize, cls: usize| scores[(y * w + x) * k + cls];
        let mut found = Vec::new();

        for cls in 0..k {
            for y in 0..h {
                for x in 0..w {
                    let s = at(x, y, cls);
                    if (s as f32) < threshold * 255.0 || s == 0 {
                        continue;
                    }

                    let mut is_peak = true;
                    let (mut sum, mut sx, mut sy) = (0.0f32, 0.0f32, 0.0f32);
                    for ny in y.saturating_sub(1)..=(y + 1).min(h - 1) {
                        for nx in x.saturating_sub(1)..=(x + 1).min(w - 1) {
                            let n = at(nx, ny, cls);
                            // Ties go to the first cell in raster order, so a
                            // flat plateau yields one detection, not two.
                            let earlier = (ny, nx) < (y, x);
                            if n > s || (earlier && n == s) {
                                is_peak = false;
                            }
                            sum += n as f32;
                            sx += n as f32 * (nx as f32 + 0.5);
                            sy += n as f32 * (ny as f32 + 0.5);
                        }
                    }
                    if !is_peak {
                        continue;
                    }

                    found.push(Detection {
                        class_id: cls + 1,
                        grid_x: x,
                        grid_y: y,
                        x: sx / sum / w as f32,
                        y: sy / sum / h as f32,
                        score: s as f32 / 255.0,
                    });
                }
            }
        }

        found.sort_by(|a, b| b.score.total_cmp(&a.score));
        found.truncate(max);
        found
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const NAMES: [&str; 3] = ["background", "sports_ball", "person"];

    fn tensor(w: usize, h: usize, c: usize, scale: f32, zp: i32) -> OutputTensor {
        OutputTensor {
            grid_w: w,
            grid_h: h,
            channels: c,
            scale,
            zero_point: zp,
        }
    }

    #[test]
    fn picks_layout_and_activation_from_tensor() {
        let d = Decoder::new(tensor(4, 4, 3, 0.1, 0), &NAMES, None).unwrap();
        assert_eq!(
            (d.first_fg_channel, d.activation()),
            (1, Activation::Softmax)
        );

        let d = Decoder::new(tensor(4, 4, 2, 0.1, 0), &NAMES, None).unwrap();
        assert_eq!(
            (d.first_fg_channel, d.activation()),
            (0, Activation::Sigmoid)
        );

        let d = Decoder::new(tensor(4, 4, 2, 1.0 / 256.0, -128), &NAMES, None).unwrap();
        assert_eq!(d.activation(), Activation::Identity);

        let d = Decoder::new(tensor(4, 4, 2, 0.1, 0), &NAMES, Some(Activation::Softmax));
        assert_eq!(d.unwrap().activation(), Activation::Softmax);

        assert!(Decoder::new(tensor(4, 4, 5, 0.1, 0), &NAMES, None).is_err());
    }

    #[test]
    fn sigmoid_lut_matches_formula() {
        let d = Decoder::new(tensor(1, 1, 2, 0.05, -10), &NAMES, None).unwrap();
        let mut s = [0u8; 2];
        d.scores(&[-10, 40], &mut s);
        assert_eq!(s[0], 128); // x = 0 → 0.5
        let p = 1.0 / (1.0 + (-(50.0f32 * 0.05)).exp());
        assert_eq!(s[1], (p * 255.0).round() as u8);
    }

    #[test]
    fn softmax_skips_background_and_sums_to_one() {
        let d = Decoder::new(tensor(1, 1, 3, 0.5, 0), &NAMES, None).unwrap();
        let mut s = [0u8; 2];
        d.scores(&[0, 4, 2], &mut s); // logits 0, 2, 1
        let e = [1.0f32, 2f32.exp(), 1f32.exp()];
        let sum: f32 = e.iter().sum();
        assert_eq!(s[0], (e[1] / sum * 255.0).round() as u8);
        assert_eq!(s[1], (e[2] / sum * 255.0).round() as u8);
    }

    #[test]
    fn one_peak_per_blob_with_subcell_centroid() {
        // 5×5 grid, one class (no background channel), identity activation
        let d = Decoder::new(tensor(5, 5, 2, 1.0 / 256.0, -128), &NAMES, None).unwrap();
        let mut scores = vec![0u8; d.score_len()];
        let mut set = |x: usize, y: usize, v: u8| scores[(y * 5 + x) * 2] = v;
        set(2, 2, 200);
        set(3, 2, 200); // tie with (2,2): only the first in raster order wins
        set(2, 1, 100);
        let dets = d.peaks(&scores, 0.5, 10);
        assert_eq!(dets.len(), 1);
        let det = &dets[0];
        assert_eq!((det.class_id, det.grid_x, det.grid_y), (1, 2, 2));
        // pulled right (towards the tie) and up (towards the 100)
        assert!(det.x > 2.5 / 5.0 && det.y < 2.5 / 5.0);
    }

    #[test]
    fn threshold_and_max_are_respected() {
        let d = Decoder::new(tensor(5, 1, 2, 1.0 / 256.0, -128), &NAMES, None).unwrap();
        let mut scores = vec![0u8; d.score_len()];
        for (x, v) in [(0, 250u8), (2, 120), (4, 200)] {
            scores[x * 2] = v;
        }
        let dets = d.peaks(&scores, 0.5, 10);
        assert_eq!(
            dets.iter().map(|d| d.grid_x).collect::<Vec<_>>(),
            vec![0, 4]
        );
        assert_eq!(d.peaks(&scores, 0.5, 1).len(), 1);
    }
}
