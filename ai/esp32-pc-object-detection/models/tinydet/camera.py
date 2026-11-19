"""Simulation of the ESP32-S3 camera's own image pipeline (OV2640 + esp32-camera + firmware).

What the real input path does to a frame, in physical order, and how each step is modelled
(``strength`` scales sensor/ISP effects, ``motion`` scales the car's movement, ``lens``
picks the optics):

1. Optics: a cheap plastic lens. Barrel distortion (stock ~66 deg lens = "mild", 120-160 deg
   fisheye modules = "wide"), radial lens shading that the ISP's LENC only partly corrects
   (vignetting), slight lateral chromatic aberration on wide lenses.
2. Exposure: indoor light forces long exposures and high analog gain. AEC lags and misses
   (+-EV errors with highlight clipping); low light means fewer photons, so shot noise
   (signal-dependent) and read noise are amplified by the gain AGC applies.
3. Rolling shutter: rows are read top to bottom, so a turning car shears the frame
   horizontally; chassis vibration adds a small per-row wobble.
4. Motion blur: exposure times of 1/30-1/15 s indoors blur along the direction of motion
   (mostly horizontal when the car turns).
5. ISP: auto white balance errors (colour casts), saturation, a tone curve, then denoise
   followed by edge sharpening (halos around high-contrast edges such as a ball's rim).
   High gain leaves blotchy, low-frequency chroma noise and faint row banding.
6. Scaler: the DSP downsizes the sensor window to QQVGA with little filtering (aliasing);
   modelled by nearest-neighbour resampling in data.py.
7. Output format: RGB565 (5/6/5 bits) expanded back to 8 bits by the firmware, so dark
   gradients band. Always applied when the simulation is on.

Geometric effects (lens distortion, rolling-shutter shear) move boxes, so they are
Albumentations transforms built by ``geometric()``. Everything else is pixel-only and lives
in ``CameraSim``, which takes an explicit RNG so a seeded copy can build a deterministic
evaluation split (``val_dev``).
"""

from __future__ import annotations

import cv2
import numpy as np

LENS_DISTORT = {"none": None, "mild": (0.05, 0.15), "wide": (0.2, 0.4)}  # albumentations: + = barrel


def geometric(strength: float, motion: float, lens: str):
    """Box-moving camera effects as Albumentations transforms (empty list when off)."""
    import albumentations as A
    ops = []
    d = LENS_DISTORT.get(lens)
    if d is not None:
        try:  # reflect so no black corners appear: the real sensor image fills the frame
            ops.append(A.OpticalDistortion(distort_limit=d, border_mode=cv2.BORDER_REFLECT_101, p=0.9))
        except TypeError:
            ops.append(A.OpticalDistortion(distort_limit=d, p=0.9))
    if motion > 0:
        sh = 12.0 * motion  # degrees of horizontal shear from a turning car
        try:
            ops.append(A.Affine(shear={"x": (-sh, sh), "y": (0, 0)}, border_mode=cv2.BORDER_REFLECT_101,
                                p=0.6 * motion))
        except TypeError:
            ops.append(A.Affine(shear={"x": (-sh, sh), "y": (0, 0)}, p=0.6 * motion))
    return ops


def _srgb_to_lin(x):
    return np.power(x, 2.2)


def _lin_to_srgb(x, gamma=2.2):
    return np.power(np.clip(x, 0, 1), 1.0 / gamma)


def rgb565(img: np.ndarray) -> np.ndarray:
    r = img[..., 0] >> 3
    g = img[..., 1] >> 2
    b = img[..., 2] >> 3
    out = np.empty_like(img)
    out[..., 0] = (r << 3) | (r >> 2)
    out[..., 1] = (g << 2) | (g >> 4)
    out[..., 2] = (b << 3) | (b >> 2)
    return out


def _motion_kernel(length: int, angle_deg: float) -> np.ndarray:
    k = np.zeros((length, length), np.float32)
    c = (length - 1) / 2
    t = np.deg2rad(angle_deg)
    for s in np.linspace(-c, c, length * 3):
        x, y = int(round(c + s * np.cos(t))), int(round(c + s * np.sin(t)))
        k[y, x] = 1.0
    return k / k.sum()


class CameraSim:
    """Pixel-only camera effects on an RGB uint8 image (H, W, 3)."""

    def __init__(self, strength: float = 0.6, motion: float = 0.3, lens: str = "mild",
                 p: float = 1.0):
        self.s = float(np.clip(strength, 0, 1))
        self.m = float(np.clip(motion, 0, 1))
        self.lens = lens
        self.p = p

    def __call__(self, img: np.ndarray, rng: np.random.Generator | None = None) -> np.ndarray:
        rng = rng or np.random.default_rng()
        if self.s <= 0 and self.m <= 0:
            return img
        if rng.random() > self.p:
            return rgb565(img)
        s, m = self.s, self.m
        h, w = img.shape[:2]
        x = _srgb_to_lin(img.astype(np.float32) / 255.0)

        # 1. lens shading (vignetting) and chromatic aberration
        yy, xx = np.mgrid[0:h, 0:w].astype(np.float32)
        r2 = ((xx - w / 2) / (w / 2)) ** 2 + ((yy - h / 2) / (h / 2)) ** 2
        v = rng.uniform(0.0, 0.45 * s + (0.25 if self.lens == "wide" else 0.0))
        x *= (1.0 - v * np.clip(r2, 0, 2) / 2)[..., None]
        if self.lens == "wide" and rng.random() < 0.7:
            for ch, sc in ((0, 1 + rng.uniform(0.002, 0.008)), (2, 1 - rng.uniform(0.002, 0.008))):
                M = cv2.getRotationMatrix2D((w / 2, h / 2), 0, sc)
                x[..., ch] = cv2.warpAffine(x[..., ch], M, (w, h), borderMode=cv2.BORDER_REFLECT_101)

        # 2. exposure: scene light, AEC error, AGC gain, shot + read noise, clipping
        light = np.exp(rng.uniform(np.log(1 / 8), 0)) if rng.random() < 0.6 * s else 1.0
        aec = 2.0 ** rng.normal(0, 0.45 * s)
        signal = x * light * aec
        gain = float(np.clip(1.0 / light, 1.0, 16.0))
        # per-pixel noise after the sensor-to-QQVGA downscale (which averages many photosites)
        shot = rng.uniform(0.0005, 0.002) * (0.3 + s)
        read = rng.uniform(0.0005, 0.002) * (0.3 + s)
        noise = rng.normal(0, 1, signal.shape).astype(np.float32) * np.sqrt(shot * np.clip(signal, 0, None) + read ** 2)
        x = np.clip((signal + noise) * gain, 0, 1)   # sensor saturates: highlights clip here

        # 3. rolling-shutter wobble from vibration (shear itself is geometric())
        if m > 0 and rng.random() < 0.5 * m:
            amp, freq, ph = rng.uniform(0.2, 1.2) * m, rng.uniform(0.02, 0.1), rng.uniform(0, 2 * np.pi)
            shift = amp * np.sin(2 * np.pi * freq * np.arange(h) + ph)
            mapx = (xx + shift[:, None]).astype(np.float32)
            x = cv2.remap(x, mapx, yy, cv2.INTER_LINEAR, borderMode=cv2.BORDER_REFLECT_101)

        # 4. motion blur, mostly horizontal
        if m > 0 and rng.random() < 0.7 * m:
            length = int(rng.integers(2, 2 + max(1, int(7 * m)))) * 2 + 1
            angle = rng.normal(0, 15) if rng.random() < 0.8 else rng.uniform(0, 180)
            x = cv2.filter2D(x, -1, _motion_kernel(length, angle), borderType=cv2.BORDER_REFLECT_101)

        # 5. ISP: white balance, saturation, tone curve, denoise + sharpen, chroma noise, banding
        wb = np.exp(rng.normal(0, 0.10 * s, 3)).astype(np.float32)
        wb[1] = 1.0
        x = np.clip(x * wb, 0, 1)
        y = _lin_to_srgb(x, gamma=rng.uniform(2.0, 2.5))
        luma = (0.299 * y[..., 0] + 0.587 * y[..., 1] + 0.114 * y[..., 2])[..., None]
        sat = rng.uniform(1 - 0.3 * s, 1 + 0.35 * s)
        y = np.clip(luma + (y - luma) * sat, 0, 1)
        if rng.random() < 0.6 * s:
            y = cv2.GaussianBlur(y, (0, 0), rng.uniform(0.4, 0.8))
            blur = cv2.GaussianBlur(y, (0, 0), 1.0)
            y = np.clip(y + rng.uniform(0.4, 1.4) * s * (y - blur), 0, 1)
        if gain > 2 and rng.random() < s:
            cn = rng.normal(0, 1, (max(1, h // 8), max(1, w // 8), 3)).astype(np.float32)
            cn = cv2.resize(cn, (w, h), interpolation=cv2.INTER_CUBIC)
            cn -= cn.mean(axis=2, keepdims=True)  # chroma only
            y = np.clip(y + cn * 0.004 * gain * s, 0, 1)
        if gain > 2 and rng.random() < 0.5 * s:
            y = np.clip(y + rng.normal(0, 0.002 * gain, (h, 1, 1)).astype(np.float32), 0, 1)

        out = (y * 255.0 + 0.5).astype(np.uint8)
        return rgb565(out)  # 7. the camera's output format
