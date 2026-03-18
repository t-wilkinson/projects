// Use PIXFORMAT_GRAYSCALE
// memcpy image into tensor

/// Copy camera frambuffer to input tensor
// Fast Single-Pass Copy
int8_t *dst = input_tensor->data.int8;
uint8_t *src = fb->buf;
for (int i = 0; i < 9216; i++) {   // 96x96
  dst[i] = (int8_t)(src[i] - 128); // Simple shift to signed int8
}

/// Raw int8 parsing best class prediction
// Replace the inner loop of parse_output in fomo.cpp
for (int cls = 1; cls < c; cls++) { // Start at 1 to skip background
  int8_t raw_val = is_nchw ? data[cls * h * w + gy * w + gx]
                           : data[gy * w * c + gx * c + cls];

  // Use a raw int8 threshold (e.g., -10) instead of a float 0.45
  if (raw_val > best_raw && raw_val > INT8_CONF_THRESH) {
    best_raw = raw_val;
    best_cls = cls;
  }
}

if (best_cls > 0) {
  fomo_detection_t *d = &dets[count++];
  d->class_id = (uint8_t)best_cls;
  // Store raw_val or a simple mapping instead of float confidence
  d->confidence = (float)best_raw;
}
