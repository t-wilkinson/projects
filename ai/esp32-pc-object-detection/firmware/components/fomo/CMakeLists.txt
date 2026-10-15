idf_component_register(
    SRCS "fomo.cpp"
    INCLUDE_DIRS "include"
    REQUIRES esp-tflite-micro esp32-camera esp_timer
)
