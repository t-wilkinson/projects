# nix/versions.nix - Pinned versions
{
  idf = "5.3.2";
  xtensaGccTag = "esp-13.2.0_20240530";
  xtensaGccVer = "13.2.0_20240530";
  xtensaRust = "1.85.0.0";
  # Espressif's LLVM fork; bindgen needs its libclang to parse headers for Xtensa
  espClang = "19.1.2_20250225";
}
