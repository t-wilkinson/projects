with import <nixpkgs> {};

let
pymunk = python36.pkgs.buildPythonPackage rec {
  pname = "pymunk";
  version = "5.4.2";

  src = fetchurl {
    url = https://files.pythonhosted.org/packages/f5/4d/5ace2c6c1d2bf100e22ba7d3c671bea11307049f4bfe2febe5d71cc599ee/pymunk-5.6.0.zip;
    sha256 = "02y227g1wsy9m2br4d9kykqiy0jr2hvnndk82jiwqh5c9qz9va6v";
  };

#   src = python36.pkgs.fetchPypi {
#     inherit pname version;
#     sha256 = "0000000000000000000000000000000000000000000000000000000000000000";
#   };

  doCheck = false;

  meta = with lib; {
    homepage = https://github.com/viblo/pymunk;
    description = "Pymunk is a easy-to-use pythonic 2d physics library";
    license = licenses.mit;
  };
};
in

mkShell {
  buildInputs = [
    (python36.withPackages (p: with p; [
      numpy pyglet pymunk
    ]))
  ];

}
