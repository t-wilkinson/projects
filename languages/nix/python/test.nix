with import <nixpkgs> {};

( let
    my_toolz = python36.pkgs.buildPythonPackage rec {
      pname = "toolz";
      version = "0.7.4";

      src = python35.pkgs.fetchPypi {
        inherit pname version;
        sha256 = "43c2c9e5e7a16b6c88ba3088a9bfc82f7db8e13378be7c78d6c14a5f8ed05afd";
      };
      doCheck = false;
      meta = {
        homepage = "https://github.com/pytoolz/toolz/";
        description = "List processing tools and functional utilities";
      };
    };

    my_greenlet = python36.pkgs.buildPythonPackage rec {
      pname = "greenlet";
      version = "0.4.16";
      src = python36.pkgs.fetchPypi {
        inherit pname version;
        sha256 = "6e06eac722676797e8fce4adb8ad3dc57a1bb3adfb0dd3fdf8306c055a38456c";
      };
      doCheck = false;
    };

    my_msgpack = python36.pkgs.buildPythonPackage rec {
      pname = "msgpack";
      version = "0.5.0";
      src = python36.pkgs.fetchPypi {
        inherit pname version;
        sha256 = "9ed78ef17937222b0467566487bab1ca1bb28158f66c6528ff3d5aced93ff49d";
      };
      doCheck = false;
      propagatedBuildInputs = [ my_greenlet ];
    };

    my_pynvim = python36.pkgs.buildPythonPackage rec {
      pname = "pynvim";
      version = "0.4.1";
      src = python36.pkgs.fetchPypi {
        inherit pname version;
        sha256 = "0n2cx22lrmbq7xk7356lyn6k77ryqvkxplw9k0fglk35ckb1isam";
      };
      propagatedBuildInputs = [ my_msgpack ];
      doCheck = false;
    };

    my_neovim = python36.pkgs.buildPythonPackage rec {
      pname = "neovim";
      version = "0.3.1";
      src = python36.pkgs.fetchPypi {
        inherit pname version;
        sha256 = "a6a0e7a5b4433bf4e6ddcbc5c5ff44170be7d84259d002b8e8d8fb4ee78af60f";
      };
      propagatedBuildInputs = [ my_pynvim ]; # runtime dependency
      buildInputs = [ ]; # only for build-time dependency
      doCheck = false;
    };

  in python36.withPackages (ps: [ps.numpy my_toolz my_neovim ])
).env
