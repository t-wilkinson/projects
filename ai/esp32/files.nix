/*
 * bindgen
 */
{ stdenv, fetchFromGitHub, rustPlatform, pkgs }:
let
  llvm-xtensa = pkgs.callPackage ./llvm-xtensa.nix {};
in
rustPlatform.buildRustPackage rec {
  version = "v0.53.2";
  pname = "rust-bindgen";

  src = fetchFromGitHub {
    owner = "rust-lang";
    repo = "rust-bindgen";
    rev = version;

    sha256 = "01dkaa2akqrhpxxf0g2zyfdb3nx16y14qsg0a9d5n92c4yyvmwjg";
  };

  cargoSha256 = "1yvpj2bz11pcyaadp5vc6yf1q04asr7id6aiw1n875dggvnwb3i8";

  /*
   * Copied from upstream crates.io
   */

# [2020-05-11T21:21:31Z ERROR bindgen::ir::item] Unhandled cursor kind 25: Cursor(~String kind: CXXDestructor, loc: /build/source/tests/headers/public-dtor.hpp:11:9, usr: Some("c:@N@cv@S@String@F@~String#"))
# [2020-05-11T21:21:31Z ERROR bindgen::ir::ty] unsupported type: kind = 162; ty = Type(ObjectType, kind: ObjCTypeParam, cconv: 100, decl: Cursor( kind: NoDeclFound, loc: builtin definitions, usr: None), canon: Cursor( kind: NoDeclFound, loc: builtin definitions, usr: None)); at Cursor(get kind: ObjCInstanceMethodDecl, loc: /build/source/tests/headers/objc_template.h:5:15, usr: Some("c:objc(cs)Foo(im)get"))
# [2020-05-11T21:21:31Z ERROR bindgen::ir::ty] unsupported type: kind = 162; ty = Type(KeyType, kind: ObjCTypeParam, cconv: 100, decl: Cursor( kind: NoDeclFound, loc: builtin definitions, usr: None), canon: Cursor( kind: NoDeclFound, loc: builtin definitions, usr: None)); at Cursor(key kind: ParmDecl, loc: /build/source/tests/headers/objc_template.h:9:46, usr: Some("c:objc_template.h@250objc(cs)FooMultiGeneric(im)objectForKey:@key"))
# [2020-05-11T21:21:31Z ERROR bindgen::ir::ty] unsupported type: kind = 162; ty = Type(ObjectType, kind: ObjCTypeParam, cconv: 100, decl: Cursor( kind: NoDeclFound, loc: builtin definitions, usr: None), canon: Cursor( kind: NoDeclFound, loc: builtin definitions, usr: None)); at Cursor(objectForKey: kind: ObjCInstanceMethodDecl, loc: /build/source/tests/headers/objc_template.h:9:24, usr: Some("c:objc(cs)FooMultiGeneric(im)objectForKey:"))
# [2020-05-11T21:21:31Z ERROR bindgen::ir::item] Unhandled cursor kind 24: Cursor(a<type-parameter-0-0 (type-parameter-0-1...)> kind: CXXConstructor, loc: /build/source/tests/headers/issue-544-stylo-creduce.hpp:5:50, usr: Some("c:@SP>2#T#pT@a>#Ft0.0(#Pt0.1)@F@a#&1>@ST>1#T@a1S0_#"))
# [2020-05-11T21:21:31Z ERROR bindgen::ir::item] Unhandled cursor kind 24: Cursor(Bar<Foo> kind: CXXConstructor, loc: /build/source/tests/headers/issue-1464.hpp:5:3, usr: Some("c:@ST>1#NI@Bar@F@Bar#"))
# [2020-05-11T21:21:31Z ERROR bindgen::ir::item] Unhandled cursor kind 25: Cursor(~Bar<Foo> kind: CXXDestructor, loc: /build/source/tests/headers/issue-1464.hpp:6:3, usr: Some("c:@ST>1#NI@Bar@F@~Bar#"))
# [2020-05-11T21:21:31Z ERROR bindgen::ir::item] Unhandled cursor kind 21: Cursor(doBaz kind: CXXMethod, loc: /build/source/tests/headers/constructor-tp.hpp:12:9, usr: Some("c:@ST>1#T@Foo@F@doBaz#"))
# [2020-05-11T21:21:31Z ERROR bindgen::ir::item] Unhandled cursor kind 24: Cursor(Foo<T> kind: CXXConstructor, loc: /build/source/tests/headers/constructor-tp.hpp:21:9, usr: Some("c:@ST>1#T@Foo@F@Foo#"))
# [2020-05-11T21:21:31Z ERROR bindgen::ir::item] Unhandled cursor kind 9: Cursor(kBar kind: VarDecl, loc: /build/source/tests/headers/auto.hpp:10:31, usr: Some("c:@ST>1#T@Bar@kBar"))
# test header_constify_module_enums_namespace_hpp ... [2020-05-11T21:21:31Z ERROR bindgen::ir::item] Unhandled cursor kind 24: Cursor(Bar kind: CXXConstructor, loc: /build/source/tests/headers/constructor-tp.hpp:25:6, usr: Some("c:@S@Bar@F@Bar#"))
  checkPhase = ''
    runHook preCheck
    export LLVM_CONFIG_PATH="${llvm-xtensa}/bin/llvm-config"
    #cargo test --release
    runHook postCheck
  '';

  meta = with stdenv.lib; {
    description = "Automatically generates Rust FFI bindings to C (and some C++) libraries.";
    homepage = https://github.com/rust-lang/rust-bindgen;
    maintainers = with maintainers; [ "crabtw" "fitzgen" "nox" "emilio" ];
    license = with licenses; [ bsd3 ];
    platforms = platforms.unix;
  };
}


/*
  default.nix
*/
{pkgs}:
let
  rust-xtensa = (pkgs.callPackage ./rust-xtensa.nix { });
in rec {
  inherit (rust-xtensa) rustc cargo rust-src rustPlatform;
  esp-idf = pkgs.callPackage ./esp-idf.nix {};
  esp32-toolchain = pkgs.callPackage ./esp32-toolchain.nix {};
  llvm-xtensa = (pkgs.callPackage ./llvm-xtensa.nix {});

  xbuild = pkgs.callPackage ./xbuild.nix {
    inherit rustPlatform;
  };
  bindgen = pkgs.callPackage ./bindgen.nix {
    inherit rustPlatform;
  };
  rust-analyzer = (pkgs.callPackage ./rust-analyzer.nix {
    inherit rustPlatform;
  }).rust-analyzer;
  env = ''
    export XARGO_RUST_SRC="${rust-src}/src"
    export LLVM_XTENSA="${llvm-xtensa}"
    export LIBCLANG_PATH="${llvm-xtensa}/lib"

    export IDF_PATH=${esp-idf}
    export IDF_TOOLS_PATH=${esp32-toolchain}

    export CFLAGS_COMPILE="-Wno-error=incompatible-pointer-types -Wno-error=implicit-function-declaration"
    export OPENOCD_SCRIPTS=$IDF_TOOLS_PATH/tools/openocd-esp32/share/openocd/scripts
    export NIX_CFLAGS_LINK=-lncurses
    export PATH=$PATH:${esp-idf}/tools:${esp-idf}/components/esptool_py/esptool:$IDF_TOOLS_PATH/tools/esp32ulp-elf/bin:$IDF_TOOLS_PATH/tools/openocd-esp32/bin:$IDF_TOOLS_PATH/tools/xtensa-esp32-elf/bin
  '';
}

/*
  esp-idf.nix
*/
{ stdenv, fetchFromGitHub, pkgs, makeWrapper }:

let
  version = "0a03a55c1eb44a354c9ad5d91d91da371fe23f84";

  pypkgs = python-packages: with python-packages; [
    pyserial
    click
    cryptography
    future
    pyparsing
    pyelftools
    setuptools
    pip
  ];
  python = pkgs.python2.withPackages pypkgs;

in stdenv.mkDerivation rec {
  name = "esp-idf";

  inherit python;

  src = fetchFromGitHub {
    owner = "espressif";
    repo = "esp-idf";
    rev  = "${version}";
    fetchSubmodules = true;
    sha256 = "067rgddhh4fdpnarxz7qjhfb377zdppgk8xsjqpkc07rdyagmwx6";
  };

  propagatedBuildInputs = [
    pkgs.cmake
    pkgs.ninja
    pkgs.gcc
    pkgs.git
    pkgs.ncurses
    pkgs.flex
    pkgs.bison
    pkgs.gperf
    pkgs.ccache
    python
  ];

  phases = [ "unpackPhase" "installPhase" "fixupPhase" ];

  installPhase = ''
    cp -r . $out
  '';

  meta = with stdenv.lib; {
    description = "ESP IDF";
    homepage = https://docs.espressif.com/projects/esp-idf/en/stable/get-started/linux-setup.html;
    license = licenses.gpl3;
  };
}
/*
  esp32-toolchain.nix
*/
{ pkgs ? import <nixpkgs> {} }:

let
  # , stdenv, fetchurl, makeWrapper, buildFHSUserEnv
  stdenv = pkgs.stdenv;
  fetchurl = pkgs.fetchurl;
  fetchzip = pkgs.fetchzip;
  buildFHSUserEnv = pkgs.buildFHSUserEnv;
  makeWrapper = pkgs.makeWrapper;
  lib = pkgs.lib;

  fhsEnv = buildFHSUserEnv {
    name = "esp32-toolchain-env";
    targetPkgs = pkgs: with pkgs; [ zlib libusb1 ];
    runScript = "";
  };
  toolHashes = {
    "xtensa-esp32-elf" = "06b6hw4m1jy79yw1mkj3kgibssrw4d4c5kbipbnckrivw107acw0";
    # "xtensa-esp32s2-elf"
    "esp32ulp-elf" = "02rnzkha3fvzx631y27l9nkzls2qky0v645d4pw888lxkx8p5il9";
    # "esp32s2ulp-elf"
    "openocd-esp32" = "00529xj2pmzy49w3j0wzxlw0phcbmx4vpkqbi0la88smwnqv0nqd";
  };
  version = "0a03a55c1eb44a354c9ad5d91d91da371fe23f84";

  tools = let
    toolInfoFile = fetchurl {
      url = "https://raw.githubusercontent.com/espressif/esp-idf/${version}/tools/tools.json";
      sha256 = "19dlp282mb6lpnwxc7l5i50cnqdj1qlqm5y9k98pr7wyixgj409g";
    };
    toolInfo = builtins.fromJSON (builtins.readFile toolInfoFile);
    filteredTools = builtins.filter (tool: builtins.hasAttr tool.name toolHashes) toolInfo.tools;
    
    fetchTool = tool:
      let
        fileInfo = (builtins.elemAt tool.versions 0).linux-amd64;
      in {
        name = tool.name;
        src = fetchzip {
          url = fileInfo.url;
          sha256 = toolHashes.${tool.name};
        };
      };
  in
    builtins.map fetchTool filteredTools;
in

pkgs.runCommand "esp32-toolchain" {
  buildInputs = [ makeWrapper ];
  meta = with stdenv.lib; {
    description = "ESP32 toolchain";
    homepage = https://docs.espressif.com/projects/esp-idf/en/stable/get-started/linux-setup.html;
    license = licenses.gpl3;
  };
} ''
${lib.strings.concatStrings (builtins.map ({name, src}: ''
mkdir -p $out/tools
TOOLDIR=$out/tools/${name}
cp -r ${src} $TOOLDIR
chmod u+w $TOOLDIR/bin
for FILE in $(ls $TOOLDIR/bin); do
  FILE_PATH="$TOOLDIR/bin/$FILE"
  if [[ -x $FILE_PATH ]]; then
    mv $FILE_PATH $FILE_PATH-unwrapped
    makeWrapper ${fhsEnv}/bin/esp32-toolchain-env $FILE_PATH --add-flags "$FILE_PATH-unwrapped"
  fi
done
chmod u-w $TOOLDIR/bin
'') tools)}
''
/*
  llvm-xtensa.nix
*/
{ stdenv, fetchFromGitHub, pkgs }:

stdenv.mkDerivation rec {
  name = "llvm-xtensa";
  version = "33d79cce656c8c85c38832c8f52810875a3fbddf";

  src = fetchFromGitHub {
    owner = "espressif";
    repo = "llvm-project";
    rev  = "${version}";
    fetchSubmodules = true;
    sha256 = "1a433q374in781l7sjavdlajrhbd568jdr540n2qlgzvkas44g4v";
  };

   buildInputs = [
     pkgs.python3
     pkgs.cmake
     pkgs.ninja
   ];

  phases = [ "unpackPhase" "buildPhase" "installPhase" "fixupPhase" ];

  # http://quickhack.net/nom/blog/2019-05-14-build-rust-environment-for-esp32.html
  buildPhase = ''
    mkdir llvm_build
    cd llvm_build
    cmake ../llvm -DLLVM_ENABLE_PROJECTS="clang;libc;libclc;libcxx;libcxxabi;libunwind;lld;parallel-libs" -DLLVM_INSTALL_UTILS=ON -DLLVM_EXPERIMENTAL_TARGETS_TO_BUILD="Xtensa" -DCMAKE_BUILD_TYPE=Release -G "Ninja"
    cmake --build .
  '';

  installPhase = ''
    mkdir -p $out
    cmake -DCMAKE_INSTALL_PREFIX=$out -P cmake_install.cmake
  '';

  meta = with stdenv.lib; {
    description = "LLVM xtensa";
    homepage = https://github.com/espressif/llvm-project;
    license = licenses.asl20;
  };
}
/*
  rust-xtensa.nix
*/
{pkgs}:
let
    llvm-xtensa = (pkgs.callPackage ./llvm-xtensa.nix {});
    lib = pkgs.lib;
    lists = lib.lists;
    fetchCargoTarball = pkgs.callPackage (pkgs.path + /pkgs/build-support/rust/fetchCargoTarball.nix) {};

    toRustTarget = platform: with platform.parsed; let
        cpu_ = {
            "armv7a" = "armv7";
            "armv7l" = "armv7";
            "armv6l" = "arm";
        }.${cpu.name} or platform.rustc.arch or cpu.name;
        in platform.rustc.config
        or "${cpu_}-${vendor.name}-${kernel.name}${lib.optionalString (abi.name != "unknown") "-${abi.name}"}";

    # bootstrap
    date = "2020-03-12";
    # from rust-xtensa github
    version = "1.44.0";

    rustBinary = pkgs.callPackage (pkgs.path + /pkgs/development/compilers/rust/binary.nix) rec {
        # Noted while installing out of band
        # https://static.rust-lang.org/dist/2020-03-12/rust-std-beta-x86_64-unknown-linux-gnu.tar.xz
        # https://static.rust-lang.org/dist/2020-03-12/rustc-beta-x86_64-unknown-linux-gnu.tar.xz
        # https://static.rust-lang.org/dist/2020-03-12/cargo-beta-x86_64-unknown-linux-gnu.tar.xz
        # https://static.rust-lang.org/dist/2020-01-31/rustfmt-nightly-x86_64-unknown-linux-gnu.tar.xz

        # noted by inspecting https://static.rust-lang.org/dist/2020-03-12
        # version = "1.42.0";
        # version = "nightly";
        version = "beta";

        platform = toRustTarget pkgs.stdenv.hostPlatform;
        versionType = "bootstrap";

        src = pkgs.fetchurl {
            url = "https://static.rust-lang.org/dist/${date}/rust-${version}-${platform}.tar.gz";
            # sha256 = "0llhg1xsyvww776d1wqaxaipm4f566hw1xyy778dhcwakjnhf7kx"; # 1.42.0
            # sha256 = "0jhggcwr852c4cqb4qv9a9c6avnjrinjnyzgfi7sx7n1piyaad43"; # nightly
            sha256 = "1cv402wp9dx6dqd9slc8wqsqkrb7kc66n0bkkmvgjx01n1jhv7n5"; # beta
        };
    };
    bootstrapPlatform = pkgs.makeRustPlatform rustBinary;

    src = pkgs.fetchFromGitHub {
        owner = "MabezDev";
        repo = "rust-xtensa";
        # rust 1.42++
        rev  = "25ae59a82487b8249b05a78f00a3cc35d9ac9959";
        fetchSubmodules = true;
        sha256 = "1xr8rayvvinf1vahzfchlkpspa5f2nxic1j2y4dgdnnzb3rkvkg5";
    };
in
rec {
    rust-src = src;

    rustc = (pkgs.rustc.override {
        rustPlatform = bootstrapPlatform;
    # override the rustc result attrs before calling
    }).overrideAttrs ( old: rec {
        pname = "rustc-xtensa";
        inherit version src;

        llvmSharedForBuild = llvm-xtensa;
        llvmSharedForHost = llvm-xtensa;
        llvmSharedForTarget = llvm-xtensa;
        llvmShared = llvm-xtensa;
        patches = [];

        configureFlags = 
            (lists.remove "--enable-llvm-link-shared"
            (lists.remove "--release-channel=stable" old.configureFlags)) ++ [
            "--set=build.rustfmt=${pkgs.rustfmt}/bin/rustfmt"
            "--llvm-root=${llvm-xtensa}"
            "--experimental-targets=Xtensa"
            # Nightly because xargo (which compiles a new core) can only build on nightly
            # xargo replace with cargo xbuild
            "--release-channel=nightly"
        ];

        cargoDeps = fetchCargoTarball {
            inherit pname;
            inherit src;
            sourceRoot = null;
            srcs = null;
            patches = [];
            sha256 = "0z4mb33f72ik8a1k3ckbg3rf6p0403knx5mlagib0fs2gdswg9w5";
        };

        postConfigure = ''
            ${old.postConfigure}
            unpackFile "$cargoDeps"
            mv $(stripHash $cargoDeps) vendor
            # export VERBOSE=1
        '';
    });

    cargo = (pkgs.callPackage (pkgs.path + /pkgs/development/compilers/rust/cargo.nix) {
        rustPlatform = bootstrapPlatform;
        inherit (pkgs.darwin.apple_sdk.frameworks) Security CoreFoundation; 
        inherit rustc;
    }).overrideAttrs(old: rec {
        name = "cargo-xtensa-${version}";
        inherit version src;
        cargoDeps = fetchCargoTarball {
            inherit name;
            inherit src;
            sourceRoot = null;
            srcs = null;
            patches = [];
            sha256 = "1w5fz966vf09p87xbxc5pm9xq4f1gx8a2vj7fskx30skkwb97d13";
        };

        # cargoVendorDir = builtins.trace "${cargoDeps}" null;
        postConfigure = ''
            unpackFile "$cargoDeps"
            mv $(stripHash $cargoDeps) vendor
            # export VERBOSE=1
        '';
    });

    rustPlatform = pkgs.makeRustPlatform {
        inherit rustc cargo;
    };
}
