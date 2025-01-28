{ stdenv
  , meson
  , ninja
  , pkg-config
  , cmake
  , libdrm
  , systemdLibs
  , lib

  , sanitize ? true
}:

stdenv.mkDerivation {
  pname = "ttds-display-testing";
  version = "0.1.0";
  src = ../.;

  mesonFlags = [ "--buildtype=debugoptimized" ] ++ lib.optional sanitize [ "-Db_sanitize=address" ];

  nativeBuildInputs = [ meson ninja pkg-config cmake libdrm systemdLibs ];

  installPhase = ''
    mesonInstallPhase

    mkdir -p $out/share
    cp $src/run_tests.sh $out/share/run_tests.sh
  '';

  dontStrip = true;
}
