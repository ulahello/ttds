{ mkDerivation, base, base64, bytestring, http-types, lib, process
, scotty_0_22, scrypt, text
}:
mkDerivation {
  pname = "ttds-web";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base base64 bytestring http-types process scotty_0_22 scrypt text
  ];
  license = lib.licenses.mpl20;
  mainProgram = "ttds-web";
}
