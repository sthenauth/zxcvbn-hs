{ mkDerivation, base, cereal, cereal-text, containers, stdenv, text
}:
mkDerivation {
  pname = "zxcvbn-hs";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base cereal cereal-text containers text
  ];
  license = stdenv.lib.licenses.mit;
}
