{ mkDerivation, base, cereal, cereal-text, containers, lens, stdenv
, text, vector
}:
mkDerivation {
  pname = "zxcvbn-hs";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base cereal cereal-text containers lens text vector
  ];
  license = stdenv.lib.licenses.mit;
}
