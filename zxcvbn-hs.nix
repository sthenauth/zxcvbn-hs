{ mkDerivation, base, cereal, cereal-text, containers, lens
, math-functions, stdenv, tasty, tasty-hunit, text, vector
}:
mkDerivation {
  pname = "zxcvbn-hs";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base cereal cereal-text containers lens math-functions text vector
  ];
  testHaskellDepends = [
    base cereal cereal-text containers lens math-functions tasty
    tasty-hunit text vector
  ];
  license = stdenv.lib.licenses.mit;
}
