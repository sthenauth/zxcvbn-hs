{ mkDerivation, base, cereal, cereal-text, containers, fgl
, filepath, hedgehog, lens, math-functions, pipes, pipes-safe
, pipes-text, stdenv, tasty, tasty-hedgehog, tasty-hunit, text
, vector
}:
mkDerivation {
  pname = "zxcvbn-hs";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base cereal cereal-text containers fgl lens math-functions text
    vector
  ];
  executableHaskellDepends = [
    base cereal cereal-text containers fgl filepath lens math-functions
    pipes pipes-safe pipes-text text vector
  ];
  testHaskellDepends = [
    base cereal cereal-text containers fgl hedgehog lens math-functions
    tasty tasty-hedgehog tasty-hunit text vector
  ];
  license = stdenv.lib.licenses.mit;
}
