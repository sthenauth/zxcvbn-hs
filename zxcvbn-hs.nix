{ mkDerivation, base, base64-bytestring, cereal, cereal-text
, containers, fgl, filepath, hedgehog, lens, math-functions, mtl
, optparse-applicative, pipes, pipes-safe, pipes-text, stdenv
, tasty, tasty-hedgehog, tasty-hunit, text, vector, zlib
}:
mkDerivation {
  pname = "zxcvbn-hs";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base base64-bytestring cereal cereal-text containers fgl lens
    math-functions text vector zlib
  ];
  executableHaskellDepends = [
    base base64-bytestring cereal cereal-text containers fgl filepath
    lens math-functions mtl optparse-applicative pipes pipes-safe
    pipes-text text vector zlib
  ];
  testHaskellDepends = [
    base base64-bytestring cereal cereal-text containers fgl hedgehog
    lens math-functions tasty tasty-hedgehog tasty-hunit text vector
    zlib
  ];
  license = stdenv.lib.licenses.mit;
}
