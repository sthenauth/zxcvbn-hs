{ mkDerivation, attoparsec, base, base64-bytestring, cereal
, cereal-text, containers, fgl, filepath, hedgehog, lens
, math-functions, mtl, optparse-applicative, pipes, pipes-safe
, pipes-text, stdenv, tasty, tasty-hedgehog, tasty-hunit, text
, time, vector, zlib
}:
mkDerivation {
  pname = "zxcvbn-hs";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    attoparsec base base64-bytestring cereal cereal-text containers fgl
    lens math-functions text time vector zlib
  ];
  executableHaskellDepends = [
    attoparsec base base64-bytestring cereal cereal-text containers fgl
    filepath lens math-functions mtl optparse-applicative pipes
    pipes-safe pipes-text text time vector zlib
  ];
  testHaskellDepends = [
    attoparsec base base64-bytestring cereal cereal-text containers fgl
    hedgehog lens math-functions tasty tasty-hedgehog tasty-hunit text
    time vector zlib
  ];
  license = stdenv.lib.licenses.mit;
}
