{ mkDerivation, attoparsec, base, base64-bytestring, binary
, binary-instances, containers, criterion, fgl, filepath, hedgehog
, lens, math-functions, mtl, optparse-applicative, pipes
, pipes-safe, pipes-text, stdenv, tasty, tasty-hedgehog
, tasty-hunit, text, time, unordered-containers, vector, zlib
}:
mkDerivation {
  pname = "zxcvbn-hs";
  version = "0.2.0.0";
  src = ./.;
  configureFlags = [ "-ftools" ];
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    attoparsec base base64-bytestring binary binary-instances
    containers fgl lens math-functions text time unordered-containers
    vector zlib
  ];
  executableHaskellDepends = [
    attoparsec base base64-bytestring binary binary-instances
    containers fgl filepath lens math-functions mtl
    optparse-applicative pipes pipes-safe pipes-text text time
    unordered-containers vector zlib
  ];
  testHaskellDepends = [
    attoparsec base base64-bytestring binary binary-instances
    containers fgl hedgehog lens math-functions tasty tasty-hedgehog
    tasty-hunit text time unordered-containers vector zlib
  ];
  benchmarkHaskellDepends = [
    attoparsec base base64-bytestring binary binary-instances
    containers criterion fgl lens math-functions text time
    unordered-containers vector zlib
  ];
  homepage = "https://code.devalot.com/sthenauth/zxcvbn-hs";
  description = "Password strength estimation based on zxcvbn";
  license = stdenv.lib.licenses.mit;
}
