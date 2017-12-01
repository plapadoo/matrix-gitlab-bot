{ mkDerivation, aeson, base, bytestring, containers, dhall
, filepath, http-types, HUnit, lens, lucid, matrix-bot-api, mtl
, optparse-applicative, plpd-utils, QuickCheck
, quickcheck-instances, scotty, stdenv, test-framework
, test-framework-hunit, test-framework-quickcheck2
, test-framework-th, text, text-format
}:
mkDerivation {
  pname = "matrix-gitlab";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base containers dhall filepath http-types lens lucid
    matrix-bot-api mtl optparse-applicative plpd-utils scotty text
    text-format
  ];
  executableHaskellDepends = [
    aeson base bytestring http-types lens matrix-bot-api mtl plpd-utils
    scotty text
  ];
  testHaskellDepends = [
    aeson base bytestring filepath HUnit lens matrix-bot-api plpd-utils
    QuickCheck quickcheck-instances test-framework test-framework-hunit
    test-framework-quickcheck2 test-framework-th text
  ];
  doHaddock = false;
  homepage = "https://github.com/githubuser/matrix-gitlab#readme";
  description = "Process gitlab events and post them to a Matrix channel";
  license = stdenv.lib.licenses.bsd3;
}
