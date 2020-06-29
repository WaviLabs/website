{ mkDerivation, base, dhall, lucid, lucid-lego, stdenv, text }:
mkDerivation {
  pname = "templates";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base dhall lucid lucid-lego text ];
  executableHaskellDepends = [ base dhall lucid lucid-lego text ];
  testHaskellDepends = [ base dhall lucid lucid-lego text ];
  homepage = "https://github.com/githubuser/templates#readme";
  license = stdenv.lib.licenses.bsd3;
}
