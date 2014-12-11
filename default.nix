with import <nixpkgs> {};

stdenv.mkDerivation {
  name = "panhandle";

  src = ./.;

  buildInputs = [ haskellPackages.ghc haskellPackages.pandoc ];

  buildPhase = ''
    ghc --make panhandle.hs
  '';

  installPhase = ''
    mkdir -p "$out/bin"
    cp panhandle "$out/bin/"
  '';
}