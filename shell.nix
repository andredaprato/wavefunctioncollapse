{ pkgs ? import ./nix {} }:

let
  hsPkgs = import ./default.nix { inherit pkgs; };
in 
pkgs.lib.fix (self: hsPkgs.shellFor {
  packages = ps: with ps; [
    wavefunctioncollapse
  ];

  withHoogle = true;
  buildInputs = with pkgs.haskellPackages;
  [ hlint pkgs.ghcide pkgs.niv hpack ];
  # ++ (with pkgs; [ ghcide niv ])
  exactDeps = true;
  shellHook = ''
    export NIX_GHC="${self.ghc}/bin/ghc"
    export NIX_GHCPKG="${self.ghc}/bin/ghc-pkg"
    export NIX_GHC_DOCDIR="${self.ghc}/share/doc/ghc/html"
    export NIX_GHC_LIBDIR=$( $NIX_GHC --print-libdir )
  '';
})
