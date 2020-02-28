{ sources ? import ./sources.nix
  # nixpkgs ? sources.nixpkgs
}:
with
  { overlay = self : pkgs :
    {
      inherit (import sources.niv {}) niv;
      ghcide = (import sources.ghcide-nix {}).ghcide-ghc865;
    };
    haskellnix = (import sources."haskell.nix");
  };
  
  # config = {};
  # overlays = [(super: self: {
  #   inherit (import sources.niv {}) niv;
  #   inherit (import sources.ghcide-nix {}) ghcide-ghc865 ;

  # })];
  # pkgs = import nixpkgs { inherit overlays config; };
# in
  # pkgs
  import sources.nixpkgs
    { overlays = [ overlay ] ++ haskellnix.overlays ; config = haskellnix.config ;}
