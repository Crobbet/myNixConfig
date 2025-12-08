{
  description = "Nixos config flake";

  inputs = {
    nixpkgsStable.url = "github:nixos/nixpkgs/nixos-25.05";
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    xmonad = {
      url = "github:Crobbet/myXmonad";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    
    xmonad-contrib = {
      url = "github:xmonad/xmonad-contrib";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    stylix = {
      url = "github:nix-community/stylix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nvf.url = "github:notashelf/nvf";
  };

  outputs = {
    self,
    nixpkgs,
    nixpkgsStable,
    nvf,
    stylix,
    xmonad,
    xmonad-contrib,
    ...
  } @ inputs:

    let
      system =  "x86_64-linux";
      pkgs = import nixpkgs { inherit system; };

    in
    {

    packages.${system} = {
      xmonad = inputs.xmonad.defaultPackage.${system};
      xmonad_contrib = inputs.xmonad-contrib.defaultPackage.${system};


      my-neovim =
        (nvf.lib.neovimConfiguration {
          pkgs = nixpkgs.legacyPackages.x86_64-linux;
          modules = [
            (
              {pkgs, ...}: {
                config.vim = import ./nvf.nix;
              }
            )
          ];
        })
        .neovim;
      };

    nixosConfigurations.Winter = nixpkgs.lib.nixosSystem {
      specialArgs = {inherit inputs;};
      modules = [
        stylix.nixosModules.stylix
        ./stylix.nix
        ./nixos/system.nix
        ./nixos/hardware-configuration.nix
        #        /etc/nixos/hardware-configuration.nix
        ./programs/programs.nix
        inputs.home-manager.nixosModules.default
        ({pkgs, ...}: {
          environment.systemPackages = [self.packages.${pkgs.stdenv.system}.my-neovim];
        })

        
({pkgs, ...}:
{
  services.xserver = {
    windowManager = {
      xmonad = {
        enable = true;
        enableContribAndExtras = true;
        extraPackages = haskellPackages: [
          xmonad.packages.${system}.default
          xmonad-contrib.packages.${system}.xmonad-contrib
          #haskellPackages.xmonad-contrib
          haskellPackages.xmonad-extras
          haskellPackages.xmobar
        ];
      };
    };
  };
})
      ];
    };
  };
}
