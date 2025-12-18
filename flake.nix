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
    
    nixmobar.url = "git+https://codeberg.org/xmobar/xmobar.git/?dir=nix";



  };

  outputs = {
    self,
    nixpkgs,
    nixpkgsStable,
    stylix,
    xmonad,
    xmonad-contrib,
    nixmobar,
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
     };

    nixosConfigurations.Winter = nixpkgs.lib.nixosSystem {
      specialArgs = {inherit inputs nixmobar;};
      modules = [
        stylix.nixosModules.stylix
        ./stylix.nix
        ./nixos/system.nix
        ./nixos/hardware-configuration.nix
        #        /etc/nixos/hardware-configuration.nix
        ./programs/programs.nix
        inputs.home-manager.nixosModules.default

        ({
          home-manager = {
            extraSpecialArgs = {inherit inputs;};
            users = {
            "bt" = import ./nixos/home.nix;
            };
          };
        })
        
({pkgs, ...}:
{
  services.xserver = {
    desktopManager = {
      mate.enable = true;
    };
    windowManager = {
      xmonad = {
        enable = true;
        enableContribAndExtras = true;
        extraPackages = haskellPackages: [
          xmonad.defaultPackage.${system}
          xmonad-contrib.defaultPackage.${system}
          haskellPackages.xmonad-extras
          haskellPackages.xmobar
          haskellPackages.libnotify
        ];
      };
    };
  };
})
      ];
    };
  };
}
