{
  description = "Nixos config flake";

  inputs = {
    nixpkgsStable.url = "github:nixos/nixpkgs/nixos-25.05";
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    stylix = {
      url = "github:einetuer/stylix";
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
    ...
  } @ inputs: {
    packages.x86_64-linux.my-neovim =
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

    nixosConfigurations.Winter = nixpkgs.lib.nixosSystem {
      specialArgs = {inherit inputs;};
      modules = [
        stylix.nixosModules.stylix
({pkgs, ...}:
{
  stylix = {
    enable = true;
    base16Scheme = "${pkgs.base16-schemes}/share/themes/gruvbox-dark-hard.yaml";
    };
})
        ./nixos/system.nix
        ./nixos/hardware-configuration.nix
        #        /etc/nixos/hardware-configuration.nix
        ./programs/programs.nix
        inputs.home-manager.nixosModules.default
        ({pkgs, ...}: {
          environment.systemPackages = [self.packages.${pkgs.stdenv.system}.my-neovim];
        })
      ];
    };
  };
}
