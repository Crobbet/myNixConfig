{
  description = "Nixos config flake";

  inputs = {

    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nvf.url = "github:notashelf/nvf";

  };

  outputs = { self, nixpkgs, nvf, ... }@inputs:
    {
    packages.x86_64-linux.my-neovim =
      (nvf.lib.neovimConfiguration {
        pkgs = nixpkgs.legacyPackages.x86_64-linux;
        modules = [
        (
            {pkgs, ...}: {
             config.vim = {
                theme.enable = true;
              };
            }
          )
        ];
      })
      .neovim;

     nixosConfigurations.Winter = nixpkgs.lib.nixosSystem {
      specialArgs = {inherit inputs;};
      modules = [
        ./nixos/system.nix
        ./nixos/hardware-configuration.nix        
#        /etc/nixos/hardware-configuration.nix
        ./programs/programs.nix

        ./xmonad.nix
        inputs.home-manager.nixosModules.default
      ];
    };
  };
}
