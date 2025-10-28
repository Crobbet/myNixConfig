{
  description = "Nixos config flake";

  inputs = {

    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nvf = {
      url = "github:notashelf/nvf";
    };

  };

  outputs = { self, nixpkgs, nvf, ... }@inputs:
    {
     nixosConfigurations.Winter = nixpkgs.lib.nixosSystem {
      specialArgs = {inherit inputs;};
      modules = [
        ./nixos/system.nix
        ./nixos/hardware-configuration.nix        
#        /etc/nixos/hardware-configuration.nix
        ./programs/programs.nix

        inputs.home-manager.nixosModules.default

        ./xmonad.nix

        ({config, pkgs, ...}:
          {
            environment.systemPackages = with pkgs; [
              (nvf.lib.neovimConfiguration {
                pkgs = pkgs;
                config = config;
                modules = [
                  {
                    config.vim = {
                      theme.enable = true;
                      treesitter.enable = true;
                    };
                  }
                ];
              })
            ];
          })
      ];
    };
  };
}
