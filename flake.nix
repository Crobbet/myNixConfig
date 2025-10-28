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
                viAlias = true;
                vimAlias = true;
                debugMode = {
                  enable = false;
                  level = 16;
                  logFile = "/tmp/nvim.log";
                };
                spellcheck = {
                  enable = true;
                };

                lsp = {
                  enable = true;

                  formatOnSave = true;
                  lspkind.enable = false;
                  lightbulb.enable = true;
                  lspsaga.enable = false;
                  trouble.enable = true;
               };

              debugger = {
                nvim-dap = {
                  enable = true;
                  ui.enable = true;
                };
              };

             languages = {
               enableFormat = true;
               enableTreesitter = true;
               enableExtraDiagnostics = true;
               nix.enable = true;
               markdown.enable = true;
               bash.enable = true;
               clang.enable = true;
               css.enable = true;
               html.enable = true;
               lua.enable = true;
               zig.enable = true;
               python.enable = true;
               assembly.enable = true;
               nu.enable = true;
               julia.enable = true;
               gleam.enable = true;
               haskell.enable = true;
               fsharp.enable = false;
               nim.enable = false;
               };
                visuals = {
                  nvim-scrollbar.enable = true;
                  nvim-web-devicons.enable = true;
                  nvim-cursorline.enable = true;
                  cinnamon-nvim.enable = true;
                  fidget-nvim.enable = true;

                 highlight-undo.enable = true;
                 indent-blankline.enable = true;
                 cellular-automaton.enable = false;
                };

             };
          })
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

        ({pkgs, ...}: {
            environment.systemPackages = [self.packages.${pkgs.stdenv.system}.my-neovim];
          })

      ];
    };
  };
}
