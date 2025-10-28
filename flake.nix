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
                options = {
                tabstop = 2;
                shiftwidth = 4;
                expandtab = true;
                };
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
                 fsharp.enable = true;
                 nim.enable = true;
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

                statusline = {
                  lualine = {
                    enable = true;
                    theme = "catppuccin";
                  };
                };

               theme = {
                 enable = true;
                 name = "catppuccin";
                 style = "mocha";
                 transparent = true;
                };

              autopairs.nvim-autopairs.enable = true;
              autocomplete = {
                    nvim-cmp.enable = false;
                    blink-cmp.enable = true;
              };

              snippets.luasnip.enable = true;
              filetree = {
                neo-tree = {
                  enable = true;
                };
              };

              tabline = {
                nvimBufferline.enable = true;
              };

              treesitter.context.enable = true;

              binds = {
                whichKey.enable = true;
                cheatsheet.enable = true;
              };

              telescope.enable = true;
              
              git = {
                enable = true;
                gitsigns.enable = true;
                gitsigns.codeActions.enable = false;
                neogit.enable = true;
              };
             minimap = {
               minimap-vim.enable = false;
               codewindow.enable = true;
              };

             
             notify = {
               nvim-notify.enable = true;
              };
             
             projects = {
               project-nvim.enable = true;
              };
             utility = {
              ccc.enable = false;
              vim-wakatime.enable = false;
              diffview-nvim.enable = true;
              yanky-nvim.enable = false;
              icon-picker.enable = true;
              surround.enable = true;
              leetcode-nvim.enable = true;
              multicursors.enable = true;
              smart-splits.enable = true;
              undotree.enable = true;
              nvim-biscuits.enable = true;

              motion = {
                hop.enable = true;
                leap.enable = true;
                precognition.enable = true;
               };
             images = {
               image-nvim.enable = false;
               img-clip.enable = true;
               };
              };
            notes = {
              obsidian.enable = false;
              neorg.enable = false;
              orgmode.enable = false;
              mind-nvim.enable = true;
              todo-comments.enable = true;
             };

            terminal = {
              toggleterm = {
                enable = true;
                lazygit.enable = true;
               };
              };

            ui = {
              borders.enable = true;
              noice.enable = true;
              colorizer.enable = true;
              modes-nvim.enable = false;
              illuminate.enable = true;
              breadcrumbs = {
                enable = true;
                navbuddy.enable = true;
              };
            smartcolumn = {
              enable = true;
              setupOpts.custom_colorcolumn = {
              nix = "110";
              ruby = "120";
              java = "130";
              go = ["90" "130"];
              };
            };
            fastaction.enable = true;
              };
            
            session = {
            nvim-session-manager.enable = true;
            };

            gestures = {
              gesture-nvim.enable = true;
            };

            comments = {
              comment-nvim.enable = true;
              };

            presence = {
              neocord.enable = false;
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
