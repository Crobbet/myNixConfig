
        {pkgs, ...}: {
          stylix = {
            enable = true;
            #    base16Scheme = "${pkgs.base16-schemes}/share/themes/catppuccin-mocha.yaml";
            image = ./nixos/miku.jpg;
            polarity = "dark";
            fonts = {
              serif = {
                package = pkgs.comic-mono;
                name = "Comic Sans";
              };

              sansSerif = {
                package = pkgs.comic-mono;
                name = "Comic";
              };

              monospace = {
                package = pkgs.nerd-fonts.comic-shanns-mono;
                name = "ComicShans";
              };

              emoji = {
                package = pkgs.noto-fonts-color-emoji;
                name = "Noto Color Emoji";
              };
            };
          };
        }
