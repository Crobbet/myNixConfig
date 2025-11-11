{pkgs, ...}: {
  stylix = {
    enable = true;
    #    base16Scheme = "${pkgs.base16-schemes}/share/themes/catppuccin-mocha.yaml";
    image = ./violet-evergarden-eddited.png;
    polarity = "dark";
#    fonts = {
#      serif = {
#        package = pkgs.iosevka;
#        name = "Comic Sans";
#      };
#
#      sansSerif = {
#        package = pkgs.iosevka;
#        name = "Comic";
#      };
#
#      monospace = {
#        package = pkgs.nerd-fonts.hack;
#        name = "ComicShans";
#      };
#
#      emoji = {
#        package = pkgs.iosevka;
#        name = "Noto Color Emoji";
#      };
#    };
  };
}
