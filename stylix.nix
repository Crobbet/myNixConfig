{pkgs, ...}: {
  stylix = {
    enable = true;
    #    base16Scheme = "${pkgs.base16-schemes}/share/themes/catppuccin-mocha.yaml";
    image = ./nixos/miku.jpg;
    polarity = "dark";
 };
}
