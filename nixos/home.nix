{ config, pkgs, ... }:

{
  home.username = "bt";
  home.homeDirectory = "/home/bt";
  home.stateVersion = "25.05";
  home.packages = with pkgs; [

    ente-desktop
    
    nerd-fonts.jetbrains-mono
    nerd-fonts.zed-mono

    bash-language-server

    libreoffice-fresh-unwrapped
    
    gruvbox-plus-icons
    google-cursor
    everforest-gtk-theme

    fastfetch
    htop
    nitrogen
    github-cli
  ];

  home.file = {
    ".xmonad/xmonad.hs".source = ./xmonad/xmonad.hs;
    ".xmonad/picom.conf".source = ./xmonad/picom.conf;
    ".xmonad/autorun.sh".source = ./xmonad/autorun.sh;
    ".xmonad/xmonad_term".source = ./xmonad/xmonad_term;
  };

  gtk.enable = true;

  programs.helix = {
    enable = true;
    settings = {
    theme = "ttox";
    editor = {
      line-number = "relative";
      lsp.display-messages = true;
    };
  keys.normal = {
      space.space = "file_picker";
      space.w = ":w";
      space.q = ":q";
      esc = [ "collapse_selection" "keep_primary_selection" ];
      };
    };
  };

  home.sessionVariables = {
    EDITOR = "hx";
  };

  programs.home-manager.enable = true;
}
