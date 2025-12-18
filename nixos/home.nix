{
  config,
  lib,
  pkgs,
  inputs,
  ...
}: {
  home.username = "bt";
  home.homeDirectory = "/home/bt";
  home.stateVersion = "25.11";
  home.packages = with pkgs; [
    qutebrowser
    nautilus
    tint2
    polybar
    eww
    rofi

    xfce.xfce4-pulseaudio-plugin
    xfce.xfce4-battery-plugin
    xfce.xfce4-clipman-plugin
    xfce.xfce4-docklike-plugin
    xfce.xfce4-power-manager
    xfce.xfce4-volumed-pulse

    xfce.xfce4-notifyd
    libnotify

    
    whitesur-icon-theme

    ente-desktop

    nerd-fonts.jetbrains-mono
    nerd-fonts.zed-mono
    nerd-fonts.dejavu-sans-mono

    bash-language-server

    gruvbox-plus-icons

    google-cursor
    everforest-gtk-theme

    fastfetch
    htop
    nitrogen
    github-cli
  ];

  services.xsettingsd = {
    enable = true;
    settings = {
      "Net/IconThemeName" = "WhiteSur";
      #"Xcursor/theme" = "GoogleDot-Red";
      #"Xcursor/size" = "24";
    };
  };

  programs = {
    yazi = {
      enableNushellIntegration = true;
     plugins = {
        projects = pkgs.yaziPlugins.projects;
        yatline = pkgs.yaziPlugins.yatline-githead;
        yatline-catppuccin = pkgs.yaziPlugins.yatline-catppuccin;
      };
    };
    fd.enable = true;

    eza = {
      enable = true;
      enableNushellIntegration = true;
    };

    zoxide = {
      enable = true;
      enableNushellIntegration = true;
    };

    skim.enable = true;

    atuin = {
      enable = true;
      enableNushellIntegration = true;
      settings = {
        auto_sync = false;
        search_mode = "prefix";
      };
    };
  };
  programs.hyfetch = {
    enable = true;
    settings = {
      pride_month_disable = true;
      backend = "fastfetch";
      preset = "asexual";
      mode = "rgb";
      color_align = {
        mode = "horizontal";
      };
    };
  };

  programs = {
    nushell = {
      enable = true;
      environmentVariables = {
        GUIX_PROFILE = "/home/bt/.config/guix/current";
      };
      extraConfig = ''
        let carapace_completer = {|spans|
        carapace $spans.0 nushell ...$spans | from json
        }
        $env.config = {
         show_banner: false,
         completions: {
         case_sensitive: false # case-sensitive completions
         quick: true    # set to false to prevent auto-selecting completions
         partial: true    # set to false to prevent partial filling of the prompt
         algorithm: "fuzzy"    # prefix or fuzzy
         external: {
             enable: true
             max_results: 100
             completer: $carapace_completer # check 'carapace_completer'
           }
         }
        }
        $env.PATH = ($env.PATH |
        split row (char esep) |
        prepend /home/myuser/.apps |
        append /usr/bin/env
        )
      '';
    };
    carapace.enable = true;
    carapace.enableNushellIntegration = true;

    starship = {
      enable = true;
      settings = {
        add_newline = false;
        format = lib.concatStrings [
          "$line_break"
          "[░▒▓]()"
          "[ ❄️ ]()"
          "$time"
          "$package"
          "$line_break"
          "$character"
        ];
        right_format = lib.concatStrings [
          "[⅀ ░▒▓  ]( bg:#394260)"
          "$directory"
          "$git_branch"
          "$git_status"
          "$nodejs"
          "$rust"
          "$golang"
          "$php"
        ];
        scan_timeout = 10;
        character = {
        success_symbol = " ∫ ";
        error_symbol = " ✘ ";
        };
      };
      enableNushellIntegration = true;
    };

   };

  programs.helix = {
    enable = true;
    settings = {
      editor = {
        line-number = "relative";
        lsp.display-messages = true;
      };
      keys.normal = {
        space.space = "file_picker";
        space.w = ":w";
        space.q = ":q";
        esc = ["collapse_selection" "keep_primary_selection"];
      };
    };
  };

  programs.ghostty = {
    enable = true;
    settings = {
      command = "nu";
      background-opacity = "0.7";
      unfocused-split-opacity = "0.2";
      window-decoration = "none";
      window-padding-x = "10, 10";
      window-padding-y = "10, 10";
      font-size = 12;
      keybind = [
        "ctrl+h=goto_split:left"
        "ctrl+l=goto_split:right"
      ];
    };
    themes = {
      catppuccin-mocha = {
        background = "1e1e2e";
        cursor-color = "f5e0dc";
        foreground = "cdd6f4";
        palette = [
          "0=#45475a"
          "1=#f38ba8"
          "2=#a6e3a1"
          "3=#f9e2af"
          "4=#89b4fa"
          "5=#f5c2e7"
          "6=#94e2d5"
          "7=#bac2de"
          "8=#585b70"
          "9=#f38ba8"
          "10=#a6e3a1"
          "11=#f9e2af"
          "12=#89b4fa"
          "13=#f5c2e7"
          "14=#94e2d5"
          "15=#a6adc8"
        ];
        selection-background = "353749";
        selection-foreground = "cdd6f4";
      };
    };
  };

  home.sessionVariables = {
    EDITOR = "hx";
  };

  programs.home-manager.enable = true;
    imports = [
      inputs.nixmobar.homeModules.mainmodule
      ./xmobar.nix
    ];
}
