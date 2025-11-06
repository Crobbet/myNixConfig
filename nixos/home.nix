{
  config,
  lib,
  pkgs,
  ...
}: {
  home.username = "bt";
  home.homeDirectory = "/home/bt";
  home.stateVersion = "25.11";
  home.packages = with pkgs; [
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

  home.file = {
    ".xmonad/xmonad.hs".source = ./xmonad/xmonad.hs;
    ".xmonad/picom.conf".source = ./xmonad/picom.conf;
    ".xmonad/autorun.sh".source = ./xmonad/autorun.sh;
    ".xmonad/xmonad_term".source = ./xmonad/xmonad_term;
  };

  services.xsettingsd = {
    enable = true;
    settings = {
      "Net/IconThemeName" = "WhiteSur";
      "Xcursor/theme" = "GoogleDot-Red";
      "Xcursor/size" = "24";
    };
  };

  gtk = {
    enable = true;
    cursorTheme = {
      name = "GoogleDot-Red";
      package = pkgs.google-cursor;
      size = 20;
    };
  };
  programs = {
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

    # -----------------------------------------------------------------
    # 1️⃣  Whole‑prompt format (left side)
    # -----------------------------------------------------------------
    # The triple‑quoted string ('''…''') lets us keep the exact
    # formatting (including back‑slashes) that you had in the TOML.
    format = '''
[░▒▓](#a3aed2)\ [  ](bg:#a3aed2 fg:#090c0c)\ [](bg:#769ff0 fg:#a3aed2)\ $directory\ [](fg:#769ff0 bg:#394260)\ $git_branch\ $git_status\ [](fg:#394260 bg:#212736)\ $nodejs\ $rust\ $golang\ $php\ [](fg:#212736 bg:#1d2230)\ $time\ [ ](fg:#1d2230)\ 
\n$character'''
    ;

    # -----------------------------------------------------------------
    # 2️⃣  Module‑specific customisations
    # -----------------------------------------------------------------

    ## ── Directory ───────────────────────────────────────────────────────
    directory = {
      style = "fg:#e3e5e5 bg:#769ff0";
      format = "[ $path ]($style)";
      truncation_length = 3;
      truncation_symbol = "…/";
    };

    ## ── Directory substitutions (icons for special folders) ─────────────
    "directory.substitutions" = {
      Documents = "󰈙 ";
      Downloads = " ";
      Music    = " ";
      Pictures = " ";
    };

    ## ── Git branch ───────────────────────────────────────────────────────
    git_branch = {
      symbol = "";
      style  = "bg:#394260";
      format = ''[[ $symbol $branch ](fg:#769ff0 bg:#394260)]($style)'';
    };

    ## ── Git status ───────────────────────────────────────────────────────
    git_status = {
      style  = "bg:#394260";
      format = ''[[( $all_status$ahead_behind )](fg:#769ff0 bg:#394260)]($style)'';
    };

    ## ── Node.js ────────────────────────────────────────────────────────
    nodejs = {
      symbol = "";
      style  = "bg:#212736";
      format = ''[[ $symbol ($version) ](fg:#769ff0 bg:#212736)]($style)'';
    };

    ## ── Rust ───────────────────────────────────────────────────────────
    rust = {
      symbol = "";
      style  = "bg:#212736";
      format = ''[[ $symbol ($version) ](fg:#769ff0 bg:#212736)]($style)'';
    };

    ## ── Go (Golang) ─────────────────────────────────────────────────────
    golang = {
      symbol = "";
      style  = "bg:#212736";
      format = ''[[ $symbol ($version) ](fg:#769ff0 bg:#212736)]($style)'';
    };

    ## ── PHP ────────────────────────────────────────────────────────────
    php = {
      symbol = "";
      style  = "bg:#212736";
      format = ''[[ $symbol ($version) ](fg:#769ff0 bg:#212736)]($style)'';
    };

    ## ── Time ───────────────────────────────────────────────────────────
    time = {
      disabled = false;
      time_format = "%R";               # Hour:Minute (24‑h)
      style = "bg:#1d2230";
      format = ''[[  $time ](fg:#a0a9cb bg:#1d2230)]($style)'';
    };

    # -----------------------------------------------------------------
    # 3️⃣  Prompt character (same as in your original config)
    # -----------------------------------------------------------------
    character = {
      success_symbol = "[❯](bold green)";
      error_symbol   = "[✖](bold red)";
    };

    # -----------------------------------------------------------------
    # 4️⃣  Miscellaneous tweaks you already had
    # -----------------------------------------------------------------
    scan_timeout = 10;
  };
}
       };
      };
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
}
