{
  config,
  pkgs,
  inputs,
  lib,
  ...
}: {
  imports = [
    inputs.home-manager.nixosModules.default
  ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.kernelPackages = pkgs.linuxPackages_latest;

  security.polkit.enable = true;

  networking.hostName = "Winter";
  networking.networkmanager.enable = true;

  time.timeZone = "Asia/Dhaka";

  i18n.defaultLocale = "en_US.UTF-8";
  i18n.extraLocaleSettings = {
    LC_ADDRESS = "en_US.UTF-8";
    LC_IDENTIFICATION = "en_US.UTF-8";
    LC_MEASUREMENT = "en_US.UTF-8";
    LC_MONETARY = "en_US.UTF-8";
    LC_NAME = "en_US.UTF-8";
    LC_NUMERIC = "en_US.UTF-8";
    LC_PAPER = "en_US.UTF-8";
    LC_TELEPHONE = "en_US.UTF-8";
    LC_TIME = "en_US.UTF-8";
  };

  users.users.bt = {
    isNormalUser = true;
    description = "bt";
    extraGroups = [
      "networkmanager"
      "wheel"
    ];

    packages = with pkgs; [
    ];
  };

  nixpkgs.config.allowUnfree = true;

  system.stateVersion = "25.11";

  home-manager = {
    extraSpecialArgs = {inherit inputs;};
    users = {
      "bt" = import ./home.nix;
    };
  };

  nix.settings.experimental-features = ["nix-command" "flakes"];

  specialisation.zen.configuration = {
    boot.kernelPackages = lib.mkForce pkgs.linuxPackages_zen;
  };

  # Virtualisation

  users.groups.libvirtd.members = ["bt"];
  virtualisation.libvirtd.enable = true;

  virtualisation.containers.enable = true;
  virtualisation = {
    podman = {
      enable = true;
      dockerCompat = true;
      defaultNetwork.settings.dns_enabled = true;
    };
  };

  # Programs

  programs = {
    virt-manager.enable = true;
    firefox.enable = true;
    appimage.enable = true;
    appimage.binfmt = true;
    steam.enable = true;
  };

  # Services
  services.xserver.enable = true;
  services.xserver.desktopManager.xterm.enable = false;
  services.displayManager.ly.enable = true;
  services.xserver.desktopManager.cinnamon.enable = true;
  services.guix.enable = true;

  services.xserver.xkb = {
    layout = "us";
    variant = "";
  };

  services.pulseaudio.enable = false;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
  };

  systemd.services.disableFaultyUsbPort = {
    description = "Disable faulty USB port at boot";
    wantedBy = ["multi-user.target"];
    after = ["sysinit.target" "local-fs.target"];

    serviceConfig = {
      Type = "oneshot";
      ExecStart = "${pkgs.bash}/bin/bash -c 'echo 1 > /sys/bus/usb/devices/1-0:1.0/usb1-port4/disable'";
    };
  };

  environment.systemPackages = with pkgs; [
    xfce.xfce4-panel
    xfce.xfce4-panel-profiles

    xfce.xfce4-appfinder
    xfce.xfce4-battery-plugin

    acpi
    light

    killall
    haskell-language-server
    xsettingsd

    picom-pijulius
    iosevka
    nerd-fonts.iosevka
  ];

  services.xserver = {
    desktopManager = {
      xfce = {
        enable = true;
        noDesktop = true;
        enableXfwm = false;
      };
    };
    windowManager = {
      i3.enable = true;
      xmonad = {
        enable = true;
        enableContribAndExtras = true;
        extraPackages = haskellPackages: [
          haskellPackages.xmonad-contrib
          haskellPackages.xmonad-extras
          haskellPackages.xmonad
        ];
      };
    };
  };

  stylix = {
    enable = true;
    base16Scheme = "${pkgs.base16-schemes}/share/themes/g.yaml";
  };
}
