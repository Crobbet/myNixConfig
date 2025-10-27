{ config, pkgs, inputs, lib, ... }:

{
  imports =
    [
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
      "incus-admin"
      "networkmanager"
      "wheel"
    ];

    packages = with pkgs; [
    ];
  };

  nixpkgs.config.allowUnfree = true;

  system.stateVersion = "25.11";

  home-manager = {
    extraSpecialArgs = { inherit inputs; };
    users = {
      "bt" = import ./home.nix;
    };
  };
  
  nix.settings.experimental-features = [ "nix-command" "flakes" ];

  specialisation.zen.configuration = {
    boot.kernelPackages = lib.mkForce pkgs.linuxPackages_zen;
  };


  # Virtualisation

  networking.nftables.enable = true;
  networking.firewall.trustedInterfaces = [ "incusbr0" ];

  users.groups.libvirtd.members = ["bt"];
  virtualisation.libvirtd.enable = true;

      virtualisation.containers.enable = true;
      virtualisation = {
        podman = {
          enable = true;
          dockerCompat = true;
          defaultNetwork.settings.dns_enabled = true;
            };

         incus.enable = true;
         };

  # Programs
  
  programs = {
    virt-manager.enable = true;
    firefox.enable = true;
    appimage.enable = true;
    appimage.binfmt = true;
    steam.enable = true;
    nvf = { 
    enable = true;
    settings = {
        vim.viAlias = false;
        vim.vimAlias = true;
        vim.lsp = {
            enable = true;
            nix.enable = true;
         }; 
      };
    };
  };

  # Services
    services.xserver.enable = true;
    services.xserver.desktopManager.xterm.enable = false;
    services.displayManager.sddm.enable = true;
    services.xserver.desktopManager.mate.enable = true;
    services.xserver.windowManager.i3.enable = true;
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

    boot.initrd.services.udev.rules =''
      ACTION=="add", SUBSYSTEM=="usb", KERNEL=="1-4", ATTR{authorized}="0"
    '';
}
