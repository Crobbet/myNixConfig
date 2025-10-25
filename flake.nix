{
  description = "Nixos config flake";

  inputs = {

    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    stylix.url = "github:nix-community/stylix";

  };

  outputs = { self, nixpkgs, stylix, ... }@inputs:
  let
    myServices = {
      services.xserver.enable = true;
      services.xserver.desktopManager.xterm.enable = false;
      services.displayManager.sddm.enable = true;
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
    };

 #   myStylix =
 #     {pkgs, ...}:
 #     {
 #         stylix.enable = true;
 #         stylix.base16Scheme = "${pkgs.base16-schemes}/share/themes/gruvbox-dark-hard.yaml";
  #    };

  in
    {
     nixosConfigurations.Winter = nixpkgs.lib.nixosSystem {
      specialArgs = {inherit inputs;};
      modules = [
         myServices
#        myStylix

        ./system/configuration.nix
        ./system/hardware-configuration.nix   
        ./programs/programs.nix

        inputs.home-manager.nixosModules.default
        inputs.stylix.nixosModules.stylix

        ./virtualization.nix 
        ./xmonad.nix
      ];
    };
  };
}
