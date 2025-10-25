{pkgs, ...}:

{
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
      xmonad = {
        enable = true;
        enableContribAndExtras = true;
        extraPackages = haskellPackages : [
          haskellPackages.xmonad-contrib
          haskellPackages.xmonad-extras
          haskellPackages.xmonad
        ];
      };
    };
  };
}
