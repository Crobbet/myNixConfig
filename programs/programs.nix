{pkgs, ...}: {
  environment.systemPackages = with pkgs; [
    xmonadctl

    nixd
 
    sxhkd

    krita
    weston
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

    emacs
    ((emacsPackagesFor emacs).emacsWithPackages (
      epkgs: [epkgs.embark-consult epkgs.evil]
    ))
    gnome-disk-utility
    genymotion

    yazi

    ghostty

    usbutils
    uhubctl
    coreutils-full

    clang
    lldb

    localsend

    sakura
    alacritty

    kiwix

    efibootmgr

    kdePackages.filelight

    torrential
    unzip
    zip
    appimage-run
    media-downloader

    notesnook
    obsidian
    logseq

    heroic

    ppsspp
    mgba
    pcsx2

    podman-compose
    distrobox
    boxbuddy

    wget
    git
    helix
    love
    lua
    lua-language-server

    nixd

    brave
  ];
}
