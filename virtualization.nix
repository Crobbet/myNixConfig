{
  programs.virt-manager.enable = true;

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
}
