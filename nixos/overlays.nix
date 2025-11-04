self: super: {
  emacsPackages =
    super.emacsPackages
    // {
      auctex = super.emacsPackages.auctex.overrideAttrs (oldAttrs: {
        src = super.fetchurl {
          # Official CTAN mirror (always up‑to‑date)
          url = "https://mirrors.ctan.org/macros/latex/contrib/auctex/auctex-14.1.tar.gz";

          # Let Nix compute the hash for you the first time:
          #   $ nix-build -A emacsPackages.auctex
          #   → it will fail with “hash mismatch”, copy the printed hash here.
          # For now we use a placeholder; replace it after the first run.
          sha256 = "sha256-AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=";
        };
      });
    };
}
