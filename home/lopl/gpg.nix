{pkgs, ...}: {
  programs.gpg.enable = true;

  services.gpg-agent = {
    enable = true;
    pinentry.package = pkgs.pinentry-emacs;
    enableSshSupport = true;
    defaultCacheTtl = 3600;
    extraConfig = ''
      allow-loopback-pinentry
    '';
  };
}
