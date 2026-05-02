{
  config,
  pkgs,
  lib,
  ...
}: {
  virtualisation.oci-containers = {
    backend = "podman";
    containers."tg-captcha-bot" = {
      image = "mxssl/tg-captcha-bot:latest";
      environmentFiles = [
        config.sops.templates."captcha-bot-env".path
      ];
      environment = {
      };

      autoStart = true;
      volumes = [
        "${config.sops.templates."captcha-bot-config".path}:/config.toml:ro"
      ];
    };
  };

  sops.templates."captcha-bot-env" = {
    content = ''
      TGTOKEN=${config.sops.placeholder.tg-captcha-bot-token}
    '';
  };

  sops.templates."captcha-bot-config" = {
    content = ''
      button_text = "Non sono un robot!"
      welcome_message = "Ciao! Per favore premi il pulsante entro 30 secondi."
      after_success_message = "L'utente ha superato la validazione."
      after_fail_message = "L'utente non ha superato la validazione ed è stato bannato."
      success_message_strategy = "del"
      fail_message_strategy = "del"
      welcome_timeout = "30"
      ban_duration = "forever"
      delete_join_message_on_fail = "yes"
      use_socks5_proxy = "no"
    '';
  };
}
