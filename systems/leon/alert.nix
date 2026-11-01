{
  config,
  pkgs,
}:
pkgs.writeShellApplication {
  name = "security-alert";
  runtimeInputs = with pkgs; [curl util-linux coreutils];
  excludeShellChecks = ["SC1091"];
  text = ''
    if [ "$#" -gt 0 ]; then msg="$*"; else msg=$(cat); fi
    printf '%s\n' "$msg" | logger -t security-alert

    . ${config.sops.templates."security-alert-env".path}
    printf 'url = "https://api.telegram.org/bot%s/sendMessage"\n' "$TG_TOKEN" |
      curl -fsS --max-time 20 -K - \
        --data-urlencode "chat_id=$TG_CHAT" \
        --data-urlencode "text=$(printf '%s' "$msg" | head -c 4000)" \
        -o /dev/null ||
      logger -t security-alert "Telegram delivery failed"
  '';
}
