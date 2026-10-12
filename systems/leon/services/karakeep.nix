{
  services.karakeep = {
    enable = true;
    extraEnvironment = {
      PORT = "3010";
      NEXTAUTH_URL = "https://keep.lopl.dev";
      DISABLE_SIGNUPS = "true";
      DISABLE_NEW_RELEASE_CHECK = "true";
    };
  };

  lopl.proxies.keep = 3010;
}
