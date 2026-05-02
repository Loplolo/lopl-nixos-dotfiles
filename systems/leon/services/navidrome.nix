{
  config,
  pkgs,
  ...
}: {
  services.navidrome = {
    enable = true;
    settings = {
      MusicFolder = "/mnt/media/music";
      Address = "127.0.0.1";
      Port = 4533;
      BaseUrl = "https://music.lopl.dev";
    };
  };
}
