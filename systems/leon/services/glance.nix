{
  config,
  pkgs,
  lib,
  ...
}: {
  systemd.services.glance.serviceConfig.EnvironmentFile = [
    config.sops.secrets.steam-api-key.path
    config.sops.secrets.tailscale-api-key.path
  ];

  services.glance = {
    enable = true;
    settings = {
      server = {
        port = 8280;
        host = "127.0.0.1";
      };

      pages = [
        {
          name = "Home";
          columns = [
            {
              size = "small";
              widgets = [
                {
                  type = "clock";
                  hour-format = "24h";
                }
                {type = "calendar";}
                {
                  type = "weather";
                  title = "Weather";
                  units = "metric";
                  hour-format = "24h";
                  location = "Novara, Italy";
                }
              ];
            }
            {
              size = "full";
              widgets = [
                {
                  type = "search";
                  autofocus = true;
                  search-engine = "https://search.lopl.dev/search?q={QUERY}";
                  placeholder = "Search with bangs !w !ia !aa !yt !t !gh !nix ...";
                  bangs = [
                    {
                      title = "Wikipedia";
                      shortcut = "!w";
                      url = "https://en.wikipedia.org/w/index.php?search={QUERY}";
                    }
                    {
                      title = "Internet Archive";
                      shortcut = "!ia";
                      url = "https://archive.org/search.php?query={QUERY}";
                    }
                    {
                      title = "Anna's Archive";
                      shortcut = "!aa";
                      url = "https://annas-archive.gl/search?q={QUERY}";
                    }
                    {
                      title = "1337x";
                      shortcut = "!t";
                      url = "https://1337x.to/search/{QUERY}";
                    }
                    {
                      title = "YouTube";
                      shortcut = "!yt";
                      url = "https://www.youtube.com/results?search_query={QUERY}";
                    }
                    {
                      title = "GitHub";
                      shortcut = "!gh";
                      url = "https://github.com/search?q={QUERY}";
                    }
                    {
                      title = "NixOS Options";
                      shortcut = "!nix";
                      url = "https://search.nixos.org/options?query={QUERY}";
                    }
                  ];
                }
                {
                  type = "group";
                  widgets = [
                    {
                      type = "rss";
                      title = "News";
                      style = "detailed-list";
                      collapse-after = 6;
                      feeds = [
                        {
                          title = "ANSA";
                          url = "https://www.ansa.it/sito/notizie/topnews/topnews_rss.xml";
                        }
                        {
                          title = "Il Sole 24 Ore";
                          url = "https://www.ilsole24ore.com/rss/italia.xml";
                        }
                        {
                          title = "La Repubblica";
                          url = "https://www.repubblica.it/rss/homepage/rss2.0.xml";
                        }
                      ];
                    }
                    {
                      type = "rss";
                      title = "Tech";
                      style = "detailed-list";
                      collapse-after = 6;
                      feeds = [
                        {
                          title = "NixOS News";
                          url = "https://nixos.org/blog/announcements-rss.xml";
                        }
                        {
                          title = "Quanta Magazine";
                          url = "https://api.quantamagazine.org/feed/";
                        }
                        {
                          title = "Hacker News";
                          url = "https://hnrss.org/frontpage";
                        }
                      ];
                    }
                    {
                      type = "rss";
                      title = "Humanitarian";
                      style = "detailed-list";
                      collapse-after = 6;
                      feeds = [
                        {
                          title = "Anti-Auth";
                          url = "https://antiauth.space/feed";
                        }
                        {
                          title = "HRW Reports";
                          url = "https://www.hrw.org/rss/news";
                        }
                        {
                          title = "ICRC News";
                          url = "https://www.icrc.org/en/rss.xml";
                        }
                      ];
                    }
                  ];
                }
              ];
            }
            {
              size = "small";
              widgets = [
                {
                  type = "bookmarks";
                  groups = [
                    {
                      title = "Internal Services";
                      links = [
                        {
                          title = "Home Assistant";
                          url = "https://ha.lopl.dev";
                          icon = "si:homeassistant";
                        }
                        {
                          title = "Nextcloud";
                          url = "https://cloud.lopl.dev";
                          icon = "si:nextcloud";
                        }
                        {
                          title = "Immich";
                          url = "https://immich.lopl.dev";
                          icon = "si:immich";
                        }
                        {
                          title = "Navidrome";
                          url = "https://music.lopl.dev";
                          icon = "ph:music-notes-fill";
                        }
                        {
                          title = "Syncthing";
                          url = "https://syncthing.lopl.dev";
                          icon = "si:syncthing";
                        }
                        {
                          title = "Stirling PDF";
                          url = "https://pdf.lopl.dev";
                          icon = "si:adobeacrobatreader";
                        }
                        {
                          title = "MicroBin";
                          url = "https://bin.lopl.dev";
                          icon = "si:microbin";
                        }
                        {
                          title = "AdGuard Admin";
                          url = "https://adguard.lopl.dev";
                          icon = "si:adguard";
                        }
                        {
                          title = "Forgejo";
                          url = "https://forgejo.lopl.dev";
                          icon = "si:forgejo";
                        }
                      ];
                    }
                  ];
                }
                {
                  type = "bookmarks";
                  groups = [
                    {
                      title = "University";
                      links = [
                        {
                          title = "Outlook";
                          url = "https://outlook.cloud.microsoft/mail/";
                          icon = "si:microsoftoutlook";
                        }
                        {
                          title = "unimia";
                          url = "https://unimia.unimi.it/";
                          icon = "ph:graduation-cap-fill";
                        }
                        {
                          title = "MyAriel";
                          url = "https://ariel.unimi.it/";
                          icon = "ph:graduation-cap-fill";
                        }
                      ];
                    }
                  ];
                }
                {
                  type = "custom-api";
                  title = "xkcd";
                  cache = "2m";
                  url = "https://xkcd.com/info.0.json";
                  template = ''
                    <body> {{ .JSON.String "title" }}</body>
                    <img src="{{ .JSON.String "img" }}"></img>
                  '';
                }
              ];
            }
          ];
        }
        {
          name = "Gaming";
          columns = [
            {
              size = "small";
              widgets = [
                {
                  type = "custom-api";
                  title = "Steam User";
                  url = "https://api.steampowered.com/ISteamUser/GetPlayerSummaries/v0002/?key=\${STEAM_API_KEY}&steamids=76561198143610528";
                  cache = "5m";
                  template = ''
                    <div class="steam-user-stats" style="display: flex; align-items: flex-start;">
                      {{ $players := .JSON.Array "response.players" }}
                      {{ if gt (len $players) 0 }}
                        {{ $player := index $players 0 }}
                        <div style="flex-shrink: 0; margin-right: 1em; display: flex; flex-direction: column; align-items: center;">
                          <a href="{{ $player.String "profileurl" }}"><img src="{{ $player.String "avatarmedium" }}" alt="Avatar" style="border-radius: var(--border-radius);"></a>
                        </div>
                        <div>
                          <h2 class="size-h3 color-primary">{{ $player.String "personaname" }}</h2>
                          <p>Status:
                            {{ $status := $player.Int "personastate" }}
                            {{ if eq $status 0 }}<span class="color-negative">Offline</span>{{ end }}
                            {{ if eq $status 1 }}<span class="color-positive">Online</span>{{ end }}
                            {{ if eq $status 2 }}<span class="color-highlight">Busy</span>{{ end }}
                            {{ if eq $status 3 }}<span class="color-subdue">Away</span>{{ end }}
                            {{ if eq $status 4 }}<span class="color-subdue">Snooze</span>{{ end }}
                            {{ if eq $status 5 }}<span class="color-primary">Looking to trade</span>{{ end }}
                            {{ if eq $status 6 }}<span class="color-primary">Looking to play</span>{{ end }}
                          </p>
                          <p>Last Logoff: <span class="color-subdue" data-dynamic-relative-time="{{ $player.Int "lastlogoff" }}"></span></p>
                          <p>Account Created: <span class="color-subdue" data-dynamic-relative-time="{{ $player.Int "timecreated" }}"></span></p>
                          <p>Country: <span class="color-subdue">{{ $player.String "loccountrycode" }}</span></p>
                        </div>
                      {{ else }}
                        <p class="color-negative">No player data found.</p>
                      {{ end }}
                    </div>
                  '';
                }
                {
                  type = "custom-api";
                  title = "Recently Played Games";
                  url = "http://api.steampowered.com/IPlayerService/GetRecentlyPlayedGames/v0001/?key=\${STEAM_API_KEY}&steamid=76561198143610528&format=json";
                  cache = "5m";
                  template = ''
                    <div>
                      {{ if eq .Response.StatusCode 200 }}
                        {{ $games := .JSON.Array "response.games" }}
                        {{ if gt (len $games) 0 }}
                          <ul class="list list-gap-10 collapsible-container" data-collapse-after="5">
                            {{ range $games }}
                              <li style="display: flex; align-items: center;">
                                <a href="https://store.steampowered.com/app/{{ .Int "appid" }}" target="_blank">
                                  <img src="http://media.steampowered.com/steamcommunity/public/images/apps/{{ .Int "appid" }}/{{ .String "img_icon_url" }}.jpg" alt="{{ .String "name" }}" style="width: 32px; border-radius: var(--border-radius); margin-right: 10px;">
                                </a>
                                <div>
                                  <a href="https://store.steampowered.com/app/{{ .Int "appid" }}" class="size-base color-primary" target="_blank">{{ .String "name" }}</a>
                                  <br>
                                  <span class="size-h5 color-subdue">
                                    Last 2 weeks:
                                    {{ if gt (toFloat (.Int "playtime_2weeks")) 60.0 }}
                                      {{ printf "%.2f hrs" (div (toFloat (.Int "playtime_2weeks")) 60.0) }}
                                    {{ else }}
                                      {{ .Int "playtime_2weeks" }} mins
                                    {{ end }}
                                  </span>
                                </div>
                              </li>
                            {{ end }}
                          </ul>
                        {{ else }}
                          <p class="color-negative">No recently played games found.</p>
                        {{ end }}
                      {{ else }}
                        <p class="color-negative">An unexpected error occurred. Please try again later.</p>
                      {{ end }}
                    </div>
                  '';
                }
                {
                  type = "bookmarks";
                  groups = [
                    {
                      title = "Steam";
                      links = [
                        {
                          title = "SteamDB Sales";
                          url = "https://steamdb.info/sales/";
                          icon = "si:steamdb";
                        }
                        {
                          title = "ProtonDB";
                          url = "https://www.protondb.com/";
                          icon = "si:protondb";
                        }
                      ];
                    }
                    {
                      title = "Quake & idTech";
                      links = [
                        {
                          title = "Quaddicted";
                          url = "https://www.quaddicted.com/";
                          icon = "ph:crosshair-fill";
                        }
                        {
                          title = "Func_Msgboard";
                          url = "http://www.celephais.net/board/";
                          icon = "ph:chat-text-fill";
                        }
                        {
                          title = "idTech Forge";
                          url = "https://code.idtech.space/";
                          icon = "ph:code-block-fill";
                        }
                        {
                          title = "Slipgate Sightseer";
                          url = "https://www.slipseer.com/";
                          icon = "ph:map-pin-fill";
                        }
                        {
                          title = "Quake Wiki";
                          url = "https://quakewiki.org/";
                          icon = "ph:book-open-fill";
                        }
                        {
                          title = "ModDB Quake";
                          url = "https://www.moddb.com/games/quake";
                          icon = "ph:package-fill";
                        }
                      ];
                    }
                    {
                      title = "PSP";
                      links = [
                        {
                          title = "PPSSPP";
                          url = "https://www.ppsspp.org/";
                          icon = "ph:si:playstationportable";
                        }
                        {
                          title = "PSP ISO";
                          url = "https://archive.org/search?query=subject%3A%22PSP%22";
                          icon = "si:playstationportable";
                        }
                        {
                          title = "GBAtemp PSP";
                          url = "https://gbatemp.net/forums/sony-psp-vita.165/";
                          icon = "ph:chats-fill";
                        }
                        {
                          title = "Time Extension PSP";
                          url = "https://www.timeextension.com/games/psp";
                          icon = "si:playstationportable";
                        }
                        {
                          title = "Wololo PSP";
                          url = "https://wololo.net/category/psp/";
                          icon = "si:playstationportable";
                        }
                      ];
                    }
                  ];
                }
              ];
            }
            {
              size = "full";
              widgets = [
                {
                  type = "videos";
                  title = "YouTube Subscriptions";
                  style = "grid-cards";
                  channels = [
                    "UCiNLr9wX35KksK77mrQgxiw"
                    "UCYO_jab_esuFRV4b17AJtAw"
                    "UCmu9PVIZBk-ZCi-Sk2F2utA"
                    "UCXfjk8C1Hvkax10rrmmjM2Q"
                    "UCx3b18RQVMGx-1R7jM2ModA"
                    "UCqzFQr1LCC8xrr0fMcRctYg"
                    "UCiDJtJKMICpb9B1qf7qjEOA"
                    "UCVS89U86PwqzNkK2qYNbk5A"
                    "UCkkoWSV0fCPmoXWUKJW83Hg"
                    "UCivA7_KLKWo43tFcCkFvydw"
                    "UC696NqzEPDhmGju3Fc5hHaQ"
                    "UCDLkzWN1rHY4eYkGnVruHVw"
                    "UCSl5Uxu2LyaoAoMMGp6oTJA"
                    "UCHdP7MMS6HdXePwYycHLjYw"
                    "UCdCDj4v4eWN5kvVCeJ1KgXA"
                    "UClu2e7S8atp6tG2galK9hgg"
                    "UCY0DAi7uJia-9BUUQ65wHzQ"
                    "UCJPHc5wprYeNF0K5ITwPBgA"
                    "UC3tFZR3eL1bDY8CqZDOQh-w"
                    "UCq6aw03lNILzV96UvEAASfQ"
                    "UCeNyKC-_M2EKYqxa22_KY6w"
                    "UCMmusV3FZmMN6ajUUQ4h4GQ"
                    "UCvQ4C0f9_OWRf1uyobwqOwA"
                    "UCStF2E9_qNtoJnxi92XZysQ"
                    "UCex2B-k-ZIJhcjRdlYUz4MQ"
                    "UCCkRC1CsETPXMVWR82bvDcA"
                    "UChn336v-fUYkJH09vA4ZuCA"
                    "UCVF8wEEVylFI38nogvdJl5g"
                    "UCSqp5S44hj8Ryw7r0pYLbIg"
                    "UC_fUVciZMu7_EvdVjFLZeXw"
                    "UC2C_jShtL725hvbm1arSV9w"
                    "UC9mLpr85Lt8oXU7_UoawaLA"
                    "UC5ttdyjmIWMQyS2NoedlbWQ"
                    "UCbmJJpXjESJv5G98DrnQyqQ"
                    "UC21uZkfXpT8rPY-gPgMiCwA"
                    "UCTyMu1ZgRlfIP_gDbM7W5eQ"
                    "UCrsLPlwxUInDoNubOP85EWQ"
                    "UCAr4xiCUQaH00Pt3TW9zg9Q"
                    "UC9PEf34hGelTBsjf4gjR27w"
                    "UC9-y-6csu5WGm29I7JiwpnA"
                    "UCS4FAVeYW_IaZqAbqhlvxlA"
                    "UCwwK5Klzlt-9s9zzMTe_63w"
                    "UCA1s35ZIAQNYTUTCNvFDqbA"
                    "UC9lJXqw4QZw-HWaZH6sN-xw"
                    "UCsrxrOtjmWnH5FJhJnkTRBg"
                    "UC39k-B64wkfmI-npqqoReiA"
                    "UCdC0An4ZPNr_YiFiYoVbwaw"
                    "UCck4YyykoNPTUmXntpQrZrg"
                    "UC47LbJhlJUAXT__zc_htuZQ"
                    "UCQpkMe-SLNg0HwWCP3eeTxw"
                    "UCCoAJ5JYKYTMubpTIsWi70w"
                    "UCaqXyB5XqFfBaQJACRGqJng"
                    "UCdpWKLNfbROyoGPV46-zaUQ"
                    "UC7ZLM59reVlFScXB_adpP3g"
                    "UC4nJnJ-HO5vVbGlJ14rf5yg"
                    "UCcySihzoq5uj2ChYa9qfjnQ"
                    "UC3zcG8WfuzxjMjw4ggMirNw"
                    "UC4O9HKe9Jt5yAhKuNv3LXpQ"
                    "UCROkFwHWMk5QeZK_LHJPu5g"
                    "UCI5qWAMf5PHLNcM13R8pfiQ"
                    "UCubcFvyp8sAiHaSFZz_7s4Q"
                    "UCu7lohtMPJWRSGgFxEpNBHg"
                    "UCBvbNd2SVJEvtb3-_WEET9Q"
                    "UCJfAntp9CIFvjmlXeDK7Oxg"
                    "UCF502yOYr_olPaw6xgnYmaQ"
                    "UC0dPtXzZ_PqA7FglrtCBBjA"
                    "UCUTGVkFPQjgaXfYvbT_n9BA"
                    "UCJ0-OtVpF0wOKEqT2Z1HEtA"
                    "UC3_rz0ss9O7Yy0ypBG4M6lg"
                    "UCITYW3eAeHzfiFfBx2JXOsw"
                    "UCmgKPcqFtsGdoMffS4qLJmg"
                    "UCZO_hYEMUyxvaPOYIUGmc-w"
                    "UCjvyz4uEfbeAJNZCWhJHPlA"
                    "UC738SsV6BSLUVvMgKnEFFzQ"
                    "UCFKDEp9si4RmHFWJW1vYsMA"
                    "UCkndrGoNpUDV-uia6a9jwVg"
                    "UCVovvq34gd0ps5cVYNZrc7A"
                    "UCWXCrItCF6ZgXrdozUS-Idw"
                    "UCIyZiiHXIH7KkqfaDvBmG-Q"
                    "UCQYxX79_CYn2qe3U8R8TBZQ"
                    "UClj6GpwUTb8mtqacKkvkDig"
                    "UCMX3NJp9ucDYmpyYNMv3hZw"
                    "UC2xxSSB9thJYPDHY68UQTnw"
                    "UCGhs9S33RAeT5DEuKTO4Oew"
                    "UCTCpOFIu6dHgOjNJ0rTymkQ"
                    "UC_k30fo2S2RSc-At1n77P9w"
                    "UCqJ-Xo29CKyLTjn6z2XwYAw"
                    "UCR_zEwdjJfR4Hd92oIlUVVg"
                    "UCUBsjvdHcwZd3ztdY1Zadcw"
                    "UCyXsGEVtsqMzpCl9KmctYnw"
                    "UCtMd_E7GgZcooEVuvjQ9ieg"
                    "UCjWsE9cjRu5gImk99yFhwVg"
                    "UCRtyLX-ej-H1PSiaw8g9aIA"
                    "UCodkNmk9oWRTIYZdr_HuSlg"
                    "UCN-ghdBp-TTYgZfWnDeG__w"
                    "UCMAKW4cqyo-7Xm_zHewDtTQ"
                    "UCR9Gcq0CMm6YgTzsDxAxjOQ"
                    "UCqoivAc6Td_AOBxqHWy0Xow"
                    "UCAxBpbVbSXT2wsCwZfrIIVg"
                    "UC-FHoOa_jNSZy3IFctMEq2w"
                    "UCnv0gfLQFNGPJ5MHSGuIAkw"
                    "UCuCkxoKLYO_EQ2GeFtbM_bw"
                    "UCgdTVe88YVSrOZ9qKumhULQ"
                    "UCQBOAYwPdqzr1TxmjT-91Sg"
                    "UC32w6uX5qtmUtF4QQQ2PKaQ"
                    "UCWQsYRaVx7vycS8co-rLZsQ"
                    "UCJLZe_NoiG0hT7QCX_9vmqw"
                    "UC6x7GwJxuoABSosgVXDYtTw"
                    "UCBolyOhViWxyc1vdHaboZMQ"
                    "UCv1xxFkEiAdCVy6foEEUIvw"
                    "UCpDmn2FfVYdPIDwRTcf5-OA"
                    "UCrEtZMErQXaSYy_JDGoU5Qw"
                    "UCpF62eV02XJpqJxBZAcsDCw"
                    "UCwuHUotb8UHg4QixeF9560w"
                    "UCVX2I3KruzTnDTsKXGwbGkw"
                    "UCuMJPFqazQI4SofSFEd-5zA"
                    "UCoIo9DwqKu83bw6LGaO8pMA"
                    "UCSMex9kCmlfvbxuLFeWqODA"
                    "UCe6ABcJkH_Gso9HJOt4x9fg"
                    "UCGwu0nbY2wSkW8N-cghnLpA"
                    "UCMb0O2CdPBNi-QqPk5T3gsQ"
                    "UCPfjB94mhe8Gsnd9qIzUKHA"
                    "UCshVweYiHqrepiq6b8tWvLQ"
                    "UCOfUgZerixN10d0xje6sg0Q"
                    "UC4QobU6STFB0P71PMvOGN5A"
                    "UCtWObtiLCNI_BTBHwEOZNqg"
                    "UCtN2A8bFL_NoxClFyklbMRQ"
                    "UCRnrgGApQSu8h1ZnTkLYCFQ"
                    "UCSkzHxIcfoEr69MWBdo0ppg"
                    "UCdJdEguB1F1CiYe7OEi3SBg"
                    "UCuJho2C1WQSv1AS_WJ0jKkw"
                    "UCwCk5zX6fbTrfxQQJFwKp2A"
                    "UC2buJRIGgvkpMpaaCyaIqGA"
                    "UCcJgOennb0II4a_qi9OMkRA"
                    "UCsjjQ7nvc2wYxOwm1URnMUw"
                    "UC0a8nteER_pU4Aj6hmEyJAQ"
                    "UCdifikQ1vH3D2sVp8oRmg4w"
                    "UCcfHrPCey2M3L2QDWn_belA"
                    "UCxghgktsTOR_VbxNjcRm3Rg"
                    "UCHv5KTkCnk6eOfRCIzuC77w"
                    "UCfrcSLWe-EOvPzgH0zcA6Nw"
                    "UCS5tt2z_DFvG7-39J3aE-bQ"
                    "UCsfb8-1iE_rRO29_ZFe4rlQ"
                    "UCXuqSBlHAE6Xw-yeJA0Tunw"
                    "UCfADWv9JCX4bvId7PtaSFkw"
                    "UCm9K6rby98W8JigLoZOh6FQ"
                    "UCXmHLj2vO0XjsdcZSgBic3A"
                    "UCRELfrtyz_teOEQXjF04BIA"
                    "UC3fqFQTePdN8VnTRVByBLqA"
                    "UCbDcrWUilJhw-bbtC17hEQw"
                    "UCY1kMZp36IQSyNx_9h4mpCg"
                    "UCPAhQQChRHxhePO4uk2qEsA"
                    "UCW4BscaN7XuLrLsCnbrV9wA"
                    "UCyFjeoG9X83dVWHKqXCKwHg"
                    "UCDDkudYT4ZZjL9AKbpOnGFQ"
                    "UC5rM5xw7l2Y5n8YbdZx7-ow"
                    "UChrYe70o7NmDioL02PRVWVg"
                    "UCb31gOY6OD8ES0zP8M0GhAw"
                    "UCrJCzC7Jkq72tVF5bVu7-ew"
                    "UCYdXHOv7srjm-ZsNsTcwbBw"
                    "UC2wNnyb3vWhOt0K6LpBrtGg"
                    "UCEFymXY4eFCo_AchSpxwyrg"
                    "UCgzy7JqiE7qcpdnwu79dF_A"
                    "UConwz41J5aOPT0hY3gk4xVQ"
                    "UCAWQEAjn8udSFKN6D4NlqWQ"
                    "UChEi3KWmiI11nnH5ImDNisw"
                    "UCEBb1b_L6zDS3xTUrIALZOw"
                    "UCbzPhyLcO8VP25dZ7kaUyAw"
                    "UCjFaPUcJU1vwk193mnW_w1w"
                    "UCu7Zwf4X_OQ-TEnou0zdyRA"
                    "UCnDDucQDLncrauOCmanCIgw"
                    "UCHpGJH5NykJwTDx3yR7snwA"
                    "UCP2cNcC-2pqDRCFeFO4JLLw"
                    "UCmJRQK0-y7jNtq__l8cdYSQ"
                    "UC11OPzwn5Wt0-LN3rARunmg"
                    "UChFur_NwVSbUozOcF_F2kMg"
                    "UCSSV5AL__sSfDsXJC6hDcAQ"
                    "UCYM1s-hev6U6C432oVZwSfw"
                    "UCIF3c8VDluDybpfX2sVs9XQ"
                    "UCzmwnaMIJjJwDGB_cct6qrw"
                    "UC_GmeicoGToX7sy_zjOuTFg"
                    "UCZzOG5dwYZWCBzfQwgLrMnA"
                    "UCvrLvII5oxSWEMEkszrxXEA"
                    "UCSb_Sui6FBxVS4_ROsrU_Iw"
                    "UCiNS0ikfd1DT73nn8WPvGsQ"
                    "UCsoZWD7Uagg2yv3YcQ3zqBA"
                    "UCWBye16kz0yJRdCGKDvlbYg"
                    "UC1D3yD4wlPMico0dss264XA"
                    "UCFhXFikryT4aFcLkLw2LBLA"
                    "UCdievj3szPeYJzqqOjsLnfQ"
                    "UCvK38D7rwGcfk2EBBbF6aFg"
                    "UCUxV8b_z4xI8Qbo1tsOTABg"
                    "UCrJafvCz4M68OMcfoaSJuyQ"
                    "UCEArja5slNmUXjN2rO7Z3RA"
                    "UCcbca_MnFMLrBXGYiRh1aNQ"
                    "UChQlb1CdktgAt4k7U0TqisQ"
                    "UC1GJ5aeqpEWklMBQ3oXrPQQ"
                    "UC9UhWlkLfeypFt7pp7v_aBw"
                    "UCgmyZijl7MUwDu2WygA1Jdw"
                    "UCZBdaKw97n3P9qWeP6rs_7g"
                    "UC9XFvuObhfVUNAGNcH8Y_fw"
                    "UCsm5n2R0jIcOPTK1Mmsaekg"
                    "UC3HKaAtgfloc0TEzIgqdB0w"
                    "UCa76yP47g-py85TJ6TfRNrw"
                    "UC7pOiEeCNqePyNygNB8ls2w"
                    "UCWv7vMbMWH4-V0ZXdmDpPBA"
                    "UCV2y-v-8oAkeXCabpHqQSQg"
                    "UCOMk1WI8daCFFa3CYHlCmqQ"
                    "UCzr_sr5-QnRMVPSdDM6EiXg"
                    "UCoO9Ufjz-q_vKpd_ZLOmRCg"
                    "UCqjTN5ZTN69baab1pJgYW0w"
                    "UC5Fe2eFjG2AKqMsNZTumFlg"
                    "UCaquFi5JOPWPFNwy11oPx2w"
                    "UCEYJKE6dCR0K4c9gZa_fb3A"
                    "UCo1QvnZm1qs88sfQdUueMnw"
                    "UChw9vGHrtY17HKkNXOBCclA"
                    "UCK8XIGR5kRidIw2fWqwyHRA"
                    "UCDpdtiUfcdUCzokpRWORRqA"
                    "UCGJaDZC7PChgd-XMwcbZkiw"
                    "UCTlIv_U9A_hvXQU8lgYc_RA"
                    "UCnB6RaOBZw71i_IMe3D0D4w"
                    "UC7QE72cxiBkiwnvGoFfqYOg"
                    "UC1DTYW241WD64ah5BFWn4JA"
                    "UCRd9JHiQvqwT8O4d0QGI9jQ"
                    "UCsTOp3sFBkQH1qKhc8N_qOQ"
                    "UCPPjuJTt9jiOZNHScvEmvRg"
                    "UC8aG3LDTDwNR1UQhSn9uVrw"
                    "UCnW2hq-0-3Urmz12oK2z3mQ"
                    "UCn7_7g2H_D4R9blvlGb0AaQ"
                    "UCXh7_fyUPlTjBQi9Oy1MaHQ"
                    "UC5jPbUx9GfeQNlbARHKZ6-w"
                    "UC6gsPC4t87a-hIpoDXVaSGA"
                    "UChSrI9uTjJ27_b3d4FvuBQA"
                    "UCXoqL7S2DjbrJe5ewfmlDvw"
                    "UCQmzgjEb7X6--w7CSE1erJg"
                    "UCMkmAYBVLAC9jGIUD4LjacA"
                    "UC4qdHN4zHhd4VvNy3zNgXPA"
                    "UC6YCOVVl4oD0-2H4_z2Xbtw"
                    "UCDzVUXiTr3hClI-zzCWbYzg"
                    "UCTsNuW5bNxWF3qcali589PQ"
                    "UCam_6nCM4Yd-zmAOz6IYLNg"
                    "UCMvpSORaKKMxSYScghSftbQ"
                    "UCEIwxahdLz7bap-VDs9h35A"
                    "UCAqpjjMMSUCx8IfAcoe9k-A"
                    "UCAiiOTio8Yu69c3XnR7nQBQ"
                    "UC4YUKOBld2PoOLzk0YZ80lw"
                    "UCxCEaQ_uUv_okCK_AYg9NaA"
                    "UCz9mH-PsyV4ZuMGAQqeS8IQ"
                    "UCT5C7yaO3RVuOgwP8JVAujQ"
                    "UCC1yB03N8tAjQiUyed5iw9g"
                    "UCFAiFyGs6oDiF1Nf-rRJpZA"
                    "UC-FOJ8XsQHRhR2m1GzvKW3A"
                    "UCQn-2rpF54OkVfe--KBuT6A"
                    "UCvjgXvBlbQiydffZU7m1_aw"
                    "UCephVCPj97LWTeGagjlsPWQ"
                    "UCAIM55C7Ye-44BGt9sO4x7w"
                    "UC5UAwBUum7CPN5buc-_N1Fw"
                    "UCRHXUZ0BxbkU2MYZgsuFgkQ"
                    "UCO-zhhas4n_kAPHaeLe1qnQ"
                    "UCV5vCi3jPJdURZwAOO_FNfQ"
                    "UCIq2xNjGAof0cCUaKbco6HQ"
                    "UCKnREqBN0kCt3ryMvE6brPw"
                    "UCpFcHE36IoySjYj1Rytxyog"
                    "UCQ6X1sJpGUCXAzE0s7zJ8Ew"
                    "UCvbLX67F72gQBrqE4IX5yGA"
                    "UCiquJMumEKysG4mNP0NwERg"
                    "UCBa659QWEk1AI4Tg--mrJ2A"
                    "UCZrrEuHiQjN2CUo84g5tk7w"
                    "UCDiznUV6dtdB9PQj__XVhnA"
                    "UCYQssdJfeReFSaD2tKajHww"
                    "UCm0kZaFPwD48KzaWj5172Jg"
                    "UCB4NFn-8oipHct0IfAQBQrQ"
                    "UCXDy5N9QdoNqUNAGQ5Ro8Xg"
                    "UCg0FSqPeiGD_lIiPaaAehQg"
                    "UCMiyV_Ib77XLpzHPQH_q0qQ"
                    "UCsvn_Po0SmunchJYOWpOxMg"
                    "UCu17Sme-KE87ca9OTzP0p7g"
                    "UC6nSFpj9HTCZ5t-N3Rm3-HA"
                    "UCIqOntfIErRuC1u6UyFfy8g"
                    "UCKEJZ-dqIA03evnzEy1_owg"
                    "UCyEKvaxi8mt9FMc62MHcliw"
                    "UCL5W9kuKIQtXEVAxSadB2BQ"
                    "UCU9pX8hKcrx06XfOB-VQLdw"
                    "UC6IxnFzHofFJ5X2PycSMsww"
                    "UCNUOu9mkwYsKsfLyPJF8YbQ"
                    "UCIPsK5xspHC3-ZFNPTx2X_w"
                    "UCMxsVULY_qsk_aNdAqzcNUA"
                    "UCUW49KGPezggFi0PGyDvcvg"
                    "UCPK5G4jeoVEbUp5crKJl6CQ"
                    "UC1vOiUKVlVu3yivohsYk5qw"
                    "UCjI5qxhtyv3srhWr60HemRw"
                    "UCOmYd1qy5qOyAXvK4p0LYfw"
                    "UCvQY4t8zGcyikvjvKp3wiCg"
                  ];
                }
              ];
            }
            {
              size = "small";
              widgets = [
                {
                  type = "reddit";
                  title = "r/quake";
                  subreddit = "quake";
                  collapse-after = 5;
                }
                {
                  type = "reddit";
                  title = "Deadlock News";
                  subreddit = "DeadlockTheGame";
                  collapse-after = 5;
                }
              ];
            }
          ];
        }
        {
          name = "Status";
          columns = [
            {
              size = "small";
              widgets = [
                {
                  type = "bookmarks";
                  groups = [
                    {
                      title = "Management";
                      links = [
                        {
                          title = "AdGuard Admin";
                          url = "https://adguard.lopl.dev";
                          icon = "si:adguard";
                        }
                        {
                          title = "MxRoute Dashboard";
                          url = "https://management.mxroute.com/dashboard";
                          icon = "ph:envelope-simple-fill";
                        }
                        {
                          title = "MxRoute Mail";
                          url = "https://witcher.mxrouting.net/webmail/";
                          icon = "ph:envelope-simple-fill";
                        }
                        {
                          title = "Cloudflare Dashboard";
                          url = "https://dash.cloudflare.com/";
                          icon = "si:cloudflare";
                        }
                        {
                          title = "Forgejo";
                          url = "https://forgejo.lopl.dev";
                          icon = "si:forgejo";
                        }
                      ];
                    }
                    {
                      title = "Infra Docs";
                      links = [
                        {
                          title = "NixOS Options";
                          url = "https://search.nixos.org/options";
                          icon = "si:nixos";
                        }
                        {
                          title = "Home Manager";
                          url = "https://nix-community.github.io/home-manager/options.xhtml";
                          icon = "si:nixos";
                        }
                        {
                          title = "Glance Docs";
                          url = "https://github.com/glanceapp/glance/blob/main/docs/configuration.md";
                          icon = "ph:book-open-fill";
                        }
                      ];
                    }
                  ];
                }
              ];
            }
            {
              size = "full";
              widgets = [
                {
                  type = "server-stats";
                  title = "Resource Usage (Btop Style)";
                }
                {
                  type = "monitor";
                  title = "Service Uptime";
                  sites = [
                    {
                      title = "SearXNG";
                      url = "https://search.lopl.dev";
                    }
                    {
                      title = "Nextcloud";
                      url = "https://cloud.lopl.dev";
                    }
                    {
                      title = "Home Assistant";
                      url = "https://ha.lopl.dev";
                    }
                    {
                      title = "Matrix";
                      url = "https://matrix.lopl.dev";
                    }
                    {
                      title = "AdGuard";
                      url = "https://adguard.lopl.dev";
                    }
                    {
                      title = "Immich";
                      url = "https://immich.lopl.dev";
                    }
                    {
                      title = "Navidrome";
                      url = "https://music.lopl.dev";
                    }
                    {
                      title = "Syncthing";
                      url = "https://syncthing.lopl.dev";
                    }
                    {
                      title = "Stirling PDF";
                      url = "https://pdf.lopl.dev";
                      allow-insecure = false;
                    }
                    {
                      title = "MicroBin";
                      url = "https://bin.lopl.dev";
                    }
                    {
                      title = "Forgejo";
                      url = "https://forgejo.lopl.dev";
                    }
                  ];
                }
              ];
            }
            {
              size = "small";
              widgets = [
                {
                  type = "monitor";
                  title = "Network Check";
                  sites = [
                    {
                      title = "Router";
                      url = "http://192.168.1.1";
                    }
                    {
                      title = "External IP";
                      url = "https://icanhazip.com";
                    }
                    {
                      title = "DNS (1.1.1.1)";
                      url = "https://one.one.one.one";
                    }
                  ];
                }
              ];
            }
          ];
        }
      ];
    };
  };
}
