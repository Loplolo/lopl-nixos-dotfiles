{pkgs, ...}: {
  xdg.enable = true;

  xdg.portal = {
    enable = true;
    extraPortals = [ pkgs.xdg-desktop-portal-gtk ];
    config.common.default = [ "gtk" ];
  };

  xdg.mimeApps = {
    enable = true;

    defaultApplications = {
	
      # Browse
      "inode/directory" = ["thunar.desktop"];

      # Xarchiver
      "application/zip" = ["xarchiver.desktop"];
      "application/x-tar" = ["xarchiver.desktop"];
      "application/x-compressed-tar" = ["xarchiver.desktop"];
      "application/x-gzip" = ["xarchiver.desktop"];
      "application/x-bzip2" = ["xarchiver.desktop"];
      "application/x-7z-compressed" = ["xarchiver.desktop"];
      "application/x-rar" = ["xarchiver.desktop"];

      # Images
      "image/bmp" = ["imv.desktop"];
      "image/gif" = ["imv.desktop"];
      "image/jpeg" = ["imv.desktop"];
      "image/jpg" = ["imv.desktop"];
      "image/png" = ["imv.desktop"];
      "image/svg+xml" = ["imv.desktop"];
      "image/tiff" = ["imv.desktop"];
      "image/webp" = ["imv.desktop"];

      # Video and Audio
      "video/mp4" = ["mpv.desktop"];
      "video/mkv" = ["mpv.desktop"];
      "video/webm" = ["mpv.desktop"];
      "video/x-matroska" = ["mpv.desktop"];
      "video/quicktime" = ["mpv.desktop"];
      "video/x-flv" = ["mpv.desktop"];
      "audio/mpeg" = ["mpv.desktop"];
      "audio/x-wav" = ["mpv.desktop"];
      "audio/x-flac" = ["mpv.desktop"];
      "audio/aac" = ["mpv.desktop"];

      # Word / Text
      "application/vnd.oasis.opendocument.text" = ["libreoffice-writer.desktop"];
      "application/msword" = ["libreoffice-writer.desktop"];
      "application/vnd.openxmlformats-officedocument.wordprocessingml.document" = ["libreoffice-writer.desktop"];

      # Excel / Spreadsheets
      "application/vnd.oasis.opendocument.spreadsheet" = ["libreoffice-calc.desktop"];
      "application/vnd.ms-excel" = ["libreoffice-calc.desktop"];
      "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet" = ["libreoffice-calc.desktop"];

      # Powerpoint / Presentations
      "application/vnd.oasis.opendocument.presentation" = ["libreoffice-impress.desktop"];
      "application/vnd.ms-powerpoint" = ["libreoffice-impress.desktop"];
      "application/vnd.openxmlformats-officedocument.presentationml.presentation" = ["libreoffice-impress.desktop"];

      # Text / Code
      "text/plain" = ["emacsclient.desktop"];
      "text/markdown" = ["emacsclient.desktop"];
      "text/x-csrc" = ["emacsclient.desktop"];
      "application/json" = ["emacsclient.desktop"];
      "application/x-shellscript" = ["emacsclient.desktop"];

      # Web
      "text/html" = ["firefox.desktop"];
      "x-scheme-handler/http" = ["firefox.desktop"];
      "x-scheme-handler/https" = ["firefox.desktop"];
      "x-scheme-handler/about" = ["firefox.desktop"];
      "x-scheme-handler/unknown" = ["firefox.desktop"]; 

      # PDF
      "application/pdf" = ["firefox.desktop"];
    };
  };
}
