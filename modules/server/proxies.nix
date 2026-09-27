{lib, ...}: {
  options.lopl.proxies = lib.mkOption {
    type = lib.types.attrsOf lib.types.port;
    default = {};
  };
}
