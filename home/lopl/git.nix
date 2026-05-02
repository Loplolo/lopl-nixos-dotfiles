{pkgs, ...}: {
  programs.git = {
    enable = true;
    settings = {
      user.name = "Loplolo";
      user.email = "accounts@lopl.dev";
    };

    settings = {
      init.defaultBranch = "main";
      pull.rebase = false;
    };
  };
}
