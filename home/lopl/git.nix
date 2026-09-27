{
  programs.git = {
    enable = true;
    settings = {
      user.name = "Loplolo";
      user.email = "accounts@lopl.dev";
      init.defaultBranch = "main";
      pull.rebase = false;
    };
  };
}
