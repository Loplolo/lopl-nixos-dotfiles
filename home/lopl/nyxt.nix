{pkgs, ... }:

{
  home.packages = [ pkgs.nyxt ];

  xdg.configFile."nyxt/config.lisp".text = ''
    (in-package #:nyxt-user)

    ;; Keybindings Configuration
    (define-configuration buffer
      ((override-map (let ((map (make-keymap "override-map")))
                       (define-key map "C-s s" 'search-buffer)
                       (define-key map "C-s S" 'search-buffers)
                       (define-key map "C-x K" 'delete-all-buffers)
                       (define-key map "M-c" 'delete-current-buffer)
                       (define-key map "M-tab" 'buffers-panel)
                       (define-key map "C-L" 'set-url-new-buffer)
                       map))))

    ;; Search Engines Configuration
    (define-configuration context-buffer
      ((search-engines
        (list
         (make-instance 'search-engine
                        :shortcut "wiki"
                        :search-url "https://en.wikipedia.org/wiki/Special:Search?search=~a"
                        :fallback-url "https://en.wikipedia.org/")
         (make-instance 'search-engine
                        :shortcut "g"
                        :search-url "https://google.com/search?q=~a"
                        :fallback-url "https://google.com")
         (make-instance 'search-engine
                        :shortcut "ddg"
                        :search-url "https://duckduckgo.com/?q=~a"
                        :fallback-url "https://duckduckgo.com")
         ))))
  '';
}
