{ config, lib, pkgs, inputs, ... }: {
  home.packages = with pkgs; [ nyxt ];
  xdg =
    let
      fonts = config.stylix.fonts;
      font = "${fonts.sansSerif.name}";
      palette = pkgs.custom.catppuccin-palette.${config.catppuccin.flavour};
      plugins = [
        "nx-fruit"
        "nx-search-engines"
        "nx-router"
        "demeter"
      ];
    in
    {
      # configFile."nyxt".source = ./.config/nyxt;
      configFile."nyxt/config.lisp".text = # scheme (not really but common-lisp does not work)
        ''
          ;;; misc settings
          (define-configuration browser
             ;; never restore session
            ((restore-session-on-startup-p nil)
             ;; set external editor
             (external-editor-program '("handlr" "open"))))

          ;;; enable default modes
          (define-configuration buffer
            ((default-modes (append '(nyxt/mode/blocker:blocker-mode
                                      nyxt/mode/reduce-tracking:reduce-tracking-mode
                                      nyxt/mode/password:password-mode
                                      nyxt/mode/vi:vi-normal-mode)
                                    %slot-value%))))

          ;;; password manager config
          (defmethod initialize-instance :after
                     ((interface password:keepassxc-interface)
                      &key &allow-other-keys)
            (setf (password:password-file interface) "/home/anomalocaris/Sync/Keepass Databases/Personal.kdbx"
                  (password:yubikey-slot interface) "2"))

          (define-configuration nyxt/mode/password:password-mode
            ((nyxt/mode/password:password-interface
              (make-instance 'password:keepassxc-interface))))

          ;;; enable glyphs in status buffer
          (define-configuration status-buffer
            ((glyph-mode-presentation-p t)))

          ;;; TODO: when extracting into port make sure to use whisker templating
          ;;; to replace +/- colors that are just duplicates of the main color
          ;;; and also account for how +/- are currently calibrated for dark theme
          (define-configuration browser
            ((theme (make-instance 'theme:theme
                                   :background-color "#${palette.base.hex}"
                                   :background-color+ "#${palette.mantle.hex}"
                                   :background-color- "#${palette.surface0.hex}"

                                   :primary-color "#${palette.text.hex}"
                                   :primary-color+ "#${palette.text.hex}"
                                   :primary-color- "#${palette.subtext1.hex}"

                                   :secondary-color "#${palette.surface0.hex}"
                                   :secondary-color+ "#${palette.surface1.hex}"
                                   :secondary-color- "#${palette.base.hex}"

                                   :action-color "#${palette.${config.catppuccin.accent}.hex}"
                                   :action-color+ "#${palette.${config.catppuccin.accent}.hex}"
                                   :action-color- "#${palette.${config.catppuccin.accent}.hex}"

                                   :success-color "#${palette.green.hex}"
                                   :success-color+ "#${palette.green.hex}"
                                   :success-color- "#${palette.green.hex}"

                                   :warning-color "#${palette.red.hex}"
                                   :warning-color+ "#${palette.red.hex}"
                                   :warning-color- "#${palette.red.hex}"

                                   :highlight-color "#${palette.${config.catppuccin.accent}.hex}"
                                   :highlight-color+ "#${palette.${config.catppuccin.accent}.hex}"
                                   :highlight-color- "#${palette.${config.catppuccin.accent}.hex}"

                                   :codeblock-color "#${palette.mantle.hex}"
                                   :codeblock-color+ "#${palette.crust.hex}"
                                   :codeblock-color- "#${palette.base.hex}"

                                   :text-color "#${palette.text.hex}"
                                   :text-color+ "#${palette.text.hex}"
                                   :text-color- "#${palette.subtext1.hex}"

                                   :contrast-text-color "#${palette.mantle.hex}"
                                   :contrast-text-color+ "#${palette.crust.hex}"
                                   :contrast-text-color- "#${palette.base.hex}"

                                   :font-family "${font}"))))

          (defmacro load-extensions (&rest extensions)
            "Helper macro to load extensions along with config files with the same name."
            `(progn ,@(loop for extension
                            in extensions
                            collect `(nyxt:define-nyxt-user-system-and-load
                                      ,(alexandria:symbolicate 'nyxt-user/ extension '-proxy)
                                      :description ,(format t "This proxy system saves us if ~a fails to load.
                                      Otherwise it will break all the config loading." extension)
                                      :depends-on (,extension)))))

          (load-extensions ${builtins.concatStringsSep "\n" (map (p: ":" + p) plugins)})

          (defmacro define-glyphs (&rest glyphs)
            "Helper macro to set `glyph' slot for multiple modes at once."
            `(progn
              ,@(loop for (mode glyph extension-p)
                      in glyphs 
                      collect `(define-configuration
                                 ,(read-from-string
                                   (format nil "~a~a:~a-MODE"
                                               (if extension-p
                                                 ""
                                                 "NYXT/MODE/")
                                               mode
                                               mode))
                                 ((glyph ,glyph))))))

          ;;; define glyphs for modes
          (define-glyphs (blocker         "󰂭")
                         (reduce-tracking "")
                         (repl            "")
                         (demeter         "" t))

          ;; search engine config
          (define-configuration (buffer)
            ((search-engines
              (append %slot-default%
                      (list
                        (engines:duckduckgo :shortcut "ddg"
                                            :advertisements nil                                  
                                            :install-duckduckgo nil
                                            :install-reminders nil
                                            :privacy-newsletter nil
                                            :newsletter-reminders nil
                                            :help-improve-duckduckgo nil
                                            :homepage-privacy-tips nil
                                            :theme :dark
                                            :font "${font}"
                                            ; theme based on catppuccin/duckduckgo
                                            :background-color "${palette.base.hex}"
                                            :header-color "${palette.mantle.hex}"
                                            :result-title-font "${font}"
                                            :result-title-color "${palette.${config.catppuccin.accent}.hex}"
                                            :result-visited-title-color "${palette.lavender.hex}"
                                            :result-description-color "${palette.text.hex}"
                                            :result-url-color "${palette.rosewater.hex}"
                                            :result-module-color "${palette.mantle.hex}"
                                            :result-full-urls t
                                            :result-urls-above-snippet t
                                            :result-visible-checkmark t))))))

          ; router config
          (define-configuration web-buffer
            ((default-modes (pushnew 'nx-router:router-mode %slot-value%))))

          (define-configuration router:router-mode
            ((router:routers
              (list
                (make-instance 'router:redirector
                               :name 'wikiwand
                               :route (match-domain "wikipedia.org")
                               :redirect
                                '(("www.wikiwand.com/news" . ".*Main_Page$")
                                  ("www.wikiwand.com/en/\\1" . "www.wikipedia.org/wiki/(.*)")
                                  ("www.wikiwand.com/\\1/\\2" . "(\\w*).wikipedia.org/wiki/(.*)")))
                (make-instance 'router:redirector
                               :name 'handlr
                               :route (match-domain "youtube.com" "youtu.be")
                               :redirect "www.youtube.com")
                (make-instance 'router:opener
                               :name 'handlr
                               :resource (lambda (url)
                                           (uiop:launch-program (list "handlr" "open" url))))
                ; Use teddit specifically
                (make-instance 'router:redirector
                               :name 'teddit
                               :route (match-domain "reddit.com")
                               :redirect '(("https://farside.link/teddit/\\1" . ".*://[^/]*/(.*)$")))
                ; Does not work with generic farside method
                (make-instance 'router:redirector
                               :name 'dumb
                               :route (match-domain "genius.com")
                               :redirect '(("https://farside.link/dumb/\\1" . ".*://[^/]*/(.*)$")))
                ; Does not work with generic farside method
                (make-instance 'router:redirector
                               :name 'lingva
                               :route (match-host "translate.google.com")
                               :redirect '(("https://farside.link/lingva/\\1" . ".*://[^/]*/(.*)$")))
                ; Redirect using farside.link
                (make-instance 'router:redirector
                               :name 'farside
                               :route (match-domain "stackoverflow.com"
                                                    "reddit.com"
                                                    "twitter.com"
                                                    "instagram.com"
                                                    "fandom.com"
                                                    "imdb.com"
                                                    "tiktok.com"
                                                    "quora.com"
                                                    "imgur.com"
                                                    "medium.com")
                               :redirect '(("https://farside.link/\\1" . "https://(.*)")))))))

          ; TODO: Figure out how to reverse redirect for copying URLs
          ; TODO: Figure out how to fix cannot open in iframe errors with nyxt::on-signal-notify-uri
        '';

      dataFile = lib.attrsets.mergeAttrsList (map
        (extension: {
          "nyxt/extensions/${extension}".source = inputs.${extension};
        })
        plugins);
    };
}

