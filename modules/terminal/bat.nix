{
  unify.modules.general.home =
    {
      config,
      lib,
      pkgs,
      ...
    }:
    {
      home = {
        shellAliases = {
          cat = "bat --paging=never";
          less = "bat --paging=always";
          bgrep = lib.getExe pkgs.bat-extras.batgrep;
          man = lib.getExe pkgs.bat-extras.batman;
          diff = lib.getExe pkgs.bat-extras.batpipe;
        };
        sessionVariables = {
          PAGER = "bat";
          MANPAGER = "sh -c 'col --no-backspaces --spaces | bat --plain --language=man'";
        };
      };

      programs.bat = {
        enable = true;
        extraPackages = with pkgs; [
          unzip # required by default batpipe viewer
        ];
        config.lessopen = true;
        # TODO: Remove once $LESSOPEN support is enabled by default
        package = pkgs.bat.overrideAttrs (oldAttrs: {
          cargoBuildFeatures = (oldAttrs.cargoBuildFeatures or [ ]) ++ [ "lessopen" ];
        });
      };

      # Designate Unicode Private Use Areas as printable characters
      # Needed for Nerd Fonts icons to display properly
      home.sessionVariables.LESSUTFCHARDEF = "E000-F8FF:p,F0000-FFFFD:p,100000-10FFFD:p";

      # Custom batpipe viewers
      xdg.configFile."batpipe/viewers.d/custom.sh".text =
        let
          makeViewer =
            {
              command,
              filetype,
              header ? "",
            }:
            let
              program = lib.strings.splitString " " command |> builtins.head |> builtins.baseNameOf;
            in
            #sh
            ''
              BATPIPE_VIEWERS+=("${program}")

              viewer_${program}_supports() {
                case "$1" in
                  ${filetype}) return 0;;
                esac
                return 1
              }

              viewer_${program}_process() {
                ${header}
                ${command}
                return "$?"
              }
            '';
          batpipe_archive_header = # sh
            ''
              batpipe_header    "Viewing contents of archive: %{PATH}%s" "$1"
              batpipe_subheader "To view files within the archive, add the file path after the archive."
            '';
          batpipe_document_header = # sh
            ''
              batpipe_header "Viewing text of document: %{PATH}%s" "$1"
            '';
        in
        lib.strings.concatMapStrings makeViewer [
          {
            command = ''${lib.getExe pkgs.python3Packages.docx2txt} "$1"'';
            filetype = "*.docx";
            header = batpipe_document_header;
          }
          {
            command = ''${lib.getExe pkgs.glow} --style ${config.home.sessionVariables.GLAMOUR_STYLE} "$1"'';
            filetype = "*.md";
          }
          {
            command = ''${lib.getExe pkgs.odt2txt} "$1"'';
            filetype = "*.odt";
            header = batpipe_document_header;
          }
          {
            command = ''${lib.getExe' pkgs.pdfminer "pdf2txt"} "$1"'';
            filetype = "*.pdf";
            header = batpipe_document_header;
          }
          {
            command = ''${lib.getExe pkgs.unrar} "$1"'';
            filetype = "*.rar";
            header = batpipe_archive_header;
          }
        ];
    };
}
