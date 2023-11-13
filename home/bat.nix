{ config, lib, pkgs, inputs, ... }: {
  programs.bat = {
    enable = true;
    catppuccin.enable = true;
    extraPackages = with pkgs; [
      bat-extras.batdiff
      bat-extras.batgrep
      bat-extras.batman
      bat-extras.batpipe
      # required for batpipe
      glow
      odt2txt
      pdfminer # provides pdf2txt
      python3Packages.docx2txt
      unrar
      unzip # default viewer
    ];
    config = {
      "lessopen" = true;
    };
  };

  # Designate Unicode Private Use Areas as printable characters
  # Needed for Nerd Fonts icons to display properly
  home.sessionVariables.LESSUTFCHARDEF = "E000-F8FF:p,F0000-FFFFD:p,100000-10FFFD:p";

  # Custom batpipe viewers
  xdg.configFile."batpipe/viewers.d/custom.sh".text =
    let
      makeViewer = { command, filetype, header ? "" }:
        let
          program = builtins.elemAt (lib.strings.splitString " " command) 0;
        in
        #sh
        ''
          BATPIPE_VIEWERS+=("${program}")

          viewer_${program}_supports() {
            command -v "${program}" $> /dev/null || return 1

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
      batpipe_archive_header = #sh
        '' 
          batpipe_header    "Viewing contents of archive: %{PATH}%s" "$1"
        	batpipe_subheader "To view files within the archive, add the file path after the archive."
        '';
      batpipe_document_header = ''batpipe_header "Viewing text of document: %{PATH}%s" "$1"'';
    in
    lib.strings.concatMapStrings makeViewer [
      {
        command = ''docx2txt "$1"'';
        filetype = "*.docx";
        header = batpipe_document_header;
      }
      {
        # FIXME: Does not work with bat for some reason
        command = ''glow --style ${config.home.sessionVariables.GLAMOUR_STYLE} "$1"'';
        filetype = "*.md";
      }
      {
        command = ''odt2txt "$1"'';
        filetype = "*.odt";
        header = batpipe_document_header;
      }
      {
        command = ''pdf2txt "$1"'';
        filetype = "*.pdf";
        header = batpipe_document_header;
      }
      {
        command = ''unrar "$1"'';
        filetype = "*.rar";
        header = batpipe_archive_header;
      }
    ];

  # Remove once #4660 is merged into home-manager
  disabledModules = [ "programs/bat.nix" ];
  imports = [ "${inputs.hm-bat-fix}/modules/programs/bat.nix" ];
}
