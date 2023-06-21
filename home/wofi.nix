{ ... }: {
  programs.wofi = {
    enable = true;
    settings = {
      hide_scroll = true;
      show = "drun";
      width = "25%";
      lines = 10;
      line_wrap = "word";
      term = "handlr launch x-scheme-handler/terminal";
      allow_markup = true;
      always_parse_args = true;
      show_all = true;
      print_command = true;
      layer = "overlay";
      allow_images = true;
      insensitive = true;
      prompt = "";
      image_size = 15;
      display_generic = true;
      location = "center";
    };
    style = #css
      ''
        /* Cyberpunk-Neon colorscheme */
        @define-color dark-blue  #000b1e;
        @define-color blue       #091833;
        @define-color light-blue #133e7c;
        @define-color light-alt  #0b2956;
        @define-color cyan       #0abdc6;
        @define-color pink       #ea00d9;
        @define-color purple     #711c91;
        @define-color purple-alt #321959;
        @define-color red        #ff0000;
        @define-color orange     #f57800;
        @define-color white      #d7d7d5;
        @define-color yellow     #ffff00;
        @define-color green      #00ff00;

        @define-color highlight  @purple;
        @define-color base1      @blue;
        @define-color base2      @dark-blue;
        @define-color base3      @dark-blue;

        *{
            font-family: "Fira Code Nerd Font";
        }

        window {
        	border: 1px solid @highlight;
        }

        #input {
            margin-bottom: 15px;
            padding:3px;
            border-radius: 5px;
            border:none;
            color: @cyan;
        }

        #inner-box {
            background-color: @base3;

        }

        #outer-box {
            margin: 5px;
            padding:15px;
            background-color: @base2;
        }

        #scroll {
        }

        #text {
            padding: 5px;
            color: @cyan;
        }

        #entry:nth-child(even){
            background-color: @base1;
        }

        #entry:selected {
            background-color: @highlight;
        }

        #text:selected {
            color: @white;
        }
      '';
  };
}
