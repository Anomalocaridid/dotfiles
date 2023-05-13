{ pkgs, ... }: {
  home.packages = with pkgs; [
    glow
  ];

  xdg.configFile =
    let
      glowThemePath = "glow/cyberpunk_neon.json";
    in
    {
      "glow/glow.yml".source =
        let
          yamlFormat = pkgs.formats.yaml { };
        in
        yamlFormat.generate "glow-format" {
          # style name or JSON path (default "auto")
          style = "~/.config/${glowThemePath}";
          # show local files only; no network (TUI-mode only)
          local = true;
          # mouse support (TUI-mode only)
          mouse = true;
          # use pager to display markdown
          pager = true;
          # word-wrap at width
          width = 0;
        };
      "${glowThemePath}".source =
        let
          jsonFormat = pkgs.formats.json { };
          cyan = "#0abdc6";
          orange = "#f57800";
          pink = "#ea00d9";
          lightBlue = "#133e7c";
          purple = "#711c91";
          white = "#d7d7d5";
          darkBlue = "#000b1e";
          red = "#ff0000";
          green = "#00ff00";
        in
        jsonFormat.generate "glow-theme" rec {
          document = {
            block_prefix = "\n";
            block_suffix = "\n";
            color = cyan;
            margin = 2;
          };
          block_quote = {
            color = orange;
            indent = 1;
            indent_token = "│ ";
          };
          paragraph = { };
          list.level_indent = 2;
          heading = {
            block_suffix = "\n";
            bold = true;
            color = pink;
          };
          h1.prefix = "# ";
          h2.prefix = "## ";
          h3 = {
            prefix = "### ";
            italic = true;
          };
          h4.prefix = "#### ";
          h5 = {
            prefix = "##### ";
            italic = true;
          };
          h6 = {
            prefix = "###### ";
            bold = false;
          };
          text = { };
          strikethrough.crossed_out = true;
          emph.italic = true;
          strong.bold = true;
          hr = {
            color = lightBlue;
            format = "\n--------\n";
          };
          item.block_prefix = "• ";
          enumeration.block_prefix = ". ";
          task = {
            ticked = "[✓] ";
            unticked = "[ ] ";
          };
          link = {
            block_prefix = "(";
            block_suffix = ")";
            color = pink;
            underline = true;
          };
          link_text = {
            block_prefix = "[";
            block_suffix = "]";
            bold = true;
            color = orange;
          };
          image = link // { bold = true; };
          image_text = {
            color = link_text.color;
            format = "Image= [{{.text}}]";
          };
          code = {
            background_color = purple;
            color = white;
            prefix = " ";
            suffix = " ";
          };
          code_block = {
            color = document.color;
            margin = 2;
            chroma = {
              text.color = orange;
              error = {
                color = code_block.chroma.background.background_color;
                background_color = red;
              };
              comment.color = lightBlue;
              comment_preproc = code_block.chroma.comment;
              keyword = {
                bold = true;
                color = pink;
              };
              keyword_reserved.color = code_block.chroma.keyword.color;
              keyword_namespace.color = code_block.chroma.keyword.color;
              keyword_type.color = cyan;
              operator.color = pink;
              punctuation.color = purple;
              name.color = cyan;
              name_builtin = code_block.chroma.name;
              name_tag = code_block.chroma.name;
              name_attribute.color = pink;
              name_class = {
                color = pink;
                underline = true;
                bold = true;
              };
              name_constant.color = orange;
              name_decorator.color = orange;
              name_exception = { };
              name_function.color = pink;
              name_other = { };
              literal.color = orange;
              literal_number = code_block.chroma.literal;
              literal_date = code_block.chroma.literal;
              literal_string = code_block.chroma.literal;
              literal_string_escape = code_block.chroma.literal;
              generic_deleted.color = red;
              generic_emph = emph;
              generic_inserted.color = green;
              generic_strong = strong;
              generic_subheading.color = heading.color;
              background.background_color = darkBlue;
            };
          };
          table = {
            center_separator = "┼";
            column_separator = "│";
            row_separator = "─";
          };
          definition_list = { };
          definition_term = { };
          definition_description.block_prefix = "\n🠶 ";
          html_block = { };
          html_span = { };
        };
    };
}
