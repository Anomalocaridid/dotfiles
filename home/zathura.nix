{ ... }: {
  programs.zathura = {
    enable = true;
    options = {
      # Copy to system clipboard
      selection-clipboard = "clipboard";

      # Appearance
      completion-bg = "#091833";
      completion-fg = "#0ABDC6";
      # completion-group-bg = "";
      # completion-group-fg = "";
      completion-highlight-bg = "#711C91";
      completion-highlight-fg = "#0ABDC6";
      default-fg = "#0ABDC6";
      default-bg = "#000B1E";
      font = "FiraCode Nerd Font 9";
      inputbar-bg = "#000B1E";
      inputbar-fg = "#0ABDC6";
      notification-bg = "#091833";
      notification-fg = "#EA00D9";
      notification-error-bg = "#091833";
      notification-error-fg = "#FF0000";
      notification-warning-bg = "#091833";
      notification-warning-fg = "#F57800";
      statusbar-bg = "#091833";
      statusbar-fg = "#0ABDC6";

      highlight-color = "#EA00D9";
      highlight-fg = "#711C91";
      highlight-active-color = "#00FF00";
      highlight-transparency = "0.6";
      recolor-darkcolor = "#0ABDC6";
      recolor-lightcolor = "#000B1E";
      index-bg = "#091833";
      index-fg = "#0ABDC6";
      index-active-bg = "#711c91";
      index-active-fg = "#0ABDC6";

      window-title-basename = true;
      statusbar-home-tilde = true;
    };
  };
}
