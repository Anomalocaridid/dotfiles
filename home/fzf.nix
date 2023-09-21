{ ... }: {
  programs.fzf = {
    enable = true;
    colors =
      let
        cyan = "#0abdc6";
        pink = "#ea00d9";
        blue = "#091833";
        green = "#00ff00";
        purple = "#711c91";
      in
      {
        hl = pink;
        "bg+" = blue;
        "hl+" = purple;
        info = cyan;
        border = pink;
        prompt = pink;
        pointer = pink;
        spinner = green;
      };
  };
}
