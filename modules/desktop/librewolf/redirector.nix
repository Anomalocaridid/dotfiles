{ inputs, ... }:
{
  unify.modules.general.home =
    { pkgs, ... }:
    let
      id = "redirector@einaregilsson.com";
      package =
        inputs.firefox-extensions-declarative.packages.${pkgs.stdenv.hostPlatform.system}.redirector-declarative;
    in
    {
      programs.librewolf.policies = {
        ExtensionSettings.${id} = {
          install_url = "file://${package}/share/mozilla/extensions/{ec8030f7-c20a-464f-9b0e-13a3a9e97384}/${id}.xpi";
          installation_mode = "force_installed";
          private_browsing = true;
        };

        "3rdparty".Extensions.${id}.redirects = [
          {
            description = "FreeTube";
            exampleUrl = "https://www.youtube.com/watch?v=dQw4w9WgXcQ";
            # Normally automatically generated, but will not be properly generated if missing
            # Does not cause serious problems if missing, just mangles example in redirector list
            exampleResult = "freetube://https://www.youtube.com/watch?v=dQw4w9WgXcQ";
            includePattern = "((https://)?(www\\.)?youtu(be\\.com|\\.be)/.*)";
            redirectUrl = "freetube://$1";
            patternType = "R"; # Regular expression
            # Required or redirector will not work
            appliesTo = [ "main_frame" ];
          }
          {
            description = "Steam Client";
            exampleUrl = "https://store.steampowered.com/";
            # Normally automatically generated, but will not be properly generated if missing
            # Does not cause serious problems if missing, just mangles example in redirector list
            exampleResult = "steam://openurl/https://store.steampowered.com/";
            includePattern = "^(https://(.*\\.)?steam(powered|community).com/)$";
            redirectUrl = "steam://openurl/$1";
            patternType = "R"; # Regular expression
            appliesTo = [ "main_frame" ];
          }
          {
            description = "[Farside] General Entry";
            exampleUrl = "https://m.youtube.com/watch?v=dQw4w9WgXcQ";
            # Normally automatically generated, but will not be properly generated if missing
            # Does not cause serious problems if missing, just mangles example in redirector list
            exampleResult = "https://farside.link/youtube.com/watch?v=dQw4w9WgXcQ";
            includePattern = "^(?:https?://)?(?:www\\.)?(?:\\w{2;}\\.)?(?:mobile\\.|m\\.)?((?:imdb|imgur|instagram|medium|odysee|quora|reddit|tiktok|translate\\.google|twitter|wikipedia|x|youtube)\\.(?:com|org|au|de|co|cn).*)$";
            redirectUrl = "https://farside.link/$1";
            patternType = "R"; # Regular expression
            # Required or redirector will not work
            appliesTo = [ "main_frame" ];
          }
        ];
      };
    };
}
