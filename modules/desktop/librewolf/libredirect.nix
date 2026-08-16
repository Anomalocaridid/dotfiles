{
  unify.modules.general.home =
    let
      id = "7esoorv3@alefvanoon.anonaddy.me";
    in
    {
      programs.librewolf.policies.ExtensionSettings.${id} = {
        install_url = "https://addons.mozilla.org/firefox/downloads/latest/${id}/latest.xpi";
        installation_mode = "force_installed";
        private_browsing = true;
      };
    };
}
