# dotfiles
My dotfiles, uploaded to GitHub using [YADM](https://yadm.io)

# Programs supported
* [bat](https://github.com/sharkdp/bat)
* [bpytop](https://github.com/aristocratos/bpytop)
* clang-format
* ghci
* [kakoune](https://github.com/mawww/kakoune)
* [ranger](https://github.com/ranger/ranger)
* [wezterm](https://github.com/wez/wezterm)
* yay
* zsh

# Usage
clone this repository with

`yadm clone https://github.com/Anomalocaridid/dotfiles`

# Bootstrap script
## WARNING: BEFORE RUNNING THE BOOTSTRAP SCRIPT, READ THROUGH IT AND MAKE SURE THAT YOU ARE 100% OKAY WITH THE CHANGES IT MAKES. IN ADDITION, THIS HAS ONLY BEEN TESTED WITH CERTAIN VERSIONS OF ENDEAVOUROS AND IS NOT GUARANTEED TO WORK WITH OTHER LINUX DISTROS OR ARCH LINUX DERIVATIVES. I TAKE NO RESPONSIBILITY FOR ANY MESSED UP SYSTEMS AS A RESULT. YOU HAVE BEEN WARNED!

Takes care of installing dependencies and initializing extra elements of my personal setup. Run it with the following command:

`yadm bootstrap`

# Dependencies
Required font: Fira Code Nerd Font

### Kakoune
* clangd
* fzf
* haskell language server
	* hlint
	* ormolu
* kakoune.cr
* python-language-server
    * flake8
    * yapf
* qalculate!
* rustup
    * rls
    * rustfmt
* shellcheck
* shfmt

### Ranger
* this repository's submodules
* zoxide

### Zsh
* bat
* exa
* fzf
* lolcat
* pkgfile
* toilet
* zoxide
