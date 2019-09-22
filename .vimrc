" When started as "evim", evim.vim will already have done these settings, bail out.
if v:progname =~? "evim"
	finish
endif

" Get the defaults that most users want.
source $VIMRUNTIME/defaults.vim

if has("vms")
	set nobackup        " do not keep a backup file, use versions instead
else
	set backup        " keep a backup file (restore to previous version)
	if has('persistent_undo')
		set undofile    " keep an undo file (undo changes after closing)
	endif
endif

if &t_Co > 2 || has("gui_running")
	" Switch on highlighting the last used search pattern.
	set hlsearch
endif

" Put these in an autocmd group, so that we can delete them easily.
augroup vimrcEx
	au!

	" For all text files set 'textwidth' to 78 characters.
	autocmd FileType text setlocal textwidth=78
augroup END

" The matchit plugin makes the % command work better, but it is not backwards compatible.
" The ! means the package won't be loaded right away but when plugins are loaded during initialization.
if has('syntax') && has('eval')
	packadd! matchit
endif

" Sets line numbers
set number

" Tab appears as four columns
set tabstop=4 shiftwidth=4 softtabstop=4 smarttab autoindent

" Tab expands to spaces only for certain filetypes
autocmd BufRead,BufNewFile *.hs  setlocal expandtab
autocmd BufRead,BufNewFile *.yaml setlocal expandtab
autocmd BufRead,BufNewFile *.cabal setlocal expandtab

" Custom Keymaps
" Toggles
" <F1> Brings up help
map <F2> :NERDTreeToggle<CR>
map <F3> :ALEToggle<CR>
map <F4> :UndotreeToggle<CR>

" Navigation
map <F5> :bp<CR>
map <F6> :bn<CR>
map <F7> :tabp<CR>
map <F8> :tabn<CR>

map <F9> :bd<CR>
map <F10> :qa<CR>
map <F11> :ALEDetail<CR>

" Vim-Plug settings and plugins.
" Automatically download and install Vim-Plug if not present.
if empty(glob('~/.vim/autoload/plug.vim'))
	silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
		\ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
	autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

" Vim-Plug plugins.
call plug#begin('~/.vim/plugged')

" Asynchronous Lint Engine
Plug 'dense-analysis/ale'

" CtrlP Fuzzy file search
Plug 'kien/ctrlp.vim'

" NERDTree file navigator
Plug 'scrooloose/nerdtree',{'on': 'NERDTreeToggle'}

" Fugitive git wrapper
Plug 'tpope/vim-fugitive'

" Haskell Filetype plugin
Plug 'neovimhaskell/haskell-vim',{'for': 'haskell'}

" Airline status bar
Plug 'vim-airline/vim-airline'

" Airline theme
Plug 'vim-airline/vim-airline-themes'

" High speed HTML/CSS coding
Plug 'mattn/emmet-vim',{'for': 'html'}

" Undo Tree visualizer
Plug 'mbbill/undotree',{'on': 'UndotreeToggle'}

" Rust Filetype plugin
" Uses rustfmt for formatting
Plug 'rust-lang/rust.vim',{'for': 'rust'}

call plug#end()

" ALE linters
let g:ale_linters = {
	\ 'haskell': ['stack-ghc', 'hlint'],
	\ 'bash': ['shellcheck'],
	\ 'rust': ['rls', 'cargo', 'rustc'],
	\}

let g:ale_fixers = {
	\ '*': ['remove_trailing_lines', 'trim_whitespace'],
	\ 'haskell': ['stylish-haskell'],
	\ 'rust': ['rustfmt'],
	\}

let g:ale_fix_on_save = 1

" Set color scheme.
color cyberpunkneon

" Make background transparent.
hi Normal ctermbg=NONE

" Airline Settings
" Current Theme
let g:airline_theme='violet'
" Enable Powerline font
let g:airline_powerline_fonts = 1

" Enables tab line
let g:airline#extensions#tabline#enabled = 1

" How the tab line formats tabs' paths
let g:airline#extensions#tabline#formatter = 'unique_tail_improved'

" Closes vim if the only window open is NERDtree
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif
