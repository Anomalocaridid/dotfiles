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

" Sets line numbers.
set number

" Tab key indents with four spaces. 
set tabstop=4 expandtab shiftwidth=4 smarttab

" Custom Keymaps
map <F2> :NERDTreeToggle<CR>
" Navigation
map <F5> :bp<CR>
map <F6> :bn<CR>
map <F7> :tabp<CR>
map <F8> :tabn<CR>


" Vim-Plug settings and plugins.
" Automatically download and install Vim-Plug if not present.
if empty(glob('~/.vim/autoload/plug.vim'))
    silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
        \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
    autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

" Vim-Plug plugins.
call plug#begin('~/.vim/plugged')

" Dracula theme from https://draculatheme.com/vim
Plug 'dracula/vim',{'as':'dracula'}

" Asynchronous Lint Engine
Plug 'w0rp/ale'

" CtrlP Fuzzy file search
Plug 'kien/ctrlp.vim'

" NERDTree file navigator
Plug 'scrooloose/nerdtree',{'on':'NERDTreeToggle'}

" Fugitive git wrapper
Plug 'tpope/vim-fugitive'

" Haskell Filetype plugin
Plug 'neovimhaskell/haskell-vim',{'for': 'haskell'}

" Stylish-Haskell integration
Plug 'alx741/vim-stylishask',{'for': 'haskell'}

" ghc-mod integration
Plug 'eagletmt/ghcmod-vim',{'for': 'haskell'}

" asynchronous execution library
Plug 'Shougo/vimproc.vim',{'do': 'make'}

" Airline status bar
Plug 'vim-airline/vim-airline'

call plug#end()

" ALE linters
let g:ale_linters ={
    \ 'haskell': ['ghc-mod', 'hlint', 'hdevtools', 'hfmt']
    \}

" Set color scheme.
color dracula

" Make background transparent.
hi Normal ctermbg=NONE

" Airline Settings
" Enable Powerline font 
let g:airline_powerline_fonts = 1
" Enables tab line
let g:airline#extensions#tabline#enabled = 1
" How the tab line formats tabs' paths
let g:airline#extensions#tabline#formatter = 'unique_tail_improved'
