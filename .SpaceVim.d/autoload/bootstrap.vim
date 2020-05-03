" vim:fileencoding=utf-8:foldmethod=marker

function! bootstrap#before() abort
	" Keybindings {{{

	" Cheatsheet
	nnoremap <leader>? :Cheat40<cr>

	" Turn off highlighting after hitting Escape
	nnoremap <ESC> :noh<CR><ESC>

	" }}}

	" ALE Settings {{{

	let g:ale_linters = {
		\ 'haskell': ['hie'],
		\ 'bash': ['shellcheck'],
		\ 'rust': ['rls', 'cargo', 'rustc'],
		\ 'zsh': ['shellcheck'],
		\}

	let g:ale_fixers = {
		\ '*': ['remove_trailing_lines', 'trim_whitespace'],
		\ 'haskell': ['hindent', 'stylish-haskell'],
		\ 'sh': ['shfmt'],
		\ 'rust': ['rustfmt'],
		\}

	let g:ale_haskell_hie_executable = 'hie-wrapper'

    let g:ale_rust_cargo_use_clippy = 1

	let g:ale_lint_on_save = 0

	let g:ale_fix_on_save = 1

	" }}}

	" Misc Settings {{{

	" Highlights search and replace matches as you type
	set inccommand=nosplit

	" Set fold method to marker
	set foldmethod=marker

	" Tab expands to spaces only for certain filetypes
	autocmd BufRead,BufNewFile *.hs,*.yaml,*.cabal setlocal expandtab

	" Automatically toggle number style
	augroup numbertoggle
	    autocmd!
	    autocmd BufEnter,FocusGained,InsertLeave,WinEnter * set relativenumber
	    autocmd BufLeave,FocusLost,InsertEnter,WinLeave   * set norelativenumber
	augroup END

	" }}}

endfunction

function! bootstrap#after() abort
	" Function Keys {{{

	" Already taken keys:
	" <F1> opens the help screen
	" <F2> opens ctags
	" <F3> opens file manager
	"
	" Undotree
	" nnoremap <F4> :UndotreeToggle<cr>
	" Mundo
	nnoremap <F4> :MundoToggle<cr>

	" Misc navigation
	nnoremap <F5> :bp<cr>
	nnoremap <F6> :bn<cr>
	nnoremap <F7> :tabp<cr>
	nnoremap <F8> :tabn<cr>

	" ALE
	nnoremap <F9> :ALEDetail<cr>

	" Toggle Goyo
	nnoremap <F10> :Goyo<cr>

	" }}}

	" Show command in bottom bar
	set showcmd

	" Hide redundant mode line
	set noshowmode
endfunction
