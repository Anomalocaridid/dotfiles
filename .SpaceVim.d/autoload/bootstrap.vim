function! bootstrap#before() abort
	" Keybindings {{{
	" Slready taken keys:
	" <F1> opens the help screen
	" <F2> opens ctags
	" <F3> opens NERDTree
	"
	" Undotree
	nnoremap <F4> :UndotreeToggle<cr>

	" Misc navigation
	nnoremap <F5> :bp<cr>
	nnoremap <F6> :bn<cr>
	nnoremap <F7> :tabp<cr>
	nnoremap <F8> :tabn<cr>

	" Cheatsheet
	nnoremap <leader>? :Cheat40<cr>

	" ALE
	nnoremap <F9> :ALEDetail<cr>
	" }}}

	" ALE Settings {{{
	let g:ale_linters = {
		\ 'haskell': ['hlint', 'hdevtools'],
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

    let g:ale_rust_cargo_use_clippy = 1

	let g:ale_lint_on_save = 0

	let g:ale_fix_on_save = 1
	" }}}

	" NERDCommenter Settings {{{
	let g:NERDSpaceDelims = 1
	let g:NERDTrimTrailingWhitespace = 1
	" }}}

	" Misc Settings {{{
	" Highlights search and replace matches as you type
	set inccommand=nosplit

	" Tab expands to spaces only for certain filetypes
	autocmd BufRead,BufNewFile *.hs,*.yaml,*.cabal setlocal expandtab

	" Set fold method to marker
	set foldmethod=marker
	" }}}
endfunction

function! bootstrap#after() abort
	" Set fold method to marker
	set foldmethod=marker

	" Show command in bottom bar
	set showcmd

	" Hide redundant mode line
	set noshowmode
endfunction
