function! bootstrap#before() abort
	" Keybindings {{{
	nnoremap <F4> :UndotreeToggle<cr>
	nnoremap <F5> :bp<cr>
	nnoremap <F6> :bn<cr>
	nnoremap <F7> :tabp<cr>
	nnoremap <F8> :tabn<cr>
	nnoremap <leader>? :Cheat40<cr>
	" }}}

	" ALE Settings {{{
	let g:ale_linters = {
		\ 'haskell': ['ghc', 'hlint'],
		\ 'bash': ['shellcheck'],
		\ 'rust': ['rls', 'cargo', 'rustc'],
		\ 'zsh': ['shellcheck']
		\}

	let g:ale_fixers = {
		\ '*': ['remove_trailing_lines', 'trim_whitespace'],
		\ 'haskell': ['hindent', 'stylish-haskell'],
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
endfunction
