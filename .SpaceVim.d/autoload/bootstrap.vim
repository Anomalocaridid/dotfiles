" vim:fileencoding=utf-8:foldmethod=marker

function! bootstrap#before() abort
	" Keybindings {{{

	" Turn off highlighting after hitting Escape
	nnoremap <ESC> :noh<CR><ESC>

	" }}}

	" ALE Settings {{{

	let g:ale_linters = {
		\ 'haskell': ['ghc', 'hlint'],
		\ 'bash': ['shellcheck'],
		\ 'rust': ['rls', 'cargo', 'rustc'],
		\ 'zsh': ['shellcheck'],
		\}

	let g:ale_fixers = {
		\ '*': ['remove_trailing_lines', 'trim_whitespace'],
		\ 'haskell': ['hindent', 'stylish-haskell'],
		\ 'sh': ['shfmt'],
		\ 'rust': ['rustfmt'],
		\ 'html': ['prettier'],
		\ 'css': ['prettier'],
		\ 'yaml': ['prettier'],
		\}

    let g:ale_rust_cargo_use_clippy = 1

	let g:ale_lint_on_save = 0

	let g:ale_fix_on_save = 1

	" Also applies to other languages that use prettier
	let g:ale_javascript_prettier_options = '--tab-width 4 --use-tabs'

	" let g:ale_css_prettier_options = '--tab-width 4 --use-tabs'

	" }}}

	" Tasks {{{

	" Rust {{{
	function! s:cargo_task() abort
		if filereadable('Cargo.toml')
			let commands = ['build', 'run', 'test']
			let conf = {}
			for cmd in commands
				call extend(conf, {
							\ cmd: {
							\ 'command': 'cargo',
							\ 'args' : [cmd],
							\ 'isDetected' : 1,
							\ 'detectedName' : 'cargo:'
							\ }
							\ })
			endfor
			return conf
		else
			return {}
		endif
	endfunction
	call SpaceVim#plugins#tasks#reg_provider(funcref('s:cargo_task'))
	" }}}

	" Haskell {{{
	function! s:stack_task() abort
		if filereadable('stack.yaml')
			let commands = ['build', 'run', 'test']
			let conf = {}
			for cmd in commands
				call extend(conf, {
							\ cmd : {
							\ 'command': 'stack',
							\ 'args' : [cmd],
							\ 'isDetected' : 1,
							\ 'detectedName' : 'stack:'
							\ }
							\ })
			endfor
			return conf
		else
			return {}
		endif
	endfunction
	call SpaceVim#plugins#tasks#reg_provider(funcref('s:stack_task'))
	" }}}

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
	nnoremap <F4> :UndotreeToggle<cr>

	" Misc navigation
	nnoremap <F5> :bp<cr>
	nnoremap <F6> :bn<cr>
	nnoremap <F7> :tabp<cr>
	nnoremap <F8> :tabn<cr>

	" ALE
	nnoremap <F9> :ALEDetail<cr>

	unmap <F11>

	" }}}

	" Show command in bottom bar
	set showcmd

	" Hide redundant mode line
	set noshowmode
endfunction
