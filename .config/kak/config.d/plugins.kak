# ~/.config/kak/config.d/plugins.kak
# Kakoune plugins configuration

# Ensure that plug.kak is installed
evaluate-commands %sh{
	plugins="$kak_config/plugins"
    mkdir -p "$plugins"
    [ ! -e "$plugins/plug.kak" ] && \
        git clone -q https://github.com/andreyorst/plug.kak.git "$plugins/plug.kak"
    printf "%s\n" "source '$plugins/plug.kak/rc/plug.kak'"
}

plug-chain "andreyorst/plug.kak" noload config %{
	# Automatically install plugins
	# Instead of only when plug-install is run
  	set-option global plug_always_ensure 'true'
} plug "ul/kak-lsp" do %{
# Language Server Protocol integration
# Requires cargo
# See ~/.config/kak-lsp/kak-lsp.toml for further dependencies
	cargo install --locked --force --path .
} config %{
	# Cool unicode indicators
	set-option global lsp_diagnostic_line_error_sign "✖"
	set-option global lsp_diagnostic_line_warning_sign "➤"

	# # Only show errors and warnings
	# set-option global lsp_show_hover_format 'printf %s "${lsp_diagnostics}"'

	# Search for errors as well as warnings
	map global lsp n ": lsp-find-error --include-warnings<ret>" -docstring "find next error"
	map global lsp p ": lsp-find-error --previous --include-warnings<ret>" -docstring "find previous error"
	
	hook global WinSetOption filetype=(c|cpp|haskell|rust|python) %{
		# Format before saving
		hook buffer BufWritePre .* lsp-formatting-sync
		
		# Highlight errors in code
		lsp-inline-diagnostics-enable window

		# lsp user mode
		map window user l ": enter-user-mode lsp<ret>" -docstring "LSP"

		# alias default formatting commands
		alias buffer format lsp-formatting
		alias buffer format-selections lsp-range-formatting
	}

	# Enable lsp
	hook -once global WinSetOption filetype=(c|haskell|rust|python) %{
		lsp-enable
	}
} plug "screwtapello/kakoune-shellcheck" domain "gitlab.com" config %{
# Shellcheck integration
# Requires shellcheck
	# Create shellcheck mode with mappings that resemble the lsp mode
	declare-user-mode shellcheck
	map global shellcheck l ": lint<ret>" -docstring "lint buffer"
	map global shellcheck n ": lint-next-message<ret>" -docstring "find next error"
	map global shellcheck p ": lint-previous-message<ret>" -docstring "find previous error"
	map global shellcheck e ": buffer *lint-output*<ret>" -docstring "list buffer errors and warnings"
		
	hook global WinSetOption filetype=(sh|kak) %{
		map window user l ": enter-user-mode shellcheck<ret>" -docstring "shellcheck"
	}
} plug "eraserhd/parinfer-rust" do %{
# Parentheses and indentation inferrer for lisp
# Requires cargo
	cargo install --force --path .
	cargo clean
} config %{
	hook global WinSetOption filetype=(clojure|lisp|scheme|racket) %{
		parinfer-enable-window -smart
	}
} plug "andreyorst/smarttab.kak" defer smarttab %{
# Automatic tab handling
# No longer maintained
	set-option global softtabstop 4
} config %{
	hook global WinSetOption filetype=(kak|sh|plain) smarttab
   
	hook global WinSetOption filetype=(c|haskell|rust|yaml|lisp|python) expandtab
} plug "insipx/kak-crosshairs" config %{
# Highlight current line and/or current column
	crosshairs
} plug "alexherbo2/auto-pairs.kak" %{
# Pairing parentheses and brackets
	#default
	#set-option window "(" ")" "{" "}" "[" "]" '"' '"' "'" "'" "`" "`" "“" "”" "‘" "’" "«" "»" "‹" "›"

	# No single quotes
	hook global WinSetOption filetype=(haskell|rust) %{
		set-option window auto_pairs "(" ")" "{" "}" "[" "]" '"' '"' "`" "`" "“" "”" "‘" "’" "«" "»" "‹" "›"
	}

	# No single quotes, backticks, or parentheses
	hook global WinSetOption filetype=(lisp) %{
		set-option window auto_pairs "{" "}" "[" "]" '"' '"' "“" "”" "‘" "’" "«" "»" "‹" "›"
	}

	enable-auto-pairs
} plug "Bodhizafa/kak-rainbow" config %{
# Highlight recursive pairs of parentheses centered from cursor
	hook global WinSetOption filetype=(.*) rainbow-enable-window
} plug "delapouite/kakoune-buffers" config %{
# Easy navigation of open buffers
	# Buffer user mode
	map global user b ": enter-buffers-mode<ret>" -docstring "buffers"
	# map global user B ": enter-user-mode -lock buffers<ret>" -docstring "buffers (lock)"
	usermode-lock-add buffers

	# Command aliases
	alias global bd delete-buffer
	alias global bf buffer-first
	alias global bl buffer-last
	alias global bo buffer-only
	alias global bo! buffer-only-force
} plug "delapouite/kakoune-palette" \
plug "delapouite/kakoune-auto-percent" \
plug "delapouite/kakoune-auto-star" \
plug "delapouite/kakoune-mirror" config %{
	map global user m ": enter-user-mode -lock mirror<ret>" -docstring "enter mirror mode"
} plug "occivink/kakoune-vertical-selection" \
plug "delapouite/kakoune-text-objects" \
plug "occivink/kakoune-roguelight" config %{
# Roguelike-style light simulation
	define-command roguelight-toggle %{
		try %{
			crosshairs
			roguelight-enable
		} catch %{
			roguelight-disable
		}

		hook -group roguelight window RawKey .* roguelight-refresh
	}

	define-command roguelight-map %{
		edit -readonly "%val{config}/plugins/kakoune-roguelight/map"
		roguelight-toggle
	}
} plug "caksoylar/kakoune-smooth-scroll" config %{
# Smooth scrolling
# Requires Python
	# Automatically enable for every window
	hook global WinCreate .* %{
		hook -once window WinDisplay .* %{
			smooth-scroll-enable

			# Switch between relative and absolute line numbers
			# and update crosshairs
			hook -group smooth-scroll window User ScrollBegin %{
	            add-highlighter -override window/number-toggle number-lines
	            crosshairs
		    }

		    hook -group smooth-scroll window User ScrollEnd %{
	            add-highlighter -override window/number-toggle number-lines -relative -hlcursor
	            crosshairs
		    }
	    }
    }

	# Default mapping conflicts with text-objects
	set-option global scroll_keys_object
} plug "dead10ck/visual-mode.kak" domain "gitlab.com" demand visual-mode %{
# Vim-style visual mode
	map global normal <a-v> ": visual-mode v<ret>"
	map global normal <a-V> ": visual-line-mode V<ret>"
} plug "evanrelf/number-toggle.kak" demand "number-toggle" %{
# Switch between relative and absolute line numbers
	set-option global number_toggle_params -hlcursor
} plug "Anomalocaridid/wezterm.kak" %{
# WezTerm integration
	wezterm-integration-enable
} plug "Anomalocaridid/kakoune-cyberpunk-neon" theme config %{
# My custom colorscheme
# Keep at bottom so that it overwrites supported plugins' custom faces
	colorscheme cyberpunk-neon
} plug "andreyorst/powerline.kak" defer powerline_bufname %{
# Powerline Bar
# Keep below colorscheme so it can use its powerline theme module
# Default layout
# powerline-format "git bufname line-column mode-info filetype client session position"
	set-option global powerline_shorten_bufname short
} defer powerline_cyberpunk_neon %{
	# Use my custom powerline theme
	powerline-theme cyberpunk-neon
} config %{
	powerline-start
} plug "jpcornwell/kakoune-sokoban"
