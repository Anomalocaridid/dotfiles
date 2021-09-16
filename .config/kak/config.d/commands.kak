# ~/.config/kak/config.d/commands.kak
# Kakoune custom commands

# Toggle visible whitespace
define-command -docstring "show whitespace characters as more visible characters" whitespace-toggle %{
	try %{
		add-highlighter global/ show-whitespaces
	} catch %{
		remove-highlighter global/show-whitespaces
	}
}

# Open Tutorial
define-command -docstring "open a tutorial" trampoline %{
	evaluate-commands %sh{
		tramp_file=$(mktemp -t kakoune-trampoline.XXXXXXXX)
		echo "edit -fifo $tramp_file *TRAMPOLINE*"
		echo "echo -debug $tramp_file"
		curl -s https://raw.githubusercontent.com/mawww/kakoune/master/contrib/TRAMPOLINE -o "$tramp_file"
	}
}
# Lock user mode from inside mode
define-command -params 1 usermode-lock-add %{
	evaluate-commands %sh{
		printf 'map -docstring %s -- global %s <ret> %s\n' \
			'"lock mode"' "$1" "': enter-user-mode -lock $1<ret>'"
	}
} -shell-script-candidates %{
	printf '%s\n' $kak_user_modes
} -docstring "Add <ret> = lock to any usermode"
