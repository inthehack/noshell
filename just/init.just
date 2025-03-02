where := invocation_directory()

# Print this message
help:
    @just --list init

# Add `use <target>` statement to local .envrc
direnv target="flake":
    @grep -qi "use {{ target }}" "{{ where }}/.envrc" 2>/dev/null || \
    	echo "use {{ target }}" >> "{{ where }}/.envrc"

    @echo "added 'use {{ target }}' to {{ where }}/.envrc"
