#
# This file defines the rules that one can call from the `just` utility.
#
# Authors:
#   Julien Peeters <julien@mountainhacks.org>
#

mod init 'just/init.just'

# Print this message
[group('utility')]
help:
    @just --list

# Build the current workspace using the given PROFILE.
[group('build')]
build PROFILE *OPTS:
    #!/usr/bin/env bash
    readonly PROFILES=( "dev" "release" )

    validate() {
        local lookup="$1"

        for value in "${PROFILES[@]}"; do
            [[ x"${lookup}" == x"${value}" ]] && return 0
        done

        return 1
    }

    validate "{{ PROFILE }}" || {
        echo "error: invalid profile '{{ PROFILE }}'"
        exit 1
    }

    nix build {{ OPTS }} '.#{{ PROFILE }}'

# Clean the cargo build artifacts
[group('utility')]
clean:
    @rm -rf target

# Wipe all non-versioned data
[group("utility")]
mrproper:
    @git clean -dffx
