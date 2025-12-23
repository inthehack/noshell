{
  description = "noshell, a no_std shell for embedded systems.";

  inputs = {
    flake-parts.url = "github:hercules-ci/flake-parts";
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    treefmt-nix.url = "github:numtide/treefmt-nix";

    crane.url = "github:ipetkov/crane";
    rust-overlay.url = "github:oxalica/rust-overlay";
    rust-overlay.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs =
    inputs@{ flake-parts, ... }:

    flake-parts.lib.mkFlake { inherit inputs; } {
      # Imports of other modules.
      imports = [
        inputs.treefmt-nix.flakeModule
      ];

      # Applied systems.
      systems = [
        "x86_64-linux"
        "aarch64-linux"
      ];

      # Per-system configuration.
      perSystem =
        { pkgs, system, ... }:

        let
          craneLib = (inputs.crane.mkLib pkgs).overrideToolchain (
            somePkgs:
            somePkgs.rust-bin.stable.latest.default.override {
              extensions = [
                "llvm-tools"
                "rust-analyzer"
                "rust-src"
                "rust-std"
              ];

              targets = [
                # Cortex M7, M7F.
                "thumbv7em-none-eabi"
                "thumbv7em-none-eabihf"

                # Cortex M33.
                "thumbv8m.main-none-eabihf"

                # Host
                "x86_64-unknown-linux-gnu"
                "aarch64-unknown-linux-gnu"
              ];
            }
          );
        in
        {
          # Apply Rust overlay to flake parts.
          _module.args.pkgs = import inputs.nixpkgs {
            inherit system;
            overlays = [ (import inputs.rust-overlay) ];
          };

          # Formatter.
          treefmt = {
            # Where to look for the root of the sources.
            projectRootFile = "flake.nix";

            # What formatters are enabled.
            programs.just.enable = true;
            programs.mdformat.enable = true;
            programs.nixfmt.enable = true;
            programs.rustfmt.enable = true;
            programs.taplo.enable = true;

            # Formatter settings.
            settings.formatter.just.includes = [
              "[Jj]ustfile"
              "**/*.just"
            ];
            settings.formatter.mdformat.includes = [ "**/*.md" ];
          };

          # Developer shell.
          devShells.default = craneLib.devShell {
            # Additional environment variables here (e.g. CUSTOM_VAR = ...).

            # Extra input packages.
            packages = with pkgs; [
              # Build dependencies.
              bacon
              cargo-deny
              cargo-nextest
              cargo-semver-checks
              clippy

              # Utilities.
              gh
              just
              release-plz
            ];
          };
        };
    };
}
