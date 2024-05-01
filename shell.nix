# Assumes, there is a channel called 'nixpkgs', see https://nixos.wiki/wiki/Nix_channels
{ pkgs ? import <nixpkgs-unstable> {} }:
pkgs.mkShell {
	buildInputs = with pkgs; [
		# Package goes there
		# You can search packages via `nix search $term` or from https://search.nixos.org/packages
    cargo
    rustc
	];

	# For rust-analyzer to see std-source
	RUST_SRC_PATH = "${pkgs.rust.packages.stable.rustPlatform.rustLibSrc}";
}
