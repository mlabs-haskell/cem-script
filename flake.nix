{
  inputs = {
    raw-oura.url = "github:Renegatto/oura?rev=3be6b2883d41ced958c4d462b5e899e05cdf9b6a";
    flake-utils.follows = "raw-oura/flake-utils";
    rust-overlay.follows = "raw-oura/rust-overlay";
    nixpkgs.follows = "raw-oura/nixpkgs";
    crane = {
      url = "github:ipetkov/crane";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = {
    self,
    raw-oura,
    crane,
    flake-utils,
    rust-overlay,
    nixpkgs,
    ...
  }: let
    supportedSystems = ["x86_64-linux" "x86_64-darwin" "aarch64-linux"];
    overlays = [ (import rust-overlay) ];
    pkgs = import nixpkgs {
      inherit overlays;
      system = "x86_64-linux";
    };
    # oura: Derivation
    oura =
      let
        # oura.nix:
        #   {inputs: {crane: NixInput, oura: NixInput}} ->
        #   { perSystem: {pkgs: any} ->
        #     {  packages: { oura-${string}: Package } }
        #   }
        make-oura = import ./oura.nix {inputs = {inherit crane; oura = raw-oura;};};
      in builtins.head (builtins.attrValues (make-oura.perSystem {inherit pkgs;}).packages);

  in flake-utils.lib.eachSystem supportedSystems
    (
      system: {
        devShells = {
          default =
            pkgs.mkShell {
            nativeBuildInputs = [
              oura
              pkgs.haskellPackages.fourmolu
            ];
          };
       };
    });
}