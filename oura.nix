# {inputs: {crane: NixInput, oura: NixInput}} ->
# { perSystem: {pkgs: any} -> {  packages: { oura: Package } }
# }
{inputs}: {
  perSystem = {pkgs}: let
    craneLib = inputs.crane.mkLib pkgs;
    oura = craneLib.buildPackage {
      cargoExtraArgs = "--all-features"; # Enable all bundled plugins
      env = {
        OPENSSL_NO_VENDOR = "1"; # Use system openssl
      };
      nativeBuildInputs = [pkgs.pkg-config pkgs.m4];
      buildInputs = [pkgs.openssl];
      src = craneLib.cleanCargoSource inputs.oura;
    };
  in {
    packages = { inherit oura; };
  };
}