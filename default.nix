{ obelisk ? import ./.obelisk/impl {
    system = builtins.currentSystem;
    iosSdkVersion = "10.2";
    # You must accept the Android Software Development Kit License Agreement at
    # https://developer.android.com/studio/terms in order to build Android apps.
    # Uncomment and set this to `true` to indicate your acceptance:
    # config.android_sdk.accept_license = false;
  }
}:
with obelisk;
project ./. ({ pkgs, hackGet, ... }:
  let
    hs-abci = hackGet ./dep/hs-abci;

    extra-build-inputs = with pkgs; {
      hs-abci-types = [protobuf];
    };

    addBuildInputs = inputs: { buildInputs ? [], ... }: { buildInputs = inputs ++ buildInputs; };

  in {
    android.applicationId = "systems.obsidian.obelisk.examples.minimal";
    android.displayName = "Obelisk Minimal Example";
    ios.bundleIdentifier = "systems.obsidian.obelisk.examples.minimal";
    ios.bundleName = "Obelisk Minimal Example";

    packages =  {
      hs-abci-server = hs-abci + /hs-abci-server;
      hs-abci-types  = hs-abci + /hs-abci-types;
      hs-abci-extra  = hs-abci + /hs-abci-extra;
    };

    overrides = pkgs.lib.foldr pkgs.lib.composeExtensions  (_: _: {}) [
      (import hs-abci {}).overrides
      (self: super: builtins.mapAttrs (name: deps: super.${name}.overrideAttrs (addBuildInputs deps)) extra-build-inputs)
    ];
  }
)
