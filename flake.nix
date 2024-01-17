{
  inputs =
    {
      nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
      ps-tools.follows = "purs-nix/ps-tools";
      purs-nix.url = "github:purs-nix/purs-nix/ps-0.15";
      utils.url = "github:numtide/flake-utils";
    };

  outputs = { nixpkgs, utils, ... }@inputs:
    utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ]
      (system:
        let
          name = "481 Availability List";
          pkgs = nixpkgs.legacyPackages.${system};
          ps-tools = inputs.ps-tools.legacyPackages.${system};
          purs-nix = inputs.purs-nix { inherit system; };

          ps =
            purs-nix.purs
              {
                dependencies =
                  with purs-nix.ps-pkgs;
                  [
                    console
                    debug
                    effect
                    foreign
                    halogen
                    prelude
                    maybe
                    aff
                    run
                    strings
                    parsing
                    affjax-web
                    affjax-node
                    arrays
                    web-events
                    web-html
                    web-dom
                    tuples
                    integers
                    argonaut
                    argonaut-core
                    argonaut-codecs
                    ordered-collections
                    halogen-vdom
                    halogen-formless
                    string-parsers
                    css
                    halogen-css
                    ordered-set
                    # new
                    datetime
                    dom-indexed
                    either
                    enums
                    js-uri
                    now
                    psci-support
                    strings
                    typelevel-prelude
                    uri
                  ];

                dir = ./.;
              };
          ps-command = ps.command { };
          purs-watch = pkgs.writeShellApplication {
            name = "purs-watch";
            runtimeInputs = with pkgs; [ entr ps-command ];
            text = "find src | entr -s 'echo building && purs-nix compile'";
          };
          vite = pkgs.writeShellApplication {
            name = "vite";
            runtimeInputs = with pkgs; [ nodejs ];
            text = "npx vite --open dev/index.html";
          };
          dev = pkgs.writeShellApplication {
            name = "dev";
            runtimeInputs = with pkgs; [ concurrently ];
            text = "concurrently purs-watch vite";
          };
        in
        {
          # packages.default = ps.modules.Main.bundle { };
          packages = with ps; {
            default = ps.modules.Main.bundle {};
            bundle = bundle {};
            output = output {};
          };
          bundle.esbuild = {format = "iife";};
          # checks.default = checks;

          devShells.default =
            pkgs.mkShell
              {
                inherit name;
                packages =
                  with pkgs;
                  [
                    ps-tools.for-0_15.purescript-language-server
                    ps-command
                    purs-nix.purescript
                    purs-watch
                    vite
                    dev
                  ];
              };
        }
      );
}


# {
#   inputs = {
#     #Generic Stuff
#     nixpkgs.follows = "purs-nix/nixpkgs";
#     systems.url = "github:nix-systems/default";
#     utils.url = "github:ursi/flake-utils";

#     # Purescript stuff
#     purs-nix.url = "github:purs-nix/purs-nix/ps-0.15";
#     ps-tools.follows = "purs-nix/ps-tools";

#     npmlock2nix = {
#       flake = false;
#       url = "github:nix-community/npmlock2nix";
#     };
#   };

#   outputs = {
#     self,
#     utils,
#     nixpkgs,
#     systems,
#     ...
#   } @ inputs: let
#     name = "purely-planning";
#     systems = [
#       "aarch64-darwin"
#       "x86_64-darwin"
#       "x86_64-linux"
#     ];
#     pkgs = nixpkgs.legacyPackages.x86_64-linux;
#     npmlock2nix = (import inputs.npmlock2nix {inherit pkgs;}).v2;
#   in
#     utils.apply-systems
#     {
#       inherit inputs systems;
#     }
#     ({
#       system,
#       pkgs,
#       ...
#     }: let
#       purs-nix = inputs.purs-nix {inherit system;};
#       ps =
#         purs-nix.purs
#         {
#           # Project dir (src, test)
#           srcs = [
#             "src/**/*.purs"
#           ];
#           test = "src/test/**/*.purs";

#           test-module = "Test.Main";
#           # Dependencies
#           dependencies = with purs-nix.ps-pkgs; [
#             aff
#             arrays
#             console
#             datetime
#             dom-indexed
#             effect
#             either
#             enums
#             halogen
#             halogen-formless
#             integers
#             js-uri
#             maybe
#             now
#             ordered-collections
#             prelude
#             psci-support
#             strings
#             tuples
#             typelevel-prelude
#             uri
#             web-html
#           ];

#           # foreign.Main.node_modules = npmlock2nix.node_modules {src = ./.;} + /node_modules;
#         };

#       ps-tools = inputs.ps-tools.legacyPackages.${system};
#       ps-command = ps.command {};
#       pkgs = import nixpkgs {
#         inherit system;
#       };
#       # purs-watch = pkgs.writeShellApplication {
#       #   name = "purs-watch";
#       #   runtimeInputs = with pkgs; [entr ps-command];
#       #   text = "find src/purescript | entr -s 'echo building && purs-nix compile'";
#       # };
#       # purs-test = pkgs.writeShellApplication {
#       #   name = "purs-watch";
#       #   runtimeInputs = with pkgs; [entr ps-command];
#       #   text = "find src/purescript/test | entr -s 'echo running tests && purs-nix test'";
#       # };
#       # vite = pkgs.writeShellApplication {
#       #   name = "vite";
#       #   runtimeInputs = with pkgs; [nodejs];
#       #   text = "npx vite --open";
#       # };
#       # purs-dev = pkgs.writeShellApplication {
#       #   name = "purs-dev";
#       #   runtimeInputs = with pkgs; [concurrently];
#       #   text = "concurrently purs-watch vite";
#       # };
#     in rec {
#       defaultApp = utils.lib.mkApp {
#         type = "app";
#         drv = live-server;
#       };

#       live-server = pkgs.nodePackages.live-server;
#       # packages.default = ps.output {};

#       packages = with ps; {
#         default = ps.modules.Main.bundle {};
#         bundle = bundle {};
#         output = output {};
#       };
#       # bundle.esbuild = {format = "iife";};
#       devShells.default =
#         pkgs.mkShell
#         {
#           inherit name;
#           packages = with pkgs; [
#             ps-command
#             ps-tools.for-0_15.purescript-language-server
#             ps-tools.for-0_15.purs-tidy
#             purs-nix.esbuild
#             purs-nix.purescript
#             nodejs
#             spago
#             nodePackages.parcel

#             yarn2nix

#             # vite
#             # purs-watch
#             # purs-dev
#           ];
#           buildInputs = with pkgs; [
#             nodejs
#             nodePackages.parcel
#             purs-nix.esbuild
#             purs-nix.purescript
#             spago
#             # You can choose pnpm, yarn, or none (npm).

#             nodePackages.pnpm
#             nodePackages.live-server
#           ];
#           shellHook = ''
#             export NIX_SHELL_NAME="Purely Planning"
#             echo "Welcome to the development shell!"
#           '';
#         };
#       apps = {
#         live-server = {
#           type = "app";
#           program = "${live-server}/bin/live-server";
#         };

#         # typescript = {
#         #   type = "app";
#         #   program = "${typescript}/bin/typescript";
#         # };
#       };
#     });

#   # --- Flake Local Nix Configuration ----------------------------
#   nixConfig = {
#     # This sets the flake to use nix cache.
#     # Nix should ask for permission before using it,
#     # but remove it here if you do not want it to.
#     extra-substituters = [
#       "https://klarkc.cachix.org?priority=99"
#       "https://cache.iog.io"
#       "https://cache.zw3rk.com"
#       "https://cache.nixos.org"
#       "https://hercules-ci.cachix.org"
#     ];
#     extra-trusted-public-keys = [
#       "klarkc.cachix.org-1:R+z+m4Cq0hMgfZ7AQ42WRpGuHJumLLx3k0XhwpNFq9U="
#       "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
#       "loony-tools:pr9m4BkM/5/eSTZlkQyRt57Jz7OMBxNSUiMC4FkcNfk="
#       "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
#       "hercules-ci.cachix.org-1:ZZeDl9Va+xe9j+KqdzoBZMFJHVQ42Uu/c/1/KMC5Lw0="
#     ];
#   };
# }
