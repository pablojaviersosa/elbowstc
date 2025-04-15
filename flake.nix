{
    inputs =
        {
        nixpkgs = {
          "narHash" = "sha256-tzCdyIJj9AjysC3OuKA+tMD/kDEDAF9mICPDU7ix0JA=";
          "owner" = "NixOS";
          "repo" = "nixpkgs";
          "rev" = "c8cd81426f45942bb2906d5ed2fe21d2f19d95b7";
          "type" = "github";
      };

            flake-utils.url = "github:numtide/flake-utils"; # Para soportar más plataformas
        };

    outputs =
        {
            self,
            nixpkgs,
            flake-utils
        }:
        flake-utils.lib.eachDefaultSystem (system:
            let pkgs = import nixpkgs { inherit system; };
            in
            {
                devShells.default = pkgs.mkShell {
                    nativeBuildInputs = [
                        pkgs.gleam
                        pkgs.erlang
						pkgs.rebar3
                    ];

					 shellHook = ''
					      rebar3 shell --apps my_typing_app
						'';
                };
            }
        );
}
