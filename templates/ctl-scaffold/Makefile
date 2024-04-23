.PHONY: requires-nix-shell build bundle serve repl format check \

spago-args :=
purs-args := "--stash --censor-lib --censor-codes=ImplicitImport,ImplicitQualifiedImport,UserDefinedWarning"

requires-nix-shell:
	@[ "$(IN_NIX_SHELL)" ] || \
		( echo "The '$(MAKECMDGOALS)' target must be run from inside a nix shell, run 'nix develop' first." \
				&& false \
		)

format:
	@nix run .#pursFormat && nix run .#jsFormat && nix run .#nixFormat

check:
	@nix build .#checks.x86_64-linux.all

build: requires-nix-shell
	spago ${spago-args} build --purs-args ${purs-args}

bundle: build requires-nix-shell
	node bundle.js && tsc --emitDeclarationOnly

repl: requires-nix-shell
	spago repl