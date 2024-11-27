.PHONY: run-oura-daemon

clear-sockets:
	@rm ./oura-daemon-cursor || true
	@rm ./oura-listening.socket || true
	@rm ./own.socket || true

run-oura-daemon:
	@clear-sockets || true
	@oura daemon --config ./test/daemon.toml

format:
	fourmolu --mode inplace $$(git ls-files '*.hs')