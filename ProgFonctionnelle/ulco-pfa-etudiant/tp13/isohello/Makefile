all: 
	nix-shell -A client --run "cabal --builddir=dist-client --config-file=config/client.config build"
	mkdir -p static
	find dist-client -name "all.js" -exec cp {} static/ \;
	nix-shell -A server --run "cabal --builddir=dist-server --config-file=config/server.config run server"

clean:
	rm -rf dist-* static/all.js

# build client, in a "nix-shell -A client"
client:
	cabal --builddir=dist-client --config-file=config/client.config build
	mkdir -p static
	find dist-client -name "all.js" -exec cp {} static/ \;

# run server, in a "nix-shell -A server"
server:
	cabal --builddir=dist-server --config-file=config/server.config run server


