
all:
	nix-shell -A env --run "cabal --builddir=dist-ghcjs --config-file=cabal.config build"
	find dist-ghcjs/ -name "all.js" -exec cp {} public/ \;
	find public \( -name '*.html' -o -name '*.css' -o -name '*.js' -o -name '*.svg' \) -print0 | xargs -0 gzip -9 -k -f

clean:
	rm -rf dist-ghcjs
	rm -rf public/all.js public/*.gz

