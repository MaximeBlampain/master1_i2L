all:
	site build
	find public \( -name '*.html' -o -name '*.css' -o -name '*.js' -o -name '*.svg' \) -print0 | xargs -0 gzip -9 -k -f

watch:
	site watch

clean:
	site clean

clean-rm:
	rm -rf _cache public

