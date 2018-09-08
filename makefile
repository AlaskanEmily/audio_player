# Any copyright is dedicated to the Public Domain.
# http://creativecommons.org/publicdomain/zero/1.0/

# These flags are for a separate Mercury directory
FJOGG_DIR=fjogg
FJOGG_FLAGS=--search-lib-files-dir fjogg --init-file fjogg/fjogg.init --link-object fjogg/libfjogg.a
CINNAMON_FLAGS=-L Cinnamon --c-include-dir Cinnamon -l cinnamon

all: build_cinnamon build_fjogg
	mmc --make audio_player $(FJOGG_FLAGS) $(CINNAMON_FLAGS) --c-include-dir include -l opus

build_fjogg:
	cd $(FJOGG_DIR) && mmc --make libfjogg

build_cinnamon:
	$(MAKE) -C Cinnamon
	mkdir lib 2> /dev/null || echo > /dev/null
	install -C Cinnamon/libcinnamon.a lib/libcinnamon.a

.PHONY: all build_cinnamon
