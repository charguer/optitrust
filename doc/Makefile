
all: diffs

DIRS = basic combi

BROWSER ?= chromium-browser

# The function $(call in_each_dir, foo) involves targets foo in every folder from $(DIRS)
define in_each_dir
	$(foreach dir, $(DIRS), make -C ../tests/$(dir) $1;)
endef


# 'make diffs' to generate all the diff files mentioned in the doc
diffs:
	$(call in_each_dir, doc)

# 'make redoc' to force regeneration of all the diff files
redoc:
	$(call in_each_dir, redoc)

# 'make opendoc' to compile and view the documentation
opendoc: diffs open

# 'make open' to view the documentation in a browser
open:
	$(BROWSER) doc_transfo.html

# 'make open_sec' to view the documentation in a chromium browser with security disabled
open_sec:
	mkdir -p .chromium
	chromium-browser --new-window --user-data-dir=../.chromium --disable-web-security doc_transfo.html


