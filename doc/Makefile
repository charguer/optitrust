
all: diffs


DIRS = basic combi

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

# 'make open' to view the documentation
open:
	mkdir -p .chromium
	chromium-browser --new-window --user-data-dir=../.chromium --disable-web-security doc_transfo.html

retest:
	cd ../tests/basic; rm label_add_diff.js && make label_add_diff.js


