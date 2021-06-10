all:
	bash -c "$(MAKE) -C .. install 1> >( sed -u 's/\\]:/\\]-/g') 2> >( sed -u 's/make:/make-/g' >&2) 2> >( sed -u 's/File \\\"\\([^,]*\\)\\\", line \\([0-9]*\\),/\\1:\\2:/g' >&2) 1> >( sed -u 's/File \\\"\\([^,]*\\)\\\", line \\([0-9]*\\),/\\1:\\2:/g' >&1)"

