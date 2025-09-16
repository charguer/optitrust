
# The makefile is located in the parent folder

doc:
	make -C .. doc

viewdoc:
	make -C .. viewdoc

cleandoc:
	make -C .. cleandoc


#-------------------------------------------------------

# Extract binding information from ../INSTALL.md.
# The output can then be copy-pasted by hand to update 'cheatlist.odt'.

doc_shortcut:
	./make_shortcuts_cheatlist.sh
	@echo ""
	@cat shortcut_cheatlist.txt
