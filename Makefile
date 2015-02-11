#  File:	 GNU Makefile
#  Author:	 Ilya Troshkov
#  Created:	 08.05.2014 13:14:34

VSN = 1.0

INSTALL_DIR=/usr/local/lib/cbssystem/cbserver

install:
	@[ -n "$(INSTALL_DIR)" ] || (echo "Set DESTDIR before running the install target."; false)
	install -d $(INSTALL_DIR)/ebin
	install -d $(INSTALL_DIR)/priv
	install -d $(INSTALL_DIR)/deps

	rsync -rupE ebin $(INSTALL_DIR)
	rsync -rupE priv $(INSTALL_DIR)
	rsync -rupE deps $(INSTALL_DIR)

echo-version:
	@echo $(VSN)
