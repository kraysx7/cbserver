#  File:	 GNU Makefile
#  Author:	 Ilya Troshkov
#  Created:	 08.05.2014 13:14:34

VSN = 1.1

CBSYSTEM_ROOT_DIR=/usr/local/lib/cbsystem

APP_RELATIVE_NAME=cbserver
INSTALL_DIR=${CBSYSTEM_ROOT_DIR}/${APP_RELATIVE_NAME}

install:
	echo -n ${INSTALL_DIR}
	@[ -n "${INSTALL_DIR}" ] || (echo "Set INSTALL_DIR before running the install target."; false)
	install -d ${INSTALL_DIR}/ebin
	install -d ${INSTALL_DIR}/priv
	install -d ${INSTALL_DIR}/deps
	rsync -rupE ebin ${INSTALL_DIR}
	rsync -rupE priv ${INSTALL_DIR}
	rsync -rupE deps ${INSTALL_DIR}

echo-version:
	@echo $(VSN)
