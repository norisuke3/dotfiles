EMACS_D     = ${HOME}/.emacs.d
EMACS_LOCAL = ${HOME}/.emacs.d.local
SITE_LISP   = ${EMACS_D}/site-lisp

.PHONY: all
all: ;

.PHONY: tasks
tasks:
	cat Makefile | grep ^.PHONY

.PHONY: symlink
symlink:
	ln -svf `pwd`/.emacs.d ${HOME}
	ln -svf `pwd`/.zshrc ${HOME}
	ln -svf `pwd`/.screenrc ${HOME}
	ln -svf `pwd`/.zshrc.bindkeys ${HOME}
