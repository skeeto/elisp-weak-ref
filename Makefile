.POSIX:
EMACS = emacs

compile: weak-ref.elc

clean:
	rm -f weak-ref.elc

.SUFFIXES: .el .elc
.el.elc:
	$(EMACS) -batch -Q -L . -f batch-byte-compile $<
