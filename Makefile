.POSIX:
EMACS = emacs

compile: weak-ref.elc weak-ref-tests.elc

weak-ref-tests.elc: weak-ref.elc

clean:
	rm -f weak-ref.elc weak-ref-tests.elc

check: weak-ref-tests.elc
	$(EMACS) -batch -Q -L . -l weak-ref-tests.elc -f ert-run-tests-batch

.SUFFIXES: .el .elc
.el.elc:
	$(EMACS) -batch -Q -L . -f batch-byte-compile $<
