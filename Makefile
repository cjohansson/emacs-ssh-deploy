EMACS = emacs
ifdef emacs
	EMACS = $(emacs)
endif

EMACS_CMD := $(EMACS) -Q -batch -L .

EL  := ssh-deploy-diff-mode.el ssh-deploy-test.el ssh-deploy.el
ELC := $(EL:.el=.elc)

.PHONY: tests
tests:
	$(EMACS_CMD) -l ssh-deploy-test.el

.PHONY: clean
clean:
	rm -f $(ELC)

.PHONY: compile
compile:
	$(EMACS_CMD) -f batch-byte-compile $(EL)
