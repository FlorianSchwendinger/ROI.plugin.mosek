R ?= R

.PHONY: all
all:
	$(MAKE) clean
	$(MAKE) build
	$(MAKE) install
	$(MAKE) check

.PHONY: some
some:
	$(MAKE) clean
	$(MAKE) build
	$(MAKE) install

.PHONY: clean
clean:
	$(RM) ROI.plugin.mosek_0.5-2.tar.gz

.PHONY: build
build:
	$(R) CMD build . --no-build-vignettes

.PHONY: install
install:
	$(R) CMD INSTALL ROI.plugin.mosek_0.5-2.tar.gz --no-multiarch

.PHONY: uninstall
uninstall:
	$(R) CMD REMOVE ROI.plugin.mosek || true

.PHONY: check
check:
	$(R) CMD check ROI.plugin.mosek_0.5-2.tar.gz --as-cran --no-multiarch --no-manual --no-build-vignettes
