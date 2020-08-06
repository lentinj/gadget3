PACKAGE=$(shell awk '/^Package: / { print $$2 }' DESCRIPTION)
VERSION=$(shell awk '/^Version: / { print $$2 }' DESCRIPTION)
TARBALL=$(PACKAGE)_$(VERSION).tar.gz

all: check

install:
	R CMD INSTALL --install-tests --html --example .

# Some things aren't installed by "make install", vignettes for example.
# This is slower, but more accurate.
full-install: build
	R CMD INSTALL --install-tests --html --example "$(TARBALL)"

build:
	grep -qE 'LinkingTo:.*Rcpp' DESCRIPTION && { echo 'Rcpp::compileAttributes()' | R --vanilla --quiet; } || true
	grep -q Roxygen DESCRIPTION && { echo 'devtools::document()' | R --vanilla --quiet; } || true
	R CMD build .

test: install
	for f in tests/test-*.R; do echo "=== $$f ============="; G3_TEST_TMB="y" Rscript $$f || exit 1; done

inttest: install test
	for f in run.R; do echo "=== $$f ============="; G3_TEST_TMB="y" Rscript $$f || exit 1; done

check: build
	R CMD check "$(TARBALL)"

check-as-cran: build
	R CMD check --as-cran "$(TARBALL)"

coverage:
	# NB: TMB transpiling and covr clash badly
	G3_TEST_TMB="" R --vanilla -e 'covr::package_coverage(type = "all", line_exclusions = list("R/run_tmb.R"))'

wincheck: build
	# See https://win-builder.r-project.org/ for more information
	curl --no-epsv -# -T "$(TARBALL)" ftp://win-builder.r-project.org/R-devel/

build-docs:
	R --vanilla -e 'pkgdown::build_site()'

.PHONY: all install build test inttest check check-as-cran coverage wincheck build-docs
