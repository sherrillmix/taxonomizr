VERSION:=$(shell grep Version: DESCRIPTION|sed 's/Version: //')
NAME:=$(shell grep Package: DESCRIPTION|sed 's/Package: //')
PACKAGEFILE:=../$(NAME)_$(VERSION).tar.gz

all: $(PACKAGEFILE) README.md docs

.PHONY: all install localInstall

install:
	R -e 'devtools::install_github("sherrillmix/$(NAME)")'

localInstall:
	R -e 'devtools::install()'

#docs: man README.md DESCRIPTION
#	R -e 'chooseCRANmirror(ind=1);pkgdown::build_site()'
#	touch docs

man: R/*.R 
	R -e 'devtools::document()'
	touch man

vignettes/usage.Rmd: usage.template README.Rmd
	cp usage.template vignettes/usage.Rmd
	sed -n '/\#\# Introduction/,$$p' README.Rmd >> vignettes/usage.Rmd

inst/doc: vignettes/usage.Rmd
	R -e 'devtools::build_vignettes()'
	touch inst/doc

README.md: README.Rmd R/*.R
	make localInstall
	R -e 'knitr::opts_chunk$$set(fig.path="README_files/");knitr::knit("README.Rmd")'
	
$(PACKAGEFILE): man R/*.R DESCRIPTION src/*.c tests/testthat/*.R inst/doc
	sed -i "s/^Date:.*$$/Date: `date +%Y-%m-%d`/" DESCRIPTION
	R -e 'devtools::check();devtools::build()'
