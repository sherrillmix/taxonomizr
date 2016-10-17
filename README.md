# Convert accession numbers to taxonomy

[![Build Status](https://travis-ci.org/sherrillmix/taxonomizr.svg?branch=master)](https://travis-ci.org/sherrillmix/taxonomizr)
[![codecov.io](https://codecov.io/github/sherrillmix/dnaplotr/taxonomizr.svg?branch=master)](https://codecov.io/github/sherrillmix/taxonomizr?branch=master)

## Introduction

`taxonomizr` provides a some simple functions to parse NCBI taxonomy and accession dumps and use them to assign taxonomy to accession numbers.

## Installation
To install directly from github, use the [<code>devtools</code>](https://github.com/hadley/devtools) library and run:

```r
devtools::install_github("sherrillmix/taxonomizr")
```

## Examples

To use the library, include it in R:
```
library(taxonomizr)
```

Then download the necessary files from NCBI:

```r
getNamesAndNodes()
#this is a big download
getAccession2taxid()
```

And process the download files into easily accessed forms:

```r
taxaNodes<-read.nodes('nodes.dmp')
taxaNames<-read.names('names.dmp')
read.accession2taxid(list.files('.','accession2taxid.gz$'),'accessionTaxa.sql')
```

Now everything should be ready for processing. For example, to find the taxonomy associated with NCBI accession number "LN847353.1":

```r
taxaId<-accessionToTaxa("LN847353.1","accessionTaxa.sql")
taxonomy<-getTaxonomy(taxaId,taxaNodes,taxaNames)
```
