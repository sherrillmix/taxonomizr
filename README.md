# Convert accession numbers to taxonomy

[![Build Status](https://travis-ci.org/sherrillmix/taxonomizr.svg?branch=master)](https://travis-ci.org/sherrillmix/taxonomizr)
[![codecov](https://codecov.io/gh/sherrillmix/taxonomizr/branch/master/graph/badge.svg)](https://codecov.io/gh/sherrillmix/taxonomizr)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/taxonomizr)](https://cran.r-project.org/package=taxonomizr)

## Introduction

`taxonomizr` provides some simple functions to parse NCBI taxonomy files and accession dumps and efficiently use them to assign [taxonomy](https://www.ncbi.nlm.nih.gov/Taxonomy/taxonomyhome.html/) to accession numbers or taxonomic IDs. This is useful for example to assign taxonomy to BLAST results. This is all done locally after downloading the appropriate files from NCBI using included functions (see [below](#preparation)). 

The major functions are:
 * `prepareDatabase`: download data from NCBI and prepare SQLite database
 * `accessionToTaxa`: convert accession numbers to taxonomic IDs
 * `getTaxonomy`: convert taxonomic IDs to taxonomy

More specialized functions are:
 * `getId`: convert a biological name to taxonomic ID
 * `getAccessions`: find accessions for a given taxonomic ID

And a simple use case might look like (see below for more details):


```r
library(taxonomizr)
#note this will require a lot of hard drive space, bandwidth and time to process all the data from NCBI
prepareDatabase('accessionTaxa.sql')
blastAccessions<-c("Z17430.1","Z17429.1","X62402.1") 
ids<-accessionToTaxa(blastAccessions,'accessionTaxa.sql')
getTaxonomy(ids,'accessionTaxa.sql')
```

## Requirements
This package downloads a few databases from NCBI and stores them in an easily accessible form on the hard drive. This ends up taking a decent amount of space so you'll probably want around 75 Gb of free hard drive space. 

## Installation
The package is on CRAN, so it should install with a simple:

```r
install.packages("taxonomizr")
```
If you want the development version directly from github, use the [<code>devtools</code>](https://github.com/hadley/devtools) library and run:

```r
devtools::install_github("sherrillmix/taxonomizr")
```

To use the library, load it in R:

```r
library(taxonomizr)
```

## Preparation<a name="preparation"></a>
Since version 0.5.0, there is a simple function to run all preparations. Note that you'll need a bit of time, download bandwidth and hard drive space before running this command (we're downloading taxonomic assignments for every record in NCBI). To create a SQLite database called `accessionTaxa.sql` in the current working directory (you may want to store this somewhere more centrally located so it does not need to be duplicated with every project), we can run:


```r
prepareDatabase('accessionTaxa.sql')
```

```
## Downloading names and nodes with getNamesAndNodes()
```

```
## Downloading accession2taxid with getAccession2taxid()
```

```
## This can be a big (several gigabytes) download. Please be patient and use a fast connection.
```

```
## Preprocessing names with read.names.sql()
```

```
## Preprocessing nodes with read.nodes.sql()
```

```
## Preprocessing accession2taxid with read.accession2taxid()
```

```
## Reading ./nucl_gb.accession2taxid.gz.
```

```
## Reading ./nucl_est.accession2taxid.gz.
```

```
## Reading ./nucl_gss.accession2taxid.gz.
```

```
## Reading ./nucl_wgs.accession2taxid.gz.
```

```
## Reading in values. This may take a while.
```

```
## Adding index. This may also take a while.
```

```
## [1] "accessionTaxa.sql"
```

If everything works then that should have prepared a SQLite database ready for use. You can skip the "Manual preparation" steps below.

All files are cached locally and so the preparation is only required once (delete/rename the SQLite database and recall the function to regenerate the database). It is not necessary to manually check for the presence of the database since the function checks to see if SQLite database is present and if so skips downloading/processing. For example, running the command again produces:


```r
prepareDatabase('accessionTaxa.sql')
```

```
## SQLite database accessionTaxa.sql already exists. Delete to regenerate
```

```
## [1] "accessionTaxa.sql"
```



## Assigning taxonomy

### Finding taxonomy for NCBI accession numbers

Now we are ready to convert NCBI accession numbers to taxonomic IDs. For example, to find the taxonomic IDs associated with NCBI accession numbers "LN847353.1" and "AL079352.3":

```r
taxaId<-accessionToTaxa(c("LN847353.1","AL079352.3"),"accessionTaxa.sql")
print(taxaId)
```

```
## [1] 1313 9606
```

And to get the taxonomy for those IDs:


```r
getTaxonomy(taxaId,'accessionTaxa.sql')
```

```
##      superkingdom phylum       class      order            
## 1313 "Bacteria"   "Firmicutes" "Bacilli"  "Lactobacillales"
## 9606 "Eukaryota"  "Chordata"   "Mammalia" "Primates"       
##      family             genus           species                   
## 1313 "Streptococcaceae" "Streptococcus" "Streptococcus pneumoniae"
## 9606 "Hominidae"        "Homo"          "Homo sapiens"
```

You can also get taxonomy for NCBI accession numbers without versions (the .X following the main number e.g. the ".1" in LN847353.1) using the `version='base'` argument of `accessionToTaxa`:



```r
taxaId<-accessionToTaxa(c("LN847353","AL079352"),"accessionTaxa.sql")
print(taxaId)
```

```
## [1] NA NA
```


```r
taxaId<-accessionToTaxa(c("LN847353","AL079352"),"accessionTaxa.sql",version='base')
print(taxaId)
```

```
## [1] 1313 9606
```



### Finding taxonomy for taxonomic names

If you'd like to find IDs for taxonomic names then you can do something like:

```r
taxaId<-getId(c('Homo sapiens','Bos taurus','Homo','Alces alces'),'accessionTaxa.sql')
print(taxaId)
```

```
## [1] "9606" "9913" "9605" "9852"
```

And again to get the taxonomy for those IDs use `getTaxonomy`:


```r
taxa<-getTaxonomy(taxaId,'accessionTaxa.sql')
print(taxa)
```

```
##      superkingdom phylum     class      order      family      genus  
## 9606 "Eukaryota"  "Chordata" "Mammalia" "Primates" "Hominidae" "Homo" 
## 9913 "Eukaryota"  "Chordata" "Mammalia" NA         "Bovidae"   "Bos"  
## 9605 "Eukaryota"  "Chordata" "Mammalia" "Primates" "Hominidae" "Homo" 
## 9852 "Eukaryota"  "Chordata" "Mammalia" NA         "Cervidae"  "Alces"
##      species       
## 9606 "Homo sapiens"
## 9913 "Bos taurus"  
## 9605 NA            
## 9852 "Alces alces"
```

### Condensing taxonomy
You can use the `condenseTaxa` function to find the agreements among taxonomic hits. For example to condense the taxonomy from the previous section to the lowest taxonomic rank shared by all three taxa:


```r
condenseTaxa(taxa)
```

```
##   superkingdom phylum     class      order family genus species
## 1 "Eukaryota"  "Chordata" "Mammalia" NA    NA     NA    NA
```

This function can also be fed a large number of grouped hits, e.g. BLAST hits for high throughput sequencing reads after filtering for the best hits for each read, and output a condensed taxonomy for each grouping:


```r
groupings<-c('read1','read2','read1','read2')
condenseTaxa(taxa,groupings)
```

```
##       superkingdom phylum     class      order      family      genus 
## read1 "Eukaryota"  "Chordata" "Mammalia" "Primates" "Hominidae" "Homo"
## read2 "Eukaryota"  "Chordata" "Mammalia" NA         NA          NA    
##       species
## read1 NA     
## read2 NA
```

### Finding accessions for a given taxonomic ID

To find all the accessions for a given taxonomic ID, you can use the `getAccessions` function. This is a bit of an unusual use case so to preserve space, an index is not created by default in `read.accession2taxid`. If you are going to use this function, you will want to rebuild the SQLite database with the `indexTaxa` argument set to true with something like:


```r
read.accession2taxid(list.files('.','accession2taxid.gz$'),'accessionTaxa.sql',indexTaxa=TRUE)
```

Then you can get the accessions for taxa 3702 with a command like (note that the limit argument is used here in order to preserve space):


```r
getAccessions(3702,'accessionTaxa.sql',limit=10)
```

```
##    taxa accession
## 1  3702  Z17427.1
## 2  3702  Z17428.1
## 3  3702  Z17429.1
## 4  3702  Z17430.1
## 5  3702  Z17431.1
## 6  3702  Z17432.1
## 7  3702  Z17433.1
## 8  3702  Z17434.1
## 9  3702  Z17435.1
## 10 3702  Z17436.1
```

## Switch from data.table to SQLite
Version 0.5.0 marked a change for name and node lookups from using data.table to using SQLite. This was necessary to increase performance (10-100x speedup for `getTaxonomy`) and create a simpler interface (a single SQLite database contains all necessary data). Unfortunately, this switch requires a couple breaking changes: 
  * `getTaxonomy` changes from `getTaxonomy(ids,namesDT,nodesDT)` to `getTaxonomy(ids,sqlFile)`
  * `getId` changes from  `getId(taxa,namesDT)` to `getId(taxa,sqlFile)`
  * `read.names` is deprecated, instead use `read.names.sql`. For example, instead of calling `names<-read.names('names.dmp')` in every session, simply call `read.names.sql('names.dmp','accessionTaxa.sql')` once (or use the convenient `prepareDatabase` as <a href='#preparation'>above</a>)).
  * `read.nodes` is deprecated, instead use `read.names.sql`. For example. instead of calling `nodes<-read.names('nodes.dmp')` in every session, simply call `read.nodes.sql('nodes.dmp','accessionTaxa.sql')` once (or use the convenient `prepareDatabase` as <a href='#preparation'>above</a>).

  I've tried to ease any problems with this by overloading `getTaxonomy` and `getId` to still function (with a warning) if passed a data.table names and nodes argument and providing a simpler `prepareDatabase` function for completing all setup steps (hopefully avoiding direct calls to `read.names` and `read.nodes` for most users). 

I plan to eventually remove data.table functionality to avoid a split codebase so please switch to the new SQLite format in all new code.
 

## Manual preparation of database (usually not necessary)
**Note:** Since version 0.5.0, it is usually not necessary to run the following manually, the function `prepareDatabase()` should do most of this automatically for you (see <a href='#preparation'>above</a>).

In order to avoid constant internet access and slow APIs, the first step in using the package is to downloads all necessary files from NCBI. This uses a bit of disk space but makes future access reliable and fast.

**Note:** It is not necessary to manually check for the presence of these files since the functions automatically check to see if their output is present and if so skip downloading/processing. Delete the local files if you would like to redownload or reprocess them.

### Download names and nodes
First, download the necessary names and nodes files from [NCBI](ftp://ftp.ncbi.nih.gov/pub/taxonomy/):

```r
getNamesAndNodes()
```

```
## [1] "./names.dmp" "./nodes.dmp"
```

### Download accession to taxa files

Then download accession to taxa id conversion files from [NCBI](ftp://ftp.ncbi.nih.gov/pub/taxonomy/accession2taxid/). **Note:** this is a pretty _big_ download (several gigabytes):

```r
#this is a big download
getAccession2taxid()
```

```
## [1] "./nucl_gb.accession2taxid.gz"  "./nucl_est.accession2taxid.gz"
## [3] "./nucl_gss.accession2taxid.gz" "./nucl_wgs.accession2taxid.gz"
```

If you would also like to identify protein accession numbers, also download the prot file from NCBI (again this is a _big_ download):

```r
#this is a big download
getAccession2taxid(types='prot')
```

```
## [1] "./prot.accession2taxid.gz"
```

### Convert names, nodes and accessions to database
Then process the downloaded names and nodes files into a more easily accessed form:


```r
read.names.sql('names.dmp','accessionTaxa.sql')
read.nodes.sql('nodes.dmp','accessionTaxa.sql')
```

Next process the downloaded accession files into the same database (this one could take a while):


```r
read.accession2taxid(list.files('.','accession2taxid.gz$'),'accessionTaxa.sql')
```

```
## Reading nucl_est.accession2taxid.gz.
```

```
## Reading nucl_gb.accession2taxid.gz.
```

```
## Reading nucl_gss.accession2taxid.gz.
```

```
## Reading nucl_wgs.accession2taxid.gz.
```

```
## Reading in values. This may take a while.
```

```
## Adding index. This may also take a while.
```

Now everything should be ready for processing. All files are cached locally and so the preparation is only required once (or whenever you would like to update the data). It is not necessary to manually check for the presence of these files since the functions automatically check to see if their output is present and if so skip downloading/processing. Delete the local files if you would like to redownload or reprocess them.




