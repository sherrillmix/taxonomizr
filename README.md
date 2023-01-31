# Convert accession numbers to taxonomy

[![Build Status](https://travis-ci.org/sherrillmix/taxonomizr.svg?branch=master)](https://travis-ci.org/sherrillmix/taxonomizr)
[![codecov](https://codecov.io/gh/sherrillmix/taxonomizr/branch/master/graph/badge.svg)](https://app.codecov.io/gh/sherrillmix/taxonomizr)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/taxonomizr)](https://cran.r-project.org/package=taxonomizr)

## Note: NCBI Name changes in early 2023
Please note that the [NCBI is planning to change their naming of several major prokaryote phylums](https://ncbiinsights.ncbi.nlm.nih.gov/2022/11/14/prokaryotic-phylum-name-changes/) e.g. [Firmicutes will become Bacillota](https://ftp.ncbi.nih.gov/pub/taxonomy/Major_phylum_updates_for_prokaryotes_2023.txt). The exact date that this transition will percolate into the taxonomy downloads used for this package is not precisely defined but it seems likely to be sometime early in 2023.

Please watch out for any problems that could arise. For example: 
  * names of assigned taxonomy may shift after updating a database to a post-change version
  * comparisons of old analyses performed pre-change to new analyses performed post-change will need to be done with care

If I understand things correctly, then the actual taxonomy ID will not change so it might be wise to retain the taxonomy ID for all analyses. Then on final analysis, the taxonomic names can be assigned based on whatever naming scheme is in use at that time.

## Introduction

`taxonomizr` provides some simple functions to parse NCBI taxonomy files and accession dumps and efficiently use them to assign [taxonomy](https://www.ncbi.nlm.nih.gov/Taxonomy/taxonomyhome.html/) to accession numbers or taxonomic IDs. This is useful for example to assign taxonomy to BLAST results. This is all done locally after downloading the appropriate files from NCBI using included functions (see [below](#preparation)). 

The major functions are:
 * `prepareDatabase`: download data from NCBI and prepare SQLite database
 * `accessionToTaxa`: convert accession numbers to taxonomic IDs
 * `getTaxonomy`: convert taxonomic IDs to taxonomy

More specialized functions are:
 * `getId`: convert a biological name to taxonomic ID
 * `getRawTaxonomy`: find all taxonomic ranks for a taxonomic ID
 * `normalizeTaxa`: combine raw taxonomies with different taxonomic ranks
 * `condenseTaxa`: condense a set of taxa to their most specific common branch
 * `makeNewick`: generate a Newick formatted tree from taxonomic output
 * `getAccessions`: find accessions for a given taxonomic ID
 * `getDescendants`: find descendants for a given taxonomic ID

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
If you want the development version directly from github, use the [<code>devtools</code>](https://github.com/r-lib/devtools) library and run:

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

Note that if you only want the taxonomic data and do not want to assign taxonomy to accession ID then you can just get the much smaller `names` and `nodes` data sets and exclude the large download and time consuming databasing of accession IDs by setting `getAccessions=FALSE` e.g.:



```r
prepareDatabase(getAccessions=FALSE)
```

```
## Downloading names and nodes with getNamesAndNodes()
##  [100%] Downloaded 57373562 bytes...
##  [100%] Downloaded 49 bytes...
## Preprocessing names with read.names.sql()
## Preprocessing nodes with read.nodes.sql()
## [1] "nameNode.sqlite"
```

And if you area assigning taxonomy to protein data, then you would want to grab the `prot.accession2taxid.gz` from NCBI by specifying the `types='prot'` argument (or `types=c("nucl_gb", "nucl_wgs","prot")` for proteins and nucleotides):


```r
prepareDatabase(types='prot')
```

```
## Downloading names and nodes with getNamesAndNodes()
```

```
## Preprocessing names with read.names.sql()
```

```
## Preprocessing nodes with read.nodes.sql()
```

```
## Downloading accession2taxid with getAccession2taxid()
```

```
## This can be a big (several gigabytes) download. Please be patient and use a fast connection.
```

```
## Preprocessing accession2taxid with read.accession2taxid()
```

```
## Reading ./prot.accession2taxid.gz.
```

```
## Reading in values. This may take a while.
```

```
## Adding index. This may also take a while.
```

```
## [1] "nameNode.sqlite"
```



## Assigning taxonomy

### Producing accession numbers

NCBI accession numbers are often obtained when doing a BLAST search (usually the second column of output from blastn, blastx, blastp, ...). For example the output might look like:

```
read1   gi|326539903|gb|CP002582.1|     69.68   1745    448     69      3       1702    3517898 3519606 3e-169  608
read2   gi|160426828|gb|CP000885.1|     68.46   1763    452     82      3       1711    1790367 1788655 4e-140  511
...
```

So to identify a taxon for a given sequence you would blast it against e.g. the NCBI nt database and load the results into R. For NCBI databases, the accession number is often the 4th item in the `|` (pipe) separated reference field (often the second column in a tab separated result). For example, the `CP002582.1` in the gi|326539903|gb|**CP002582.1**| above.

So just as an example, reading in blast results might look something like:


```r
blastResults<-read.table('XXXX.blast',header=FALSE,stringsAsFactors=FALSE)
#grab the 4th |-separated field from the reference name in the second column
accessions<-sapply(strsplit(blastResults[,2],'\\|'),'[',4)
```


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
### Finding descendants for a given taxa
The function `getDescents` can be used to find all the descendants at a taxonomic level for a given taxa. For example to find all species (the default) in the Homininae subfamily (taxonomic ID 207598):

```r
getDescendants(207598,'accessionTaxa.sql')
```

```
## [1] "Gorilla gorilla"                   "Gorilla beringei"                 
## [3] "Pan paniscus"                      "Pan troglodytes"                  
## [5] "Homo sapiens"                      "Homo heidelbergensis"             
## [7] "Homo sapiens environmental sample" "Homo sp."
```

Or all genuses:

```r
getDescendants(207598,'accessionTaxa.sql','genus')
```

```
## [1] "Gorilla" "Pan"     "Homo"
```


Note that an index for the nodes table was added in v0.10.1 to make this run faster. If your database was created prior to v0.10.1 and you need maximum speed for finding descendants then then please regenerate the database.

### Finding common names for taxonomic IDs
If you'd like to find all common and other types of names for a given taxa ID then you can use `getCommon`:


```r
getCommon(c(9913,9606),'accessionTaxa.sql')
```

```
## [[1]]
##                         name                type
## 1                  Bos bovis             synonym
## 2     Bos primigenius taurus             synonym
## 3  Bos taurus Linnaeus, 1758           authority
## 4                 Bos taurus     scientific name
## 5      Bovidae sp. Adi Nefas            includes
## 6                     bovine         common name
## 7                     cattle genbank common name
## 8                        cow         common name
## 9                  dairy cow         common name
## 10           domestic cattle         common name
## 11              domestic cow         common name
## 12                        ox         common name
## 13                      oxen         common name
## 
## [[2]]
##                          name                type
## 1 Homo sapiens Linnaeus, 1758           authority
## 2                Homo sapiens     scientific name
## 3                       human genbank common name
```

Or specify only a certain type(s) of name ("common" names seem to often be split between "common name" and "genbank common name"):


```r
getCommon(c(9913,9606,9894),'accessionTaxa.sql',c('genbank common name','common name'))
```

```
## [[1]]
##              name                type
## 1          bovine         common name
## 2          cattle genbank common name
## 3             cow         common name
## 4       dairy cow         common name
## 5 domestic cattle         common name
## 6    domestic cow         common name
## 7              ox         common name
## 8            oxen         common name
## 
## [[2]]
##    name                type
## 1 human genbank common name
## 
## [[3]]
##      name                type
## 1 giraffe genbank common name
```

Note that databases created with `taxonomizr` versions earlier than v0.9.4 do not contain the `type` field and so the database will have to be reloaded to use this function. For example, this could be done by calling:


```r
taxonomizr::getNamesAndNodes()
taxonomizr::read.names.sql('names.dmp','nameNode.sqlite',overwrite=TRUE)
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

### Find all taxonomic assignments for a given taxa
To get all taxonomic assignments for a given taxa regardless of their particular rank, you can use the `getRawTaxonomy` function. Note that there are often many intermediate ranks outside the more common taxonomic ranks. The function returns a list since different IDs can have differing numbers of ranks. It is used similarly to `getTaxonomy`:


```r
getRawTaxonomy(c(9606,9913),'accessionTaxa.sql')
```

```
## $`9606`
##                species                  genus              subfamily 
##         "Homo sapiens"                 "Homo"            "Homininae" 
##                 family            superfamily              parvorder 
##            "Hominidae"           "Hominoidea"           "Catarrhini" 
##             infraorder               suborder                  order 
##          "Simiiformes"          "Haplorrhini"             "Primates" 
##             superorder                  clade                clade.1 
##     "Euarchontoglires"        "Boreoeutheria"             "Eutheria" 
##                clade.2                  class                clade.3 
##               "Theria"             "Mammalia"              "Amniota" 
##                clade.4                class.1             superclass 
##            "Tetrapoda" "Dipnotetrapodomorpha"        "Sarcopterygii" 
##                clade.5                clade.6                clade.7 
##         "Euteleostomi"           "Teleostomi"        "Gnathostomata" 
##                clade.8              subphylum                 phylum 
##           "Vertebrata"             "Craniata"             "Chordata" 
##                clade.9               clade.10               clade.11 
##        "Deuterostomia"            "Bilateria"            "Eumetazoa" 
##                kingdom               clade.12           superkingdom 
##              "Metazoa"         "Opisthokonta"            "Eukaryota" 
##                no rank 
##   "cellular organisms" 
## 
## $`9913`
##                species                  genus              subfamily 
##           "Bos taurus"                  "Bos"              "Bovinae" 
##                 family             infraorder               suborder 
##              "Bovidae"               "Pecora"           "Ruminantia" 
##                  order             superorder                  clade 
##         "Artiodactyla"       "Laurasiatheria"        "Boreoeutheria" 
##                clade.1                clade.2                  class 
##             "Eutheria"               "Theria"             "Mammalia" 
##                clade.3                clade.4                class.1 
##              "Amniota"            "Tetrapoda" "Dipnotetrapodomorpha" 
##             superclass                clade.5                clade.6 
##        "Sarcopterygii"         "Euteleostomi"           "Teleostomi" 
##                clade.7                clade.8              subphylum 
##        "Gnathostomata"           "Vertebrata"             "Craniata" 
##                 phylum                clade.9               clade.10 
##             "Chordata"        "Deuterostomia"            "Bilateria" 
##               clade.11                kingdom               clade.12 
##            "Eumetazoa"              "Metazoa"         "Opisthokonta" 
##           superkingdom                no rank 
##            "Eukaryota"   "cellular organisms"
```

These raw taxonomy with varying numbers of levels can be normalized so that all taxa share the same number of levels (aligning by taxonomic levels that are not the unspecific "clade") using the `normalizeTaxa` function:



```r
raw<-getRawTaxonomy(c(9606,9913),'accessionTaxa.sql')
normalizeTaxa(raw)
```

```
##      no rank              superkingdom superkingdom.1 kingdom   kingdom.1  
## 9606 "cellular organisms" "Eukaryota"  "Opisthokonta" "Metazoa" "Eumetazoa"
## 9913 "cellular organisms" "Eukaryota"  "Opisthokonta" "Metazoa" "Eumetazoa"
##      kingdom.2   kingdom.3       phylum     subphylum  subphylum.1 
## 9606 "Bilateria" "Deuterostomia" "Chordata" "Craniata" "Vertebrata"
## 9913 "Bilateria" "Deuterostomia" "Chordata" "Craniata" "Vertebrata"
##      subphylum.2     subphylum.3  subphylum.4    superclass     
## 9606 "Gnathostomata" "Teleostomi" "Euteleostomi" "Sarcopterygii"
## 9913 "Gnathostomata" "Teleostomi" "Euteleostomi" "Sarcopterygii"
##      superclass.1           superclass.2 superclass.3 class      class.1 
## 9606 "Dipnotetrapodomorpha" "Tetrapoda"  "Amniota"    "Mammalia" "Theria"
## 9913 "Dipnotetrapodomorpha" "Tetrapoda"  "Amniota"    "Mammalia" "Theria"
##      class.2    class.3         superorder         order          suborder     
## 9606 "Eutheria" "Boreoeutheria" "Euarchontoglires" "Primates"     "Haplorrhini"
## 9913 "Eutheria" "Boreoeutheria" "Laurasiatheria"   "Artiodactyla" "Ruminantia" 
##      infraorder    parvorder    superfamily  family      subfamily   genus 
## 9606 "Simiiformes" "Catarrhini" "Hominoidea" "Hominidae" "Homininae" "Homo"
## 9913 "Pecora"      NA           NA           "Bovidae"   "Bovinae"   "Bos" 
##      species       
## 9606 "Homo sapiens"
## 9913 "Bos taurus"
```

`normalizeTaxa` does its best to figure out the order of taxonomic levels automatically but can sometimes be left with ambiguous cases. This will result in an error like:


```
Error in topoSort(c(nonClade, list(lineageOrder)), errorIfAmbiguous = TRUE) : 
  Ambiguous ordering found in topoSort (suborder vs infraorder)
```

That's saying that the algorithm is unclear from the data whether suborder or infraorder is the more specific taxonomic level. To clarify, give the `lineageOrder` parameter a vector going from most to least specific like:

```
normalizeTaxa(raw,lineageOrder=c('infraorder','suborder'))
```

For especially troublesome sets, you may have to repeat this step several times getting a new error each time to find all the ambiguities. This would result in building up a vector specifying the ordering of several ambiguous levels like:

```
normalizeTaxa(raw,lineageOrder=c('infraorder','suborder','superorder','infraclass','subclass','class'))

```


### Finding accessions for a given taxonomic ID

To find all the accessions for a given taxonomic ID, you can use the `getAccessions` function. This is a bit of an unusual use case so to preserve space, an index is not created by default in `read.accession2taxid`. If you are going to use this function, you will want to rebuild the SQLite database with the `indexTaxa` argument set to true with something like:


```r
read.accession2taxid(list.files('.','accession2taxid.gz$'),'accessionTaxa.sql',indexTaxa=TRUE,overwrite=TRUE)
```

```
## Reading nucl_gb.accession2taxid.gz.
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

Then you can get the accessions for taxa 3702 with a command like (note that the limit argument is used here in order to preserve space):


```r
getAccessions(3702,'accessionTaxa.sql',limit=10)
```

```
##    taxa accession
## 1  3702  X58148.1
## 2  3702  X66414.1
## 3  3702  X60045.1
## 4  3702  X07376.1
## 5  3702  X54927.1
## 6  3702  X54926.1
## 7  3702  X54928.1
## 8  3702  X54930.1
## 9  3702  X54929.1
## 10 3702  X52320.1
```

### Convert taxonomy to Newick tree

This is probably only useful in a few specific cases but a convenience function `makeNewick` to convert taxonomy into a Newick tree is included. The function takes a matrix with columns corresponding to taxonomic categories and rows corresponding to taxonomic assignments, e.g. the output from `condenseTaxa` or `getTaxonomy` or `normalizeTaxa` and reduces it to a Newick formatted tree. For example:




```r
taxa
```

```
##      [,1]        [,2]       [,3]       [,4]       [,5]        [,6]   
## [1,] "Eukaryota" "Chordata" "Mammalia" "Primates" "Hominidae" "Homo" 
## [2,] "Eukaryota" "Chordata" "Mammalia" "Primates" "Hominidae" "Pan"  
## [3,] "Eukaryota" "Chordata" "Mammalia" NA         "Cervidae"  "Alces"
```

```r
makeNewick(taxa)
```

```
## [1] "((((((Homo,Pan)Hominidae)Primates,((Alces)Cervidae)_)Mammalia)Chordata)Eukaryota);"
```

If quotes are needed, then specify the `quote` argument:


```r
makeNewick(taxa,quote="'")
```

```
## [1] "(((((('Homo','Pan')'Hominidae')'Primates',(('Alces')'Cervidae')_)'Mammalia')'Chordata')'Eukaryota');"
```

By default, `makeNewick` includes trailing nodes that are all NA in the tree e.g.:


```r
taxa[3,3:6]<-NA
print(taxa)
```

```
##      [,1]        [,2]       [,3]       [,4]       [,5]        [,6]  
## [1,] "Eukaryota" "Chordata" "Mammalia" "Primates" "Hominidae" "Homo"
## [2,] "Eukaryota" "Chordata" "Mammalia" "Primates" "Hominidae" "Pan" 
## [3,] "Eukaryota" "Chordata" NA         NA         NA          NA
```

```r
makeNewick(taxa)
```

```
## [1] "((((((Homo,Pan)Hominidae)Primates)Mammalia,(((_)_)_)_)Chordata)Eukaryota);"
```

If these nodes are not desired then set `excludeTerminalNAs` to `FALSE`:

```r
makeNewick(taxa,excludeTerminalNAs=TRUE)
```

```
## [1] "((((((Homo,Pan)Hominidae)Primates)Mammalia)Chordata)Eukaryota);"
```

Note that taxa may be the most specific taxon for a given taxa in the taxonomy matrix but will not be a leaf in the resulting tree if it appears in other taxonomy e.g. Chordata in this example. 



## Changelog
### v0.10.2
  * Behind the scenes switch to `multi_download` function from `curl` package to allow download resumption on interrupted downloads. This adds a dependency that `curl` package be >=5.0.0.
  * Add `protocol` option to choose between FTP and HTTP protocols for downloading. The two protocols should perform similarly and the relative speeds of NCBI's ftp and http servers seem to vary so probably not a whole lot of reason to choose one over the other unless a firewall is blocking FTP ports.

### v0.10.1
  * Add `getDescendants` function to get all descendants for a given taxon

### v0.9.4
  * Add `getCommon` function to get all names in the database for a given taxa ID

### v0.9.3
  * Fix bug in testing script

### v0.9.2
  * Allow factors as input to `accessionToTaxa`
  * Document sqlite pragmas for `read.accession2taxid`
  * Inherit ... argument documentation for `prepareDatabase`
  * Catch input/output error while processing large files
  * Update various user-facing links from ftp to https for easier access

### v0.8.4
  * Add quote option to `makeNewick`
  * Trim trailing NAs off the tree in `makeNewick` if `excludeTerminalNAs` is TRUE
  * Add terminal semicolon to end of `makeNewick` tree unless `terminator` is NULL

### v0.8.3
  * Add "no rank" to `normalizeTaxa`'s default exclusion
  * Expand README

### v0.8.2
  * Add `normalizeTaxa` function

### v0.8.1
  * Fix minor typos

### v0.8.0
  * Switch to `curl::curl_download` to avoid Windows issues

### v0.7.1
  * Add md5 check for downloads

### v0.7.0
  * Add `getRawTaxonomy` function
  * Add option to not download accessions

### v0.6.0
  * Fix named vector bug in `accessionToTaxa`
  * Add `makeNewick` function
  * Deal with default 60 second timeout for downloads in R

### v0.5.3
  * Remove `nucl_est` and `nucl_gss` from defaults since NCBI folded them into `nucl_gb` and removed
  * Squash R:devel bug

### v0.5.0
  * Transitioned from data.table to SQLite
  * Addeded convenience `prepareDatabase()` function
  * Squashed Windows testing errors

## Manual preparation of database (usually not necessary)
**Note:** Since version 0.5.0, it is usually not necessary to run the following manually, the function `prepareDatabase()` should do most of this automatically for you (see <a href='#preparation'>above</a>).

In order to avoid constant internet access and slow APIs, the first step in using the package is to downloads all necessary files from NCBI. This uses a bit of disk space but makes future access reliable and fast.

**Note:** It is not necessary to manually check for the presence of these files since the functions automatically check to see if their output is present and if so skip downloading/processing. Delete the local files if you would like to redownload or reprocess them.

### Download names and nodes
First, download the necessary names and nodes files from [NCBI](https://ftp.ncbi.nih.gov/pub/taxonomy/):

```r
getNamesAndNodes()
```

```
## [1] "./names.dmp" "./nodes.dmp"
```

### Download accession to taxa files

Then download accession to taxa id conversion files from [NCBI](https://ftp.ncbi.nih.gov/pub/taxonomy/accession2taxid/). **Note:** this is a pretty _big_ download (several gigabytes):

```r
#this is a big download
getAccession2taxid()
```

```
## This can be a big (several gigabytes) download. Please be patient and use a fast connection.
```

```
## [1] "./nucl_gb.accession2taxid.gz"  "./nucl_wgs.accession2taxid.gz"
```


If you would also like to identify protein accession numbers, also download the prot file from NCBI (again this is a _big_ download):

```r
#this is a big download
getAccession2taxid(types='prot')
```

```
## This can be a big (several gigabytes) download. Please be patient and use a fast connection.
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
## Reading nucl_gb.accession2taxid.gz.
```

```
## Reading nucl_wgs.accession2taxid.gz.
```

```
## Reading prot.accession2taxid.gz.
```

```
## Reading in values. This may take a while.
```

```
## Adding index. This may also take a while.
```


Now everything should be ready for processing. All files are cached locally and so the preparation is only required once (or whenever you would like to update the data). It is not necessary to manually check for the presence of these files since the functions automatically check to see if their output is present and if so skip downloading/processing. Delete the local files if you would like to redownload or reprocess them.



## Switch from data.table to SQLite
Version 0.5.0 marked a change for name and node lookups from using data.table to using SQLite. This was necessary to increase performance (10-100x speedup for `getTaxonomy`) and create a simpler interface (a single SQLite database contains all necessary data). Unfortunately, this switch requires a couple breaking changes: 
  * `getTaxonomy` changes from `getTaxonomy(ids,namesDT,nodesDT)` to `getTaxonomy(ids,sqlFile)`
  * `getId` changes from  `getId(taxa,namesDT)` to `getId(taxa,sqlFile)`
  * `read.names` is deprecated, instead use `read.names.sql`. For example, instead of calling `names<-read.names('names.dmp')` in every session, simply call `read.names.sql('names.dmp','accessionTaxa.sql')` once (or use the convenient `prepareDatabase` as <a href='#preparation'>above</a>)).
  * `read.nodes` is deprecated, instead use `read.names.sql`. For example. instead of calling `nodes<-read.names('nodes.dmp')` in every session, simply call `read.nodes.sql('nodes.dmp','accessionTaxa.sql')` once (or use the convenient `prepareDatabase` as <a href='#preparation'>above</a>).

  I've tried to ease any problems with this by overloading `getTaxonomy` and `getId` to still function (with a warning) if passed a data.table names and nodes argument and providing a simpler `prepareDatabase` function for completing all setup steps (hopefully avoiding direct calls to `read.names` and `read.nodes` for most users). 

I plan to eventually remove data.table functionality to avoid a split codebase so please switch to the new SQLite format in all new code.
 


