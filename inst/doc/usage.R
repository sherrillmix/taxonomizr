## ----eval=FALSE----------------------------------------------------------
#  library(taxonomizr)
#  #note this will require a lot of hard drive space, bandwidth and time to process all the data from NCBI
#  prepareDatabase('accessionTaxa.sql')
#  blastAccessions<-c("Z17430.1","Z17429.1","X62402.1")
#  ids<-accessionToTaxa(blastAccessions,'accessionTaxa.sql')
#  getTaxonomy(ids,'accessionTaxa.sql')

## ---- eval=FALSE---------------------------------------------------------
#  install.packages("taxonomizr")

## ---- eval=FALSE---------------------------------------------------------
#  devtools::install_github("sherrillmix/taxonomizr")

## ------------------------------------------------------------------------
library(taxonomizr)

## ----eval=FALSE----------------------------------------------------------
#  prepareDatabase('accessionTaxa.sql')

## ----eval=FALSE----------------------------------------------------------
#  prepareDatabase('accessionTaxa.sql')

## ----eval=FALSE----------------------------------------------------------
#  taxaId<-accessionToTaxa(c("LN847353.1","AL079352.3"),"accessionTaxa.sql")
#  print(taxaId)

## ----eval=FALSE----------------------------------------------------------
#  getTaxonomy(taxaId,'accessionTaxa.sql')

## ----eval=FALSE----------------------------------------------------------
#  taxaId<-accessionToTaxa(c("LN847353","AL079352"),"accessionTaxa.sql")
#  print(taxaId)

## ----eval=FALSE----------------------------------------------------------
#  taxaId<-accessionToTaxa(c("LN847353","AL079352"),"accessionTaxa.sql",version='base')
#  print(taxaId)

## ----eval=FALSE----------------------------------------------------------
#  taxaId<-getId(c('Homo sapiens','Bos taurus','Homo','Alces alces'),'accessionTaxa.sql')
#  print(taxaId)

## ----eval=FALSE----------------------------------------------------------
#  taxa<-getTaxonomy(taxaId,'accessionTaxa.sql')
#  print(taxa)

## ----eval=FALSE----------------------------------------------------------
#  condenseTaxa(taxa)

## ----eval=FALSE----------------------------------------------------------
#  groupings<-c('read1','read2','read1','read2')
#  condenseTaxa(taxa,groupings)

## ----eval=FALSE----------------------------------------------------------
#  read.accession2taxid(list.files('.','accession2taxid.gz$'),'accessionTaxa.sql',indexTaxa=TRUE,overwrite=TRUE)

## ----eval=FALSE----------------------------------------------------------
#  getAccessions(3702,'accessionTaxa.sql',limit=10)

## ----eval=FALSE----------------------------------------------------------
#  getNamesAndNodes()

## ----eval=FALSE----------------------------------------------------------
#  #this is a big download
#  getAccession2taxid()

## ----eval=FALSE----------------------------------------------------------
#  #this is a big download
#  getAccession2taxid(types='prot')

## ----eval=FALSE----------------------------------------------------------
#  read.names.sql('names.dmp','accessionTaxa.sql')
#  read.nodes.sql('nodes.dmp','accessionTaxa.sql')

## ----eval=FALSE----------------------------------------------------------
#  read.accession2taxid(list.files('.','accession2taxid.gz$'),'accessionTaxa.sql')

