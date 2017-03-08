## ---- eval=FALSE---------------------------------------------------------
#  install.packages("taxonomizr")

## ---- eval=FALSE---------------------------------------------------------
#  devtools::install_github("sherrillmix/taxonomizr")

## ------------------------------------------------------------------------
library(taxonomizr)

## ----eval=FALSE----------------------------------------------------------
#  getNamesAndNodes()

## ----eval=FALSE----------------------------------------------------------
#  #this is a big download
#  getAccession2taxid()

## ----eval=FALSE----------------------------------------------------------
#  #this is a big download
#  getAccession2taxid(types='prot')

## ----eval=FALSE----------------------------------------------------------
#  read.accession2taxid(list.files('.','accession2taxid.gz$'),'accessionTaxa.sql')

## ----eval=FALSE----------------------------------------------------------
#  taxaNodes<-read.nodes('nodes.dmp')
#  taxaNames<-read.names('names.dmp')

## ----eval=FALSE----------------------------------------------------------
#  taxaId<-accessionToTaxa(c("LN847353.1","AL079352.3"),"accessionTaxa.sql")
#  print(taxaId)

## ----eval=FALSE----------------------------------------------------------
#  getTaxonomy(taxaId,taxaNodes,taxaNames)

## ----eval=FALSE----------------------------------------------------------
#  taxaId<-getId(c('Homo sapiens','Bos taurus','Homo'),taxaNames)
#  print(taxaId)

## ----eval=FALSE----------------------------------------------------------
#  getTaxonomy(taxaId,taxaNodes,taxaNames)

