## ---- eval=FALSE---------------------------------------------------------
#  install.packages("taxonomizr")

## ---- eval=FALSE---------------------------------------------------------
#  devtools::install_github("sherrillmix/taxonomizr")

## ------------------------------------------------------------------------
library(taxonomizr)

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
#  taxaId<-getId(c('Homo sapiens','Bos taurus','Homo'),'accessionTaxa.sql')
#  print(taxaId)

## ----eval=FALSE----------------------------------------------------------
#  getTaxonomy(taxaId,'accessionTaxa.sql')

## ----eval=FALSE----------------------------------------------------------
#  read.accession2taxid(list.files('.','accession2taxid.gz$'),'accessionTaxa.sql',indexTaxa=TRUE)

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

