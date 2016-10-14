#' Read NCBI names file
#' 
#' Take an NCBI names file, keep only scientific names and convert it to a data.table
#'
#' @param nameFile string giving the path to an NCBI name file to read from (both gzipped or uncompressed files are ok)
#' @return a data.table with columns id and name with a key on id
#' @export
#' @examples
#' names<-c(
#'   "1\t|\tall\t|\t\t|\tsynonym\t|",
#'   "1\t|\troot\t|\t\t|\tscientific name\t|",
#'   "2\t|\tBacteria\t|\tBacteria <prokaryotes>\t|\tscientific name\t|",
#'   "2\t|\tMonera\t|\tMonera <Bacteria>\t|\tin-part\t|",
#'   "2\t|\tProcaryotae\t|\tProcaryotae <Bacteria>\t|\tin-part\t|"
#' )
#' readNames(textConnection(names))
readNames<-function(nameFile){
  #giTaxa<-readGiToTaxa('dump/gi_taxid_nucl.dmp.gz')
  splitLines<-do.call(rbind,strsplit(readLines(nameFile),'\\s*\\|\\s*'))
  splitLines<-splitLines[splitLines[,4]=='scientific name',-(3:4)]
  colnames(splitLines)<-c('id','name')
  splitLines<-data.frame('id'=as.numeric(splitLines[,'id']),'name'=splitLines[,'name'],stringsAsFactors=FALSE)
  out<-data.table(splitLines,key='id')
  return(out)
}

readGiToTaxa<-function(giTaxaFile){
  giTaxa<-read.table(giTaxaFile,header=FALSE)
  colnames(giTaxa)<-c('gi','taxa')
  out<-data.table(giTaxa,key='gi')
  return(out)
}
readAccessionToTaxa<-function(taxaFiles,sqlFile){
  #zcat, cut off first line, cut out extra columns, read into sqlite, index 
  #db <- dbConnect(SQLite(), dbname = 'my_db.sqlite')
  #dbWriteTable(conn=db, name='my_table', value='my_file.csv', sep='\t')
  message('Reading accessions')
  tmp<-tempfile()
  writeLines('accession\ttaxa',tmp)
  for(ii in taxaFiles){
    cmd<-sprintf('zcat %s|sed 1d|cut -f2,3>>%s',ii,tmp)
    message(cmd)
    system(cmd)
  }
  db <- dbConnect(SQLite(), dbname=sqlFile)
  dbWriteTable(conn = db, name = "accessionTaxa", value =tmp, row.names = FALSE, header = TRUE,sep='\t')
  dbGetQuery(db,"CREATE INDEX index_accession ON accessionTaxa (accession)")
  dbDisconnect(db)
  #f<-file(tmp)
  #out<-sqldf("select * from f", dbname = tempfile(), file.format = list(header = T, row.names = F))
  #setkey(out,key='accession.version')
  return(sqlFile)
}
readNodes<-function(nodeFile){
  splitLines<-do.call(rbind,strsplit(readLines(nodeFile),'\\s*\\|\\s*'))
  splitLines<-splitLines[,1:3]
  colnames(splitLines)<-c('id','parent','rank')
  splitLines<-data.frame('id'=as.numeric(splitLines[,'id']),'rank'=splitLines[,'rank'],'parent'=as.numeric(splitLines[,'parent']),stringsAsFactors=FALSE)
  out<-data.table(splitLines,key='id')
  return(out)
}

getTaxonomy<-function (ids,taxaNodes ,taxaNames, desiredTaxa=c('superkingdom','phylum','class','order','family','genus','species'),mc.cores=round(detectCores()/2)-1){
  uniqIds<-unique(ids)
  taxa<-do.call(rbind,mclapply(uniqIds,function(id){
      out<-structure(rep(NA,length(desiredTaxa)),names=desiredTaxa)
      thisId<-id    
      while(thisId!=1){
        thisNode<-taxaNodes[J(thisId),]
        if(is.na(thisNode$parent))break() #unknown taxa
        if(thisNode$rank %in% desiredTaxa)out[thisNode$rank]<-taxaNames[J(thisId),]$name
        thisId<-thisNode$parent
      }
      return(out)
  },mc.cores=mc.cores))
  rownames(taxa)<-format(uniqIds,scientific=FALSE)
  out<-taxa[format(ids,scientific=FALSE),]
  return(out)
}
giToTaxa<-function(gi,giTaxa){
  giTaxa[gi,]$taxa
}
accessionToTaxa<-function(accession,sqlFile){
  db <- dbConnect(SQLite(), dbname=sqlFile)
  dbWriteTable(db,'query',data.frame(accession,stringsAsFactors=FALSE),overwrite=TRUE)
  taxaDf<-dbGetQuery(db,'SELECT query.accession, taxa FROM query LEFT OUTER JOIN accessionTaxa ON query.accession=accessionTaxa.accession')
  dbGetQuery(db,'DROP TABLE query')
  dbDisconnect(db)
  if(any(taxaDf$accession!=accession))stop(simpleError('Query and SQL mismatch'))
  return(taxaDf$taxa)
}
condenseTaxa<-function(taxaTable){
  nTaxa<-apply(taxaTable,2,function(x)length(unique(x[!is.na(x)])))
  singles<-which(nTaxa==1)
  mostSpecific<-max(c(0,singles))
  out<-taxaTable[1,]
  if(mostSpecific<ncol(taxaTable))out[(mostSpecific+1):ncol(taxaTable)]<-NA
  return(out)
}
lastNotNa<-function(x,default='Unknown'){
  tail(na.omit(c(default,x)),1)
}

##DOWNLOAD NCBI DUMP##
######################
if(!file.exists('dump/names.dmp.gz')){
  dir.create('dump')
  setwd('dump')
  system('wget ftp://ftp.ncbi.nih.gov/pub/taxonomy/taxdump.tar.gz')
  system('tar xvfz taxdump.tar.gz')
  system('gzip nodes.dmp names.dmp')
  system('wget ftp://ftp.ncbi.nih.gov/pub/taxonomy/accession2taxid/nucl_gb.accession2taxid.gz')
  system('wget ftp://ftp.ncbi.nih.gov/pub/taxonomy/accession2taxid/nucl_est.accession2taxid.gz')
  system('wget ftp://ftp.ncbi.nih.gov/pub/taxonomy/accession2taxid/nucl_gss.accession2taxid.gz')
  system('wget ftp://ftp.ncbi.nih.gov/pub/taxonomy/accession2taxid/nucl_wgs.accession2taxid.gz')
  #system('wget ftp://ftp.ncbi.nih.gov/pub/taxonomy/gi_taxid_nucl.dmp.gz')
  setwd('..')
  accessionTaxa<-readAccessionToTaxa(list.files('dump','nucl_.*accession2taxid.gz',full.names=TRUE),'dump/accessionTaxa.sql')
}


##READ NCBI DUMP##
##################
if(!exists('taxaNodes')){
  taxaNodes<-readNodes('../chlorophyll/dump/nodes.dmp.gz')
  taxaNames<-readNames('../chlorophyll/dump/names.dmp.gz')
}


