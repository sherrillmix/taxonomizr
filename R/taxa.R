#' Read NCBI names file
#'
#' Take an NCBI names file, keep only scientific names and convert it to a data.table
#'
#' @param nameFile string giving the path to an NCBI name file to read from (both gzipped or uncompressed files are ok)
#' @return a data.table with columns id and name with a key on id
#' @export
#' @references \url{ftp://ftp.ncbi.nih.gov/pub/taxonomy/}
#' @seealso \code{\link{read.nodes}}
#' @examples
#' names<-c(
#'   "1\t|\tall\t|\t\t|\tsynonym\t|",
#'   "1\t|\troot\t|\t\t|\tscientific name\t|",
#'   "2\t|\tBacteria\t|\tBacteria <prokaryotes>\t|\tscientific name\t|",
#'   "2\t|\tMonera\t|\tMonera <Bacteria>\t|\tin-part\t|",
#'   "2\t|\tProcaryotae\t|\tProcaryotae <Bacteria>\t|\tin-part\t|"
#' )
#' read.names(textConnection(names))
read.names<-function(nameFile){
  splitLines<-do.call(rbind,strsplit(readLines(nameFile),'\\s*\\|\\s*'))
  splitLines<-splitLines[splitLines[,4]=='scientific name',-(3:4)]
  colnames(splitLines)<-c('id','name')
  splitLines<-data.frame('id'=as.numeric(splitLines[,'id']),'name'=splitLines[,'name'],stringsAsFactors=FALSE)
  out<-data.table::data.table(splitLines,key='id')
  return(out)
}

#' Read NCBI nodes file
#'
#' Take an NCBI nodes file and convert it to a data.table
#'
#' @param nodeFile string giving the path to an NCBI node file to read from (both gzipped or uncompressed files are ok)
#' @return a data.table with columns id, parent and rank with a key on id
#' @references \url{ftp://ftp.ncbi.nih.gov/pub/taxonomy/}
#' @seealso \code{\link{read.names}}
#' @export
#' @examples
#' nodes<-c(
#'  "1\t|\t1\t|\tno rank\t|\t\t|\t8\t|\t0\t|\t1\t|\t0\t|\t0\t|\t0\t|\t0\t|\t0\t|\t\t|",
#'  "2\t|\t131567\t|\tsuperkingdom\t|\t\t|\t0\t|\t0\t|\t11\t|\t0\t|\t0\t|\t0\t|\t0\t|\t0\t|\t\t|",
#'  "6\t|\t335928\t|\tgenus\t|\t\t|\t0\t|\t1\t|\t11\t|\t1\t|\t0\t|\t1\t|\t0\t|\t0\t|\t\t|",
#'  "7\t|\t6\t|\tspecies\t|\tAC\t|\t0\t|\t1\t|\t11\t|\t1\t|\t0\t|\t1\t|\t1\t|\t0\t|\t\t|",
#'  "9\t|\t32199\t|\tspecies\t|\tBA\t|\t0\t|\t1\t|\t11\t|\t1\t|\t0\t|\t1\t|\t1\t|\t0\t|\t\t|"
#' )
#' read.nodes(textConnection(nodes))
read.nodes<-function(nodeFile){
  splitLines<-do.call(rbind,strsplit(readLines(nodeFile),'\\s*\\|\\s*'))
  splitLines<-splitLines[,1:3]
  colnames(splitLines)<-c('id','parent','rank')
  splitLines<-data.frame('id'=as.numeric(splitLines[,'id']),'rank'=splitLines[,'rank'],'parent'=as.numeric(splitLines[,'parent']),stringsAsFactors=FALSE)
  out<-data.table::data.table(splitLines,key='id')
  return(out)
}

#' Return last not NA value
#'
#' A convenience function to return the last value which is not NA in a vector
#'
#' @param x a vector to look for the last value in
#' @param default a default value to use when all values are NA in a vector
#' @return a single element from the last non NA value in x (or the default)
#' @export
#' @examples
#' lastNotNa(c(1:4,NA,NA))
#' lastNotNa(c(letters[1:4],NA,'z',NA))
#' lastNotNa(c(NA,NA))
lastNotNa<-function(x,default='Unknown'){
  out<-x[!is.na(x)]
  if(length(out)==0)return(default)
  return(out[length(out)])
}

#' Process a large file piecewise
#'
#' A convenience function to read in a large file piece by piece, process it (hopefully reducing the size either by summarizing or removing extra rows or columns) and return the output
#'
#' @param bigFile a string giving the path to a file to be read in or a connection opened with "r" mode
#' @param n number of lines to read per chuck
#' @param FUN a function taking the unparsed lines from a chunk of the bigfile as a single argument and returning the desired output
#' @param vocal if TRUE cat a "." as each chunk is processed
#' @param ... any additional arguments to FUN
#' @return a list containing the results from applying func to the multiple chunks of the file
#' @export
#' @examples
#' streamingRead(textConnection(LETTERS),10,head,1)
#' temp<-tempfile()
#' writeLines(letters,temp)
#' streamingRead(temp,2,paste,collapse='',vocal=TRUE)
#' unlist(streamingRead(temp,2,sample,1))
streamingRead<-function(bigFile,n=1e6,FUN=function(xx)sub(',.*','',xx),...,vocal=FALSE){
  FUN<-match.fun(FUN)
  if(is.character(bigFile))handle<-file(bigFile,'r')
  else handle<-bigFile
  if(!isOpen(handle))open(handle)
  out<-list()
  while(length(piece<-readLines(handle,n=n))>0){
    if(vocal)cat('.')
    out<-c(out,list(FUN(piece,...)))
  }
  close(handle)
  return(out)
}


#' Trim columns from taxa file
#'
#' A simple script to delete the first row and then delete the first and fourth column of a four column tab delimited file and write to another file.
#'
#' @param inFile a single string giving the 4 column tab separated file to read from
#' @param outFile a single string giving the file path to write to
#' 
#' @useDynLib taxonomizr taxaTrim
trimTaxa<-function(inFile,outFile){
  inFile<-as.character(inFile)
  outFile<-as.character(outFile)
  if(!file.exists(inFile))stop(simpleError(sprintf('%s file not found',inFile)))
  .C('taxaTrim',c(inFile,outFile),PACKAGE='taxonomizr')
}

#' Read NCBI accession2taxid files
#'
#' Take NCBI accession2taxid files, keep only accession and taxa and save it as a sqlite database
#'
#' @param taxaFiles a string or vector of strings giving the path(s) to files to be read in
#' @param sqlFile a string giving the path where the output sqlite file should be saved
#' @param vocal if TRUE output status messages
#' @param n an integer giving how many lines from an accession files to read at a time
#' @return TRUE if sucessful
#' @export
#' @references \url{ftp://ftp.ncbi.nih.gov/pub/taxonomy/accession2taxid}
#' @seealso \code{\link{read.nodes}}, \code{\link{read.names}}
#' @examples
#' taxa<-c(
#'   "accession\taccession.version\ttaxid\tgi",
#'   "Z17427\tZ17427.1\t3702\t16569",
#'   "Z17428\tZ17428.1\t3702\t16570",
#'   "Z17429\tZ17429.1\t3702\t16571",
#'   "Z17430\tZ17430.1\t3702\t16572"
#' )
#' inFile<-tempfile()
#' outFile<-tempfile()
#' writeLines(taxa,inFile)
#' read.accession2taxid(inFile,outFile)
#' db<-RSQLite::dbConnect(RSQLite::SQLite(),dbname=outFile)
#' RSQLite::dbGetQuery(db,'SELECT * FROM accessionTaxa')
read.accession2taxid<-function(taxaFiles,sqlFile,n=1e6,vocal=TRUE){
  tmp<-tempfile()
  writeLines('accession\ttaxa',tmp)
  for(ii in taxaFiles){
    if(vocal)message('Reading ',ii,'. This may take a while.')
    trimTaxa(ii,tmp)
  }
  db <- RSQLite::dbConnect(RSQLite::SQLite(), dbname=sqlFile)
  if(vocal)message('Reading in values')
  RSQLite::dbWriteTable(conn = db, name = "accessionTaxa", value =tmp, row.names = FALSE, header = TRUE,sep='\t')
  if(vocal)message('Adding index')
  RSQLite::dbGetQuery(db,"CREATE INDEX index_accession ON accessionTaxa (accession)")
  RSQLite::dbDisconnect(db)
  return(TRUE)
}

getTaxonomy<-function (ids,taxaNodes ,taxaNames, desiredTaxa=c('superkingdom','phylum','class','order','family','genus','species'),mc.cores=round(parallel::detectCores()/2)-1){
  uniqIds<-unique(ids)
  taxa<-do.call(rbind,parallel::mclapply(uniqIds,function(id){
      out<-structure(rep(NA,length(desiredTaxa)),names=desiredTaxa)
      thisId<-id
      while(thisId!=1){
        thisNode<-taxaNodes[list(thisId),] #was J
        if(is.na(thisNode$parent))break() #unknown taxa
        if(thisNode$rank %in% desiredTaxa)out[thisNode$rank]<-taxaNames[list(thisId),]$name #was J
        thisId<-thisNode$parent
      }
      return(out)
  },mc.cores=mc.cores))
  rownames(taxa)<-format(uniqIds,scientific=FALSE)
  out<-taxa[format(ids,scientific=FALSE),]
  return(out)
}

#' Convert accessions to taxa
#'
#' Convert a vector of NCBI accession numbers to their assigned taxonomy
#'
#' @param accessions a vector of NCBI accession strings to convert to taxa
#' @param sqlFile a string giving the path to a sqlite file screated by \code{\link{read.accession2taxid}}
#' @return a vector of NCBI taxa ids
#' @export
#' @references \url{ftp://ftp.ncbi.nih.gov/pub/taxonomy/accession2taxid}
#' @seealso \code{\link{getTaxonomy}}, \code{\link{read.accession2taxid}}
#' @examples
#' taxa<-c(
#'  "accession\taccession.version\ttaxid\tgi",
#'  "Z17427\tZ17427.1\t3702\t16569",
#'  "Z17428\tZ17428.1\t3702\t16570",
#'  "Z17429\tZ17429.1\t3702\t16571",
#'  "Z17430\tZ17430.1\t3702\t16572",
#'  "X62402\tX62402.1\t9606\t30394"
#' )
#' inFile<-tempfile()
#' sqlFile<-tempfile()
#' writeLines(taxa,inFile)
#' read.accession2taxid(inFile,sqlFile)
#' accessionToTaxa(c("Z17430.1","Z17429.1","X62402.1",'NOTREAL'),sqlFile)
accessionToTaxa<-function(accessions,sqlFile){
  if(length(accessions)==0)return(c())
  db <- RSQLite::dbConnect(RSQLite::SQLite(), dbname=sqlFile)
  tmp<-tempfile()
  RSQLite::dbGetQuery(db, sprintf("ATTACH '%s' AS tmp",tmp))
  RSQLite::dbWriteTable(db,'tmp.query',data.frame('accession'=accessions,stringsAsFactors=FALSE),overwrite=TRUE)
  taxaDf<-RSQLite::dbGetQuery(db,'SELECT tmp.query.accession, taxa FROM tmp.query LEFT OUTER JOIN accessionTaxa ON tmp.query.accession=accessionTaxa.accession')
  RSQLite::dbGetQuery(db,'DROP TABLE tmp.query')
  RSQLite::dbGetQuery(db,'DETACH tmp')
  RSQLite::dbDisconnect(db)
  file.remove(tmp)
  if(any(taxaDf$accession!=accessions))stop(simpleError('Query and SQL mismatch'))
  return(taxaDf$taxa)
}


#' Condense a taxa table for a single read
#'
#' Take a table of taxonomic assignments from hits to a single read and condense it to a single vector with NAs where there are disagreements between the hits
#'
#' @param taxaTable a matrix or data.frame with hits on the rows and various levels of taxonomy in the columns
#' @return a vector of length \code{ncol(taxaTable)} with NAs where the is not complete agreement
#' @export
#' @examples
#' taxas<-matrix(c(
#'  'a','b','c','e',
#'  'a','b','d','e'
#' ),nrow=2,byrow=TRUE)
#' condenseTaxa(taxas)
condenseTaxa<-function(taxaTable){
  nTaxa<-apply(taxaTable,2,function(x)length(unique(x[!is.na(x)])))
  firstDisagree<-min(c(Inf,which(nTaxa!=1)))
  out<-taxaTable[1,]
  if(firstDisagree<=ncol(taxaTable))out[(firstDisagree):ncol(taxaTable)]<-NA
  return(out)
}

##DOWNLOAD NCBI DUMP##
######################
#if(!file.exists('dump/names.dmp.gz')){
  #dir.create('dump')
  #setwd('dump')
  #system('wget ftp://ftp.ncbi.nih.gov/pub/taxonomy/taxdump.tar.gz')
  #system('tar xvfz taxdump.tar.gz')
  #system('gzip nodes.dmp names.dmp')
  #system('wget ftp://ftp.ncbi.nih.gov/pub/taxonomy/accession2taxid/nucl_gb.accession2taxid.gz')
  #system('wget ftp://ftp.ncbi.nih.gov/pub/taxonomy/accession2taxid/nucl_est.accession2taxid.gz')
  #system('wget ftp://ftp.ncbi.nih.gov/pub/taxonomy/accession2taxid/nucl_gss.accession2taxid.gz')
  #system('wget ftp://ftp.ncbi.nih.gov/pub/taxonomy/accession2taxid/nucl_wgs.accession2taxid.gz')
  ##system('wget ftp://ftp.ncbi.nih.gov/pub/taxonomy/gi_taxid_nucl.dmp.gz')
  #setwd('..')
  #accessionTaxa<-readAccessionToTaxa(list.files('dump','nucl_.*accession2taxid.gz',full.names=TRUE),'dump/accessionTaxa.sql')
#}


##READ NCBI DUMP##
##################
#if(!exists('taxaNodes')){
 #taxaNodes<-read.nodes('../chlorophyll/dump/nodes.dmp.gz')
 #taxaNames<-read.names('../chlorophyll/dump/names.dmp.gz')
#}

#readLines(list.files('../chlorophyll/dump','nucl_.*accession2taxid.gz',full.names=TRUE)[1],n=5)
#accessionTaxa<-read.accession2taxid(list.files('../chlorophyll/dump','nucl_.*accession2taxid.gz',full.names=TRUE),'test.sql')

