#' @details
#' taxonomizr provides some simple functions to parse NCBI taxonomy files and accession dumps and efficiently use them to assign taxonomy to accession numbers or taxonomic IDs (\url{https://www.ncbi.nlm.nih.gov/Taxonomy/taxonomyhome.html/}). This is useful for example to assign taxonomy to BLAST results. This is all done locally after downloading the appropriate files from NCBI using included functions. The major functions are:
#' \itemize{
#'   \item \code{\link{prepareDatabase}}: download data from NCBI and prepare SQLite database
#'   \item \code{link{accessionToTaxa}}: convert accession numbers to taxonomic IDs
#'   \item \code{\link{getTaxonomy}}: convert taxonomic IDs to taxonomy
#' }
#' More specialized functions are:
#' \itemize{
#'  \item \code{\link{getId}}: convert a biological name to taxonomic ID
#'  \item \code{\link{getAccessions}}: find accessions for a given taxonomic ID
#' }
#'
#' @examples
#' \dontrun{
#'   if(readline(
#'     "This will download a lot data and take a while to process.
#'      Make sure you have space and bandwidth. Type y to continue: "
#'   )!='y')
#'     stop('This is a stop to make sure no one downloads a bunch of data unintentionally')
#'
#'   prepareDatabase('accessionTaxa.sql')
#'   blastAccessions<-c("Z17430.1","Z17429.1","X62402.1")
#'   ids<-accessionToTaxa(blastAccessions,'accessionTaxa.sql')
#'   getTaxonomy(ids,'accessionTaxa.sql')
#' }
#' @keywords internal
#' @seealso  \code{\link{prepareDatabase}}, \code{\link{accessionToTaxa}}, \code{\link{getTaxonomy}}
"_PACKAGE"
#> [1] "_PACKAGE"

#' Read NCBI names file
#'
#' Take an NCBI names file, keep only scientific names and convert it to a data.table. NOTE: This function is now deprecated for \code{\link{read.names.sql}} (using SQLite rather than data.table).
#'
#' @param nameFile string giving the path to an NCBI name file to read from (both gzipped or uncompressed files are ok)
#' @param onlyScientific If TRUE, only store scientific names. If FALSE, synonyms and other types are included (increasing the potential for ambiguous taxonomic assignments).
#' @return a data.table with columns id and name with a key on id
#' @export
#' @references \url{https://ftp.ncbi.nih.gov/pub/taxonomy/}
#' @seealso \code{\link{read.nodes}}, \code{\link{read.names.sql}}
#' @examples
#' namesText<-c(
#'   "1\t|\tall\t|\t\t|\tsynonym\t|",
#'   "1\t|\troot\t|\t\t|\tscientific name\t|",
#'   "2\t|\tBacteria\t|\tBacteria <prokaryotes>\t|\tscientific name\t|",
#'   "2\t|\tMonera\t|\tMonera <Bacteria>\t|\tin-part\t|",
#'   "2\t|\tProcaryotae\t|\tProcaryotae <Bacteria>\t|\tin-part\t|"
#' )
#' tmpFile<-tempfile()
#' writeLines(namesText,tmpFile)
#' read.names(tmpFile)
read.names<-function(nameFile,onlyScientific=TRUE){
  .Deprecated('read.names.sql','taxonomizr',"taxonomizr is moving from data.table to SQLite databases to improve performance. This will require changing nodes and names processing. Please see ?read.names.sql or ?taxonomizrSwitch")
  splitLines<-do.call(rbind,strsplit(readLines(nameFile),"\t\\|\t?"))
  if(onlyScientific)splitLines<-splitLines[splitLines[,4]=='scientific name',]
  splitLines<-splitLines[,-(3:4)]
  colnames(splitLines)<-c('id','name')
  splitLines<-data.frame('id'=as.numeric(splitLines[,'id']),'name'=splitLines[,'name'],stringsAsFactors=FALSE)
  out<-data.table::data.table(splitLines,key='id')
  data.table::setindex(out,'name')
  return(out)
}

#' Read NCBI names file
#'
#' Take an NCBI names file, keep only scientific names and convert it to a SQLite table
#'
#' @param nameFile string giving the path to an NCBI name file to read from (both gzipped or uncompressed files are ok)
#' @param sqlFile a string giving the path where the output SQLite file should be saved
#' @param overwrite If TRUE, delete names table in database if present and regenerate
#' @return invisibly returns a string with path to sqlfile
#' @export
#' @references \url{https://ftp.ncbi.nih.gov/pub/taxonomy/}
#' @seealso \code{\link{read.nodes}}
#' @examples
#' namesText<-c(
#'   "1\t|\tall\t|\t\t|\tsynonym\t|",
#'   "1\t|\troot\t|\t\t|\tscientific name\t|",
#'   "2\t|\tBacteria\t|\tBacteria <prokaryotes>\t|\tscientific name\t|",
#'   "2\t|\tMonera\t|\tMonera <Bacteria>\t|\tin-part\t|",
#'   "2\t|\tProcaryotae\t|\tProcaryotae <Bacteria>\t|\tin-part\t|"
#' )
#' tmpFile<-tempfile()
#' writeLines(namesText,tmpFile)
#' sqlFile<-tempfile()
#' read.names.sql(tmpFile,sqlFile)
read.names.sql<-function(nameFile,sqlFile='nameNode.sqlite',overwrite=FALSE){
  if(file.exists(sqlFile)){
    dbTest <- RSQLite::dbConnect(RSQLite::SQLite(), dbname=sqlFile)
    on.exit(RSQLite::dbDisconnect(dbTest))
    if('names' %in% RSQLite::dbListTables(dbTest)){
      if(overwrite){
        RSQLite::dbExecute(dbTest,'DROP TABLE names')
      }else{
        message(sqlFile,' already contains table names. Delete file or set overwrite=TRUE to reload')
        return(invisible(sqlFile))
      }
    }
  }
  splitLines<-do.call(rbind,strsplit(readLines(nameFile),'\\s*\\|\\s*'))
  isScientific<-splitLines[,4]=='scientific name'
  splitLines<-splitLines[,-(3)]
  colnames(splitLines)<-c('id','name','type')
  splitLines<-data.frame('id'=as.integer(splitLines[,'id']),'name'=splitLines[,'name'],'scientific'=isScientific,'type'=splitLines[,'type'],stringsAsFactors=FALSE)
  db <- RSQLite::dbConnect(RSQLite::SQLite(), dbname=sqlFile)
  on.exit(RSQLite::dbDisconnect(db),add=TRUE)
  RSQLite::dbWriteTable(conn = db, name = "names", value=splitLines)
  RSQLite::dbExecute(db,"CREATE INDEX index_names_id ON names (id)")
  RSQLite::dbExecute(db,"CREATE INDEX index_names_name ON names (name,scientific)")
  return(invisible(sqlFile))
}

#' Read NCBI nodes file
#'
#' Take an NCBI nodes file and convert it to a data.table. NOTE: This function is now deprecated for \code{\link{read.nodes.sql}} (using SQLite rather than data.table).
#'
#' @param nodeFile string giving the path to an NCBI node file to read from (both gzipped or uncompressed files are ok)
#' @return a data.table with columns id, parent and rank with a key on id
#' @references \url{https://ftp.ncbi.nih.gov/pub/taxonomy/}
#' @seealso \code{\link{read.names}}, \code{\link{read.nodes.sql}}
#' @export
#' @examples
#' nodes<-c(
#'  "1\t|\t1\t|\tno rank\t|\t\t|\t8\t|\t0\t|\t1\t|\t0\t|\t0\t|\t0\t|\t0\t|\t0\t|\t\t|",
#'  "2\t|\t131567\t|\tsuperkingdom\t|\t\t|\t0\t|\t0\t|\t11\t|\t0\t|\t0\t|\t0\t|\t0\t|\t0\t|\t\t|",
#'  "6\t|\t335928\t|\tgenus\t|\t\t|\t0\t|\t1\t|\t11\t|\t1\t|\t0\t|\t1\t|\t0\t|\t0\t|\t\t|",
#'  "7\t|\t6\t|\tspecies\t|\tAC\t|\t0\t|\t1\t|\t11\t|\t1\t|\t0\t|\t1\t|\t1\t|\t0\t|\t\t|",
#'  "9\t|\t32199\t|\tspecies\t|\tBA\t|\t0\t|\t1\t|\t11\t|\t1\t|\t0\t|\t1\t|\t1\t|\t0\t|\t\t|"
#' )
#' tmpFile<-tempfile()
#' writeLines(nodes,tmpFile)
#' read.nodes(tmpFile)
read.nodes<-function(nodeFile){
  .Deprecated('read.nodes.sql','taxonomizr',"taxonomizr is moving from data.table to SQLite databases to improve performance. This will require changing nodes and names processing. Please see ?read.nodes.sql or ?taxonomizrSwitch")
  splitLines<-do.call(rbind,lapply(strsplit(readLines(nodeFile),"\t\\|\t?"),'[',1:3))
  colnames(splitLines)<-c('id','parent','rank')
  splitLines<-data.frame('id'=as.numeric(splitLines[,'id']),'rank'=splitLines[,'rank'],'parent'=as.numeric(splitLines[,'parent']),stringsAsFactors=FALSE)
  out<-data.table::data.table(splitLines,key='id')
  return(out)
}

#' Read NCBI nodes file
#'
#' Take an NCBI nodes file and convert it to a data.table
#'
#' @param nodeFile string giving the path to an NCBI node file to read from (both gzipped or uncompressed files are ok)
#' @param sqlFile a string giving the path where the output SQLite file should be saved
#' @param overwrite If TRUE, delete nodes table in database if present and regenerate
#' @return a data.table with columns id, parent and rank with a key on id
#' @references \url{https://ftp.ncbi.nih.gov/pub/taxonomy/}
#' @seealso \code{\link{read.names.sql}}
#' @export
#' @examples
#' nodes<-c(
#'  "1\t|\t1\t|\tno rank\t|\t\t|\t8\t|\t0\t|\t1\t|\t0\t|\t0\t|\t0\t|\t0\t|\t0\t|\t\t|",
#'  "2\t|\t131567\t|\tsuperkingdom\t|\t\t|\t0\t|\t0\t|\t11\t|\t0\t|\t0\t|\t0\t|\t0\t|\t0\t|\t\t|",
#'  "6\t|\t335928\t|\tgenus\t|\t\t|\t0\t|\t1\t|\t11\t|\t1\t|\t0\t|\t1\t|\t0\t|\t0\t|\t\t|",
#'  "7\t|\t6\t|\tspecies\t|\tAC\t|\t0\t|\t1\t|\t11\t|\t1\t|\t0\t|\t1\t|\t1\t|\t0\t|\t\t|",
#'  "9\t|\t32199\t|\tspecies\t|\tBA\t|\t0\t|\t1\t|\t11\t|\t1\t|\t0\t|\t1\t|\t1\t|\t0\t|\t\t|"
#' )
#' tmpFile<-tempfile()
#' sqlFile<-tempfile()
#' writeLines(nodes,tmpFile)
#' read.nodes.sql(tmpFile,sqlFile)
read.nodes.sql<-function(nodeFile,sqlFile='nameNode.sqlite',overwrite=FALSE){
  if(file.exists(sqlFile)){
    dbTest <- RSQLite::dbConnect(RSQLite::SQLite(), dbname=sqlFile)
    on.exit(RSQLite::dbDisconnect(dbTest))
    if('nodes' %in% RSQLite::dbListTables(dbTest)){
      if(overwrite){
        RSQLite::dbExecute(dbTest,'DROP TABLE nodes')
      }else{
        message(sqlFile,' already contains table nodes. Delete file or set overwrite=TRUE to reload')
        return(invisible(sqlFile))
      }
    }
  }
  splitLines<-do.call(rbind,lapply(strsplit(readLines(nodeFile),'\\s*\\|\\s*'),'[',1:3))
  colnames(splitLines)<-c('id','parent','rank')
  splitLines<-data.frame('id'=as.integer(splitLines[,'id']),'rank'=splitLines[,'rank'],'parent'=as.numeric(splitLines[,'parent']),stringsAsFactors=FALSE)
  db <- RSQLite::dbConnect(RSQLite::SQLite(), dbname=sqlFile)
  on.exit(RSQLite::dbDisconnect(db),add=TRUE)
  RSQLite::dbWriteTable(conn = db, name = "nodes", value =splitLines)
  RSQLite::dbExecute(db,"CREATE INDEX index_nodes_id ON nodes (id)")
  RSQLite::dbExecute(db,"CREATE INDEX index_nodes_parent ON nodes (parent)")
  return(invisible(sqlFile))
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
  if(length(x)==0)return(default)
  out<-x[!is.na(x)]
  if(length(out)==0)return(default)
  return(out[length(out)])
}

#' Process a large file piecewise
#'
#' A convenience function to read in a large file piece by piece, process it (hopefully reducing the size either by summarizing or removing extra rows or columns) and return the output
#'
#' @param bigFile a string giving the path to a file to be read in or a connection opened with "r" mode
#' @param n number of lines to read per chunk
#' @param FUN a function taking the unparsed lines from a chunk of the bigfile as a single argument and returning the desired output
#' @param vocal if TRUE cat a "." as each chunk is processed
#' @param ... any additional arguments to FUN
#' @return a list containing the results from applying func to the multiple chunks of the file
#' @export
#' @examples
#' tmpFile<-tempfile()
#' writeLines(LETTERS,tmpFile)
#' streamingRead(tmpFile,10,head,1)
#' writeLines(letters,tmpFile)
#' streamingRead(tmpFile,2,paste,collapse='',vocal=TRUE)
#' unlist(streamingRead(tmpFile,2,sample,1))
streamingRead<-function(bigFile,n=1e6,FUN=function(xx)sub(',.*','',xx),...,vocal=FALSE){
  FUN<-match.fun(FUN)
  if(is.character(bigFile))handle<-file(bigFile,'r')
  else handle<-bigFile
  if(!isOpen(handle))open(handle)
  on.exit(close(handle))
  out<-list()
  while(length(piece<-readLines(handle,n=n))>0){
    if(vocal)cat('.')
    out<-c(out,list(FUN(piece,...)))
  }
  return(out)
}


#' Trim columns from taxa file
#'
#' A simple script to delete the first row and then delete the first and fourth column of a four column tab delimited file and write to another file.
#'
#' @param inFile a single string giving the 4 column tab separated file to read from
#' @param outFile a single string giving the file path to write to
#' @param desiredCols the integer IDs for columns to pull out from file
#'
#' @useDynLib taxonomizr, .registration=TRUE
trimTaxa<-function(inFile,outFile,desiredCols=c(2,3)){
  inFile<-as.character(inFile)
  outFile<-as.character(outFile)
  desiredCols<-sort(desiredCols)
  if(!file.exists(inFile))stop(simpleError(sprintf('%s file not found',inFile)))
  isCompressed<-R.utils::isGzipped(inFile,method='content')
  if(isCompressed){
    tmp<-tempfile()
    R.utils::gunzip(inFile,tmp,remove=FALSE)
    if(!file.exists(tmp))stop('Problem unzipping ',inFile,' to temporary file ',tmp,'. Could be out of space on temp drive or permission issue?')
    inFile<-tmp
    on.exit(file.remove(tmp))
  }
  #too much memory
  #out<-data.table::fread(sprintf('gzip -dcf %s',inFile),select=c(2,3))
  #write.table(out,outFile,row.names=FALSE,col.names=FALSE)
  .C('taxaTrim',c(inFile,outFile),as.integer(desiredCols-1),length(desiredCols),PACKAGE='taxonomizr')
}

#' Read NCBI accession2taxid files
#'
#' Take NCBI accession2taxid files, keep only accession and taxa and save it as a SQLite database
#'
#' @param taxaFiles a string or vector of strings giving the path(s) to files to be read in
#' @param sqlFile a string giving the path where the output SQLite file should be saved
#' @param vocal if TRUE output status messages
#' @param extraSqlCommand for advanced use. A string giving a command to be called on the SQLite database before loading data. A couple potential uses: 
#' \itemize{\item "PRAGMA temp_store_directory = '/MY/TMP/DIR'" to store SQLite temporary files in directory /MY/TMP/DIR. Useful if the temporary directory used by SQLite (which is not necessarily in the same location as R's) is small on your system \item "pragma temp_store = 2;" to keep all SQLite temp files in memory. Don't do this unless you have a lot (>100 Gb) of RAM}
#' @param indexTaxa if TRUE add an index for taxa ID. This would only be necessary if you want to look up accessions by taxa ID e.g. \code{\link{getAccessions}}
#' @param overwrite If TRUE, delete accessionTaxa table in database if present and regenerate
#' @return TRUE if sucessful
#' @export
#' @references \url{https://ftp.ncbi.nih.gov/pub/taxonomy/accession2taxid/}
#' @seealso \code{\link{read.nodes.sql}}, \code{\link{read.names.sql}}
#' @examples
#' taxa<-c(
#'   "accession\taccession.version\ttaxid\tgi",
#'   "Z17427\tZ17427.1\t3702\t16569",
#'   "Z17428\tZ17428.1\t3702\t16570",
#'   "Z17429\tZ17429.1\t3702\t16571",
#'   "Z17430\tZ17430.1\t3702\t16572"
#' )
#' inFile<-tempfile()
#' sqlFile<-tempfile()
#' writeLines(taxa,inFile)
#' read.accession2taxid(inFile,sqlFile,vocal=FALSE)
#' db<-RSQLite::dbConnect(RSQLite::SQLite(),dbname=sqlFile)
#' RSQLite::dbGetQuery(db,'SELECT * FROM accessionTaxa')
#' RSQLite::dbDisconnect(db)
read.accession2taxid<-function(taxaFiles,sqlFile,vocal=TRUE,extraSqlCommand='',indexTaxa=FALSE,overwrite=FALSE){
  if(file.exists(sqlFile)){
    dbTest <- RSQLite::dbConnect(RSQLite::SQLite(), dbname=sqlFile)
    on.exit(RSQLite::dbDisconnect(dbTest))
    if('accessionTaxa' %in% RSQLite::dbListTables(dbTest)){
      if(overwrite){
        RSQLite::dbExecute(dbTest,'DROP TABLE accessionTaxa')
      }else{
        message(sqlFile,' already contains table accessionTaxa. Delete file or set overwrite=TRUE to reload')
        return(invisible(sqlFile))
      }
    }
  }
  tmp<-tempfile()
  writeLines('base\taccession\ttaxa',tmp)
  for(ii in taxaFiles){
    if(vocal)message('Reading ',ii,'.')
    trimTaxa(ii,tmp,1:3)
  }
  db <- RSQLite::dbConnect(RSQLite::SQLite(), dbname=sqlFile)
  on.exit(RSQLite::dbDisconnect(db),add=TRUE)
  if(extraSqlCommand!='')RSQLite::dbExecute(db,extraSqlCommand)
  if(vocal)message('Reading in values. This may take a while.')
  RSQLite::dbWriteTable(conn = db, name = "accessionTaxa", value =tmp, row.names = FALSE, header = TRUE,sep='\t')
  if(vocal)message('Adding index. This may also take a while.')
  RSQLite::dbExecute(db,"CREATE INDEX index_accession ON accessionTaxa (accession)")
  RSQLite::dbExecute(db,"CREATE INDEX index_base ON accessionTaxa (base)")
  if(indexTaxa)RSQLite::dbExecute(db,"CREATE INDEX index_taxa ON accessionTaxa (taxa)")
  return(invisible(sqlFile))
}

#' Get taxonomic ranks for a taxa
#'
#' Take NCBI taxa IDs and get the corresponding taxa ranks from name and node data.tables. NOTE: This function is now deprecated for \code{\link{getTaxonomy}} (using SQLite rather than data.table).
#'
#' @param ids a vector of ids to find taxonomy for
#' @param taxaNodes a nodes data.table from \code{\link{read.nodes}}
#' @param taxaNames a names data.table from \code{\link{read.names}}
#' @param desiredTaxa a vector of strings giving the desired taxa levels
#' @param mc.cores DEPRECATED the number of cores to use when processing. Note this option is now deprecated and has no effect. Please switch to \code{\link{getTaxonomy}} (see \link{taxonomizrSwitch}) for much faster processing without requiring multiple cores.
#' @param debug if TRUE output node and name vectors with dput for each id (probably useful only for development)
#' @return a matrix of taxonomic strings with a row for each id and a column for each desiredTaxa rank
#' @import data.table
#' @export
#' @seealso \code{\link{read.nodes}}, \code{\link{read.names}}, \code{\link{getTaxonomy}}
#' @examples
#' namesText<-c(
#'   "1\t|\tall\t|\t\t|\tsynonym\t|",
#'   "1\t|\troot\t|\t\t|\tscientific name\t|",
#'   "2\t|\tBacteria\t|\tBacteria <prokaryotes>\t|\tscientific name\t|",
#'   "2\t|\tMonera\t|\tMonera <Bacteria>\t|\tin-part\t|",
#'   "2\t|\tProcaryotae\t|\tProcaryotae <Bacteria>\t|\tin-part\t|",
#'   "9606\t|\tHomo sapiens\t|\t\t|\tscientific name",
#'   "9605\t|\tHomo\t|\t\t|\tscientific name",
#'   "207598\t|\tHomininae\t|\t\t|\tscientific name",
#'   "9604\t|\tHominidae\t|\t\t|\tscientific name",
#'   "314295\t|\tHominoidea\t|\t\t|\tscientific name",
#'   "9526\t|\tCatarrhini\t|\t\t|\tscientific name",
#'   "314293\t|\tSimiiformes\t|\t\t|\tscientific name",
#'   "376913\t|\tHaplorrhini\t|\t\t|\tscientific name",
#'   "9443\t|\tPrimates\t|\t\t|\tscientific name",
#'   "314146\t|\tEuarchontoglires\t|\t\t|\tscientific name",
#'   "1437010\t|\tBoreoeutheria\t|\t\t|\tscientific name",
#'   "9347\t|\tEutheria\t|\t\t|\tscientific name",
#'   "32525\t|\tTheria\t|\t\t|\tscientific name",
#'   "40674\t|\tMammalia\t|\t\t|\tscientific name",
#'   "32524\t|\tAmniota\t|\t\t|\tscientific name",
#'   "32523\t|\tTetrapoda\t|\t\t|\tscientific name",
#'   "1338369\t|\tDipnotetrapodomorpha\t|\t\t|\tscientific name",
#'   "8287\t|\tSarcopterygii\t|\t\t|\tscientific name",
#'   "117571\t|\tEuteleostomi\t|\t\t|\tscientific name",
#'   "117570\t|\tTeleostomi\t|\t\t|\tscientific name",
#'   "7776\t|\tGnathostomata\t|\t\t|\tscientific name",
#'   "7742\t|\tVertebrata\t|\t\t|\tscientific name",
#'   "89593\t|\tCraniata\t|\t\t|\tscientific name",
#'   "7711\t|\tChordata\t|\t\t|\tscientific name",
#'   "33511\t|\tDeuterostomia\t|\t\t|\tscientific name",
#'   "33213\t|\tBilateria\t|\t\t|\tscientific name",
#'   "6072\t|\tEumetazoa\t|\t\t|\tscientific name",
#'   "33208\t|\tMetazoa\t|\t\t|\tscientific name",
#'   "33154\t|\tOpisthokonta\t|\t\t|\tscientific name",
#'   "2759\t|\tEukaryota\t|\t\t|\tscientific name",
#'   "131567\t|\tcellular organisms\t|\t\t|\tscientific name"
#' )
#' tmpFile<-tempfile()
#' writeLines(namesText,tmpFile)
#' taxaNames<-read.names(tmpFile)
#' nodesText<-c(
#'  "1\t|\t1\t|\tno rank\t|\t\t|\t8\t|\t0\t|\t1\t|\t0\t|\t0\t|\t0\t|\t0\t|\t0\t|\t\t|",
#'   "2\t|\t131567\t|\tsuperkingdom\t|\t\t|\t0\t|\t0\t|\t11\t|\t0\t|\t0\t|\t0\t|\t0\t|\t0\t|\t\t|",
#'   "6\t|\t335928\t|\tgenus\t|\t\t|\t0\t|\t1\t|\t11\t|\t1\t|\t0\t|\t1\t|\t0\t|\t0\t|\t\t|",
#'   "7\t|\t6\t|\tspecies\t|\tAC\t|\t0\t|\t1\t|\t11\t|\t1\t|\t0\t|\t1\t|\t1\t|\t0\t|\t\t|",
#'   "9\t|\t32199\t|\tspecies\t|\tBA\t|\t0\t|\t1\t|\t11\t|\t1\t|\t0\t|\t1\t|\t1\t|\t0\t|\t\t|",
#'   "9606\t|\t9605\t|\tspecies", "9605\t|\t207598\t|\tgenus", "207598\t|\t9604\t|\tsubfamily",
#'   "9604\t|\t314295\t|\tfamily", "314295\t|\t9526\t|\tsuperfamily",
#'   "9526\t|\t314293\t|\tparvorder", "314293\t|\t376913\t|\tinfraorder",
#'   "376913\t|\t9443\t|\tsuborder", "9443\t|\t314146\t|\torder",
#'   "314146\t|\t1437010\t|\tsuperorder", "1437010\t|\t9347\t|\tno rank",
#'   "9347\t|\t32525\t|\tno rank", "32525\t|\t40674\t|\tno rank",
#'   "40674\t|\t32524\t|\tclass", "32524\t|\t32523\t|\tno rank", "32523\t|\t1338369\t|\tno rank",
#'   "1338369\t|\t8287\t|\tno rank", "8287\t|\t117571\t|\tno rank",
#'   "117571\t|\t117570\t|\tno rank", "117570\t|\t7776\t|\tno rank",
#'   "7776\t|\t7742\t|\tno rank", "7742\t|\t89593\t|\tno rank", "89593\t|\t7711\t|\tsubphylum",
#'   "7711\t|\t33511\t|\tphylum", "33511\t|\t33213\t|\tno rank", "33213\t|\t6072\t|\tno rank",
#'   "6072\t|\t33208\t|\tno rank", "33208\t|\t33154\t|\tkingdom",
#'   "33154\t|\t2759\t|\tno rank", "2759\t|\t131567\t|\tsuperkingdom",
#'   "131567\t|\t1\t|\tno rank"
#' )
#' writeLines(nodesText,tmpFile)
#' taxaNodes<-read.nodes(tmpFile)
#' getTaxonomy2(c(9606,9605),taxaNodes,taxaNames,mc.cores=1)
getTaxonomy2<-function(ids,taxaNodes ,taxaNames, desiredTaxa=c('superkingdom','phylum','class','order','family','genus','species'),mc.cores=1,debug=FALSE){
  .Deprecated('getTaxonomy','taxonomizr',"taxonomizr is moving from data.table to SQLite databases to improve performance. This will require changing nodes and names processing. Please see ?getTaxonomy or ?taxonomizrSwitch")
  ids<-as.numeric(ids)
  if(length(ids)==0)return(NULL)
  uniqIds<-unique(ids)
  taxa<-do.call(rbind,lapply(uniqIds,function(id){
      out<-structure(rep(as.character(NA),length(desiredTaxa)),names=desiredTaxa)
      if(is.na(id))return(out)
      thisId<-id
      if(debug){
        tmp<-c()
        tmp2<-c()
      }
      while(thisId!=1){
        thisNode<-taxaNodes[list(thisId),]
        if(debug){
          tmp<-c(tmp,sprintf('%d\t|\t%d\t|\t%s',thisNode$id,thisNode$parent,thisNode$rank))
          tmp2<-c(tmp2,sprintf('%d\t|\t%s\t|\t\t|\tscientific name',taxaNames[list(thisId),]$id,taxaNames[list(thisId),]$name))
        }
        if(is.na(thisNode$parent))break() #unknown taxa
        if(thisNode$rank %in% desiredTaxa)out[thisNode$rank]<-taxaNames[list(thisId),]$name
        thisId<-thisNode$parent
      }
      if(debug){
        dput(tmp)
        dput(tmp2)
      }
      return(out)
  }))
  rownames(taxa)<-format(uniqIds,scientific=FALSE)
  out<-taxa[format(ids,scientific=FALSE),,drop=FALSE]
  return(out)
}


getParentNodes<-function(ids,sqlFile='nameNode.sqlite',getDescendants=FALSE){
  ids<-as.numeric(ids)
  tmp<-tempfile()
  on.exit(file.remove(tmp))
  tmpDb <- RSQLite::dbConnect(RSQLite::SQLite(), dbname=tmp)
  on.exit(RSQLite::dbDisconnect(tmpDb),add=TRUE)
  RSQLite::dbWriteTable(tmpDb,'query',data.frame('id'=ids),overwrite=TRUE)
  #attach the temp table
  db <- RSQLite::dbConnect(RSQLite::SQLite(), dbname=sqlFile)
  on.exit(RSQLite::dbDisconnect(db),add=TRUE)
  RSQLite::dbExecute(db, sprintf("ATTACH '%s' AS tmp",tmp))
  if(getDescendants){
    taxaDf<-RSQLite::dbGetQuery(db,'SELECT nodes.id as descendant, name, rank FROM tmp.query LEFT OUTER JOIN nodes ON tmp.query.id=nodes.parent LEFT OUTER JOIN names ON descendant=names.id WHERE (names.scientific=1 OR names.scientific IS NULL) AND nodes.id != nodes.parent')
    return(taxaDf[,c('name','descendant','rank')])
  }else{
    taxaDf<-RSQLite::dbGetQuery(db,'SELECT tmp.query.id, name,parent, rank FROM tmp.query LEFT OUTER JOIN nodes ON tmp.query.id=nodes.id LEFT OUTER JOIN names ON tmp.query.id=names.id WHERE names.scientific=1 OR names.scientific IS NULL')
    if(!identical(taxaDf$id,unname(ids))&!getDescendants)stop(simpleError('Problem finding ids')) #don't actually need the unname here since as.numeric strips names but good to be safe
    return(taxaDf[,c('name','parent','rank')])
  }
}


checkDownloadMd5<-function(url,file,errorIfNoMd5=FALSE){
  md5<-sprintf('%s.md5',url)
  tmp<-tempfile()
  check<-tryCatch(curl::curl_download(md5,tmp,mode='wb',quiet=TRUE),warning=function(xx)FALSE,error=function(xx)FALSE)
  if(check==FALSE){
    if(errorIfNoMd5)stop("Problem downloading md5 ",md5)
    else return(list('result'=TRUE,'remote'=as.character(NA),'local'=as.character(NA)))
  }
  hash<-strsplit(readLines(tmp),' ')[[1]][1]
  localHash<-tools::md5sum(file)
  return(list('result'=unname(hash==localHash),'remote'=hash,'local'=unname(localHash)))
}


#' Get taxonomic ranks for a taxa
#'
#' Take NCBI taxa IDs and get the corresponding taxa ranks from a name and node SQLite database
#'
#' @param ids a vector of ids to find taxonomy for
#' @param sqlFile a string giving the path to a SQLite file containing names and nodes tables
#' @param desiredTaxa a vector of strings giving the desired taxa levels
#' @param ... legacy additional arguments to original data.table based getTaxonomy function. Used only for support for deprecated function, do not use in new code.
#' @return a matrix of taxonomic strings with a row for each id and a column for each desiredTaxa rank
#' @export
#' @seealso \code{\link{read.nodes.sql}}, \code{\link{read.names.sql}}
#' @examples
#' sqlFile<-tempfile()
#' namesText<-c(
#'   "1\t|\tall\t|\t\t|\tsynonym\t|",
#'   "1\t|\troot\t|\t\t|\tscientific name\t|",
#'   "2\t|\tBacteria\t|\tBacteria <prokaryotes>\t|\tscientific name\t|",
#'   "2\t|\tMonera\t|\tMonera <Bacteria>\t|\tin-part\t|",
#'   "2\t|\tProcaryotae\t|\tProcaryotae <Bacteria>\t|\tin-part\t|",
#'   "9606\t|\tHomo sapiens\t|\t\t|\tscientific name",
#'   "9605\t|\tHomo\t|\t\t|\tscientific name",
#'   "207598\t|\tHomininae\t|\t\t|\tscientific name",
#'   "9604\t|\tHominidae\t|\t\t|\tscientific name",
#'   "314295\t|\tHominoidea\t|\t\t|\tscientific name",
#'   "9526\t|\tCatarrhini\t|\t\t|\tscientific name",
#'   "314293\t|\tSimiiformes\t|\t\t|\tscientific name",
#'   "376913\t|\tHaplorrhini\t|\t\t|\tscientific name",
#'   "9443\t|\tPrimates\t|\t\t|\tscientific name",
#'   "314146\t|\tEuarchontoglires\t|\t\t|\tscientific name",
#'   "1437010\t|\tBoreoeutheria\t|\t\t|\tscientific name",
#'   "9347\t|\tEutheria\t|\t\t|\tscientific name",
#'   "32525\t|\tTheria\t|\t\t|\tscientific name",
#'   "40674\t|\tMammalia\t|\t\t|\tscientific name",
#'   "32524\t|\tAmniota\t|\t\t|\tscientific name",
#'   "32523\t|\tTetrapoda\t|\t\t|\tscientific name",
#'   "1338369\t|\tDipnotetrapodomorpha\t|\t\t|\tscientific name",
#'   "8287\t|\tSarcopterygii\t|\t\t|\tscientific name",
#'   "117571\t|\tEuteleostomi\t|\t\t|\tscientific name",
#'   "117570\t|\tTeleostomi\t|\t\t|\tscientific name",
#'   "7776\t|\tGnathostomata\t|\t\t|\tscientific name",
#'   "7742\t|\tVertebrata\t|\t\t|\tscientific name",
#'   "89593\t|\tCraniata\t|\t\t|\tscientific name",
#'   "7711\t|\tChordata\t|\t\t|\tscientific name",
#'   "33511\t|\tDeuterostomia\t|\t\t|\tscientific name",
#'   "33213\t|\tBilateria\t|\t\t|\tscientific name",
#'   "6072\t|\tEumetazoa\t|\t\t|\tscientific name",
#'   "33208\t|\tMetazoa\t|\t\t|\tscientific name",
#'   "33154\t|\tOpisthokonta\t|\t\t|\tscientific name",
#'   "2759\t|\tEukaryota\t|\t\t|\tscientific name",
#'   "131567\t|\tcellular organisms\t|\t\t|\tscientific name"
#' )
#' tmpFile<-tempfile()
#' writeLines(namesText,tmpFile)
#' taxaNames<-read.names.sql(tmpFile,sqlFile)
#' nodesText<-c(
#'  "1\t|\t1\t|\tno rank\t|\t\t|\t8\t|\t0\t|\t1\t|\t0\t|\t0\t|\t0\t|\t0\t|\t0\t|\t\t|",
#'   "2\t|\t131567\t|\tsuperkingdom\t|\t\t|\t0\t|\t0\t|\t11\t|\t0\t|\t0\t|\t0\t|\t0\t|\t0\t|\t\t|",
#'   "6\t|\t335928\t|\tgenus\t|\t\t|\t0\t|\t1\t|\t11\t|\t1\t|\t0\t|\t1\t|\t0\t|\t0\t|\t\t|",
#'   "7\t|\t6\t|\tspecies\t|\tAC\t|\t0\t|\t1\t|\t11\t|\t1\t|\t0\t|\t1\t|\t1\t|\t0\t|\t\t|",
#'   "9\t|\t32199\t|\tspecies\t|\tBA\t|\t0\t|\t1\t|\t11\t|\t1\t|\t0\t|\t1\t|\t1\t|\t0\t|\t\t|",
#'   "9606\t|\t9605\t|\tspecies", "9605\t|\t207598\t|\tgenus", "207598\t|\t9604\t|\tsubfamily",
#'   "9604\t|\t314295\t|\tfamily", "314295\t|\t9526\t|\tsuperfamily",
#'   "9526\t|\t314293\t|\tparvorder", "314293\t|\t376913\t|\tinfraorder",
#'   "376913\t|\t9443\t|\tsuborder", "9443\t|\t314146\t|\torder",
#'   "314146\t|\t1437010\t|\tsuperorder", "1437010\t|\t9347\t|\tno rank",
#'   "9347\t|\t32525\t|\tno rank", "32525\t|\t40674\t|\tno rank",
#'   "40674\t|\t32524\t|\tclass", "32524\t|\t32523\t|\tno rank", "32523\t|\t1338369\t|\tno rank",
#'   "1338369\t|\t8287\t|\tno rank", "8287\t|\t117571\t|\tno rank",
#'   "117571\t|\t117570\t|\tno rank", "117570\t|\t7776\t|\tno rank",
#'   "7776\t|\t7742\t|\tno rank", "7742\t|\t89593\t|\tno rank", "89593\t|\t7711\t|\tsubphylum",
#'   "7711\t|\t33511\t|\tphylum", "33511\t|\t33213\t|\tno rank", "33213\t|\t6072\t|\tno rank",
#'   "6072\t|\t33208\t|\tno rank", "33208\t|\t33154\t|\tkingdom",
#'   "33154\t|\t2759\t|\tno rank", "2759\t|\t131567\t|\tsuperkingdom",
#'   "131567\t|\t1\t|\tno rank"
#' )
#' writeLines(nodesText,tmpFile)
#' taxaNodes<-read.nodes.sql(tmpFile,sqlFile)
#' getTaxonomy(c(9606,9605),sqlFile)
getTaxonomy<-function(ids,sqlFile='nameNode.sqlite',..., desiredTaxa=c('superkingdom','phylum','class','order','family','genus','species')){
  if('data.table' %in% class(sqlFile)){
    return(getTaxonomy2(ids,sqlFile,...,desiredTaxa=desiredTaxa))
  }
  ids<-as.numeric(ids)
  if(length(ids)==0)return(NULL)
  uniqIds<-unique(ids)
  taxa<-matrix(as.character(NA),ncol=length(desiredTaxa),nrow=length(uniqIds),dimnames=list(format(uniqIds,scientific=FALSE),desiredTaxa))
  rep<-0
  currentIds<-uniqIds
  while(any(stillWorking<-!is.na(currentIds)&currentIds!=1)){
    parents<-getParentNodes(currentIds[stillWorking],sqlFile)
    for(ii in desiredTaxa[desiredTaxa %in% parents$rank]){
      selector<-parents[,'rank']==ii&!is.na(parents[,'rank'])
      taxa[which(stillWorking)[selector],ii]<-parents[selector,'name']
    }
    rep<-rep+1
    currentIds[stillWorking]<-parents$parent
    if(rep>200)stop('Found cycle in taxonomy')
  }
  out<-taxa[format(ids,scientific=FALSE),,drop=FALSE]
  return(out)
}

#' Get descendant ranks for a taxa
#'
#' Take a NCBI taxa ID and get the descendant taxa matching a given rank from a name and node SQLite database
#'
#' @param ids a vector of ids to find descendants for
#' @param sqlFile a string giving the path to a SQLite file containing names and nodes tables
#' @param desiredTaxa a vector of strings giving the desired taxa levels
#' @return a vector of strings giving the names a for each descendant taxa
#' @export
#' @seealso \code{\link{read.nodes.sql}}, \code{\link{read.names.sql}}
#' @examples
#' sqlFile<-tempfile()
#' namesText<-c(
#'   "1\t|\troot\t|\t\t|\tscientific name\t|",
#'   "2\t|\tBacteria\t|\tBacteria <prokaryotes>\t|\tscientific name\t|",
#'   "2\t|\tProcaryotae\t|\tProcaryotae <Bacteria>\t|\tin-part\t|",
#'   "9606\t|\tHomo sapiens\t|\t\t|\tscientific name",
#'   "9605\t|\tHomo\t|\t\t|\tscientific name",
#'   "207598\t|\tHomininae\t|\t\t|\tscientific name",
#'   "9604\t|\tHominidae\t|\t\t|\tscientific name",
#'   "314295\t|\tHominoidea\t|\t\t|\tscientific name",
#'   "9526\t|\tCatarrhini\t|\t\t|\tscientific name",
#'   "314293\t|\tSimiiformes\t|\t\t|\tscientific name",
#'   "376913\t|\tHaplorrhini\t|\t\t|\tscientific name",
#'   "9443\t|\tPrimates\t|\t\t|\tscientific name",
#'   "314146\t|\tEuarchontoglires\t|\t\t|\tscientific name",
#'   "1437010\t|\tBoreoeutheria\t|\t\t|\tscientific name",
#'   "9347\t|\tEutheria\t|\t\t|\tscientific name",
#'   "32525\t|\tTheria\t|\t\t|\tscientific name",
#'   "40674\t|\tMammalia\t|\t\t|\tscientific name",
#'   "32524\t|\tAmniota\t|\t\t|\tscientific name",
#'   "32523\t|\tTetrapoda\t|\t\t|\tscientific name",
#'   "1338369\t|\tDipnotetrapodomorpha\t|\t\t|\tscientific name",
#'   "8287\t|\tSarcopterygii\t|\t\t|\tscientific name",
#'   "117571\t|\tEuteleostomi\t|\t\t|\tscientific name",
#'   "117570\t|\tTeleostomi\t|\t\t|\tscientific name",
#'   "7776\t|\tGnathostomata\t|\t\t|\tscientific name",
#'   "7742\t|\tVertebrata\t|\t\t|\tscientific name",
#'   "89593\t|\tCraniata\t|\t\t|\tscientific name",
#'   "7711\t|\tChordata\t|\t\t|\tscientific name",
#'   "33511\t|\tDeuterostomia\t|\t\t|\tscientific name",
#'   "33213\t|\tBilateria\t|\t\t|\tscientific name",
#'   "6072\t|\tEumetazoa\t|\t\t|\tscientific name",
#'   "33208\t|\tMetazoa\t|\t\t|\tscientific name",
#'   "33154\t|\tOpisthokonta\t|\t\t|\tscientific name",
#'   "2759\t|\tEukaryota\t|\t\t|\tscientific name",
#'   "131567\t|\tcellular organisms\t|\t\t|\tscientific name",
#'   "1425170\t|\tHomo heidelbergensis\t|\t\t|\tscientific name"
#' )
#' tmpFile<-tempfile()
#' writeLines(namesText,tmpFile)
#' taxaNames<-read.names.sql(tmpFile,sqlFile)
#' nodesText<-c(
#'  "1\t|\t1\t|\tno rank\t|\t\t|\t8\t|\t0\t|\t1\t|\t0\t|\t0\t|\t0\t|\t0\t|\t0\t|\t\t|",
#'   "2\t|\t131567\t|\tsuperkingdom\t|\t\t|\t0\t|\t0\t|\t11\t|\t0\t|\t0\t|\t0\t|\t0\t|\t0\t|\t\t|",
#'   "6\t|\t335928\t|\tgenus\t|\t\t|\t0\t|\t1\t|\t11\t|\t1\t|\t0\t|\t1\t|\t0\t|\t0\t|\t\t|",
#'   "7\t|\t6\t|\tspecies\t|\tAC\t|\t0\t|\t1\t|\t11\t|\t1\t|\t0\t|\t1\t|\t1\t|\t0\t|\t\t|",
#'   "9\t|\t32199\t|\tspecies\t|\tBA\t|\t0\t|\t1\t|\t11\t|\t1\t|\t0\t|\t1\t|\t1\t|\t0\t|\t\t|",
#'   "9606\t|\t9605\t|\tspecies", "9605\t|\t207598\t|\tgenus", "207598\t|\t9604\t|\tsubfamily",
#'   "9604\t|\t314295\t|\tfamily", "314295\t|\t9526\t|\tsuperfamily",
#'   "9526\t|\t314293\t|\tparvorder", "314293\t|\t376913\t|\tinfraorder",
#'   "376913\t|\t9443\t|\tsuborder", "9443\t|\t314146\t|\torder",
#'   "314146\t|\t1437010\t|\tsuperorder", "1437010\t|\t9347\t|\tno rank",
#'   "9347\t|\t32525\t|\tno rank", "32525\t|\t40674\t|\tno rank",
#'   "40674\t|\t32524\t|\tclass", "32524\t|\t32523\t|\tno rank", "32523\t|\t1338369\t|\tno rank",
#'   "1338369\t|\t8287\t|\tno rank", "8287\t|\t117571\t|\tno rank",
#'   "117571\t|\t117570\t|\tno rank", "117570\t|\t7776\t|\tno rank",
#'   "7776\t|\t7742\t|\tno rank", "7742\t|\t89593\t|\tno rank", "89593\t|\t7711\t|\tsubphylum",
#'   "7711\t|\t33511\t|\tphylum", "33511\t|\t33213\t|\tno rank", "33213\t|\t6072\t|\tno rank",
#'   "6072\t|\t33208\t|\tno rank", "33208\t|\t33154\t|\tkingdom",
#'   "33154\t|\t2759\t|\tno rank", "2759\t|\t131567\t|\tsuperkingdom",
#'   "131567\t|\t1\t|\tno rank", '1425170\t|\t9605\t|\tspecies'
#' )
#' writeLines(nodesText,tmpFile)
#' taxaNodes<-read.nodes.sql(tmpFile,sqlFile)
#' getDescendants(c(9604),sqlFile)
getDescendants<-function(ids,sqlFile='nameNode.sqlite', desiredTaxa='species'){
  ids<-unique(as.numeric(ids))
  if(length(ids)==0)return(NULL)
  rep<-0
  currentIds<-ids
  allIds<-c()
  while(length(currentIds)>0){
    descendants<-getParentNodes(currentIds,sqlFile,TRUE)
    descendants<-descendants[!is.na(descendants$descendant),]
    allIds<-unique(c(allIds,descendants[descendants$rank %in% desiredTaxa,'name']))
    rep<-rep+1
    currentIds<-descendants$descendant
    if(rep>200)stop('Found cycle in taxonomy')
  }
  return(allIds)
}

#' Get all taxonomy for a taxa
#'
#' Take NCBI taxa IDs and get all taxonomic ranks from name and node SQLite database. Ranks that occur more than once are made unique with a postfix through \code{\link{make.unique}}
#'
#' @param ids a vector of ids to find taxonomy for
#' @param sqlFile a string giving the path to a SQLite file containing names and nodes tables
#' @return a list of vectors with each element containing a vector of taxonomic strings with names corresponding to the taxonomic rank
#' @export
#' @seealso \code{\link{read.nodes.sql}}, \code{\link{read.names.sql}}, \code{\link{normalizeTaxa}}
#' @examples
#' sqlFile<-tempfile()
#' namesText<-c(
#'   "1\t|\tall\t|\t\t|\tsynonym\t|",
#'   "1\t|\troot\t|\t\t|\tscientific name\t|",
#'   "2\t|\tBacteria\t|\tBacteria <prokaryotes>\t|\tscientific name\t|",
#'   "2\t|\tMonera\t|\tMonera <Bacteria>\t|\tin-part\t|",
#'   "2\t|\tProcaryotae\t|\tProcaryotae <Bacteria>\t|\tin-part\t|",
#'   "9606\t|\tHomo sapiens\t|\t\t|\tscientific name",
#'   "9605\t|\tHomo\t|\t\t|\tscientific name",
#'   "207598\t|\tHomininae\t|\t\t|\tscientific name",
#'   "9604\t|\tHominidae\t|\t\t|\tscientific name",
#'   "314295\t|\tHominoidea\t|\t\t|\tscientific name",
#'   "9526\t|\tCatarrhini\t|\t\t|\tscientific name",
#'   "314293\t|\tSimiiformes\t|\t\t|\tscientific name",
#'   "376913\t|\tHaplorrhini\t|\t\t|\tscientific name",
#'   "9443\t|\tPrimates\t|\t\t|\tscientific name",
#'   "314146\t|\tEuarchontoglires\t|\t\t|\tscientific name",
#'   "1437010\t|\tBoreoeutheria\t|\t\t|\tscientific name",
#'   "9347\t|\tEutheria\t|\t\t|\tscientific name",
#'   "32525\t|\tTheria\t|\t\t|\tscientific name",
#'   "40674\t|\tMammalia\t|\t\t|\tscientific name",
#'   "32524\t|\tAmniota\t|\t\t|\tscientific name",
#'   "32523\t|\tTetrapoda\t|\t\t|\tscientific name",
#'   "1338369\t|\tDipnotetrapodomorpha\t|\t\t|\tscientific name",
#'   "8287\t|\tSarcopterygii\t|\t\t|\tscientific name",
#'   "117571\t|\tEuteleostomi\t|\t\t|\tscientific name",
#'   "117570\t|\tTeleostomi\t|\t\t|\tscientific name",
#'   "7776\t|\tGnathostomata\t|\t\t|\tscientific name",
#'   "7742\t|\tVertebrata\t|\t\t|\tscientific name",
#'   "89593\t|\tCraniata\t|\t\t|\tscientific name",
#'   "7711\t|\tChordata\t|\t\t|\tscientific name",
#'   "33511\t|\tDeuterostomia\t|\t\t|\tscientific name",
#'   "33213\t|\tBilateria\t|\t\t|\tscientific name",
#'   "6072\t|\tEumetazoa\t|\t\t|\tscientific name",
#'   "33208\t|\tMetazoa\t|\t\t|\tscientific name",
#'   "33154\t|\tOpisthokonta\t|\t\t|\tscientific name",
#'   "2759\t|\tEukaryota\t|\t\t|\tscientific name",
#'   "131567\t|\tcellular organisms\t|\t\t|\tscientific name"
#' )
#' tmpFile<-tempfile()
#' writeLines(namesText,tmpFile)
#' taxaNames<-read.names.sql(tmpFile,sqlFile)
#' nodesText<-c(
#'  "1\t|\t1\t|\tno rank\t|\t\t|\t8\t|\t0\t|\t1\t|\t0\t|\t0\t|\t0\t|\t0\t|\t0\t|\t\t|",
#'   "2\t|\t131567\t|\tsuperkingdom\t|\t\t|\t0\t|\t0\t|\t11\t|\t0\t|\t0\t|\t0\t|\t0\t|\t0\t|\t\t|",
#'   "6\t|\t335928\t|\tgenus\t|\t\t|\t0\t|\t1\t|\t11\t|\t1\t|\t0\t|\t1\t|\t0\t|\t0\t|\t\t|",
#'   "7\t|\t6\t|\tspecies\t|\tAC\t|\t0\t|\t1\t|\t11\t|\t1\t|\t0\t|\t1\t|\t1\t|\t0\t|\t\t|",
#'   "9\t|\t32199\t|\tspecies\t|\tBA\t|\t0\t|\t1\t|\t11\t|\t1\t|\t0\t|\t1\t|\t1\t|\t0\t|\t\t|",
#'   "9606\t|\t9605\t|\tspecies", "9605\t|\t207598\t|\tgenus", "207598\t|\t9604\t|\tsubfamily",
#'   "9604\t|\t314295\t|\tfamily", "314295\t|\t9526\t|\tsuperfamily",
#'   "9526\t|\t314293\t|\tparvorder", "314293\t|\t376913\t|\tinfraorder",
#'   "376913\t|\t9443\t|\tsuborder", "9443\t|\t314146\t|\torder",
#'   "314146\t|\t1437010\t|\tsuperorder", "1437010\t|\t9347\t|\tno rank",
#'   "9347\t|\t32525\t|\tno rank", "32525\t|\t40674\t|\tno rank",
#'   "40674\t|\t32524\t|\tclass", "32524\t|\t32523\t|\tno rank", "32523\t|\t1338369\t|\tno rank",
#'   "1338369\t|\t8287\t|\tno rank", "8287\t|\t117571\t|\tno rank",
#'   "117571\t|\t117570\t|\tno rank", "117570\t|\t7776\t|\tno rank",
#'   "7776\t|\t7742\t|\tno rank", "7742\t|\t89593\t|\tno rank", "89593\t|\t7711\t|\tsubphylum",
#'   "7711\t|\t33511\t|\tphylum", "33511\t|\t33213\t|\tno rank", "33213\t|\t6072\t|\tno rank",
#'   "6072\t|\t33208\t|\tno rank", "33208\t|\t33154\t|\tkingdom",
#'   "33154\t|\t2759\t|\tno rank", "2759\t|\t131567\t|\tsuperkingdom",
#'   "131567\t|\t1\t|\tno rank"
#' )
#' writeLines(nodesText,tmpFile)
#' taxaNodes<-read.nodes.sql(tmpFile,sqlFile)
#' getRawTaxonomy(c(9606,9605),sqlFile)
getRawTaxonomy<-function (ids,sqlFile='nameNode.sqlite'){
  ids<-as.numeric(ids)
  if(length(ids)==0)return(NULL)
  uniqIds<-unique(ids)
  taxa<-rep(list(NULL),length(uniqIds))
  names(taxa)<-format(uniqIds,scientific=FALSE)
  rep<-0
  currentIds<-uniqIds
  while(any(stillWorking<-!is.na(currentIds)&currentIds!=1)){
    parents<-getParentNodes(currentIds[stillWorking],sqlFile)
    taxa[stillWorking]<-mapply(function(xx,rank,name){
      # while(rank %in% names(xx))rank<-sprintf('%s_',rank)
      rank<-utils::tail(make.unique(c(names(xx),rank)),1)
      xx[rank]<-name
      return(xx)
    },taxa[stillWorking],parents[,'rank'],parents[,'name'],SIMPLIFY=FALSE)
    rep<-rep+1
    currentIds[stillWorking]<-parents$parent
    if(rep>200)stop('Found cycle in taxonomy')
  }
  out<-taxa[format(ids,scientific=FALSE)]
  names(out)<-format(ids,scientific=FALSE)
  return(out)
}

#' Convert accessions to taxa
#'
#' Convert a vector of NCBI accession numbers to their assigned taxonomy
#'
#' @param accessions a vector of NCBI accession strings to convert to taxa
#' @param sqlFile a string giving the path to a SQLite file screated by \code{\link{read.accession2taxid}}
#' @param version either 'version' indicating that taxaids are versioned e.g. Z17427.1 or 'base' indicating that taxaids do not have version numbers e.g. Z17427
#' @return a vector of NCBI taxa ids
#' @export
#' @references \url{https://ftp.ncbi.nih.gov/pub/taxonomy/accession2taxid/}
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
#' read.accession2taxid(inFile,sqlFile,vocal=FALSE)
#' accessionToTaxa(c("Z17430.1","Z17429.1","X62402.1",'NOTREAL'),sqlFile)
accessionToTaxa<-function(accessions,sqlFile,version=c('version','base')){
  version<-match.arg(version)
  if(version=='version')version<-'accession'
  if(!file.exists(sqlFile))stop(sqlFile,' does not exist.')
  if(length(accessions)==0)return(NULL)
  tmp<-tempfile()
  #set up a new table of accessions in a temp db (avoiding concurrency issues)
  #some trouble with dbWriteTable writing to "tmp.xxx" in the main database if we do this inside the attach
  tmpDb <- RSQLite::dbConnect(RSQLite::SQLite(), dbname=tmp)
  on.exit(if(file.exists(tmp))file.remove(tmp))
  on.exit(RSQLite::dbDisconnect(tmpDb),add=TRUE)
  RSQLite::dbWriteTable(tmpDb,'query',data.frame('accession'=as.character(accessions),stringsAsFactors=FALSE),overwrite=TRUE)
  #load the big sql
  db <- RSQLite::dbConnect(RSQLite::SQLite(), dbname=sqlFile)
  on.exit(RSQLite::dbDisconnect(db),add=TRUE)
  #attach the temp table
  RSQLite::dbExecute(db, sprintf("ATTACH '%s' AS tmp",tmp))
  #hangs on next if accessions are numeric
  taxaDf<-RSQLite::dbGetQuery(db,sprintf('SELECT tmp.query.accession, taxa FROM tmp.query LEFT OUTER JOIN accessionTaxa ON tmp.query.accession=accessionTaxa.%s',version))
  RSQLite::dbExecute(db,'DROP TABLE tmp.query')
  RSQLite::dbExecute(db,'DETACH tmp')
  file.remove(tmp)
  if(!identical(taxaDf$accession,unname(as.character(accessions))))stop(simpleError('Query and SQL mismatch'))
  return(taxaDf$taxa)
}

#' Condense multiple taxonomic assignments to their most recent common branch
#'
#' Take a table of taxonomic assignments, e.g. assignments from hits to a read, and condense it to a single vector with NAs where there are disagreements between the hits.
#'
#' @param taxaTable a matrix or data.frame with hits on the rows and various levels of taxonomy in the columns
#' @param groupings a vector of groups e.g. read queries to condense taxa within
#' @return a matrix with \code{ncol(taxaTable)} taxonomy columns with a row for each unique id (labelled on rownames) with NAs where there was not complete agreement for an id
#' @export
#' @examples
#' taxas<-matrix(c(
#'  'a','b','c','e',
#'  'a','b','d','e'
#' ),nrow=2,byrow=TRUE)
#' condenseTaxa(taxas)
#' condenseTaxa(taxas[c(1,2,2),],c(1,1,2))
condenseTaxa<-function(taxaTable,groupings=rep(1,nrow(taxaTable))){
  nCol<-ncol(taxaTable)
  if(nrow(taxaTable)==0)return(NULL)
  tmp<-tempfile()
  #mask commas if present
  #matrix() to make sure it stays matrix if only a single row
  taxaTable<-matrix(apply(taxaTable,2,function(xx)gsub(',','_!_!_',xx)),ncol=nCol,dimnames=dimnames(taxaTable))
  #mask NAs if present (otherwise not counted in concatenate)
  taxaTable[is.na(taxaTable)]<-'__NAFILLER__'
  on.exit(file.remove(tmp))
  tmpDb <- RSQLite::dbConnect(RSQLite::SQLite(), dbname=tmp)
  on.exit(RSQLite::dbDisconnect(tmpDb),add=TRUE)
  if(is.null(colnames(taxaTable)))colnames(taxaTable)<-sprintf("V%d",1:nCol)
  rownames(taxaTable)<-NULL
  RSQLite::dbWriteTable(tmpDb,'tmp',cbind(as.data.frame(taxaTable,stringsAsFactors=FALSE),'id'=groupings),overwrite=TRUE)
  RSQLite::dbExecute(tmpDb,"CREATE INDEX index_id ON tmp (id)")
  colSelects<-sprintf('GROUP_CONCAT(DISTINCT(`%s`)) AS `%s`',colnames(taxaTable),colnames(taxaTable))
  query<-sprintf("SELECT id, %s FROM tmp GROUP BY id",paste(colSelects,collapse=', '))
  out<-RSQLite::dbGetQuery(tmpDb,query)
  out<-t(apply(out,1,function(xx){
    isBad<-grepl(',',xx)
    firstDisagree<-min(c(Inf,which(isBad)))
    if(firstDisagree<=length(xx))xx[firstDisagree:length(xx)]<-NA
    return(xx)
  }))
  #turn commas back
  out[,colnames(out)!='id']<-apply(out[,colnames(out)!='id',drop=FALSE],2,function(xx)gsub('_!_!_',',',xx))
  #turn NAs back
  out[out=='__NAFILLER__']<-NA
  #remove extra spaces added by sqlite
  out[,'id']<-trimws(out[,'id'])
  rownames(out)<-out[,'id']
  return(out[,colnames(out)!='id',drop=FALSE])
}



#' Download names and nodes files from NCBI
#'
#' Download a taxdump.tar.gz file from NCBI servers and extract the names.dmp and nodes.dmp files from it. These can then be used to create a SQLite database with \code{\link{read.names.sql}} and \code{\link{read.nodes.sql}}. Note that if the files already exist in the target directory then this function will not redownload them. Delete the files if a fresh download is desired.
#'
#' @param outDir the directory to put names.dmp and nodes.dmp in
#' @param url the url where taxdump.tar.gz is located
#' @param fileNames the filenames desired from the tar.gz file
#' @param protocol the protocol to be used for downloading. Probably either \code{'http'} or \code{'ftp'}. Overridden if \code{url} is provided directly
#' @param resume if TRUE attempt to resume downloading an interrupted file without starting over from the beginning
#' @return a vector of file path strings of the locations of the output files
#' @seealso \code{\link{read.nodes.sql}}, \code{\link{read.names.sql}}
#' @references \url{https://ftp.ncbi.nih.gov/pub/taxonomy/}, \url{https://www.ncbi.nlm.nih.gov/Taxonomy/taxonomyhome.html/}
#' @export
#' @examples
#' \dontrun{
#'   getNamesAndNodes()
#' }
getNamesAndNodes<-function(outDir='.',url=sprintf('%s://ftp.ncbi.nih.gov/pub/taxonomy/taxdump.tar.gz',protocol),fileNames=c('names.dmp','nodes.dmp'),protocol='ftp',resume=TRUE){
  outFiles<-file.path(outDir,fileNames)
  if(all(file.exists(outFiles))){
    message(paste(outFiles,collapse=', '),' already exist. Delete to redownload')
    return(outFiles)
  }
  base<-basename(url)
  tmpDir<-tempfile()
  dir.create(tmpDir)
  tarFile<-file.path(tempdir(),base)
  resumableDownload(url,tarFile,resume=resume)
  check<-checkDownloadMd5(url,tarFile)
  if(!check[['result']])stop('Downloaded file does not match ',url,' File corrupted or download ended early?\nLocal: ',check[['local']],'\nRemote: ',check[['remote']])
  utils::untar(tarFile,fileNames,exdir=tmpDir,tar='internal')
  tmpFiles<-file.path(tmpDir,fileNames)
  if(!all(file.exists(tmpFiles)))stop("Problem finding files ",paste(tmpFiles[!file.exists(tmpFiles)],collapse=', '))
  mapply(file.copy,tmpFiles,outFiles)
  if(!all(file.exists(outFiles)))stop("Problem copying files ",paste(outFiles[!file.exists(outFiles)],collapse=', '))
  file.remove(c(tarFile,tmpFiles))
  return(outFiles)
}

#' Download accession2taxid files from NCBI
#'
#' Download a nucl_xxx.accession2taxid.gz from NCBI servers. These can then be used to create a SQLite datanase with \code{\link{read.accession2taxid}}. Note that if the files already exist in the target directory then this function will not redownload them. Delete the files if a fresh download is desired.
#'
#' @param outDir the directory to put the accession2taxid.gz files in
#' @param baseUrl the url of the directory where accession2taxid.gz files are located
#' @param types the types if accession2taxid.gz files desired where type is the prefix of xxx.accession2taxid.gz. The default is to download all nucl_ accessions. For protein accessions, try \code{types=c('prot')}.
#' @param protocol the protocol to be used for downloading. Probably either \code{'http'} or \code{'ftp'}. Overridden if \code{baseUrl} is provided directly
#' @param resume if TRUE attempt to resume downloading an interrupted file without starting over from the beginning
#' @return a vector of file path strings of the locations of the output files
#' @seealso \code{\link{read.accession2taxid}}
#' @references \url{https://ftp.ncbi.nih.gov/pub/taxonomy/}, \url{https://www.ncbi.nlm.nih.gov/genbank/acc_prefix/}
#' @export
#' @examples
#' \dontrun{
#'   if(readline(
#'     "This will download a lot data and take a while to process.
#'      Make sure you have space and bandwidth. Type y to continue: "
#'   )!='y')
#'     stop('This is a stop to make sure no one downloads a bunch of data unintentionally')
#'
#'   getAccession2taxid()
#' }
getAccession2taxid<-function(outDir='.',baseUrl=sprintf('%s://ftp.ncbi.nih.gov/pub/taxonomy/accession2taxid/',protocol),types=c('nucl_gb','nucl_wgs'),protocol='ftp',resume=TRUE){
  fileNames<-sprintf('%s.accession2taxid.gz',types)
  outFiles<-file.path(outDir,fileNames)
  if(all(file.exists(outFiles))){
    message(paste(outFiles,collapse=', '),' already exist. Delete to redownload')
    return(outFiles)
  }
  message('This can be a big (several gigabytes) download. Please be patient and use a fast connection.')
  if(!substring(baseUrl,nchar(baseUrl)) %in% c('/','\\'))baseUrl<-sprintf('%s/',baseUrl)
  urls<-paste(baseUrl,fileNames,sep='')
  mapply(function(xx,yy){
    resumableDownload(xx,yy,resume=resume)
    check<-checkDownloadMd5(xx,yy)
    if(!check[['result']])stop('Downloaded file does not match ',xx,' File corrupted or download ended early?\nLocal: ',check[['local']],'\nRemote: ',check[['remote']])
  },urls,outFiles)
  return(outFiles)
}

#' Find a given taxa by name
#'
#' Find a taxa by string in the NCBI taxonomy. Note that NCBI species are stored as Genus species e.g. "Bos taurus". Ambiguous taxa names will return a comma concatenated string e.g. "123,234" and generate a warning. NOTE: This function is now deprecated for \code{\link{getId}} (using SQLite rather than data.table).
#'
#' @param taxa a vector of taxonomic names
#' @param taxaNames a names data.table from \code{\link{read.names}}
#' @return a vector of character strings giving taxa IDs (potentially comma concatenated for any taxa with ambiguous names)
#' @seealso \code{\link{getId}}
#' @export
#' @examples
#' namesText<-c(
#'   "1\t|\tall\t|\t\t|\tsynonym\t|",
#'   "1\t|\troot\t|\t\t|\tscientific name\t|",
#'   "3\t|\tMulti\t|\tBacteria <prokaryotes>\t|\tscientific name\t|",
#'   "4\t|\tMulti\t|\tBacteria <prokaryotes>\t|\tscientific name\t|",
#'   "2\t|\tBacteria\t|\tBacteria <prokaryotes>\t|\tscientific name\t|",
#'   "2\t|\tMonera\t|\tMonera <Bacteria>\t|\tin-part\t|",
#'   "2\t|\tProcaryotae\t|\tProcaryotae <Bacteria>\t|\tin-part\t|"
#' )
#' tmpFile<-tempfile()
#' writeLines(namesText,tmpFile)
#' names<-read.names(tmpFile)
#' getId2('Bacteria',names)
#' getId2('Not a real name',names)
#' getId2('Multi',names)
getId2<-function(taxa,taxaNames){
  .Deprecated('getId','taxonomizr',"taxonomizr is moving from data.table to SQLite databases to improve performance. This will require changing nodes and names processing. Please see ?getId or ?taxonomizrSwitch")
  uniqTaxa<-unique(taxa)
  out<-lapply(uniqTaxa,function(xx){
    ids<-taxaNames[as.list(xx),on='name']$id
  })
  multiHits<-sapply(out,length)>1
  if(any(multiHits)){
    warning('Multiple taxa ids found for ',paste(taxa[multiHits],collapse=', '),'. Collapsing with commas')
    out<-sapply(out,function(xx)ifelse(all(is.na(xx))||is.null(xx),NA,paste(xx,collapse=',')))
  }
  out<-as.character(unlist(out))
  names(out)<-uniqTaxa
  return(unname(out[taxa]))
}

#' Find a given taxa by name
#'
#' Find a taxa by string in the NCBI taxonomy. Note that NCBI species are stored as Genus species e.g. "Bos taurus". Ambiguous taxa names will return a comma concatenated string e.g. "123,234" and generate a warning.
#'
#' @param taxa a vector of taxonomic names
#' @param sqlFile a string giving the path to a SQLite file containing a names tables
#' @param onlyScientific If TRUE then only match to scientific names. If FALSE use all names in database for matching (potentially increasing ambiguous matches).
#' @return a vector of character strings giving taxa IDs (potentially comma concatenated for any taxa with ambiguous names)
#' @seealso \code{\link{getTaxonomy}}, \code{\link{read.names.sql}}, \code{\link{getCommon}}
#' @export
#' @examples
#' namesText<-c(
#'   "1\t|\tall\t|\t\t|\tsynonym\t|",
#'   "1\t|\troot\t|\t\t|\tscientific name\t|",
#'   "3\t|\tMulti\t|\tBacteria <prokaryotes>\t|\tscientific name\t|",
#'   "4\t|\tMulti\t|\tBacteria <prokaryotes>\t|\tscientific name\t|",
#'   "2\t|\tBacteria\t|\tBacteria <prokaryotes>\t|\tscientific name\t|",
#'   "2\t|\tMonera\t|\tMonera <Bacteria>\t|\tin-part\t|",
#'   "2\t|\tProcaryotae\t|\tProcaryotae <Bacteria>\t|\tin-part\t|"
#' )
#' tmpFile<-tempfile()
#' writeLines(namesText,tmpFile)
#' sqlFile<-tempfile()
#' read.names.sql(tmpFile,sqlFile)
#' getId('Bacteria',sqlFile)
#' getId('Not a real name',sqlFile)
#' getId('Multi',sqlFile)
getId<-function(taxa,sqlFile='nameNode.sqlite',onlyScientific=TRUE){
  if('data.table' %in% class(sqlFile))return(getId2(taxa,sqlFile))
  tmp<-tempfile()
  on.exit(file.remove(tmp))
  uniqTaxa<-unique(taxa)
  tmpDb <- RSQLite::dbConnect(RSQLite::SQLite(), dbname=tmp)
  on.exit(RSQLite::dbDisconnect(tmpDb),add=TRUE)
  RSQLite::dbWriteTable(tmpDb,'query',data.frame('name'=uniqTaxa,stringsAsFactors=FALSE),overwrite=TRUE)
  db <- RSQLite::dbConnect(RSQLite::SQLite(), dbname=sqlFile)
  on.exit(RSQLite::dbDisconnect(db),add=TRUE)
  RSQLite::dbExecute(db, sprintf("ATTACH '%s' AS tmp",tmp))
  taxaDf<-RSQLite::dbGetQuery(db,sprintf('SELECT tmp.query.name, id FROM tmp.query LEFT OUTER JOIN names ON tmp.query.name=names.name%s',ifelse(onlyScientific,' WHERE names.scientific','')))
  taxaN<-tapply(taxaDf$id,taxaDf$name,length)
  if(any(taxaN>1)){
    warning('Multiple taxa ids found for ',paste(names(taxaN)[taxaN>1],collapse=', '),'. Collapsing with commas')
  }
  out<-tapply(taxaDf$id,taxaDf$name,FUN=function(xx)paste(sort(xx),collapse=','))
  return(as.character(unname(out[taxa])))
}

#' Find common names for a given taxa
#'
#' Find all common names recorded for a taxa in the NCBI taxonomy. Use \code{\link{getTaxonomy}} for scientific names.
#'
#' @param taxa a vector of accession numbers
#' @param sqlFile a string giving the path to a SQLite file containing a names tables
#' @param types a vector of strings giving the type of names desired e.g. "common name". If NULL then all types are returned
#' @return a named list of data.frames where each element corresponds to the query taxa IDs. Each data.frame contains columns name and type and each gives an available names and its name type
#' @seealso \code{\link{getTaxonomy}}, \code{\link{read.names.sql}}, \code{\link{getId}}
#' @export
#' @examples
#' namesText<-"9894\t|\tGiraffa camelopardalis (Linnaeus, 1758)\t|\t\t|\tauthority\t|
#' 9894\t|\tGiraffa camelopardalis\t|\t\t|\tscientific name\t|
#' 9894\t|\tgiraffe\t|\t\t|\tgenbank common name\t|
#' 9909\t|\taurochs\t|\t\t|\tgenbank common name\t|
#' 9909\t|\tBos primigenius Bojanus, 1827\t|\t\t|\tauthority\t|
#' 9909\t|\tBos primigenius\t|\t\t|\tscientific name\t|
#' 9913\t|\tBos bovis\t|\t\t|\tsynonym\t|
#' 9913\t|\tBos primigenius taurus\t|\t\t|\tsynonym\t|
#' 9913\t|\tBos taurus Linnaeus, 1758\t|\t\t|\tauthority\t|
#' 9913\t|\tBos taurus\t|\t\t|\tscientific name\t|
#' 9913\t|\tBovidae sp. Adi Nefas\t|\t\t|\tincludes\t|
#' 9913\t|\tbovine\t|\t\t|\tcommon name\t|
#' 9913\t|\tcattle\t|\t\t|\tgenbank common name\t|
#' 9913\t|\tcow\t|\t\t|\tcommon name\t|
#' 9913\t|\tdairy cow\t|\t\t|\tcommon name\t|
#' 9913\t|\tdomestic cattle\t|\t\t|\tcommon name\t|
#' 9913\t|\tdomestic cow\t|\t\t|\tcommon name\t|
#' 9913\t|\tox\t|\t\t|\tcommon name\t|
#' 9913\t|\toxen\t|\t\t|\tcommon name\t|
#' 9916\t|\tBoselaphus\t|\t\t|\tscientific name\t|"
#' tmpFile<-tempfile()
#' writeLines(namesText,tmpFile)
#' sqlFile<-tempfile()
#' read.names.sql(tmpFile,sqlFile)
#' getCommon(9909,sqlFile)
#' sapply(getCommon(c(9894,9913),sqlFile),function(xx)paste(xx$name,collapse='; '))
#' getCommon(c(9999999,9916,9894,9913),sqlFile,c("common name","genbank common name"))
getCommon<-function(taxa,sqlFile='nameNode.sqlite',types=NULL){
  if('data.table' %in% class(sqlFile))stop('data.table name nodes file not supported')
  tmp<-tempfile()
  on.exit(file.remove(tmp))
  uniqTaxa<-unique(taxa)
  tmpDb <- RSQLite::dbConnect(RSQLite::SQLite(), dbname=tmp)
  on.exit(RSQLite::dbDisconnect(tmpDb),add=TRUE)
  RSQLite::dbWriteTable(tmpDb,'query',data.frame('accession'=uniqTaxa,stringsAsFactors=FALSE),overwrite=TRUE)
  db <- RSQLite::dbConnect(RSQLite::SQLite(), dbname=sqlFile)
  on.exit(RSQLite::dbDisconnect(db),add=TRUE)
  if(!'type' %in% RSQLite::dbListFields(db,'names'))stop('The type field is not included in the ',sqlFile,' database. Please recreate the database to update')
  RSQLite::dbExecute(db, sprintf("ATTACH '%s' AS tmp",tmp))
  query<-sprintf('SELECT tmp.query.accession, names.name, names.type FROM tmp.query JOIN names ON tmp.query.accession=names.id%s',ifelse(is.null(types),'', sprintf(' WHERE names.type IN ("%s")',paste(types,collapse='","'))))
  taxaDf<-RSQLite::dbGetQuery(db,query)
  out<-split(taxaDf[,c('name','type')],taxaDf$accession)
  out<-lapply(out,function(xx){rownames(xx)<-NULL;xx})
  return(unname(out[as.character(taxa)]))
}

#attachTempDb<-function(db,df){
  #tmp<-tempfile()
  #on.exit(file.remove(tmp)) #will this kill the connection for the upstream function? Move to object?
  #uniqTaxa<-unique(taxa)
  #tmpDb <- RSQLite::dbConnect(RSQLite::SQLite(), dbname=tmp)
  #on.exit(RSQLite::dbDisconnect(tmpDb),add=TRUE)
  #RSQLite::dbWriteTable(tmpDb,'query',data.frame('accession'=uniqTaxa,stringsAsFactors=FALSE),overwrite=TRUE)
  #RSQLite::dbExecute(db, sprintf("ATTACH '%s' AS tmp",tmp))
#}


#' Download data from NCBI and set up SQLite database
#'
#' Convenience function to do all necessary preparations downloading names, nodes and accession2taxid data from NCBI and preprocessing into a SQLite database for downstream use.
#'
#' @param sqlFile character string giving the file location to store the SQLite database
#' @param tmpDir location for storing the downloaded files from NCBI. (Note that it may be useful to store these somewhere convenient to avoid redownloading)
#' @param getAccessions if TRUE download the very large accesssion2taxid files necessary to convert accessions to taxonomic IDs
#' @param vocal if TRUE output messages describing progress
#' @param ... additional arguments to getNamesAndNodes, getAccession2taxid or read.accession2taxid:
#' @inheritDotParams getNamesAndNodes -outDir
#' @inheritDotParams getAccession2taxid -outDir
#' @inheritDotParams read.accession2taxid -taxaFiles -sqlFile
#' @return a vector of character string giving the path to the SQLite file
#' @seealso \code{\link{getNamesAndNodes}}, \code{\link{getAccession2taxid}}, \code{\link{read.accession2taxid}}, \code{\link{read.nodes.sql}}, \code{\link{read.names.sql}}
#' @export
#' @examples
#' \dontrun{
#'   if(readline(
#'     "This will download a lot data and take a while to process.
#'      Make sure you have space and bandwidth. Type y to continue: "
#'   )!='y')
#'     stop('This is a stop to make sure no one downloads a bunch of data unintentionally')
#'
#'   prepareDatabase()
#' }
prepareDatabase<-function(sqlFile='nameNode.sqlite',tmpDir='.',getAccessions=TRUE,vocal=TRUE,...){
  if(!dir.exists(tmpDir))dir.create(tmpDir)
  if(file.exists(sqlFile)){
    message('SQLite database ',sqlFile,' already exists. Delete to regenerate')
    return(sqlFile)
  }
  argnames <- names(list(...))
  if(vocal)message('Downloading names and nodes with getNamesAndNodes()')
  args <- intersect(argnames, names(as.list(args(getNamesAndNodes))))
  do.call(getNamesAndNodes,c(list(tmpDir),list(...)[args]))
  nameFile<-file.path(tmpDir,'names.dmp')
  if(vocal)message('Preprocessing names with read.names.sql()')
  read.names.sql(nameFile,sqlFile=sqlFile)
  if(vocal)message('Preprocessing nodes with read.nodes.sql()')
  nodeFile<-file.path(tmpDir,'nodes.dmp')
  read.nodes.sql(nodeFile,sqlFile=sqlFile)
  if(getAccessions){
    if(vocal)message('Downloading accession2taxid with getAccession2taxid()')
    args <- intersect(argnames, names(as.list(args(getAccession2taxid))))
    accessionFiles<-do.call(getAccession2taxid,c(list(outDir=tmpDir),list(...)[args]))
    if(vocal)message('Preprocessing accession2taxid with read.accession2taxid()')
    args <- intersect(argnames, names(as.list(args(read.accession2taxid))))
    do.call(read.accession2taxid,c(list(accessionFiles,sqlFile,vocal=vocal),list(...)[args]))
  }
  return(sqlFile)
}

#' Find all accessions for a taxa
#'
#' Find accessions numbers for a given taxa ID the NCBI taxonomy. This will be pretty slow unless the database was built with indexTaxa=TRUE since the database would not have an index for taxaId.
#' @param taxaId a vector of taxonomic IDs
#' @param sqlFile a string giving the path to a SQLite file created by \code{\link{read.accession2taxid}}
#' @param version either 'version' indicating that taxaids are versioned e.g. Z17427.1 or 'base' indicating that taxaids do not have version numbers e.g. Z17427
#' @param limit return only this number of accessions or NULL for no limits
#' @return a vector of character strings giving taxa IDs (potentially comma concatenated for any taxa with ambiguous names)
#' @seealso \code{\link{read.accession2taxid}}
#' @export
#' @examples
#' taxa<-c(
#'   "accession\taccession.version\ttaxid\tgi",
#'   "Z17427\tZ17427.1\t3702\t16569",
#'   "Z17428\tZ17428.1\t3702\t16570",
#'   "Z17429\tZ17429.1\t3702\t16571",
#'   "Z17430\tZ17430.1\t3702\t16572"
#' )
#' inFile<-tempfile()
#' sqlFile<-tempfile()
#' writeLines(taxa,inFile)
#' read.accession2taxid(inFile,sqlFile,vocal=FALSE)
#' getAccessions(3702,sqlFile)
getAccessions<-function(taxaId,sqlFile,version=c('version','base'),limit=NULL){
  version<-match.arg(version)
  if(version=='version')version<-'accession'
  if(!file.exists(sqlFile))stop(sqlFile,' does not exist.')
  if(length(taxaId)==0)return(NULL)
  tmp<-tempfile()
  #set up a new table of accessions in a temp db (avoiding concurrency issues)
  #some trouble with dbWriteTable writing to "tmp.xxx" in the main database if we do this inside the attach
  tmpDb <- RSQLite::dbConnect(RSQLite::SQLite(), dbname=tmp)
  on.exit(RSQLite::dbDisconnect(tmpDb))
  RSQLite::dbWriteTable(tmpDb,'query',data.frame('taxa'=taxaId,stringsAsFactors=FALSE),overwrite=TRUE)
  #load the big sql
  db <- RSQLite::dbConnect(RSQLite::SQLite(), dbname=sqlFile)
  on.exit(RSQLite::dbDisconnect(db),add=TRUE)
  #attach the temp table
  RSQLite::dbExecute(db, sprintf("ATTACH '%s' AS tmp",tmp))
  taxaDf<-RSQLite::dbGetQuery(db,sprintf('SELECT tmp.query.taxa, %s FROM tmp.query LEFT OUTER JOIN accessionTaxa ON tmp.query.taxa=accessionTaxa.taxa%s',version,ifelse(!is.null(limit),sprintf(' LIMIT %s',limit),'')))
  RSQLite::dbExecute(db,'DROP TABLE tmp.query')
  RSQLite::dbExecute(db,'DETACH tmp')
  file.remove(tmp)
  colnames(taxaDf)<-c('taxa','accession')
  return(taxaDf)
}

#' Create a Newick tree from taxonomy
#'
#' Create a Newick formatted tree from a data.frame of taxonomic assignments
#' @param taxa a matrix with a row for each leaf of the tree and a column for each taxonomic classification e.g. the output from getTaxonomy
#' @param naSub a character string to substitute in place of NAs in the taxonomy
#' @param excludeTerminalNAs If TRUE then do not output nodes downstream of the last named taxonomic level in a row
#' @param quote If not NULL then wrap all entries with this character
#' @param terminator If not NULL then add this character to the end of the tree
#' @return a string giving a Newick formatted tree
#' @seealso \code{\link{getTaxonomy}}
#' @export
#' @examples
#' taxa<-matrix(c('A','A','A','B','B','C','D','D','E','F','G','H'),nrow=3)
#' makeNewick(taxa)
#' taxa<-matrix(c('A','A','A','B',NA,'C','D','D',NA,'F','G',NA),nrow=3)
#' makeNewick(taxa)
#' makeNewick(taxa,excludeTerminalNAs=TRUE)
#' makeNewick(taxa,quote="'")
makeNewick<-function(taxa,naSub='_',excludeTerminalNAs=FALSE,quote=NULL,terminator=';'){
  if(ncol(taxa)==0)return('')
  if(!is.null(quote))taxa<-apply(taxa,2,function(xx)ifelse(is.na(xx),xx,sprintf('%s%s%s',quote,xx,quote)))
  if(!is.null(naSub))taxa[is.na(taxa)]<-naSub
  bases<-unique(taxa[,1])
  innerTree<-sapply(bases,function(ii)makeNewick(taxa[taxa[,1]==ii,-1,drop=FALSE],naSub=naSub,excludeTerminalNAs=excludeTerminalNAs,terminator=NULL))
  #check if innertree is just NAs
  if(excludeTerminalNAs){
    innerTree[grepl(sprintf('^([()]|%s)+$',naSub),innerTree)]<-''
    select<-innerTree!=''|bases!=naSub
    innerTree<-innerTree[select]
    bases<-bases[select]
  }
  out<-sprintf('(%s)',paste(sprintf('%s%s',innerTree,bases),collapse=','))
  if(!is.null(terminator))out<-sprintf('%s%s',out,terminator)
  return(out)
}



#' Bring multiple raw taxonomies into alignment
#'
#' Combine the raw taxonomy of several taxa into a single matrix where each row corresponds to a taxa and each column a taxonomic level. Named taxonomic levels are aligned between taxa then any unspecified clades are combined between the named levels. Taxonomic levels between named levels are arbitrarily combined from most generic to most specific. Working from the data provided in the NCBI taxonomy results in ambiguities so results should be used with care.
#' @param rawTaxa A list of vectors with each vector containing a named character vector with entries specifying taxonomy for a clade and names giving the corresponding taxonomic levels e.g. the output from \code{\link{getRawTaxonomy}}
#' @param cladeRegex A regex to identify ambiguous taxonomic levels. In the case of NCBI taxonomy, these unidentified levels are all labelled "clade" and \code{\link{getRawTaxonomy}} may attach a unique digit attach to the end for uniqueness.
#' @param rootFill If a clade is upstream of the highest taxonomic level then it will be labeled with this prefix
#' @param lineageOrder A vector giving an ordering for lineages from most specific to most generic. This should be unnecessary unless the taxonomy contains ambiguities e.g. one taxa goes from species to kingdom while another goes from genus to kingdom leaving it ambiguous whether genus or species is more specific
#' @return a matrix with a row for each taxa and a column for each taxonomic level
#' @seealso \code{\link{getRawTaxonomy}}
#' @export
#' @examples
#' rawTaxa<-list(
#'    '81907' = c(species = "Alectura lathami", genus = "Alectura",
#'      family = "Megapodiidae", order = "Galliformes", superorder = "Galloanserae",
#'      infraclass = "Neognathae", class = "Aves", clade = "Coelurosauria",
#'      clade.1 = "Theropoda", clade.2 = "Saurischia", clade.3 = "Dinosauria",
#'      clade.4 = "Archosauria", clade.5 = "Archelosauria", clade.6 = "Sauria",
#'      clade.7 = "Sauropsida", clade.8 = "Amniota", clade.9 = "Tetrapoda",
#'      clade.10 = "Dipnotetrapodomorpha", superclass = "Sarcopterygii",
#'      clade.11 = "Euteleostomi", clade.12 = "Teleostomi", clade.13 = "Gnathostomata",
#'      clade.14 = "Vertebrata", subphylum = "Craniata", phylum = "Chordata",
#'      clade.15 = "Deuterostomia", clade.16 = "Bilateria", clade.17 = "Eumetazoa",
#'      kingdom = "Metazoa", clade.18 = "Opisthokonta", superkingdom = "Eukaryota",
#'      'no rank' = "cellular organisms"),
#'    '8496' = c(species = "Alligator mississippiensis",
#'      genus = "Alligator", subfamily = "Alligatorinae", family = "Alligatoridae",
#'      order = "Crocodylia", clade = "Archosauria", clade.1 = "Archelosauria",
#'      clade.2 = "Sauria", clade.3 = "Sauropsida", clade.4 = "Amniota",
#'      clade.5 = "Tetrapoda", clade.6 = "Dipnotetrapodomorpha", superclass = "Sarcopterygii",
#'      clade.7 = "Euteleostomi", clade.8 = "Teleostomi", clade.9 = "Gnathostomata",
#'      clade.10 = "Vertebrata", subphylum = "Craniata", phylum = "Chordata",
#'      clade.11 = "Deuterostomia", clade.12 = "Bilateria", clade.13 = "Eumetazoa",
#'      kingdom = "Metazoa", clade.14 = "Opisthokonta", superkingdom = "Eukaryota",
#'      'no rank' = "cellular organisms"),
#'    '38654' = c(species = "Alligator sinensis",
#'      genus = "Alligator", subfamily = "Alligatorinae", family = "Alligatoridae",
#'      order = "Crocodylia", clade = "Archosauria", clade.1 = "Archelosauria",
#'      clade.2 = "Sauria", clade.3 = "Sauropsida", clade.4 = "Amniota",
#'      clade.5 = "Tetrapoda", clade.6 = "Dipnotetrapodomorpha", superclass = "Sarcopterygii",
#'      clade.7 = "Euteleostomi", clade.8 = "Teleostomi", clade.9 = "Gnathostomata",
#'      clade.10 = "Vertebrata", subphylum = "Craniata", phylum = "Chordata",
#'      clade.11 = "Deuterostomia", clade.12 = "Bilateria", clade.13 = "Eumetazoa",
#'      kingdom = "Metazoa", clade.14 = "Opisthokonta", superkingdom = "Eukaryota",
#'      'no rank' = "cellular organisms")
#' )
#' normalizeTaxa(rawTaxa)
normalizeTaxa<-function(rawTaxa,cladeRegex='^clade$|^clade\\.[0-9]+$|^$|no rank',rootFill='_ROOT_',lineageOrder=c()){
  if(!is.list(rawTaxa))rawTaxa<-list(rawTaxa)
  levels<-lapply(rawTaxa,names)
  nonClade<-lapply(levels,function(xx)xx[!grepl(cladeRegex,xx)])
  sortLevels<-topoSort(c(nonClade,list(lineageOrder)),errorIfAmbiguous=TRUE)
  if(length(sortLevels)==0)stop('No unambiguous clades found')
  if(any(sapply(nonClade,length)==0))stop('Taxa with no unambiguous clades found')
  upDowns<-do.call(rbind,lapply(1:length(rawTaxa),function(ii){
    xx<-rawTaxa[[ii]]
    cladeIds<-grep(cladeRegex,names(xx))
    goodIds<-which(names(xx) %in% sortLevels)
    if(length(cladeIds)==0)return(NULL)
    data.frame('id'=ii,'clade'=names(xx)[cladeIds],'taxa'=xx[cladeIds],'up'=sapply(cladeIds,function(ii)names(xx)[min(c(goodIds[goodIds>ii],Inf))]),'down'=sapply(cladeIds,function(ii)names(xx)[max(c(goodIds[goodIds<ii],-Inf))]),stringsAsFactors=FALSE)
  }))
  if(any(is.na(upDowns$up))){
    upDowns[is.na(upDowns$up),'up']<-rootFill
    sortLevels<-c(sortLevels,rootFill)
  }
  if(is.null(upDowns))inserts<-structure(rep(0,length(sortLevels)),.Names=sortLevels)
  else inserts<-tapply(upDowns$id,upDowns$up,function(xx)max(table(xx)))[sortLevels]
  inserts[is.na(inserts)]<-0
  names(inserts)<-sortLevels
  outLevels<-rev(make.unique(rev(rep(names(inserts),inserts+1))))
  out<-do.call(rbind,lapply(rawTaxa,function(xx){
    xx<-c(xx,structure(NA,.Names=rootFill))
    names(xx)<-rev(make.unique(rev(names(xx))[cummax(ifelse(names(rev(xx)) %in% sortLevels,1:length(xx),0))]))
    xx[outLevels]
  }))
  colnames(out)<-outLevels
  out<-out[,colnames(out)!=rootFill]
  out<-out[,ncol(out):1]
  return(out)
}


#' Combine multiple sorted vectors into a single sorted vector 
#'
#' Combine multiple sorted vectors into a single vector assuming there are no cycles or weird topologies. Where a global position is ambiguous, the result is placed arbitrarily.
#' @param vectors A list of vectors each vector containing sorted elements to be merged into a global sorted vector
#' @param maxIter An integer specifying the maximum number of iterations before bailing out. This should be unnecessary and is just a safety feature in case of some unexpected input or bug.
#' @param errorIfAmbiguous If TRUE then error if any ambiguities arise
#' @return a vector with all unique elements sorted by the combined ordering provided by the input vectors
#' @seealso \code{\link{normalizeTaxa}}
#' @export
#' @examples
#' topoSort(list(c('a','b','f','g'),c('b','e','g','y','z'),c('b','d','e','f','y')))
topoSort<-function(vectors,maxIter=1000,errorIfAmbiguous=FALSE){
  out<-c()
  pointers<-rep(1,length(vectors))
  ns<-sapply(vectors,length)
  iters<-1
  while(any(active<-pointers<=ns)){
    currents<-mapply('[',vectors[active],pointers[active])
    if(all(currents==currents[1])){
      out<-c(out,currents[1])
      pointers[active]<-pointers[active]+1
    }else{
      upstream<-sapply(currents,function(taxa){
        any(mapply(function(vec,point,n){
          if((point+1)>n)FALSE else taxa %in% vec[(point+1):n]
        },vectors[active],pointers[active],ns[active]))
      })
      if(all(upstream))stop('Found cycle in topological sort (all of ',paste(unique(currents),collapse=', '),' appear higher in a taxonomy)')
      if(errorIfAmbiguous && length(unique(currents[!upstream]))>1)stop('Ambiguous ordering found in topoSort (',paste(unique(currents[!upstream]),collapse=' vs '),')')
      select<-currents[min(which(!upstream))]
      out<-c(out,select)
      pointers[active][currents==select]<-pointers[active][currents==select]+1
    }
    if(iters>maxIter)stop('Maximum iterations exceeded in topological sort. A bug or pathological topology?')
    iters<-iters+1
  }
  return(unname(out))
}

#' Download file using curl allowing resumption of interrupted files
#'
#' A helper function that uses the \code{curl} package's \code{multi_download} to download a file using a temporary file to store progress and resume downloading on interruption.
#' @param url The address to download from
#' @param outFile The file location to store final download at
#' @param tmpFile The file location to store the intermediate download at
#' @param quiet If TRUE show the progress reported by \code{multi_download}
#' @param resume If TRUE try to resume interrupted downloads using intermediate file \code{tmpFile}. Otherwise delete \code{tempFile} on error
#' @param ... Additional arguments to \code{multi_download}
#' @return invisibly return the output from multi_download
#' @seealso \code{\link[curl]{multi_download}}
#' @examples
#' \dontrun{
#'   url<-'https://ftp.ncbi.nih.gov/pub/taxonomy/accession2taxid/prot.accession2taxid.FULL.1.gz'
#'   resumableDownload(url,'downloadedFile.gz')
#' }
resumableDownload<-function(url,outFile=basename(url),tmpFile=sprintf('%s.__TMP__',outFile),quiet=FALSE,resume=TRUE,...){
  minTmpFileSize<-10000
  if(!resume) on.exit(unlink(tmpFile))
  out<-curl::multi_download(url,tmpFile,progress=!quiet,resume=resume,...)
  if(out$status_code >399){ #could also use not %in% c(0,200,206) here but assuming outside 200/300 range = error
    out$success<-FALSE
    if(!'error' %in% colnames(out) || is.na(out$error))out$error<-sprintf('Error status code %d returned',out$status_code)
  }
  if(is.na(out$success)||!out$success){
    if(length(out$error)>0&&!is.na(out$error))extraError<-sprintf(' with error: "%s"',out$error)
    else extraError<-''
    if(resume&&file.exists(tmpFile)){
      if(file.size(tmpFile)>minTmpFileSize){
        extraError<-sprintf('%s. Progress is saved in %s and continued download can be attempted by repeating the previous command.\nDelete %s or set resume=FALSE to start from scratch',extraError,tmpFile,tmpFile)
      }else{
        #too small to be useful so clear
        unlink(tmpFile)
      }
    }
    stop('Download failed',extraError,'.')
  }
  file.rename(tmpFile,outFile)
  if(!quiet)message('Downloaded file: ',out$url,'\nModified: ',out$modified,'\nStatus: ',out$status_code)
  invisible(out)
}


#' Switch from data.table to SQLite
#'
#' In version 0.5.0, taxonomizr switched from data.table to SQLite name and node lookups. See below for more details.
#'
#' Version 0.5.0 marked a change for name and node lookups from using data.table to using SQLite. This was necessary to increase performance (10-100x speedup for \code{\link{getTaxonomy}}) and create a simpler interface (a single SQLite database contains all necessary data). Unfortunately, this switch requires a couple breaking changes:
#' \itemize{
#'  \item \code{\link{getTaxonomy}} changes from \code{getTaxonomy(ids,namesDT,nodesDT)} to \code{getTaxonomy(ids,sqlFile)}
#'  \item  \code{\link{getId}} changes from  \code{getId(taxa,namesDT)} to \code{getId(taxa,sqlFile)}
#'  \item \code{\link{read.names}} is deprecated, instead use \code{\link{read.names.sql}}. For example, instead of calling \code{names<-read.names('names.dmp')} in every session, simply call \code{read.names.sql('names.dmp','accessionTaxa.sql')} once (or use the convenient \code{\link{prepareDatabase}})).
#'  \item \code{\link{read.nodes}} is deprecated, instead use \code{\link{read.names.sql}}. For example. instead of calling \code{nodes<-read.names('nodes.dmp')} in every session, simply call \code{read.nodes.sql('nodes.dmp','accessionTaxa.sql')} once (or use the convenient \code{\link{prepareDatabase}}).
#' }
#'
#' I've tried to ease any problems with this by overloading \code{\link{getTaxonomy}} and \code{\link{getId}} to still function (with a warning) if passed a data.table names and nodes argument and providing a simpler \code{\link{prepareDatabase}} function for completing all setup steps (hopefully avoiding direct calls to \code{\link{read.names}} and \code{\link{read.nodes}} for most users).
#'
#' I plan to eventually remove data.table functionality to avoid a split codebase so please switch to the new SQLite format in all new code.
#'
#' @seealso \code{\link{getTaxonomy}}, \code{\link{read.names.sql}}, \code{\link{read.nodes.sql}}, \code{\link{prepareDatabase}}, \code{\link{getId}}
#' @keywords interal
#' @name taxonomizrSwitch
NULL


