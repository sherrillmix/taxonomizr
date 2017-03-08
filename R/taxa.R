#' Read NCBI names file
#'
#' Take an NCBI names file, keep only scientific names and convert it to a data.table
#'
#' @param nameFile string giving the path to an NCBI name file to read from (both gzipped or uncompressed files are ok)
#' @param onlyScientific If TRUE, only store scientific names. If FALSE, synonyms and other types are included (increasing the potential for ambiguous taxonomic assignments).
#' @return a data.table with columns id and name with a key on id
#' @export
#' @references \url{ftp://ftp.ncbi.nih.gov/pub/taxonomy/}
#' @seealso \code{\link{read.nodes}}
#' @examples
#' namesText<-c(
#'   "1\t|\tall\t|\t\t|\tsynonym\t|",
#'   "1\t|\troot\t|\t\t|\tscientific name\t|",
#'   "2\t|\tBacteria\t|\tBacteria <prokaryotes>\t|\tscientific name\t|",
#'   "2\t|\tMonera\t|\tMonera <Bacteria>\t|\tin-part\t|",
#'   "2\t|\tProcaryotae\t|\tProcaryotae <Bacteria>\t|\tin-part\t|"
#' )
#' read.names(textConnection(namesText))
read.names<-function(nameFile,onlyScientific=TRUE){
  splitLines<-do.call(rbind,strsplit(readLines(nameFile),'\\s*\\|\\s*'))
  if(onlyScientific)splitLines<-splitLines[splitLines[,4]=='scientific name',]
  splitLines<-splitLines[,-(3:4)]
  colnames(splitLines)<-c('id','name')
  splitLines<-data.frame('id'=as.numeric(splitLines[,'id']),'name'=splitLines[,'name'],stringsAsFactors=FALSE)
  out<-data.table::data.table(splitLines,key='id')
  data.table::setindex(out,'name')
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
  splitLines<-do.call(rbind,lapply(strsplit(readLines(nodeFile),'\\s*\\|\\s*'),'[',1:3))
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
#' @useDynLib taxonomizr, .registration=TRUE
trimTaxa<-function(inFile,outFile){
  inFile<-as.character(inFile)
  outFile<-as.character(outFile)
  if(!file.exists(inFile))stop(simpleError(sprintf('%s file not found',inFile)))
  isCompressed<-R.utils::isGzipped(inFile)
  if(isCompressed){
    tmp<-tempfile()
    R.utils::gunzip(inFile,tmp,remove=FALSE)
    inFile<-tmp
    on.exit(file.remove(tmp))
  }
  #too much memory
  #out<-data.table::fread(sprintf('gzip -dcf %s',inFile),select=c(2,3))
  #write.table(out,outFile,row.names=FALSE,col.names=FALSE)
  .C('taxaTrim',c(inFile,outFile),PACKAGE='taxonomizr')
}

#' Read NCBI accession2taxid files
#'
#' Take NCBI accession2taxid files, keep only accession and taxa and save it as a sqlite database
#'
#' @param taxaFiles a string or vector of strings giving the path(s) to files to be read in
#' @param sqlFile a string giving the path where the output sqlite file should be saved
#' @param vocal if TRUE output status messages
#' @param extraSqlCommand for advanced use. A string giving a command to be called on the sqlite databse before loading data e.g. "pragma temp_store = 2;" to keep all temp files in memory (don't do this unless you have a lot (>100 Gb) of RAM)
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
read.accession2taxid<-function(taxaFiles,sqlFile,vocal=TRUE,extraSqlCommand=''){
  if(file.exists(sqlFile)){
    message(sqlFile,' already exists. Delete to reprocess data')
    return(TRUE)
  }
  tryCatch({
    tmp<-tempfile()
    writeLines('accession\ttaxa',tmp)
    for(ii in taxaFiles){
      if(vocal)message('Reading ',ii,'.')
      trimTaxa(ii,tmp)
    }
    db <- RSQLite::dbConnect(RSQLite::SQLite(), dbname=sqlFile)
    if(extraSqlCommand!='')RSQLite::dbGetQuery(db,extraSqlCommand)
    if(vocal)message('Reading in values. This may take a while.')
    RSQLite::dbWriteTable(conn = db, name = "accessionTaxa", value =tmp, row.names = FALSE, header = TRUE,sep='\t')
    if(vocal)message('Adding index. This may also take a while.')
    RSQLite::dbGetQuery(db,"CREATE INDEX index_accession ON accessionTaxa (accession)")
    RSQLite::dbDisconnect(db)
  },error=function(e){
    message('Error: Problem creating sql file. Deleting.')
    file.remove(sqlFile)
    stop(e)
  }
  )
  return(TRUE)
}

#' Get taxonomic ranks for a taxa
#'
#' Take NCBI taxa IDs and get the corresponding taxa ranks from name and node data.tables
#'
#' @param ids a vector of ids to find taxonomy for
#' @param taxaNodes a nodes data.table from \code{\link{read.nodes}}
#' @param taxaNames a names data.table from \code{\link{read.names}}
#' @param desiredTaxa a vector of strings giving the desired taxa levels
#' @param mc.cores the number of cores to use when processing
#' @param debug if TRUE output node and name vectors with dput for each id (probably useful only for development)
#' @return a matrix of taxonomic strings with a row for each id and a column for each desiredTaxa rank
#' @import data.table
#' @export
#' @seealso \code{\link{read.nodes}}, \code{\link{read.names}}
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
#' taxaNames<-read.names(textConnection(namesText))
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
#' taxaNodes<-read.nodes(textConnection(nodesText))
#' getTaxonomy(c(9606,9605),taxaNodes,taxaNames,mc.cores=1)
getTaxonomy<-function (ids,taxaNodes ,taxaNames, desiredTaxa=c('superkingdom','phylum','class','order','family','genus','species'),mc.cores=1,debug=FALSE){
  ids<-as.numeric(ids)
  if(length(ids)==0)return(NULL)
  uniqIds<-unique(ids)
  taxa<-do.call(rbind,parallel::mclapply(uniqIds,function(id){
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
  },mc.cores=mc.cores))
  rownames(taxa)<-format(uniqIds,scientific=FALSE)
  out<-taxa[format(ids,scientific=FALSE),,drop=FALSE]
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
  if(!file.exists(sqlFile))stop(sqlFile,' does not exist.')
  tmp<-tempfile()
  #set up a new table of accessions in a temp db (avoiding concurrency issues)
  #some trouble with dbWriteTable writing to "tmp.xxx" in the main database if we do this inside the attach
  tmpDb <- RSQLite::dbConnect(RSQLite::SQLite(), dbname=tmp)
  RSQLite::dbWriteTable(tmpDb,'query',data.frame('accession'=accessions,stringsAsFactors=FALSE),overwrite=TRUE)
  RSQLite::dbDisconnect(tmpDb)
  #load the big sql
  db <- RSQLite::dbConnect(RSQLite::SQLite(), dbname=sqlFile)
  #attach the temp table
  RSQLite::dbGetQuery(db, sprintf("ATTACH '%s' AS tmp",tmp))
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
  nTaxa<-apply(taxaTable,2,function(x)length(unique(x)))
  firstDisagree<-min(c(Inf,which(nTaxa!=1)))
  out<-taxaTable[1,]
  if(firstDisagree<=ncol(taxaTable))out[(firstDisagree):ncol(taxaTable)]<-NA
  return(out)
}

#' Download names and nodes files from NCBI
#'
#' Download a taxdump.tar.gz file from NCBI servers and extract the names.dmp and nodes.dmp files from it. These can then be used to create data.tables with \code{\link{read.names}} and \code{\link{read.nodes}}. Note that if the files already exist in the target directory then this function will not redownload them. Delete the files if a fresh download is desired.
#'
#' @param outDir the directory to put names.dmp and nodes.dmp in
#' @param url the url where taxdump.tar.gz is located
#' @param fileNames the filenames desired from the tar.gz file
#' @return a vector of file path strings of the locations of the output files
#' @seealso \code{\link{read.nodes}}, \code{\link{read.names}}
#' @references \url{ftp://ftp.ncbi.nih.gov/pub/taxonomy/}, \url{https://www.ncbi.nlm.nih.gov/Taxonomy/taxonomyhome.html/}
#' @export
#' @examples
#' \dontrun{
#'   getNamesAndNodes()
#' }
getNamesAndNodes<-function(outDir='.',url='ftp://ftp.ncbi.nih.gov/pub/taxonomy/taxdump.tar.gz',fileNames=c('names.dmp','nodes.dmp')){
  outFiles<-file.path(outDir,fileNames)
  if(all(file.exists(outFiles))){
    message(paste(outFiles,collapse=', '),' already exist. Delete to redownload')
    return(outFiles)
  }
  base<-basename(url)
  tmp<-tempdir()
  tarFile<-file.path(tmp,base)
  utils::download.file(url,tarFile)
  utils::untar(tarFile,fileNames,exdir=tmp)
  tmpFiles<-file.path(tmp,fileNames)
  if(!all(file.exists(tmpFiles)))stop("Problem finding files ",paste(tmpFiles[!file.exists(tmpFiles)],collapse=', '))
  mapply(file.copy,tmpFiles,outFiles)
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
#' @return a vector of file path strings of the locations of the output files
#' @seealso \code{\link{read.accession2taxid}}
#' @references \url{ftp://ftp.ncbi.nih.gov/pub/taxonomy/}, \url{https://www.ncbi.nlm.nih.gov/Sequin/acc.html}
#' @export
#' @examples
#' \dontrun{
#'   getAccession2taxid()
#' }
getAccession2taxid<-function(outDir='.',baseUrl='ftp://ftp.ncbi.nih.gov/pub/taxonomy/accession2taxid/',types=c('nucl_gb','nucl_est','nucl_gss','nucl_wgs')){
  message('This can be a big (several gigabytes) download. Please be patient and use a fast connection.')
  fileNames<-sprintf('%s.accession2taxid.gz',types)
  outFiles<-file.path(outDir,fileNames)
  if(all(file.exists(outFiles))){
    message(paste(outFiles,collapse=', '),' already exist. Delete to redownload')
    return(outFiles)
  }
  urls<-paste(baseUrl,fileNames,sep='/')
  mapply(utils::download.file,urls,outFiles)
  return(outFiles)
}

#' Find a given taxa by name
#'
#' Find a taxa by string in the NCBI taxonomy. Note that NCBI species are stored as Genus species e.g. "Bos taurus". Ambiguous taxa names will return a comma concatenated string e.g. "123,234" and generate a warning.
#'
#' @param taxa a vector of taxonomic names
#' @param taxaNames a names data.table from \code{\link{read.names}}
#' @return a vector of character strings giving taxa IDs (potentially comma concatenated for any taxa with ambiguous names)
#' @seealso \code{\link{read.names}}
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
#' names<-read.names(textConnection(namesText))
#' getId('Bacteria',names)
#' getId('Not a real name',names)
#' getId('Multi',names)
getId<-function(taxa,taxaNames){
  uniqTaxa<-unique(taxa)
  out<-lapply(uniqTaxa,function(xx){
    ids<-taxaNames[as.list(xx),on='name']$id
  })
  multiHits<-sapply(out,length)>1
  if(any(multiHits)){
    warning('Multiple taxa ids found for ',paste(taxa[multiHits],collapse=', '),'. Collapsing with commas')
    out<-sapply(out,paste,collapse=',')
  }
  out<-as.character(unlist(out))
  names(out)<-uniqTaxa
  return(unname(out[taxa]))
}

