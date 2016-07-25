if(!require(data.table)){
  install.packages('data.table')
  library(data.table)
}
library(parallel)

##HELPER FUNCTIONS##
####################
readNames<-function(nameFile){
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
readAccessionToTaxa<-function(taxaFiles,cores=4){
	accessions<-mclapply(taxaFiles,function(xx){out<-read.table(xx,header=TRUE);out$file<-xx;out},mc.cores=cores,mc.preschedule=FALSE)
	out<-rbindlist(accessions)
	#out$file<-rep(taxaFiles,nLines)
	setkey(out,key='accession.version')
	return(out)
}
readNodes<-function(nodeFile){
  splitLines<-do.call(rbind,strsplit(readLines(nodeFile),'\\s*\\|\\s*'))
  splitLines<-splitLines[,1:3]
  colnames(splitLines)<-c('id','parent','rank')
  splitLines<-data.frame('id'=as.numeric(splitLines[,'id']),'rank'=splitLines[,'rank'],'parent'=as.numeric(splitLines[,'parent']),stringsAsFactors=FALSE)
  out<-data.table(splitLines,key='id')
  return(out)
}

getTaxonomy<-function (ids,taxaNodes ,taxaNames, desiredTaxa=c('superkingdom','kingdom','phylum','class','order','family','genus','species'),mc.cores=round(detectCores()/2)-1){
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
}


##READ NCBI DUMP##
##################
if(!exists('taxaNodes')){
	accessionTaxa<-readAccessionToTaxa(list.files('dump','nucl_.*accession2taxid.gz',full.names=TRUE))
	taxaNodes<-readNodes('dump/nodes.dmp.gz')
	taxaNames<-readNames('dump/names.dmp.gz')
	#giTaxa<-readGiToTaxa('dump/gi_taxid_nucl.dmp.gz')
}


##READ OUR DESIRED TAXA##
#########################
x<-read.table('BI54cytb1-4_S117_L001_R1_001.blast',stringsAsFactors=FALSE)
x$gi<-as.numeric(sapply(strsplit(x$V2,'\\|'),'[[',2))
browser()

##DO THE HEAVY PROCESSING##
###########################
x$taxa<-giToTaxa(x$gi,giTaxa)
taxonomy<-getTaxonomy(taxaIds,taxaNodes,taxaNames)
