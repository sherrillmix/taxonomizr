context("taxa functions")
test_that("Test read.names",{
  names<-c(
    "1\t|\tall\t|\t\t|\tsynonym\t|",
    "1\t|\troot\t|\t\t|\tscientific name\t|",
    "2\t|\tBacteria\t|\tBacteria <prokaryotes>\t|\tscientific name\t|",
    "2\t|\tMonera\t|\tMonera <Bacteria>\t|\tin-part\t|",
    "2\t|\tProcaryotae\t|\tProcaryotae <Bacteria>\t|\tin-part\t|"
  )
  out<-data.table::data.table('id'=1:2,'name'=c('root','Bacteria'),key='id')
  data.table::setindex(out,'name')
  expect_warning(expect_equal(read.names(textConnection(names)),out))
  out<-data.table::data.table('id'=rep(1:2,2:3),'name'=c('all','root','Bacteria','Monera','Procaryotae'),key='id')
  data.table::setindex(out,'name')
  expect_warning(expect_equal(read.names(textConnection(names),FALSE),out))
  expect_warning(read.names(textConnection(names)),'SQLite')
})

test_that("Test read.nodes",{
  nodes<-c(
    "1\t|\t1\t|\tno rank\t|\t\t|\t8\t|\t0\t|\t1\t|\t0\t|\t0\t|\t0\t|\t0\t|\t0\t|\t\t|",
    "2\t|\t131567\t|\tsuperkingdom\t|\t\t|\t0\t|\t0\t|\t11\t|\t0\t|\t0\t|\t0\t|\t0\t|\t0\t|\t\t|",
    "6\t|\t335928\t|\tgenus\t|\t\t|\t0\t|\t1\t|\t11\t|\t1\t|\t0\t|\t1\t|\t0\t|\t0\t|\t\t|",
    "7\t|\t6\t|\tspecies\t|\tAC\t|\t0\t|\t1\t|\t11\t|\t1\t|\t0\t|\t1\t|\t1\t|\t0\t|\t\t|",
    "9\t|\t32199\t|\tspecies\t|\tBA\t|\t0\t|\t1\t|\t11\t|\t1\t|\t0\t|\t1\t|\t1\t|\t0\t|\t\t|"
  )
  out<-data.table::data.table('id'=c(1:2,6:7,9),'rank'=c('no rank','superkingdom','genus','species','species'),'parent'=c(1,131567,335928,6,32199),key='id')
  expect_warning(expect_equal(read.nodes(textConnection(nodes)),out))
  expect_warning(read.nodes(textConnection(nodes)),'SQLite')
})

test_that("Test read.names.sql",{
  names<-c(
    "1\t|\tall\t|\t\t|\tsynonym\t|",
    "1\t|\troot\t|\t\t|\tscientific name\t|",
    "2\t|\tBacteria\t|\tBacteria <prokaryotes>\t|\tscientific name\t|",
    "2\t|\tMonera\t|\tMonera <Bacteria>\t|\tin-part\t|",
    "2\t|\tProcaryotae\t|\tProcaryotae <Bacteria>\t|\tin-part\t|"
  )
  tmp<-tempfile()
  out<-data.frame('id'=1:2,'name'=c('root','Bacteria'),scientific=c(1,1),stringsAsFactors=FALSE)
  out2<-data.frame('id'=rep(1:2,2:3),'name'=c('all','root','Bacteria','Monera','Procaryotae'),'scientific'=c(0,1,1,0,0),stringsAsFactors=FALSE)
  expect_warning(expect_error(read.names.sql('____NOT_A_REAL____.FILE'),'cannot open'),'cannot open')
  expect_equal(read.names.sql(textConnection(names),tmp),tmp)
  expect_true(file.exists(tmp))
  expect_message(read.names.sql(textConnection(names),tmp),'contains')
  expect_error(db <- RSQLite::dbConnect(RSQLite::SQLite(), dbname=tmp),NA)
  expect_equal(RSQLite::dbGetQuery(db,"SELECT * FROM names WHERE scientific"),out)
  expect_equal(RSQLite::dbGetQuery(db,"SELECT * FROM names"),out2)
  RSQLite::dbDisconnect(db)
  expect_equal(read.names.sql(textConnection(names[-length(names)]),tmp,overwrite=TRUE),tmp)
  expect_error(db <- RSQLite::dbConnect(RSQLite::SQLite(), dbname=tmp),NA)
  expect_equal(RSQLite::dbGetQuery(db,"SELECT * FROM names"),out2[-length(names),])
  RSQLite::dbDisconnect(db)
})

test_that("Test read.nodes.sql",{
  nodes<-c(
    "1\t|\t1\t|\tno rank\t|\t\t|\t8\t|\t0\t|\t1\t|\t0\t|\t0\t|\t0\t|\t0\t|\t0\t|\t\t|",
    "2\t|\t131567\t|\tsuperkingdom\t|\t\t|\t0\t|\t0\t|\t11\t|\t0\t|\t0\t|\t0\t|\t0\t|\t0\t|\t\t|",
    "6\t|\t335928\t|\tgenus\t|\t\t|\t0\t|\t1\t|\t11\t|\t1\t|\t0\t|\t1\t|\t0\t|\t0\t|\t\t|",
    "7\t|\t6\t|\tspecies\t|\tAC\t|\t0\t|\t1\t|\t11\t|\t1\t|\t0\t|\t1\t|\t1\t|\t0\t|\t\t|",
    "9\t|\t32199\t|\tspecies\t|\tBA\t|\t0\t|\t1\t|\t11\t|\t1\t|\t0\t|\t1\t|\t1\t|\t0\t|\t\t|"
  )
  tmp<-tempfile()
  out<-data.frame('id'=c(1:2,6:7,9),'rank'=c('no rank','superkingdom','genus','species','species'),'parent'=c(1,131567,335928,6,32199),stringsAsFactors=FALSE)
  expect_equal(read.nodes.sql(textConnection(nodes),tmp),tmp)
  expect_true(file.exists(tmp))
  expect_message(read.nodes.sql(textConnection(nodes),tmp),'contains')
  expect_equal(read.nodes.sql(textConnection(nodes),tmp),tmp)
  expect_error(db <- RSQLite::dbConnect(RSQLite::SQLite(), dbname=tmp),NA)
  expect_equal(RSQLite::dbGetQuery(db,"SELECT * FROM nodes"),out)
  RSQLite::dbDisconnect(db)
  expect_equal(read.nodes.sql(textConnection(nodes[-length(nodes)]),tmp,overwrite=TRUE),tmp)
  expect_error(db <- RSQLite::dbConnect(RSQLite::SQLite(), dbname=tmp),NA)
  expect_equal(RSQLite::dbGetQuery(db,"SELECT * FROM nodes"),out[-length(nodes),])
  RSQLite::dbDisconnect(db)
})

test_that("Test lastNotNa",{
  expect_equal(lastNotNa(1:100),100)
  expect_equal(lastNotNa(c(1:100,rep(NA,50))),100)
  expect_equal(lastNotNa(c(rep(NA,50),1:100,rep(NA,50))),100)
  expect_equal(lastNotNa(rep(NA,100),'z'),'z')
  expect_equal(lastNotNa(rep(NA,100),-999),-999)
  expect_equal(lastNotNa(rep(NA,100),NA),NA)
  expect_equal(lastNotNa(c(),999),999)
})

test_that("Test streamingRead",{
  expect_equal(streamingRead(textConnection(letters),2,paste,collapse=''),unname(as.list(tapply(letters,rep(1:13,each=2),function(x)paste(x,collapse='')))))
  expect_output(streamingRead(textConnection(letters),2,vocal=TRUE),'.............')
  temp<-tempfile()
  writeLines(letters,temp)
  expect_equal(streamingRead(temp,2,paste,collapse=''),unname(as.list(tapply(letters,rep(1:13,each=2),function(x)paste(x,collapse='')))))
  expect_equal(streamingRead(temp,100,paste,collapse=''),list(paste(letters,collapse='')))
  expect_equal(streamingRead(temp,0),list())
  expect_output(streamingRead(temp,2,vocal=TRUE),'.............')
  gz<-gzfile(temp,'w')
  writeLines(letters,gz)
  close(gz)
  expect_equal(streamingRead(temp,2,paste,collapse=''),unname(as.list(tapply(letters,rep(1:13,each=2),function(x)paste(x,collapse='')))))
  expect_output(streamingRead(temp,2,vocal=TRUE),'.............')
  handle<-file(temp)
  expect_equal(streamingRead(handle,2,paste,collapse=''),unname(as.list(tapply(letters,rep(1:13,each=2),function(x)paste(x,collapse='')))))
  handle<-file(temp,'w')
  expect_error(streamingRead(handle,2,paste,collapse=''),'read.*connection')
})

test_that("Test trimTaxa",{
  expect_error(taxonomizr:::trimTaxa('NotARealFile','test'),'file')
  expect_error(.C('taxaTrim',c('NotARealFile','test'),1:2,2,PACKAGE='taxonomizr'),'file')
  tmp<-tempfile()
  out<-c(
    'head\t1\t2\t3',
    'a\t2\t3\t4',
    'b\t3\t4\t5',
    'c\t4\t5\t6'
  )
  writeLines(out,tmp)
  tmpDir<-tempfile()
  dir.create(tmpDir)
  #should error (can't write to directory)
  expect_error(.C('taxaTrim',c(tmp,tmpDir),1:2,2,PACKAGE='taxonomizr'),'output file')
  file.remove(tmpDir)
  tmpPerm<-tempfile()
  file.create(tmpPerm)
  #some systems can't set permissions
  if(Sys.chmod(tmpPerm,'0000') && file.access(tmpPerm,6)==-1 && any(class(tryCatch(readLines(tmpPerm),error=function(xx)xx))=='error') && any(class(tryCatch(writeLines('ABC',tmpPerm),error=function(xx)xx))=='error')){
    expect_error(.C('taxaTrim',c(tmpPerm,tmp),1:2,2,PACKAGE='taxonomizr'),'input file')
    expect_error(.C('taxaTrim',c(tmp,tmpPerm),1:2,2,PACKAGE='taxonomizr'),'output file')
  }
  file.remove(tmpPerm) 
  if(file.exists('/dev/full')){
    expect_error(.C('taxaTrim',c(tmp,'/dev/full'),1:2,2,PACKAGE='taxonomizr'),'write')
    expect_error(taxonomizr:::trimTaxa(tmp,'/dev/full'),'write')
  }
  tmp2<-tempfile()
  expect_error(taxonomizr:::trimTaxa(tmp,tmp2),NA)
  expect_equal(readLines(tmp2),c('2\t3','3\t4','4\t5'))
  writeLines(c(out,'1\t2\t3\t4\t5'),tmp)
  expect_error(taxonomizr:::trimTaxa(tmp,tmp2),"line")
  #apparently win-builder has some issue with file removal here so workaround
  file.remove(tmp2);tmp2<-tempfile()
  gzHandle<-gzfile(tmp)
  writeLines(out,gzHandle)
  close(gzHandle)
  expect_error(taxonomizr:::trimTaxa(tmp,tmp2),NA)
  expect_equal(readLines(tmp2),c('2\t3','3\t4','4\t5'))
  file.remove(tmp2);tmp2<-tempfile()
  expect_error(taxonomizr:::trimTaxa(tmp,tmp2,2),NA)
  expect_equal(readLines(tmp2),c('2','3','4'))
  file.remove(tmp2);tmp2<-tempfile()
  expect_error(taxonomizr:::trimTaxa(tmp,tmp2,c(2,4)),NA)
  expect_equal(readLines(tmp2),c('2\t4','3\t5','4\t6'))
  expect_error(taxonomizr:::trimTaxa(tmp,tmp2,c(2,4)),NA)
  expect_equal(readLines(tmp2),rep(c('2\t4','3\t5','4\t6'),2))
  with_mock(`R.utils::gunzip`=function(...){},expect_error(taxonomizr:::trimTaxa(tmp,tmp2),'unzip'))
  out<-c(
    'head\t1\t2\t3',
    'a\t2\t3\t4',
    'b\t3\t4\t5\tJUNK',
    'c\t4\t5\t6'
  )
  writeLines(out,tmp)
  expect_error(.C('taxaTrim',c(tmp,tmp2),1:2,2,PACKAGE='taxonomizr'),'Malformed.*3')
  out<-c(
    'head\t1\t2\t3',
    'a\t2\t3\t4',
    'b\t3\t4\t5',
    'c\t4\t5'
  )
  writeLines(out,tmp)
  expect_error(.C('taxaTrim',c(tmp,tmp2),1:2,2,PACKAGE='taxonomizr'),'Malformed.*4')
})

test_that("Test read.accession2taxid",{
  taxa<-c(
    "accession\taccession.version\ttaxid\tgi",
    "Z17427\tZ17427.1\t3702\t16569",
    "Z17428\tZ17428.1\t3702\t16570",
    "Z17429\tZ17429.1\t3702\t16571",
    "Z17430\tZ17430.1\t3702\t16572"
  )
  outFile<-tempfile()
  outFile2<-tempfile()
  inFile<-tempfile()
  writeLines(taxa,inFile)
  file.create(outFile)
  expect_error(read.accession2taxid(inFile,outFile),NA)
  file.remove(outFile)
  expect_error(read.accession2taxid(inFile,outFile),NA)
  expect_message(read.accession2taxid(inFile,outFile),'contains')
  expect_error(read.accession2taxid(inFile,outFile,overwrite=TRUE),NA)
  db<-RSQLite::dbConnect(RSQLite::SQLite(),dbname=outFile)
  result<-data.frame('base'=c('Z17427','Z17428','Z17429','Z17430'),'accession'=c('Z17427.1','Z17428.1','Z17429.1','Z17430.1'),taxa=3702,stringsAsFactors=FALSE)
  expect_true(file.exists(outFile))
  expect_equal(RSQLite::dbGetQuery(db,'SELECT * FROM accessionTaxa'),result)
  file.remove(outFile)
  expect_error(read.accession2taxid(inFile,outFile,extraSqlCommand='pragma temp_store = 2;'),NA)
  file.remove(outFile)
  expect_error(read.accession2taxid(inFile,outFile,indexTaxa=TRUE),NA)
  expect_equal(RSQLite::dbGetQuery(db,'SELECT * FROM accessionTaxa'),result)
  RSQLite::dbDisconnect(db)
  file.remove(outFile)
  if(.Platform$OS.type == "unix"){
    #windows sqlite apparently doesn't catch this error
    expect_error(read.accession2taxid(inFile,outFile,extraSqlCommand='DROP TABLE NOTEXISTXYZ;'),'NOTEXISTXYZ')
    #don't delete now
    expect_true(file.exists(outFile))
  }
})

test_that("Test getTaxonomy and getRawTaxonomy",{
  namesText<-c(
    "1\t|\tall\t|\t\t|\tsynonym\t|",
    "1\t|\troot\t|\t\t|\tscientific name\t|",
    "2\t|\tBacteria\t|\tBacteria <prokaryotes>\t|\tscientific name\t|",
    "2\t|\tMonera\t|\tMonera <Bacteria>\t|\tin-part\t|",
    "2\t|\tProcaryotae\t|\tProcaryotae <Bacteria>\t|\tin-part\t|",
    "9606\t|\tHomo sapiens\t|\t\t|\tscientific name", "9605\t|\tHomo\t|\t\t|\tscientific name",
    "207598\t|\tHomininae\t|\t\t|\tscientific name", "9604\t|\tHominidae\t|\t\t|\tscientific name",
    "314295\t|\tHominoidea\t|\t\t|\tscientific name", "9526\t|\tCatarrhini\t|\t\t|\tscientific name",
    "314293\t|\tSimiiformes\t|\t\t|\tscientific name", "376913\t|\tHaplorrhini\t|\t\t|\tscientific name",
    "9443\t|\tPrimates\t|\t\t|\tscientific name", "314146\t|\tEuarchontoglires\t|\t\t|\tscientific name",
    "1437010\t|\tBoreoeutheria\t|\t\t|\tscientific name", "9347\t|\tEutheria\t|\t\t|\tscientific name",
    "32525\t|\tTheria\t|\t\t|\tscientific name", "40674\t|\tMammalia\t|\t\t|\tscientific name",
    "32524\t|\tAmniota\t|\t\t|\tscientific name", "32523\t|\tTetrapoda\t|\t\t|\tscientific name",
    "1338369\t|\tDipnotetrapodomorpha\t|\t\t|\tscientific name",
    "8287\t|\tSarcopterygii\t|\t\t|\tscientific name", "117571\t|\tEuteleostomi\t|\t\t|\tscientific name",
    "117570\t|\tTeleostomi\t|\t\t|\tscientific name", "7776\t|\tGnathostomata\t|\t\t|\tscientific name",
    "7742\t|\tVertebrata\t|\t\t|\tscientific name", "89593\t|\tCraniata\t|\t\t|\tscientific name",
    "7711\t|\tChordata\t|\t\t|\tscientific name", "33511\t|\tDeuterostomia\t|\t\t|\tscientific name",
    "33213\t|\tBilateria\t|\t\t|\tscientific name", "6072\t|\tEumetazoa\t|\t\t|\tscientific name",
    "33208\t|\tMetazoa\t|\t\t|\tscientific name", "33154\t|\tOpisthokonta\t|\t\t|\tscientific name",
    "2759\t|\tEukaryota\t|\t\t|\tscientific name", "131567\t|\tcellular organisms\t|\t\t|\tscientific name"
  )
  nodesText<-c(
   "1\t|\t1\t|\tno rank\t|\t\t|\t8\t|\t0\t|\t1\t|\t0\t|\t0\t|\t0\t|\t0\t|\t0\t|\t\t|",
    "2\t|\t131567\t|\tsuperkingdom\t|\t\t|\t0\t|\t0\t|\t11\t|\t0\t|\t0\t|\t0\t|\t0\t|\t0\t|\t\t|",
    "6\t|\t335928\t|\tgenus\t|\t\t|\t0\t|\t1\t|\t11\t|\t1\t|\t0\t|\t1\t|\t0\t|\t0\t|\t\t|",
    "7\t|\t6\t|\tspecies\t|\tAC\t|\t0\t|\t1\t|\t11\t|\t1\t|\t0\t|\t1\t|\t1\t|\t0\t|\t\t|",
    "9\t|\t32199\t|\tspecies\t|\tBA\t|\t0\t|\t1\t|\t11\t|\t1\t|\t0\t|\t1\t|\t1\t|\t0\t|\t\t|",
    "9606\t|\t9605\t|\tspecies", "9605\t|\t207598\t|\tgenus", "207598\t|\t9604\t|\tsubfamily",
    "9604\t|\t314295\t|\tfamily", "314295\t|\t9526\t|\tsuperfamily",
    "9526\t|\t314293\t|\tparvorder", "314293\t|\t376913\t|\tinfraorder",
    "376913\t|\t9443\t|\tsuborder", "9443\t|\t314146\t|\torder",
    "314146\t|\t1437010\t|\tsuperorder", "1437010\t|\t9347\t|\tno rank",
    "9347\t|\t32525\t|\tno rank", "32525\t|\t40674\t|\tno rank",
    "40674\t|\t32524\t|\tclass", "32524\t|\t32523\t|\tno rank", "32523\t|\t1338369\t|\tno rank",
    "1338369\t|\t8287\t|\tno rank", "8287\t|\t117571\t|\tno rank",
    "117571\t|\t117570\t|\tno rank", "117570\t|\t7776\t|\tno rank",
    "7776\t|\t7742\t|\tno rank", "7742\t|\t89593\t|\tno rank", "89593\t|\t7711\t|\tsubphylum",
    "7711\t|\t33511\t|\tphylum", "33511\t|\t33213\t|\tno rank", "33213\t|\t6072\t|\tno rank",
    "6072\t|\t33208\t|\tno rank", "33208\t|\t33154\t|\tkingdom",
    "33154\t|\t2759\t|\tno rank", "2759\t|\t131567\t|\tsuperkingdom",
    "131567\t|\t1\t|\tno rank"
  )
  tmp<-tempfile()
  read.names.sql(textConnection(namesText),tmp)
  read.nodes.sql(textConnection(nodesText),tmp)
  desiredTaxa<-c('superkingdom','phylum','class','order','family','genus','species')
  out<-matrix(c(
    "Eukaryota","Chordata","Mammalia","Primates","Hominidae","Homo","Homo sapiens",
    "Eukaryota","Chordata","Mammalia","Primates","Hominidae","Homo",NA
  ),byrow=TRUE,dimnames=list(c('9606','9605'),desiredTaxa),nrow=2)
  expect_equal(getTaxonomy(c(9606,9605),tmp,desiredTaxa=desiredTaxa),out)
  expect_equal(getTaxonomy(c(9605,9606,9605),tmp,desiredTaxa=desiredTaxa),out[c(2,1,2),])
  expect_equal(getTaxonomy(c(9605,9606,9605),tmp,desiredTaxa=desiredTaxa[3:1]),out[c(2,1,2),3:1])
  expect_equal(getTaxonomy(9606,tmp,desiredTaxa='NOTREAL'),matrix(as.character(NA),dimnames=list(9606,'NOTREAL')))
  expect_equal(getTaxonomy(9999999,tmp,desiredTaxa='class'),matrix(as.character(NA),dimnames=list(9999999,'class')))
  expect_equal(getTaxonomy(c(9999999,9606),tmp,desiredTaxa='class'),matrix(c(NA,'Mammalia'),dimnames=list(c('9999999','   9606'),'class'),nrow=2))
  expect_equal(getTaxonomy(c(),tmp,desiredTaxa=desiredTaxa),NULL)
  desiredRaw<-list(`9606` = c(species = "Homo sapiens", genus = "Homo", subfamily = "Homininae", family = "Hominidae", superfamily = "Hominoidea", parvorder = "Catarrhini", infraorder = "Simiiformes", suborder = "Haplorrhini", order = "Primates", superorder = "Euarchontoglires", `no rank` = "Boreoeutheria", `no rank.1` = "Eutheria", `no rank.2` = "Theria", class = "Mammalia", `no rank.3` = "Amniota", `no rank.4` = "Tetrapoda", `no rank.5` = "Dipnotetrapodomorpha", `no rank.6` = "Sarcopterygii", `no rank.7` = "Euteleostomi", `no rank.8` = "Teleostomi", `no rank.9` = "Gnathostomata", `no rank.10` = "Vertebrata", subphylum = "Craniata", phylum = "Chordata", `no rank.11` = "Deuterostomia", `no rank.12` = "Bilateria", `no rank.13` = "Eumetazoa", kingdom = "Metazoa", `no rank.14` = "Opisthokonta", superkingdom = "Eukaryota", `no rank.15` = "cellular organisms"), `9605` = c(genus = "Homo", subfamily = "Homininae", family = "Hominidae", superfamily = "Hominoidea", parvorder = "Catarrhini", infraorder = "Simiiformes", suborder = "Haplorrhini", order = "Primates", superorder = "Euarchontoglires", `no rank` = "Boreoeutheria", `no rank.1` = "Eutheria", `no rank.2` = "Theria", class = "Mammalia", `no rank.3` = "Amniota", `no rank.4` = "Tetrapoda", `no rank.5` = "Dipnotetrapodomorpha", `no rank.6` = "Sarcopterygii", `no rank.7` = "Euteleostomi", `no rank.8` = "Teleostomi", `no rank.9` = "Gnathostomata", `no rank.10` = "Vertebrata", subphylum = "Craniata", phylum = "Chordata", `no rank.11` = "Deuterostomia", `no rank.12` = "Bilateria", `no rank.13` = "Eumetazoa", kingdom = "Metazoa", `no rank.14` = "Opisthokonta", superkingdom = "Eukaryota", `no rank.15` = "cellular organisms"))
  expect_equal(getRawTaxonomy(c(9606,9605),tmp),desiredRaw)
  expect_equal(getRawTaxonomy(c(9606,9605,9605,9606),tmp),desiredRaw[c(1:2,2:1)])
  expect_equal(getRawTaxonomy(c(),tmp),NULL)
  expect_equal(getRawTaxonomy(c(1),tmp),list('1'=NULL))
  expect_equal(getRawTaxonomy(c(1,0),tmp),list('1'=NULL,'0'=structure(as.character(NA),.Names=NA)))
  expect_equal(getRawTaxonomy(c(NA),tmp),list('NA'=NULL))
  expect_equal(getRawTaxonomy(c(NA,NA),tmp),list('NA'=NULL,'NA'=NULL))
  expect_equal(getRawTaxonomy(c(NA,9606),tmp),c(list('  NA'=NULL),desiredRaw[1]))
  naDf<-out
  naDf[,]<-NA
  rownames(naDf)<-c('NA','NA')
  expect_equal(getTaxonomy(c(NA,NA),tmp),naDf)
  suppressWarnings(expect_equal(getTaxonomy(c(NA,9605,NA,'9604,9605'),tmp),rbind('  NA'=naDf[1,],'9605'=out[2,],'  NA'=naDf[1,],'  NA'=naDf[1,])))
  expect_equal(getTaxonomy('9605',tmp),getTaxonomy(9605,tmp))
  expect_warning(getTaxonomy('9605,123',tmp),'coercion')
  expect_warning(getRawTaxonomy('9605,123',tmp),'coercion')
  cycle<-c(
   "9606\t|\t9605\t|\tno rank\t|\t\t|\t8\t|\t0\t|\t1\t|\t0\t|\t0\t|\t0\t|\t0\t|\t0\t|\t\t|",
   "9605\t|\t9606\t|\tsuperkingdom\t|\t\t|\t0\t|\t0\t|\t11\t|\t0\t|\t0\t|\t0\t|\t0\t|\t0\t|\t\t|"
  )
  tmp2<-tempfile()
  read.names.sql(textConnection(namesText),tmp2)
  read.nodes.sql(textConnection(cycle),tmp2)
  expect_error(getTaxonomy(9606,tmp2),'cycle')
  expect_error(getRawTaxonomy(9606,tmp2),'cycle')
})

test_that("Test getTaxonomy and getRawTaxonomy with duplicated taxa ranks",{
  namesText<-c(
    "1\t|\tall\t|\t\t|\tsynonym\t|",
    "1\t|\troot\t|\t\t|\tscientific name\t|",
    "2\t|\tBacteria\t|\tBacteria <prokaryotes>\t|\tscientific name\t|",
    "2\t|\tMonera\t|\tMonera <Bacteria>\t|\tin-part\t|",
    "2\t|\tProcaryotae\t|\tProcaryotae <Bacteria>\t|\tin-part\t|",
    "3\t|\tClade A\t|\t\t|\tscientific name\t|",
    "4\t|\tClade B\t|\t\t|\tscientific name\t|",
    "5\t|\tClade C\t|\t\t|\tscientific name\t|",
    "6\t|\tClade D\t|\t\t|\tscientific name\t|",
    "7\t|\tClade E\t|\t\t|\tscientific name\t|"
  )
  nodesText<-c(
   "1\t|\t1\t|\tno rank\t|\t\t|\t8\t|\t0\t|\t1\t|\t0\t|\t0\t|\t0\t|\t0\t|\t0\t|\t\t|",
    "2\t|\t1\t|\tsuperkingdom\t|\t\t|\t0\t|\t0\t|\t11\t|\t0\t|\t0\t|\t0\t|\t0\t|\t0\t|\t\t|",
    "3\t|\t2\t|\tclade",
    "4\t|\t3\t|\tclade",
    "5\t|\t4\t|\tclade",
    "6\t|\t5\t|\tclade",
    "7\t|\t6\t|\tclade"
  )
  tmp<-tempfile()
  read.names.sql(textConnection(namesText),tmp)
  read.nodes.sql(textConnection(nodesText),tmp)
  desiredTaxa<-c('superkingdom','clade')
  out<-matrix(c(
    "Bacteria","Clade A",
    "Bacteria","Clade A"
  ),byrow=TRUE,dimnames=list(c('7','6'),desiredTaxa),nrow=2)
  expect_equal(getTaxonomy(c(7,6),tmp,desiredTaxa=desiredTaxa),out)
  expect_equal(getRawTaxonomy(c(7,6,3),tmp),list('7'=c('clade'='Clade E','clade.1'='Clade D','clade.2'='Clade C','clade.3'='Clade B','clade.4'='Clade A','superkingdom'='Bacteria'),'6'=c('clade'='Clade D','clade.1'='Clade C','clade.2'='Clade B','clade.3'='Clade A','superkingdom'='Bacteria'),'3'=c('clade'='Clade A','superkingdom'='Bacteria')))
  expect_equal(getRawTaxonomy(c(7),tmp),list('7'=c('clade'='Clade E','clade.1'='Clade D','clade.2'='Clade C','clade.3'='Clade B','clade.4'='Clade A','superkingdom'='Bacteria')))
})

test_that("Test getTaxonomy with deprecated data.tables",{
  namesText<-c(
    "1\t|\tall\t|\t\t|\tsynonym\t|",
    "1\t|\troot\t|\t\t|\tscientific name\t|",
    "2\t|\tBacteria\t|\tBacteria <prokaryotes>\t|\tscientific name\t|",
    "2\t|\tMonera\t|\tMonera <Bacteria>\t|\tin-part\t|",
    "2\t|\tProcaryotae\t|\tProcaryotae <Bacteria>\t|\tin-part\t|",
    "9606\t|\tHomo sapiens\t|\t\t|\tscientific name", "9605\t|\tHomo\t|\t\t|\tscientific name",
    "207598\t|\tHomininae\t|\t\t|\tscientific name", "9604\t|\tHominidae\t|\t\t|\tscientific name",
    "314295\t|\tHominoidea\t|\t\t|\tscientific name", "9526\t|\tCatarrhini\t|\t\t|\tscientific name",
    "314293\t|\tSimiiformes\t|\t\t|\tscientific name", "376913\t|\tHaplorrhini\t|\t\t|\tscientific name",
    "9443\t|\tPrimates\t|\t\t|\tscientific name", "314146\t|\tEuarchontoglires\t|\t\t|\tscientific name",
    "1437010\t|\tBoreoeutheria\t|\t\t|\tscientific name", "9347\t|\tEutheria\t|\t\t|\tscientific name",
    "32525\t|\tTheria\t|\t\t|\tscientific name", "40674\t|\tMammalia\t|\t\t|\tscientific name",
    "32524\t|\tAmniota\t|\t\t|\tscientific name", "32523\t|\tTetrapoda\t|\t\t|\tscientific name",
    "1338369\t|\tDipnotetrapodomorpha\t|\t\t|\tscientific name",
    "8287\t|\tSarcopterygii\t|\t\t|\tscientific name", "117571\t|\tEuteleostomi\t|\t\t|\tscientific name",
    "117570\t|\tTeleostomi\t|\t\t|\tscientific name", "7776\t|\tGnathostomata\t|\t\t|\tscientific name",
    "7742\t|\tVertebrata\t|\t\t|\tscientific name", "89593\t|\tCraniata\t|\t\t|\tscientific name",
    "7711\t|\tChordata\t|\t\t|\tscientific name", "33511\t|\tDeuterostomia\t|\t\t|\tscientific name",
    "33213\t|\tBilateria\t|\t\t|\tscientific name", "6072\t|\tEumetazoa\t|\t\t|\tscientific name",
    "33208\t|\tMetazoa\t|\t\t|\tscientific name", "33154\t|\tOpisthokonta\t|\t\t|\tscientific name",
    "2759\t|\tEukaryota\t|\t\t|\tscientific name", "131567\t|\tcellular organisms\t|\t\t|\tscientific name"
  )
  taxaNames<-expect_warning(read.names(textConnection(namesText)))
  nodesText<-c(
   "1\t|\t1\t|\tno rank\t|\t\t|\t8\t|\t0\t|\t1\t|\t0\t|\t0\t|\t0\t|\t0\t|\t0\t|\t\t|",
    "2\t|\t131567\t|\tsuperkingdom\t|\t\t|\t0\t|\t0\t|\t11\t|\t0\t|\t0\t|\t0\t|\t0\t|\t0\t|\t\t|",
    "6\t|\t335928\t|\tgenus\t|\t\t|\t0\t|\t1\t|\t11\t|\t1\t|\t0\t|\t1\t|\t0\t|\t0\t|\t\t|",
    "7\t|\t6\t|\tspecies\t|\tAC\t|\t0\t|\t1\t|\t11\t|\t1\t|\t0\t|\t1\t|\t1\t|\t0\t|\t\t|",
    "9\t|\t32199\t|\tspecies\t|\tBA\t|\t0\t|\t1\t|\t11\t|\t1\t|\t0\t|\t1\t|\t1\t|\t0\t|\t\t|",
    "9606\t|\t9605\t|\tspecies", "9605\t|\t207598\t|\tgenus", "207598\t|\t9604\t|\tsubfamily",
    "9604\t|\t314295\t|\tfamily", "314295\t|\t9526\t|\tsuperfamily",
    "9526\t|\t314293\t|\tparvorder", "314293\t|\t376913\t|\tinfraorder",
    "376913\t|\t9443\t|\tsuborder", "9443\t|\t314146\t|\torder",
    "314146\t|\t1437010\t|\tsuperorder", "1437010\t|\t9347\t|\tno rank",
    "9347\t|\t32525\t|\tno rank", "32525\t|\t40674\t|\tno rank",
    "40674\t|\t32524\t|\tclass", "32524\t|\t32523\t|\tno rank", "32523\t|\t1338369\t|\tno rank",
    "1338369\t|\t8287\t|\tno rank", "8287\t|\t117571\t|\tno rank",
    "117571\t|\t117570\t|\tno rank", "117570\t|\t7776\t|\tno rank",
    "7776\t|\t7742\t|\tno rank", "7742\t|\t89593\t|\tno rank", "89593\t|\t7711\t|\tsubphylum",
    "7711\t|\t33511\t|\tphylum", "33511\t|\t33213\t|\tno rank", "33213\t|\t6072\t|\tno rank",
    "6072\t|\t33208\t|\tno rank", "33208\t|\t33154\t|\tkingdom",
    "33154\t|\t2759\t|\tno rank", "2759\t|\t131567\t|\tsuperkingdom",
    "131567\t|\t1\t|\tno rank"
  )
  taxaNodes<-expect_warning(read.nodes(textConnection(nodesText)))
  desiredTaxa<-c('superkingdom','phylum','class','order','family','genus','species')
  out<-matrix(c(
    "Eukaryota","Chordata","Mammalia","Primates","Hominidae","Homo","Homo sapiens",
    "Eukaryota","Chordata","Mammalia","Primates","Hominidae","Homo",NA
  ),byrow=TRUE,dimnames=list(c('9606','9605'),desiredTaxa),nrow=2)
  expect_warning(expect_equal(getTaxonomy(c(9606,9605),taxaNodes,taxaNames,mc.cores=1,desiredTaxa=desiredTaxa),out))
  expect_warning(expect_equal(getTaxonomy(c(9605,9606,9605),taxaNodes,taxaNames,mc.cores=1,desiredTaxa=desiredTaxa),out[c(2,1,2),]))
  expect_warning(expect_equal(getTaxonomy(c(9605,9606,9605),taxaNodes,taxaNames,mc.cores=1,desiredTaxa=desiredTaxa[3:1]),out[c(2,1,2),3:1]))
  expect_warning(expect_output(getTaxonomy(9606,taxaNodes,taxaNames,mc.cores=1,debug=TRUE),'\\\\t'))
  expect_warning(expect_equal(getTaxonomy(9606,taxaNodes,taxaNames,mc.cores=1,desiredTaxa='NOTREAL'),matrix(as.character(NA),dimnames=list(9606,'NOTREAL'))))
  expect_warning(expect_equal(getTaxonomy(9999999,taxaNodes,taxaNames,mc.cores=1,desiredTaxa='class'),matrix(as.character(NA),dimnames=list(9999999,'class'))))
  #causes error on Windows
  if(.Platform$OS.type == "unix")expect_warning(expect_equal(getTaxonomy(c(9605,9606,9605),taxaNodes,taxaNames,mc.cores=2,desiredTaxa=desiredTaxa),out[c(2,1,2),]))
  expect_warning(expect_equal(getTaxonomy(c(),taxaNodes,taxaNames,mc.cores=1,desiredTaxa=desiredTaxa),NULL))
  naDf<-out
  naDf[,]<-NA
  rownames(naDf)<-c('NA','NA')
  expect_warning(expect_equal(getTaxonomy(c(NA,NA),taxaNodes,taxaNames),naDf))
  expect_warning(expect_equal(getTaxonomy(c(NA,9605,NA,'9604,9605'),taxaNodes,taxaNames),rbind('  NA'=naDf[1,],'9605'=out[2,],'  NA'=naDf[1,],'  NA'=naDf[1,])))
  expect_warning(expect_equal(getTaxonomy('9605',taxaNodes,taxaNames),getTaxonomy(9605,taxaNodes,taxaNames)))
  expect_warning(getTaxonomy('9605,123',taxaNodes,taxaNames),'coercion')
  expect_warning(getTaxonomy(9999999,taxaNodes,taxaNames),'SQLite')
})

test_that("Test getTaxonomy2",{
  namesText<-c(
    "1\t|\tall\t|\t\t|\tsynonym\t|",
    "1\t|\troot\t|\t\t|\tscientific name\t|",
    "2\t|\tBacteria\t|\tBacteria <prokaryotes>\t|\tscientific name\t|",
    "2\t|\tMonera\t|\tMonera <Bacteria>\t|\tin-part\t|",
    "2\t|\tProcaryotae\t|\tProcaryotae <Bacteria>\t|\tin-part\t|",
    "9606\t|\tHomo sapiens\t|\t\t|\tscientific name", "9605\t|\tHomo\t|\t\t|\tscientific name",
    "207598\t|\tHomininae\t|\t\t|\tscientific name", "9604\t|\tHominidae\t|\t\t|\tscientific name",
    "314295\t|\tHominoidea\t|\t\t|\tscientific name", "9526\t|\tCatarrhini\t|\t\t|\tscientific name",
    "314293\t|\tSimiiformes\t|\t\t|\tscientific name", "376913\t|\tHaplorrhini\t|\t\t|\tscientific name",
    "9443\t|\tPrimates\t|\t\t|\tscientific name", "314146\t|\tEuarchontoglires\t|\t\t|\tscientific name",
    "1437010\t|\tBoreoeutheria\t|\t\t|\tscientific name", "9347\t|\tEutheria\t|\t\t|\tscientific name",
    "32525\t|\tTheria\t|\t\t|\tscientific name", "40674\t|\tMammalia\t|\t\t|\tscientific name",
    "32524\t|\tAmniota\t|\t\t|\tscientific name", "32523\t|\tTetrapoda\t|\t\t|\tscientific name",
    "1338369\t|\tDipnotetrapodomorpha\t|\t\t|\tscientific name",
    "8287\t|\tSarcopterygii\t|\t\t|\tscientific name", "117571\t|\tEuteleostomi\t|\t\t|\tscientific name",
    "117570\t|\tTeleostomi\t|\t\t|\tscientific name", "7776\t|\tGnathostomata\t|\t\t|\tscientific name",
    "7742\t|\tVertebrata\t|\t\t|\tscientific name", "89593\t|\tCraniata\t|\t\t|\tscientific name",
    "7711\t|\tChordata\t|\t\t|\tscientific name", "33511\t|\tDeuterostomia\t|\t\t|\tscientific name",
    "33213\t|\tBilateria\t|\t\t|\tscientific name", "6072\t|\tEumetazoa\t|\t\t|\tscientific name",
    "33208\t|\tMetazoa\t|\t\t|\tscientific name", "33154\t|\tOpisthokonta\t|\t\t|\tscientific name",
    "2759\t|\tEukaryota\t|\t\t|\tscientific name", "131567\t|\tcellular organisms\t|\t\t|\tscientific name"
  )
  taxaNames<-expect_warning(read.names(textConnection(namesText)))
  nodesText<-c(
   "1\t|\t1\t|\tno rank\t|\t\t|\t8\t|\t0\t|\t1\t|\t0\t|\t0\t|\t0\t|\t0\t|\t0\t|\t\t|",
    "2\t|\t131567\t|\tsuperkingdom\t|\t\t|\t0\t|\t0\t|\t11\t|\t0\t|\t0\t|\t0\t|\t0\t|\t0\t|\t\t|",
    "6\t|\t335928\t|\tgenus\t|\t\t|\t0\t|\t1\t|\t11\t|\t1\t|\t0\t|\t1\t|\t0\t|\t0\t|\t\t|",
    "7\t|\t6\t|\tspecies\t|\tAC\t|\t0\t|\t1\t|\t11\t|\t1\t|\t0\t|\t1\t|\t1\t|\t0\t|\t\t|",
    "9\t|\t32199\t|\tspecies\t|\tBA\t|\t0\t|\t1\t|\t11\t|\t1\t|\t0\t|\t1\t|\t1\t|\t0\t|\t\t|",
    "9606\t|\t9605\t|\tspecies", "9605\t|\t207598\t|\tgenus", "207598\t|\t9604\t|\tsubfamily",
    "9604\t|\t314295\t|\tfamily", "314295\t|\t9526\t|\tsuperfamily",
    "9526\t|\t314293\t|\tparvorder", "314293\t|\t376913\t|\tinfraorder",
    "376913\t|\t9443\t|\tsuborder", "9443\t|\t314146\t|\torder",
    "314146\t|\t1437010\t|\tsuperorder", "1437010\t|\t9347\t|\tno rank",
    "9347\t|\t32525\t|\tno rank", "32525\t|\t40674\t|\tno rank",
    "40674\t|\t32524\t|\tclass", "32524\t|\t32523\t|\tno rank", "32523\t|\t1338369\t|\tno rank",
    "1338369\t|\t8287\t|\tno rank", "8287\t|\t117571\t|\tno rank",
    "117571\t|\t117570\t|\tno rank", "117570\t|\t7776\t|\tno rank",
    "7776\t|\t7742\t|\tno rank", "7742\t|\t89593\t|\tno rank", "89593\t|\t7711\t|\tsubphylum",
    "7711\t|\t33511\t|\tphylum", "33511\t|\t33213\t|\tno rank", "33213\t|\t6072\t|\tno rank",
    "6072\t|\t33208\t|\tno rank", "33208\t|\t33154\t|\tkingdom",
    "33154\t|\t2759\t|\tno rank", "2759\t|\t131567\t|\tsuperkingdom",
    "131567\t|\t1\t|\tno rank"
  )
  taxaNodes<-expect_warning(read.nodes(textConnection(nodesText)))
  desiredTaxa<-c('superkingdom','phylum','class','order','family','genus','species')
  out<-matrix(c(
    "Eukaryota","Chordata","Mammalia","Primates","Hominidae","Homo","Homo sapiens",
    "Eukaryota","Chordata","Mammalia","Primates","Hominidae","Homo",NA
  ),byrow=TRUE,dimnames=list(c('9606','9605'),desiredTaxa),nrow=2)
  expect_warning(expect_equal(getTaxonomy2(c(9606,9605),taxaNodes,taxaNames,mc.cores=1,desiredTaxa=desiredTaxa),out))
  expect_warning(expect_equal(getTaxonomy2(c(9605,9606,9605),taxaNodes,taxaNames,mc.cores=1,desiredTaxa=desiredTaxa),out[c(2,1,2),]))
  expect_warning(expect_equal(getTaxonomy2(c(9605,9606,9605),taxaNodes,taxaNames,mc.cores=1,desiredTaxa=desiredTaxa[3:1]),out[c(2,1,2),3:1]))
  expect_warning(expect_output(getTaxonomy2(9606,taxaNodes,taxaNames,mc.cores=1,debug=TRUE),'\\\\t'))
  expect_warning(expect_equal(getTaxonomy2(9606,taxaNodes,taxaNames,mc.cores=1,desiredTaxa='NOTREAL'),matrix(as.character(NA),dimnames=list(9606,'NOTREAL'))))
  expect_warning(expect_equal(getTaxonomy2(9999999,taxaNodes,taxaNames,mc.cores=1,desiredTaxa='class'),matrix(as.character(NA),dimnames=list(9999999,'class'))))
  #causes error on Windows
  if(.Platform$OS.type == "unix")expect_warning(expect_equal(getTaxonomy2(c(9605,9606,9605),taxaNodes,taxaNames,mc.cores=2,desiredTaxa=desiredTaxa),out[c(2,1,2),]))
  expect_warning(expect_equal(getTaxonomy2(c(),taxaNodes,taxaNames,mc.cores=1,desiredTaxa=desiredTaxa),NULL))
  naDf<-out
  naDf[,]<-NA
  rownames(naDf)<-c('NA','NA')
  expect_warning(expect_equal(getTaxonomy2(c(NA,NA),taxaNodes,taxaNames),naDf))
  expect_warning(expect_equal(getTaxonomy2(c(NA,9605,NA,'9604,9605'),taxaNodes,taxaNames),rbind('  NA'=naDf[1,],'9605'=out[2,],'  NA'=naDf[1,],'  NA'=naDf[1,])))
  expect_warning(expect_equal(getTaxonomy2('9605',taxaNodes,taxaNames),getTaxonomy2(9605,taxaNodes,taxaNames)))
  expect_warning(getTaxonomy2('9605,123',taxaNodes,taxaNames),'coercion')
})

test_that("Test getParentNodes",{
  nodesText<-c(
   "1\t|\t1\t|\tno rank\t|\t\t|\t8\t|\t0\t|\t1\t|\t0\t|\t0\t|\t0\t|\t0\t|\t0\t|\t\t|",
    "2\t|\t131567\t|\tsuperkingdom\t|\t\t|\t0\t|\t0\t|\t11\t|\t0\t|\t0\t|\t0\t|\t0\t|\t0\t|\t\t|",
    "6\t|\t335928\t|\tgenus\t|\t\t|\t0\t|\t1\t|\t11\t|\t1\t|\t0\t|\t1\t|\t0\t|\t0\t|\t\t|",
    "7\t|\t6\t|\tspecies\t|\tAC\t|\t0\t|\t1\t|\t11\t|\t1\t|\t0\t|\t1\t|\t1\t|\t0\t|\t\t|",
    "9\t|\t32199\t|\tspecies\t|\tBA\t|\t0\t|\t1\t|\t11\t|\t1\t|\t0\t|\t1\t|\t1\t|\t0\t|\t\t|",
    "9606\t|\t9605\t|\tspecies", "9605\t|\t207598\t|\tgenus", "207598\t|\t9604\t|\tsubfamily",
    "9604\t|\t314295\t|\tfamily", "314295\t|\t9526\t|\tsuperfamily",
    "9526\t|\t314293\t|\tparvorder", "314293\t|\t376913\t|\tinfraorder",
    "376913\t|\t9443\t|\tsuborder", "9443\t|\t314146\t|\torder",
    "314146\t|\t1437010\t|\tsuperorder", "1437010\t|\t9347\t|\tno rank",
    "9347\t|\t32525\t|\tno rank", "32525\t|\t40674\t|\tno rank",
    "40674\t|\t32524\t|\tclass", "32524\t|\t32523\t|\tno rank", "32523\t|\t1338369\t|\tno rank",
    "1338369\t|\t8287\t|\tno rank", "8287\t|\t117571\t|\tno rank",
    "117571\t|\t117570\t|\tno rank", "117570\t|\t7776\t|\tno rank",
    "7776\t|\t7742\t|\tno rank", "7742\t|\t89593\t|\tno rank", "89593\t|\t7711\t|\tsubphylum",
    "7711\t|\t33511\t|\tphylum", "33511\t|\t33213\t|\tno rank", "33213\t|\t6072\t|\tno rank",
    "6072\t|\t33208\t|\tno rank", "33208\t|\t33154\t|\tkingdom",
    "33154\t|\t2759\t|\tno rank", "2759\t|\t131567\t|\tsuperkingdom",
    "131567\t|\t1\t|\tno rank"
  )
  namesText<-c(
    "1\t|\tall\t|\t\t|\tsynonym\t|",
    "1\t|\troot\t|\t\t|\tscientific name\t|",
    "2\t|\tBacteria\t|\tBacteria <prokaryotes>\t|\tscientific name\t|",
    "2\t|\tMonera\t|\tMonera <Bacteria>\t|\tin-part\t|",
    "2\t|\tProcaryotae\t|\tProcaryotae <Bacteria>\t|\tin-part\t|",
    "9606\t|\tMan\t|\t\t|\tsynonym","9606\t|\thuman\t|\t\t|\tsynonym",
    "9606\t|\tHomo sapiens\t|\t\t|\tscientific name", "9605\t|\tHomo\t|\t\t|\tscientific name",
    "207598\t|\tHomininae\t|\t\t|\tscientific name", "9604\t|\tHominidae\t|\t\t|\tscientific name",
    "314295\t|\tHominoidea\t|\t\t|\tscientific name", "9526\t|\tCatarrhini\t|\t\t|\tscientific name",
    "314293\t|\tSimiiformes\t|\t\t|\tscientific name", "376913\t|\tHaplorrhini\t|\t\t|\tscientific name",
    "9443\t|\tPrimates\t|\t\t|\tscientific name", "314146\t|\tEuarchontoglires\t|\t\t|\tscientific name",
    "1437010\t|\tBoreoeutheria\t|\t\t|\tscientific name", "9347\t|\tEutheria\t|\t\t|\tscientific name",
    "32525\t|\tTheria\t|\t\t|\tscientific name", "40674\t|\tMammalia\t|\t\t|\tscientific name",
    "32524\t|\tAmniota\t|\t\t|\tscientific name", "32523\t|\tTetrapoda\t|\t\t|\tscientific name",
    "1338369\t|\tDipnotetrapodomorpha\t|\t\t|\tscientific name",
    "8287\t|\tSarcopterygii\t|\t\t|\tscientific name", "117571\t|\tEuteleostomi\t|\t\t|\tscientific name",
    "117570\t|\tTeleostomi\t|\t\t|\tscientific name", "7776\t|\tGnathostomata\t|\t\t|\tscientific name",
    "7742\t|\tVertebrata\t|\t\t|\tscientific name", "89593\t|\tCraniata\t|\t\t|\tscientific name",
    "7711\t|\tChordata\t|\t\t|\tscientific name", "33511\t|\tDeuterostomia\t|\t\t|\tscientific name",
    "33213\t|\tBilateria\t|\t\t|\tscientific name", "6072\t|\tEumetazoa\t|\t\t|\tscientific name",
    "33208\t|\tMetazoa\t|\t\t|\tscientific name", "33154\t|\tOpisthokonta\t|\t\t|\tscientific name",
    "2759\t|\tEukaryota\t|\t\t|\tscientific name", "131567\t|\tcellular organisms\t|\t\t|\tscientific name"
  )
  tmp<-tempfile()
  read.nodes.sql(textConnection(nodesText),tmp)
  read.names.sql(textConnection(namesText),tmp)
  expect_equal(taxonomizr:::getParentNodes(c(9606,9605),tmp),data.frame('name'=c('Homo sapiens','Homo'),'parent'=c(9605,207598),'rank'=c('species','genus'),stringsAsFactors=FALSE))
  expect_equal(taxonomizr:::getParentNodes(c('a'=9606,'b'=9605),tmp),data.frame('name'=c('Homo sapiens','Homo'),'parent'=c(9605,207598),'rank'=c('species','genus'),stringsAsFactors=FALSE))
  expect_equal(taxonomizr:::getParentNodes(c(NA,9606,9999999,9606),tmp),data.frame('name'=c(NA,'Homo sapiens',NA,'Homo sapiens'),'parent'=c(NA,9605,NA,9605),'rank'=c(NA,'species',NA,'species'),stringsAsFactors=FALSE))
  with_mock(`RSQLite::dbGetQuery`=function(...){data.frame('id'=c(9999))},expect_error(taxonomizr:::getParentNodes(c(9606,9605),tmp),'finding'))
  with_mock(`RSQLite::dbGetQuery`=function(...){data.frame('id'=c(9605,9606))},expect_error(taxonomizr:::getParentNodes(c(9606,9605),tmp),'finding'))
})

test_that("Test checkDownloadMd5",{
  tmp<-tempfile()
  tmp2<-tempfile()
  writeLines('THISISSOMEDATA',tmp)
  writeLines('THISISSOMEOTHERDATA',tmp2)
  md5<-tools::md5sum(tmp)
  expect_true(checkDownloadMd5(sprintf('file://%s',tmp),tmp2)) #no md5 so can't check and doesn't error
  expect_error(checkDownloadMd5(sprintf('file://%s',tmp),tmp2,TRUE),'md5') #no md5 so can't check and error flag set
  writeLines(sprintf('%s EXTRASTUFF',md5),sprintf('%s.md5',tmp))
  expect_false(checkDownloadMd5(sprintf('file://%s',tmp),tmp2))
  file.copy(tmp,tmp2,TRUE)
  expect_true(checkDownloadMd5(sprintf('file://%s',tmp),tmp2))
  writeLines(sprintf('SOMEOTHERMD5 EXTRASTUFF %s',md5),sprintf('%s.md5',tmp))
  expect_false(checkDownloadMd5(sprintf('file://%s',tmp),tmp2))
})



test_that("Test accessionToTaxa",{
  taxa<-c(
    "accession\taccession.version\ttaxid\tgi",
    "Z17427\tZ17427.1\t3702\t16569",
    "Z17428\tZ17428.1\t3702\t16570",
    "Z17429\tZ17429.1\t3702\t16571",
    "Z17430\tZ17430.1\t3702\t16572",
    "X62402\tX62402.1\t9606\t30394"
  )
  inFile<-tempfile()
  sqlFile<-tempfile()
  #not created yet
  expect_error(accessionToTaxa("Z17430.1",notARealVariable),"found")
  expect_error(accessionToTaxa("Z17430.1",sqlFile),"exist")
  expect_error(accessionToTaxa(c(),notARealVariable),"found")
  expect_error(accessionToTaxa(c(),sqlFile),"exist")
  writeLines(taxa,inFile)
  read.accession2taxid(inFile,sqlFile)
  expect_equal(accessionToTaxa(c("Z17430.1","Z17429.1","X62402.1"),sqlFile),c(3702,3702,9606))
  expect_equal(accessionToTaxa(c('A'="Z17430.1",'B'="Z17429.1",'C'="X62402.1"),sqlFile),c(3702,3702,9606))
  expect_equal(accessionToTaxa(c(),sqlFile),c())
  expect_equal(accessionToTaxa(c("Z17430.1","NOTREAL","X62402.1","Z17429.1","X62402.1"),sqlFile),c(3702,NA,9606,3702,9606))
  expect_error(accessionToTaxa("Z17430.1","NOTREAL"),"exist")
  expect_equal(accessionToTaxa(c(),sqlFile),c())
  expect_equal(accessionToTaxa(c("Z17430.1","Z17429.1","X62402.1"),sqlFile,'base'),as.integer(c(NA,NA,NA)))
  expect_equal(accessionToTaxa(c("Z17430","NOTREAL","X62402","Z17429","X62402"),sqlFile,'base'),c(3702,NA,9606,3702,9606))
  expect_equal(accessionToTaxa(as.factor(c("Z17430","NOTREAL","X62402","Z17429","X62402")),sqlFile,'base'),c(3702,NA,9606,3702,9606))
  expect_equal(accessionToTaxa(c("Z17430","NOTREAL","X62402","Z17429","X62402"),sqlFile,'version'),as.integer(c(NA,NA,NA,NA,NA)))
  with_mock(`RSQLite::dbGetQuery`=function(...){data.frame('accession'=c(9605,9606))},expect_error(accessionToTaxa(c("Z17430.1","X62402.1"),sqlFile),'mismatch'))
  with_mock(`RSQLite::dbGetQuery`=function(...){data.frame('accession'=c("X62402.1","Z17430.1"))},expect_error(accessionToTaxa(c("Z17430.1","X62402.1"),sqlFile),'mismatch'))
  with_mock(`RSQLite::dbGetQuery`=function(...){data.frame('accession'=c("NOTREAL","NOTREAL2","NOTREAL3"))},expect_error(accessionToTaxa(c("Z17430.1","X62402.1"),sqlFile),'mismatch'))
})

test_that("Test condenseTaxa",{
  taxas<-matrix(c(
   'a','b','c','e',
   'a','b','d','e'
  ),nrow=2,byrow=TRUE)
  expect_equal(condenseTaxa(taxas),matrix(c('a','b',NA,NA),nrow=1,dimnames=list('1',c('V1','V2','V3','V4'))))
  expect_equal(condenseTaxa(taxas[c(1,1,1),]),matrix(c('a','b','c','e'),nrow=1,dimnames=list('1',c('V1','V2','V3','V4'))))
  expect_equal(condenseTaxa(taxas[1,,drop=FALSE]),matrix(c('a','b','c','e'),nrow=1,dimnames=list('1',c('V1','V2','V3','V4'))))
  expect_equal(condenseTaxa(taxas[,1,drop=FALSE]),matrix(c('a'),nrow=1,dimnames=list('1',c('V1'))))
  expect_equal(condenseTaxa(taxas[1,1,drop=FALSE]),matrix(c('a'),nrow=1,dimnames=list('1',c('V1'))))
  expect_equal(condenseTaxa(taxas[c(1,1,1,2),]),matrix(c('a','b',NA,NA),nrow=1,dimnames=list('1',c('V1','V2','V3','V4'))))
  expect_equal(condenseTaxa(taxas[,3,drop=FALSE]),matrix(c(as.character(NA)),nrow=1,dimnames=list('1',c('V1'))))
  expect_equal(condenseTaxa(taxas[,3,drop=FALSE],1:2),matrix(c('c','d'),nrow=2,dimnames=list(c('1','2'),c('V1'))))
  expect_equal(condenseTaxa(taxas[0,]),NULL)
  expect_equal(condenseTaxa(taxas[c(1:2,1),],c(1,1,2)),matrix(c('a','a','b','b',NA,'c',NA,'e'),nrow=2,dimnames=list(c('1','2'),c('V1','V2','V3','V4'))))
  expect_equal(condenseTaxa(taxas[c(1:2,rep(1,10)),],c(1,1,rep(2,10))),matrix(c('a','a','b','b',NA,'c',NA,'e'),nrow=2,dimnames=list(c('1','2'),c('V1','V2','V3','V4'))))
  taxas<-matrix(c(
   'a','b',NA,'e',
   'a','b','d','e'
  ),nrow=2,byrow=TRUE,dimnames=list(NULL,c('a','b','c','d')))
  expect_equal(condenseTaxa(taxas),matrix(c('a','b',NA,NA),nrow=1,dimnames=list('1',c('a','b','c','d'))))
  expect_equal(condenseTaxa(taxas[1,,drop=FALSE]),matrix(c('a','b',NA,'e'),nrow=1,dimnames=list('1',c('a','b','c','d'))))
  expect_equal(condenseTaxa(taxas[c(1,1,1),,drop=FALSE]),matrix(c('a','b',NA,'e'),nrow=1,dimnames=list('1',c('a','b','c','d'))))
  out<-matrix(c('a','b',NA,NA),byrow=TRUE,nrow=10,ncol=4,dimnames=list(as.character(1:10),c('a','b','c','d')))
  expect_equal(condenseTaxa(taxas[rep(1:2,each=10),,drop=FALSE],rep(1:10,2)),out)
  rownames(out)<-letters[1:10]
  expect_equal(condenseTaxa(taxas[rep(1:2,each=10),,drop=FALSE],rep(letters[1:10],2)),out)
})


test_that("Test getNamesAndNodes",{
  tmp<-tempfile()
  dir.create(tmp)
  testFile<-system.file('testdata/fakeNamesNodes.tar.gz',package='taxonomizr')
  #windows download.file() of local file can't handle 1a bytes created by gzip
  if(.Platform$OS.type == "windows"){
    R.utils::gunzip(testFile,remove=FALSE,skip=TRUE)
    testFile<-system.file('testdata/fakeNamesNodes.tar',package='taxonomizr')
  }
  fakeFile<-sprintf('file://%s',testFile)
  expect_error(getNamesAndNodes(tmp,fakeFile),NA)
  expect_equal(sort(list.files(tmp,'^(names|nodes).dmp$')),c('names.dmp','nodes.dmp'))
  expect_message(getNamesAndNodes(tmp,fakeFile),'exist')
  expect_equal(getNamesAndNodes(tmp,fakeFile),file.path(tmp,c('names.dmp','nodes.dmp')))
  #windows throws "incomplete block on file"
  expect_error(getNamesAndNodes(tmp,fakeFile,'NOTREAL.FILE'),'finding|incomplete')
  tmp<-tempfile()
  with_mock(`file.copy`=function(...)TRUE,expect_error(getNamesAndNodes(tmp,fakeFile),'copying'))
  if(.Platform$OS.type == "windows")file.remove('fakeNamesNodes.tar')
  tmp<-tempfile()
  dir.create(tmp)
  newFake<-file.path(tmp,'fake')
  download.file(fakeFile,newFake,mode='wb')
  fakeMd5<-tools::md5sum(newFake)
  writeLines(sprintf('%s EXTRATEXT',fakeMd5),sprintf('%s.md5',newFake))
  expect_error(getNamesAndNodes(tmp,sprintf('file://%s',newFake)),NA)
  tmp<-tempfile()
  dir.create(tmp)
  writeLines('NOTREALHASH EXTRATEXT',sprintf('%s.md5',newFake))
  expect_error(getNamesAndNodes(tmp,sprintf('file://%s',newFake)),'match')
})

test_that("Test getAccession2taxid",{
  tmp<-tempfile()
  dir.create(tmp)
  types<-c('XxXx','XyXyX')
  targets<-sprintf('nucl_%s.accession2taxid.gz',types)
  sapply(targets,function(xx)writeLines('TEST',file.path(tmp,xx),sep='')) #avoiding newlines to avoid silly windows issues
  tmp2<-tempfile()
  dir.create(tmp2)
  expect_error(getAccession2taxid(tmp2,baseUrl=sprintf('file://%s',tmp),types=c('nucl_XxXx','nucl_XyXyX')),NA)
  expect_equal(sort(list.files(tmp2,'accession2taxid.gz$')),sort(targets))
  expect_message(getAccession2taxid(tmp2,baseUrl=sprintf('file://%s',tmp),types=c('nucl_XxXx','nucl_XyXyX')),'exist')
  file.remove(list.files(tmp2,'^nucl_.*.gz$',full.names=TRUE))
  writeLines('NOTREALHASH EXTRATEXT',sprintf('%s.md5',file.path(tmp,targets[1])))
  expect_error(getAccession2taxid(tmp2,baseUrl=sprintf('file://%s',tmp),types=c('nucl_XxXx','nucl_XyXyX')),'match')
  fakeMd5<-tools::md5sum(file.path(tmp,targets[1]))
  file.remove(list.files(tmp2,'^nucl_.*.gz$',full.names=TRUE))
  writeLines(sprintf('%s EXTRATEXT',fakeMd5),sprintf('%s.md5',file.path(tmp,targets[1])))
  expect_error(getAccession2taxid(tmp2,baseUrl=sprintf('file://%s',tmp),types=c('nucl_XxXx','nucl_XyXyX')),NA)
  fakeMd5<-tools::md5sum(file.path(tmp,targets[2]))
  file.remove(list.files(tmp2,'^nucl_.*.gz$',full.names=TRUE))
  writeLines('NOTREALHASH EXTRATEXT',sprintf('%s.md5',file.path(tmp,targets[2])))
  expect_error(getAccession2taxid(tmp2,baseUrl=sprintf('file://%s',tmp),types=c('nucl_XxXx','nucl_XyXyX')),'match')
  file.remove(list.files(tmp2,'^nucl_.*.gz$',full.names=TRUE))
  writeLines(sprintf('%s EXTRATEXT',fakeMd5),sprintf('%s.md5',file.path(tmp,targets[2])))
  expect_error(getAccession2taxid(tmp2,baseUrl=sprintf('file://%s',tmp),types=c('nucl_XxXx','nucl_XyXyX')),NA)
})

test_that("Test getId with deprecated data.table",{
 namesText<-c(
   "1\t|\troot\t|\t\t|\tscientific name\t|",
   "4\t|\tMulti\t|\tBacteria <prokaryotes>\t|\tscientific name\t|",
   "3\t|\tMulti\t|\tBacteria <prokaryotes>\t|\tscientific name\t|",
   "2\t|\tBacteria\t|\tBacteria <prokaryotes>\t|\tscientific name\t|"
 )
 expect_warning(names<-read.names(textConnection(namesText)))
 expect_warning(expect_equal(getId('Bacteria',names),'2'))
 expect_warning(expect_equal(getId(c('Bacteria','root','Bacteria','NOTREAL'),names),c('2','1','2',NA)))
 expect_warning(expect_equal(getId('Not a real name',names),as.character(NA)))
 expect_warning(expect_equal(getId('Multi',names),'3,4'))
 expect_warning(getId('Multi',names),'Multiple')
 expect_warning(getId('Bacteria',names),'SQLite')
})

test_that("Test getId2",{
 namesText<-c(
   "1\t|\troot\t|\t\t|\tscientific name\t|",
   "4\t|\tMulti1\t|\tBacteria <prokaryotes>\t|\tscientific name\t|",
   "3\t|\tMulti1\t|\tBacteria <prokaryotes>\t|\tscientific name\t|",
   "2\t|\tBacteria\t|\tBacteria <prokaryotes>\t|\tscientific name\t|",
   "5\t|\tMulti2\t|\tBacteria <prokaryotes>\t|\tscientific name\t|",
   "6\t|\tMulti2\t|\tBacteria <prokaryotes>\t|\tscientific name\t|"
 )
 expect_warning(names<-read.names(textConnection(namesText)))
 expect_warning(expect_equal(getId2('Bacteria',names),'2'))
 expect_warning(expect_equal(getId2(c('Bacteria','root','Bacteria','NOTREAL'),names),c('2','1','2',NA)))
 expect_warning(expect_equal(getId2('Not a real name',names),as.character(NA)))
 expect_warning(expect_equal(getId2(c('Bacteria','Multi1','NOTREAL'),names),c('2','3,4',NA)))
 expect_warning(getId2(c('Multi1','Bacteria','Multi2'),names),'Multiple.*Multi1, Multi2')
 expect_warning(getId2('Bacteria',names),'SQLite')
})

test_that("Test getId",{
 namesText<-c(
   "1\t|\troot\t|\t\t|\tscientific name\t|",
   "4\t|\tMulti1\t|\tBacteria <prokaryotes>\t|\tscientific name\t|",
   "3\t|\tMulti1\t|\tBacteria <prokaryotes>\t|\tscientific name\t|",
   "2\t|\tBacteria\t|\tBacteria <prokaryotes>\t|\tscientific name\t|",
   "5\t|\tMulti2\t|\tBacteria <prokaryotes>\t|\tscientific name\t|",
   "6\t|\tMulti2\t|\tBacteria <prokaryotes>\t|\tscientific name\t|"
 )
 tmp<-tempfile()
 read.names.sql(textConnection(namesText),tmp)
 expect_equal(getId('Bacteria',tmp),'2')
 expect_equal(getId(c('Bacteria','root','Bacteria','NOTREAL'),tmp),c('2','1','2',NA))
 expect_equal(getId('Not a real name',tmp),as.character(NA))
 suppressWarnings(expect_equal(getId(c('Bacteria','Multi1','NOTREAL'),tmp),c('2','3,4',NA)))
 expect_warning(getId(c('Multi1','Bacteria','Multi2'),tmp),'Multiple.*Multi1, Multi2')
})

test_that("Test getAccessions",{
  taxa<-c(
    "accession\taccession.version\ttaxid\tgi",
    "Z17427\tZ17427.1\t3702\t16569",
    "Z17428\tZ17428.1\t3702\t16570",
    "Z17429\tZ17429.1\t3702\t16571",
    "Z17430\tZ17430.1\t3702\t16572",
    "X62402\tX62402.1\t9606\t30394"
  )
  inFile<-tempfile()
  sqlFile<-tempfile()
  #not created yet
  expect_error(getAccessions("Z17430.1",notARealVariable),"found")
  expect_error(getAccessions("Z17430.1",sqlFile),"exist")
  expect_error(getAccessions(c(),notARealVariable),"found")
  expect_error(getAccessions(c(),sqlFile),"exist")
  writeLines(taxa,inFile)
  read.accession2taxid(inFile,sqlFile)
  #one taxa
  expect_equal(getAccessions(3702,sqlFile),data.frame('taxa'=3702,'accession'=c("Z17427.1","Z17428.1","Z17429.1","Z17430.1"),stringsAsFactors=FALSE))
  #two taxa
  expect_equal(getAccessions(c(3702,9606),sqlFile),data.frame('taxa'=rep(c(3702,9606),c(4,1)),'accession'=c("Z17427.1","Z17428.1","Z17429.1","Z17430.1","X62402.1"),stringsAsFactors=FALSE))
  #two taxa base
  expect_equal(getAccessions(c(3702,9606),sqlFile,'base'),data.frame('taxa'=rep(c(3702,9606),c(4,1)),'accession'=c("Z17427","Z17428","Z17429","Z17430","X62402"),stringsAsFactors=FALSE))
  #one taxa plus one NA
  expect_equal(getAccessions(c(3702,9999),sqlFile),data.frame('taxa'=rep(c(3702,9999),c(4,1)),'accession'=c("Z17427.1","Z17428.1","Z17429.1","Z17430.1",NA),stringsAsFactors=FALSE))
  #one taxa plus two NA
  expect_equal(getAccessions(c(3702,9999,'NOTREAL'),sqlFile),data.frame('taxa'=rep(c(3702,9999,'NOTREAL'),c(4,1,1)),'accession'=c("Z17427.1","Z17428.1","Z17429.1","Z17430.1",NA,NA),stringsAsFactors=FALSE))
  #one taxa plus two NA base
  expect_equal(getAccessions(c(3702,9999,'NOTREAL'),sqlFile,'base'),data.frame('taxa'=rep(c(3702,9999,'NOTREAL'),c(4,1,1)),'accession'=c("Z17427","Z17428","Z17429","Z17430",NA,NA),stringsAsFactors=FALSE))
  #check version
  expect_equal(getAccessions(c(3702,9999,'NOTREAL'),sqlFile,'version'),data.frame('taxa'=rep(c(3702,9999,'NOTREAL'),c(4,1,1)),'accession'=c("Z17427.1","Z17428.1","Z17429.1","Z17430.1",NA,NA),stringsAsFactors=FALSE))
  #check limit
  expect_equal(getAccessions(c(3702,9606),sqlFile,limit=3),data.frame('taxa'=rep(c(3702,9606),c(3,0)),'accession'=c("Z17427.1","Z17428.1","Z17429.1"),stringsAsFactors=FALSE))
  expect_equal(getAccessions(c(),sqlFile),NULL)
})


test_that("Test prepareDatabase",{
  tmp<-tempfile()
  tmpDir<-tempfile()
  dir.create(tmpDir)
  types<-c('XxXx','XyXyX')
  taxa<-list(c(
      "accession\taccession.version\ttaxid\tgi",
      "Z17427\tZ17427.1\t3702\t16569",
      "Z17428\tZ17428.1\t3702\t16570"
    ),c(
      "accession\taccession.version\ttaxid\tgi",
      "Z17429\tZ17429.1\t3702\t16571",
      "Z17430\tZ17430.1\t3702\t16572"
  ))
  targets<-sprintf('nucl_%s.accession2taxid.gz',types)
  mapply(function(xx,yy)writeLines(xx,file.path(tmpDir,yy)),taxa,targets)
  testFile<-system.file('testdata/fakeNamesNodes.tar.gz',package='taxonomizr')
  #windows download.file() of local file can't handle 1a bytes created by gzip
  if(.Platform$OS.type == "windows"){
    R.utils::gunzip(testFile,remove=FALSE,skip=TRUE)
    testFile<-system.file('testdata/fakeNamesNodes.tar',package='taxonomizr')
  }
  fakeFile<-sprintf('file://%s',testFile)
  expect_error(prepareDatabase(tmp,tmpDir,url=fakeFile,baseUrl=sprintf('file://%s',tmpDir),types=c('nucl_XxXx','nucl_XyXyX')),NA)
  db<-RSQLite::dbConnect(RSQLite::SQLite(),tmp)
  expect_equal(sort(RSQLite::dbListTables(db)),c('accessionTaxa','names','nodes'))
  expect_equal(colnames(RSQLite::dbGetQuery(db,'SELECT * FROM names LIMIT 1')),c('id','name','scientific'))
  expect_equal(colnames(RSQLite::dbGetQuery(db,'SELECT * FROM nodes LIMIT 1')),c('id','rank','parent'))
  expect_equal(colnames(RSQLite::dbGetQuery(db,'SELECT * FROM accessionTaxa LIMIT 1')),c('base','accession','taxa'))
  expect_message(prepareDatabase(tmp,tmpDir,url=fakeFile,baseUrl=sprintf('file://%s',tmpDir),types=c('nucl_XxXx','nucl_XyXyX')),'exists')
  expect_message(prepareDatabase(tmp,tmpDir,url='file://NOTAREALFILE',baseUrl='ALSONOTAREALFILE',types=c('NOTREAL','NOTREAL2')),'exists')
  #test create new directory
  tmpDir2<-tempfile()
  expect_error(prepareDatabase(tmp,tmpDir2,url=fakeFile,baseUrl=sprintf('file://%s',tmpDir),types=c('nucl_XxXx','nucl_XyXyX')),NA)
  RSQLite::dbDisconnect(db)
  if(.Platform$OS.type == "windows")file.remove('fakeNamesNodes.tar')
})

test_that("Test makeNewick",{
  expect_equal(makeNewick(matrix('A')),'(A);')
  expect_equal(makeNewick(matrix('')),'();')
  expect_equal(makeNewick(matrix(NA),naSub='Missing',excludeTerminalNAs=FALSE),'(Missing);')
  expect_equal(makeNewick(matrix(NA),naSub='Missing',excludeTerminalNAs=TRUE),'();')
  expect_equal(makeNewick(matrix(LETTERS[1:6],nrow=1)),'((((((F)E)D)C)B)A);')
  expect_equal(makeNewick(matrix(LETTERS[1:6],ncol=1)),'(A,B,C,D,E,F);')
  expect_equal(makeNewick(rbind(LETTERS[1:6],LETTERS[1:6])),'((((((F)E)D)C)B)A);')
  expect_equal(makeNewick(rbind(LETTERS[1:6],LETTERS[1:6],LETTERS[1:6])),'((((((F)E)D)C)B)A);')
  expect_equal(makeNewick(rbind(LETTERS[1:6],LETTERS[1:6],LETTERS[7:12])),'((((((F)E)D)C)B)A,(((((L)K)J)I)H)G);')
  expect_equal(makeNewick(matrix(c('A','B','D','A','C','D'),nrow=2,byrow=TRUE)),'(((D)B,(D)C)A);')
  expect_equal(makeNewick(matrix(c('A',NA,'D','A','C','D'),nrow=2,byrow=TRUE),naSub='_'),'(((D)_,(D)C)A);')
  expect_equal(makeNewick(matrix(c('A',NA,'D','A',NA,'D'),nrow=2,byrow=TRUE),naSub='_'),'(((D)_)A);')
  expect_equal(makeNewick(matrix(c('A','B',NA,'A','C',NA),nrow=2,byrow=TRUE),naSub='xx',excludeTerminalNAs=FALSE),'(((xx)B,(xx)C)A);')
  expect_equal(makeNewick(matrix(c('A','B',NA,'A','C',NA),nrow=2,byrow=TRUE),naSub='xx',excludeTerminalNAs=TRUE),'((B,C)A);')
  expect_equal(makeNewick(matrix(c('A','B',NA,'A',NA,NA),nrow=2,byrow=TRUE),naSub='xx',excludeTerminalNAs=TRUE),'((B)A);')
  expect_equal(makeNewick(matrix(c('A','B',NA,'A',NA,NA),nrow=2,byrow=TRUE),naSub='xx',excludeTerminalNAs=FALSE),'(((xx)B,(xx)xx)A);')
  expect_equal(makeNewick(matrix(c('A','B',NA,'A',NA,'C'),nrow=2,byrow=TRUE),naSub='xx',excludeTerminalNAs=FALSE),'(((xx)B,(C)xx)A);')
  expect_equal(makeNewick(matrix(c('A','B',NA,'A',NA,'C'),nrow=2,byrow=TRUE),naSub='xx',excludeTerminalNAs=TRUE),'((B,(C)xx)A);')
  expect_equal(makeNewick(matrix(c('Ab c','B','D','Ab c','C','D'),nrow=2,byrow=TRUE)),'(((D)B,(D)C)Ab c);')
  expect_equal(makeNewick(matrix(c('Ab c','B','Ab c','D','C','D'),nrow=3,byrow=TRUE)),'((B,D)Ab c,(D)C);')
  expect_equal(makeNewick(matrix(c('Ab c','D','Ab c','B','C','D'),nrow=3,byrow=TRUE)),'((D,B)Ab c,(D)C);')
  expect_equal(makeNewick(matrix(c('Ab c','D','Ab c','B','C','D'),nrow=3,byrow=TRUE),quote='Zz'),'((ZzDZz,ZzBZz)ZzAb cZz,(ZzDZz)ZzCZz);')
  expect_equal(makeNewick(matrix(c('Ab c','D','Ab c','B','C','D'),nrow=3,byrow=TRUE),quote="'"),"(('D','B')'Ab c',('D')'C');")
  expect_equal(makeNewick(matrix(c('Ab c','D','Ab c','B','C','D'),nrow=3,byrow=TRUE),quote='"'),'(("D","B")"Ab c",("D")"C");')
  expect_equal(makeNewick(matrix(c('Ab c','D','Ab c','B','C','D'),nrow=3,byrow=TRUE),quote='"',terminator=NULL),'(("D","B")"Ab c",("D")"C")')
  expect_equal(makeNewick(matrix(c('Ab c','D','Ab c','B','C','D'),nrow=3,byrow=TRUE),quote='"',terminator="Zz"),'(("D","B")"Ab c",("D")"C")Zz')
})

test_that("Test normalizeTaxa",{
  expect_error(normalizeTaxa(list(c('clade'='A'))),'ambiguous')
  expect_error(normalizeTaxa(list(c())),'ambiguous')
  expect_error(normalizeTaxa(list(c('A','B'),c('species'='A'))),'ambiguous')
  expect_error(normalizeTaxa(list(c('genus'='X','species'='Y'),c('species'='A','genus'='C'))),'cycle')
  expect_equal(normalizeTaxa(list(c('species'='X','genus'='Y'),c('species'='A','genus'='C'))),matrix(c('Y','C','X','A'),nrow=2,dimnames=list(c(),c('genus','species'))))
  expect_equal(normalizeTaxa(list(c('species'='X','clade'='Z','genus'='Y'),c('species'='A','genus'='C'))),matrix(c('Y','C','Z',NA,'X','A'),nrow=2,dimnames=list(c(),c('genus','genus.1','species'))))
  expect_equal(normalizeTaxa(list(c('species'='X','clade'='Z','genus'='Y'),c('species'='A','clade.100'='Y','genus'='C'))),matrix(c('Y','C','Z','Y','X','A'),nrow=2,dimnames=list(c(),c('genus','genus.1','species'))))
  expect_equal(normalizeTaxa(list(c('species'='X','clade'='Z','genus'='Y'),c('species'='A','clade.1'='D','clade.100'='W','genus'='C'))),matrix(c('Y','C','Z','W',NA,'D','X','A'),nrow=2,dimnames=list(c(),c('genus','genus.1','genus.2','species'))))
  expect_equal(normalizeTaxa(list(c('clade'='Z','species'='X','genus'='Y'),c('species'='A','genus'='C'))),matrix(c('Y','C','X','A','Z',NA),nrow=2,dimnames=list(c(),c('genus','species','species.1'))))
  expect_equal(normalizeTaxa(list(c('species'='X','genus'='Y','clade'='Z'),c('species'='A','genus'='C'))),matrix(c('Z',NA,'Y','C','X','A'),nrow=2,dimnames=list(c(),c('_ROOT_.1','genus','species'))))
  expect_equal(normalizeTaxa(list(c('species'='X','genus'='Y','clade'='Z'),c('species'='A','genus'='C')),rootFill='XYZ'),matrix(c('Z',NA,'Y','C','X','A'),nrow=2,dimnames=list(c(),c('XYZ.1','genus','species'))))
  expect_equal(normalizeTaxa(list(c('species'='X','clade'='Z','genus'='Y','clade'='Z','clade'='Z','kingdom'='king'),c('species'='A','clade.1'='D','clade.100'='W','genus'='C','kingdom'='king'))),matrix(c('king','king','Z',NA,'Z',NA,'Y','C','Z','W',NA,'D','X','A'),nrow=2,dimnames=list(c(),c("kingdom","kingdom.1","kingdom.2","genus","genus.1","genus.2","species"))))
  expect_equal(normalizeTaxa(list(c('species'='X','clade'='Z','genus'='Y'),c('species'='A','genus'='C'),c('species'='12'))),matrix(c('Y','C',NA,'Z',NA,NA,'X','A','12'),nrow=3,dimnames=list(c(),c('genus','genus.1','species'))))
  expect_equal(normalizeTaxa(list('A'=c('species'='X','clade'='Z','genus'='Y'),'BB'=c('species'='A','genus'='C'),'CCC'=c('species'='12'))),matrix(c('Y','C',NA,'Z',NA,NA,'X','A','12'),nrow=3,dimnames=list(c('A','BB','CCC'),c('genus','genus.1','species'))))
  expect_error(normalizeTaxa(list('A'=c('species'='X','clade'='Z','genus'='Y'),'BB'=c('species'='A','genus'='C'),'CCC'=c('kingdom'='12'))),'[Aa]mbiguous')
  expect_equal(normalizeTaxa(list('A'=c('species'='X','clade'='Z','genus'='Y'),'BB'=c('species'='A','genus'='C'),'CCC'=c('kingdom'='12')),lineageOrder=c('genus','kingdom')),matrix(c(NA,NA,'12','Y','C',NA,'Z',NA,NA,'X','A',NA),nrow=3,dimnames=list(c('A','BB','CCC'),c('kingdom','genus','genus.1','species'))))
  expect_equal(normalizeTaxa(list(c('species'='X','clade'='W','genus'='Y'),c('clade'='Z','species'='A','genus'='C'))),matrix(c('Y','C','W',NA,'X','A',NA,'Z'),nrow=2,dimnames=list(c(),c('genus','genus.1','species','species.1'))))
  expect_equal(normalizeTaxa(list(c('species'='X','TEST'='W','genus'='Y'),c('ANOTHER'='Z','species'='A','genus'='C')),cladeRegex='TEST|ANOTHER'),matrix(c('Y','C','W',NA,'X','A',NA,'Z'),nrow=2,dimnames=list(c(),c('genus','genus.1','species','species.1'))))
})

test_that("Test topoSort",{
  expect_equal(topoSort(list(c('A'))),'A')
  expect_equal(topoSort(list(c('XX'='A'))),'A')
  expect_equal(topoSort(list(c('A','B'))),c('A','B'))
  expect_equal(topoSort(list(c('B','A'))),c('B','A'))
  expect_equal(topoSort(list(c('a','b'),c('b','c','e'),c('a','c','d'),c('d','e'))),c('a','b','c','d','e'))
  expect_equal(topoSort(list(c(1:3),c(3:5))),1:5)
  expect_equal(topoSort(list()),c())
  expect_equal(topoSort(list(c(),c())),c())
  expect_error(topoSort(list(c('B','A'),c('A','B'))),'cycle')
  expect_error(topoSort(list(c('A','B'),c('B','C'),c('C','A'))),'cycle')
  expect_true(list(topoSort(list(c('A','C'),c('B','C')))) %in% list(c('A','B','C'),c('B','A','C')))
  expect_true(list(topoSort(list(c('B','C'),c('A','C')))) %in% list(c('A','B','C'),c('B','A','C')))
  expect_error(topoSort(list(c('A','C'),c('B','C')),errorIfAmbiguous=TRUE),'[Aa]mbiguous')
  expect_error(topoSort(list(c('A','D'),c('B','C','D')),errorIfAmbiguous=TRUE),'[Aa]mbiguous')
  expect_error(topoSort(list(replicate(10000,paste(sample(LETTERS,5),collapse=''))),maxIter=1000),'[iI]teration')
  expect_error(topoSort(list(c('A','B')),maxIter=0),'[iI]teration')
  expect_error(topoSort(list(c('A','B')),maxIter=1),'[iI]teration')
  expect_equal(topoSort(list(c('A','B')),maxIter=2),c('A','B'))
  expect_equal(topoSort(1:10),1:10)
})
