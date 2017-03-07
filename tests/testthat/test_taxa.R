context("taxa functions")
test_that("Test read.names",{
  names<-c(
    "1\t|\tall\t|\t\t|\tsynonym\t|",
    "1\t|\troot\t|\t\t|\tscientific name\t|",
    "2\t|\tBacteria\t|\tBacteria <prokaryotes>\t|\tscientific name\t|",
    "2\t|\tMonera\t|\tMonera <Bacteria>\t|\tin-part\t|",
    "2\t|\tProcaryotae\t|\tProcaryotae <Bacteria>\t|\tin-part\t|"
  )
  out<-data.table('id'=1:2,'name'=c('root','Bacteria'),key='id')
  setindex(out,'name')
  expect_equal(read.names(textConnection(names)),out)
  out<-data.table('id'=rep(1:2,2:3),'name'=c('all','root','Bacteria','Monera','Procaryotae'),key='id')
  setindex(out,'name')
  expect_equal(read.names(textConnection(names),FALSE),out)
})

test_that("Test read.nodes",{
  nodes<-c(
    "1\t|\t1\t|\tno rank\t|\t\t|\t8\t|\t0\t|\t1\t|\t0\t|\t0\t|\t0\t|\t0\t|\t0\t|\t\t|",
    "2\t|\t131567\t|\tsuperkingdom\t|\t\t|\t0\t|\t0\t|\t11\t|\t0\t|\t0\t|\t0\t|\t0\t|\t0\t|\t\t|",
    "6\t|\t335928\t|\tgenus\t|\t\t|\t0\t|\t1\t|\t11\t|\t1\t|\t0\t|\t1\t|\t0\t|\t0\t|\t\t|",
    "7\t|\t6\t|\tspecies\t|\tAC\t|\t0\t|\t1\t|\t11\t|\t1\t|\t0\t|\t1\t|\t1\t|\t0\t|\t\t|",
    "9\t|\t32199\t|\tspecies\t|\tBA\t|\t0\t|\t1\t|\t11\t|\t1\t|\t0\t|\t1\t|\t1\t|\t0\t|\t\t|"
  )
  out<-data.table('id'=c(1:2,6:7,9),'rank'=c('no rank','superkingdom','genus','species','species'),'parent'=c(1,131567,335928,6,32199),key='id')
  expect_equal(read.nodes(textConnection(nodes)),out)
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
  expect_error(trimTaxa('NotARealFile','test'),'file')
  expect_error(.C('taxaTrim',c('NotARealFile','test'),PACKAGE='taxonomizr'),'file')
  tmp<-tempfile()
  out<-c(
    'head\t1\t2\t3',
    'a\t2\t3\t4',
    'b\t3\t4\t5',
    'c\t4\t5\t6'
  )
  writeLines(out,tmp)
  tmp2<-tempfile()
  dir.create(tmp2)
  #should error (can't write to directory)
  expect_error(.C('taxaTrim',c(tmp,tmp2),PACKAGE='taxonomizr'),'file')
  tmp2<-tempfile()
  expect_error(trimTaxa(tmp,tmp2),NA)
  expect_equal(readLines(tmp2),c('2\t3','3\t4','4\t5'))
  writeLines(c(out,'1\t2\t3\t4\t5'),tmp)
  expect_error(trimTaxa(tmp,tmp2),"line")
  writeLines(out,gzfile(tmp))
  expect_error(trimTaxa(tmp,tmp2),NA)
  expect_equal(readLines(tmp2),c('2\t3','3\t4','4\t5'))
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
  inFile<-tempfile()
  writeLines(taxa,inFile)
  expect_error(read.accession2taxid(inFile,outFile),NA)
  expect_message(read.accession2taxid(inFile,outFile),'exists')
  db<-RSQLite::dbConnect(RSQLite::SQLite(),dbname=outFile)
  result<-data.frame('accession'=c('Z17427.1','Z17428.1','Z17429.1','Z17430.1'),taxa=3702,stringsAsFactors=FALSE)
  expect_true(file.exists(outFile))
  expect_equal(dbGetQuery(db,'SELECT * FROM accessionTaxa'),result)
  file.remove(outFile)
  expect_error(read.accession2taxid(inFile,outFile,extraSqlCommand='pragma temp_store = 2;'),NA)
  file.remove(outFile)
  if(.Platform$OS.type == "unix"){
    #windows sqlite apparently doesn't catch this error
    expect_error(read.accession2taxid(inFile,outFile,extraSqlCommand='DROP TABLE NOTEXISTXYZ;'),'NOTEXISTXYZ')
    expect_false(file.exists(outFile))
  }
})

test_that("Test getTaxonomy",{
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
  taxaNames<-read.names(textConnection(namesText))
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
  taxaNodes<-read.nodes(textConnection(nodesText))
  desiredTaxa<-c('superkingdom','phylum','class','order','family','genus','species')
  out<-matrix(c(
    "Eukaryota","Chordata","Mammalia","Primates","Hominidae","Homo","Homo sapiens",
    "Eukaryota","Chordata","Mammalia","Primates","Hominidae","Homo",NA
  ),byrow=TRUE,dimnames=list(c('9606','9605'),desiredTaxa),nrow=2)
  expect_equal(getTaxonomy(c(9606,9605),taxaNodes,taxaNames,mc.cores=1,desiredTaxa=desiredTaxa),out)
  expect_equal(getTaxonomy(c(9605,9606,9605),taxaNodes,taxaNames,mc.cores=1,desiredTaxa=desiredTaxa),out[c(2,1,2),])
  expect_equal(getTaxonomy(c(9605,9606,9605),taxaNodes,taxaNames,mc.cores=1,desiredTaxa=desiredTaxa[3:1]),out[c(2,1,2),3:1])
  expect_output(getTaxonomy(9606,taxaNodes,taxaNames,mc.cores=1,debug=TRUE),'\\\\t')
  expect_equal(getTaxonomy(9606,taxaNodes,taxaNames,mc.cores=1,desiredTaxa='NOTREAL'),matrix(as.character(NA),dimnames=list(9606,'NOTREAL')))
  expect_equal(getTaxonomy(9999999,taxaNodes,taxaNames,mc.cores=1,desiredTaxa='class'),matrix(as.character(NA),dimnames=list(9999999,'class')))
  #causes error on Windows
  if(.Platform$OS.type == "unix")expect_equal(getTaxonomy(c(9605,9606,9605),taxaNodes,taxaNames,mc.cores=2,desiredTaxa=desiredTaxa),out[c(2,1,2),])
  expect_equal(getTaxonomy(c(),taxaNodes,taxaNames,mc.cores=1,desiredTaxa=desiredTaxa),NULL)
  naDf<-out
  naDf[,]<-NA
  rownames(naDf)<-c('NA','NA')
  expect_equal(getTaxonomy(c(NA,NA),taxaNodes,taxaNames),naDf)
  suppressWarnings(expect_equal(getTaxonomy(c(NA,9605,NA,'9604,9605'),taxaNodes,taxaNames),rbind('  NA'=naDf[1,],'9605'=out[2,],'  NA'=naDf[1,],'  NA'=naDf[1,])))
  expect_equal(getTaxonomy('9605',taxaNodes,taxaNames),getTaxonomy(9605,taxaNodes,taxaNames))
  expect_warning(getTaxonomy('9605,123',taxaNodes,taxaNames),'coercion')
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
  expect_error(accessionToTaxa("Z17430.1",sqlFile),"exist")
  writeLines(taxa,inFile)
  read.accession2taxid(inFile,sqlFile)
  expect_equal(accessionToTaxa(c("Z17430.1","Z17429.1","X62402.1"),sqlFile),c(3702,3702,9606))
  expect_equal(accessionToTaxa(c(),sqlFile),c())
  expect_equal(accessionToTaxa(c("Z17430.1","NOTREAL","X62402.1","Z17429.1","X62402.1"),sqlFile),c(3702,NA,9606,3702,9606))
  expect_error(accessionToTaxa("Z17430.1","NOTREAL"),"exist")
  expect_equal(accessionToTaxa(c(),sqlFile),c())
})

test_that("Test condenseTaxa",{
  taxas<-matrix(c(
   'a','b','c','e',
   'a','b','d','e'
  ),nrow=2,byrow=TRUE)
  expect_equal(condenseTaxa(taxas),c('a','b',NA,NA))
  expect_equal(condenseTaxa(taxas[c(1,1,1),]),c('a','b','c','e'))
  expect_equal(condenseTaxa(taxas[c(1,1,1,2),]),c('a','b',NA,NA))
  expect_equal(condenseTaxa(taxas[,3,drop=FALSE]),as.character(NA))
  taxas<-matrix(c(
   'a','b',NA,'e',
   'a','b','d','e'
  ),nrow=2,byrow=TRUE)
  expect_equal(condenseTaxa(taxas),c('a','b',NA,NA))
  expect_equal(condenseTaxa(taxas[1,,drop=FALSE]),c('a','b',NA,'e'))
  expect_equal(condenseTaxa(taxas[c(1,1,1),,drop=FALSE]),c('a','b',NA,'e'))
})

test_that("Test getNamesAndNodes",{
  tmp<-tempfile()
  dir.create(tmp)
  expect_error(getNamesAndNodes(tmp,'file://fakeNamesNodes.tar.gz'),NA)
  expect_equal(sort(list.files(tmp,'^(names|nodes).dmp$')),c('names.dmp','nodes.dmp'))
})

test_that("Test getAccession2taxid",{
  tmp<-tempfile()
  dir.create(tmp)
  types<-c('XxXx','XyXyX')
  targets<-sprintf('nucl_%s.accession2taxid.gz',types)
  sapply(targets,function(xx)writeLines('TEST',file.path(tmp,xx)))
  tmp2<-tempfile()
  dir.create(tmp2)
  expect_error(getAccession2taxid(tmp2,baseUrl=sprintf('file://%s',tmp),types=c('nucl_XxXx','nucl_XyXyX')),NA)
  expect_equal(sort(list.files(tmp2,'accession2taxid.gz$')),sort(targets))
})

test_that("Test getId",{
 namesText<-c(
   "1\t|\troot\t|\t\t|\tscientific name\t|",
   "4\t|\tMulti\t|\tBacteria <prokaryotes>\t|\tscientific name\t|",
   "3\t|\tMulti\t|\tBacteria <prokaryotes>\t|\tscientific name\t|",
   "2\t|\tBacteria\t|\tBacteria <prokaryotes>\t|\tscientific name\t|"
 )
 names<-read.names(textConnection(namesText))
 expect_equal(getId('Bacteria',names),'2')
 expect_equal(getId(c('Bacteria','root','Bacteria','NOTREAL'),names),c('2','1','2',NA))
 expect_equal(getId('Not a real name',names),as.character(NA))
 suppressWarnings(expect_equal(getId('Multi',names),'3,4'))
 expect_warning(getId('Multi',names),'Multiple')
})

