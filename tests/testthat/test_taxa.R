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
  expect_equal(read.names(textConnection(names)),out)
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


test_that("Test read.accession2taxid",{
  taxa<-c(
    "accession\taccession.version\ttaxid\tgi",
    "Z17427\tZ17427.1\t3702\t16569",
    "Z17428\tZ17428.1\t3702\t16570",
    "Z17429\tZ17429.1\t3702\t16571",
    "Z17430\tZ17430.1\t3702\t16572"
  )
  temp<-tempfile()
  read.accession2taxid(list(textConnection(taxa)),temp)
  db<-RSQLite::dbConnect(RSQLite::SQLite(),dbname=temp)
  out<-data.frame('accession'=c('Z17427.1','Z17428.1','Z17429.1','Z17430.1'),taxa=3702,stringsAsFactors=FALSE)
  expect_true(file.exists(temp))
  expect_equal(dbGetQuery(db,'SELECT * FROM accessionTaxa'),out)
})
