% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/taxa.R
\name{read.accession2taxid}
\alias{read.accession2taxid}
\title{Read NCBI accession2taxid files}
\usage{
read.accession2taxid(
  taxaFiles,
  sqlFile,
  vocal = TRUE,
  extraSqlCommand = "",
  indexTaxa = FALSE,
  overwrite = FALSE
)
}
\arguments{
\item{taxaFiles}{a string or vector of strings giving the path(s) to files to be read in}

\item{sqlFile}{a string giving the path where the output SQLite file should be saved}

\item{vocal}{if TRUE output status messages}

\item{extraSqlCommand}{for advanced use. A string giving a command to be called on the SQLite database before loading data. A couple potential uses: 
\itemize{\item "PRAGMA temp_store_directory = '/MY/TMP/DIR'" to store SQLite temporary files in directory /MY/TMP/DIR. Useful if the temporary directory used by SQLite (which is not necessarily in the same location as R's) is small on your system \item "pragma temp_store = 2;" to keep all SQLite temp files in memory. Don't do this unless you have a lot (>100 Gb) of RAM}}

\item{indexTaxa}{if TRUE add an index for taxa ID. This would only be necessary if you want to look up accessions by taxa ID e.g. \code{\link{getAccessions}}}

\item{overwrite}{If TRUE, delete accessionTaxa table in database if present and regenerate}
}
\value{
TRUE if sucessful
}
\description{
Take NCBI accession2taxid files, keep only accession and taxa and save it as a SQLite database
}
\examples{
taxa<-c(
  "accession\taccession.version\ttaxid\tgi",
  "Z17427\tZ17427.1\t3702\t16569",
  "Z17428\tZ17428.1\t3702\t16570",
  "Z17429\tZ17429.1\t3702\t16571",
  "Z17430\tZ17430.1\t3702\t16572"
)
inFile<-tempfile()
sqlFile<-tempfile()
writeLines(taxa,inFile)
read.accession2taxid(inFile,sqlFile,vocal=FALSE)
db<-RSQLite::dbConnect(RSQLite::SQLite(),dbname=sqlFile)
RSQLite::dbGetQuery(db,'SELECT * FROM accessionTaxa')
RSQLite::dbDisconnect(db)
}
\references{
\url{https://ftp.ncbi.nih.gov/pub/taxonomy/accession2taxid/}
}
\seealso{
\code{\link{read.nodes.sql}}, \code{\link{read.names.sql}}
}
