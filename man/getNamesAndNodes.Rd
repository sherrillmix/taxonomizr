% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/taxa.R
\name{getNamesAndNodes}
\alias{getNamesAndNodes}
\title{Download names and nodes files from NCBI}
\usage{
getNamesAndNodes(
  outDir = ".",
  url = sprintf("\%s://ftp.ncbi.nih.gov/pub/taxonomy/taxdump.tar.gz", protocol),
  fileNames = c("names.dmp", "nodes.dmp"),
  protocol = "ftp",
  resume = TRUE
)
}
\arguments{
\item{outDir}{the directory to put names.dmp and nodes.dmp in}

\item{url}{the url where taxdump.tar.gz is located}

\item{fileNames}{the filenames desired from the tar.gz file}

\item{protocol}{the protocol to be used for downloading. Probably either \code{'http'} or \code{'ftp'}. Overridden if \code{url} is provided directly}

\item{resume}{if TRUE attempt to resume downloading an interrupted file without starting over from the beginning}
}
\value{
a vector of file path strings of the locations of the output files
}
\description{
Download a taxdump.tar.gz file from NCBI servers and extract the names.dmp and nodes.dmp files from it. These can then be used to create a SQLite database with \code{\link{read.names.sql}} and \code{\link{read.nodes.sql}}. Note that if the files already exist in the target directory then this function will not redownload them. Delete the files if a fresh download is desired.
}
\examples{
\dontrun{
  getNamesAndNodes()
}
}
\references{
\url{https://ftp.ncbi.nih.gov/pub/taxonomy/}, \url{https://www.ncbi.nlm.nih.gov/Taxonomy/taxonomyhome.html/}
}
\seealso{
\code{\link{read.nodes.sql}}, \code{\link{read.names.sql}}
}
