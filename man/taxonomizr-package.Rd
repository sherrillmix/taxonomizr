% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/taxa.R
\docType{package}
\name{taxonomizr-package}
\alias{taxonomizr}
\alias{taxonomizr-package}
\title{taxonomizr: Functions to Work with NCBI Accessions and Taxonomy}
\description{
Functions for assigning taxonomy to NCBI accession numbers and taxon IDs based on NCBI's accession2taxid and taxdump files. This package allows the user to download NCBI data dumps and create a local database for fast and local taxonomic assignment.
}
\details{
taxonomizr provides some simple functions to parse NCBI taxonomy files and accession dumps and efficiently use them to assign taxonomy to accession numbers or taxonomic IDs (\url{https://www.ncbi.nlm.nih.gov/Taxonomy/taxonomyhome.html/}). This is useful for example to assign taxonomy to BLAST results. This is all done locally after downloading the appropriate files from NCBI using included functions. The major functions are:
\itemize{
  \item \code{\link{prepareDatabase}}: download data from NCBI and prepare SQLite database
  \item \code{link{accessionToTaxa}}: convert accession numbers to taxonomic IDs
  \item \code{\link{getTaxonomy}}: convert taxonomic IDs to taxonomy
}
More specialized functions are:
\itemize{
 \item \code{\link{getId}}: convert a biological name to taxonomic ID
 \item \code{\link{getAccessions}}: find accessions for a given taxonomic ID
}
}
\examples{
\dontrun{
  if(readline(
    "This will download a lot data and take a while to process.
     Make sure you have space and bandwidth. Type y to continue: "
  )!='y')
    stop('This is a stop to make sure no one downloads a bunch of data unintentionally')

  prepareDatabase('accessionTaxa.sql')
  blastAccessions<-c("Z17430.1","Z17429.1","X62402.1")
  ids<-accessionToTaxa(blastAccessions,'accessionTaxa.sql')
  getTaxonomy(ids,'accessionTaxa.sql')
}
}
\seealso{
\code{\link{prepareDatabase}}, \code{\link{accessionToTaxa}}, \code{\link{getTaxonomy}}
}
\author{
\strong{Maintainer}: Scott Sherrill-Mix \email{ssm@msu.edu}

}
\keyword{internal}
