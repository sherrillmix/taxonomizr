% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/taxa.R
\name{normalizeTaxa}
\alias{normalizeTaxa}
\title{Bring multiple raw taxonomies into alignment}
\usage{
normalizeTaxa(
  rawTaxa,
  cladeRegex = "^clade$|^clade\\\\.[0-9]+$|^$|no rank",
  rootFill = "_ROOT_",
  lineageOrder = c()
)
}
\arguments{
\item{rawTaxa}{A list of vectors with each vector containing a named character vector with entries specifying taxonomy for a clade and names giving the corresponding taxonomic levels e.g. the output from \code{\link{getRawTaxonomy}}}

\item{cladeRegex}{A regex to identify ambiguous taxonomic levels. In the case of NCBI taxonomy, these unidentified levels are all labelled "clade" and \code{\link{getRawTaxonomy}} may attach a unique digit attach to the end for uniqueness.}

\item{rootFill}{If a clade is upstream of the highest taxonomic level then it will be labeled with this prefix}

\item{lineageOrder}{A vector giving an ordering for lineages from most specific to most generic. This should be unnecessary unless the taxonomy contains ambiguities e.g. one taxa goes from species to kingdom while another goes from genus to kingdom leaving it ambiguous whether genus or species is more specific}
}
\value{
a matrix with a row for each taxa and a column for each taxonomic level
}
\description{
Combine the raw taxonomy of several taxa into a single matrix where each row corresponds to a taxa and each column a taxonomic level. Named taxonomic levels are aligned between taxa then any unspecified clades are combined between the named levels. Taxonomic levels between named levels are arbitrarily combined from most generic to most specific. Working from the data provided in the NCBI taxonomy results in ambiguities so results should be used with care.
}
\examples{
rawTaxa<-list(
   '81907' = c(species = "Alectura lathami", genus = "Alectura",
     family = "Megapodiidae", order = "Galliformes", superorder = "Galloanserae",
     infraclass = "Neognathae", class = "Aves", clade = "Coelurosauria",
     clade.1 = "Theropoda", clade.2 = "Saurischia", clade.3 = "Dinosauria",
     clade.4 = "Archosauria", clade.5 = "Archelosauria", clade.6 = "Sauria",
     clade.7 = "Sauropsida", clade.8 = "Amniota", clade.9 = "Tetrapoda",
     clade.10 = "Dipnotetrapodomorpha", superclass = "Sarcopterygii",
     clade.11 = "Euteleostomi", clade.12 = "Teleostomi", clade.13 = "Gnathostomata",
     clade.14 = "Vertebrata", subphylum = "Craniata", phylum = "Chordata",
     clade.15 = "Deuterostomia", clade.16 = "Bilateria", clade.17 = "Eumetazoa",
     kingdom = "Metazoa", clade.18 = "Opisthokonta", domain = "Eukaryota",
     'no rank' = "cellular organisms"),
   '8496' = c(species = "Alligator mississippiensis",
     genus = "Alligator", subfamily = "Alligatorinae", family = "Alligatoridae",
     order = "Crocodylia", clade = "Archosauria", clade.1 = "Archelosauria",
     clade.2 = "Sauria", clade.3 = "Sauropsida", clade.4 = "Amniota",
     clade.5 = "Tetrapoda", clade.6 = "Dipnotetrapodomorpha", superclass = "Sarcopterygii",
     clade.7 = "Euteleostomi", clade.8 = "Teleostomi", clade.9 = "Gnathostomata",
     clade.10 = "Vertebrata", subphylum = "Craniata", phylum = "Chordata",
     clade.11 = "Deuterostomia", clade.12 = "Bilateria", clade.13 = "Eumetazoa",
     kingdom = "Metazoa", clade.14 = "Opisthokonta", domain = "Eukaryota",
     'no rank' = "cellular organisms"),
   '38654' = c(species = "Alligator sinensis",
     genus = "Alligator", subfamily = "Alligatorinae", family = "Alligatoridae",
     order = "Crocodylia", clade = "Archosauria", clade.1 = "Archelosauria",
     clade.2 = "Sauria", clade.3 = "Sauropsida", clade.4 = "Amniota",
     clade.5 = "Tetrapoda", clade.6 = "Dipnotetrapodomorpha", superclass = "Sarcopterygii",
     clade.7 = "Euteleostomi", clade.8 = "Teleostomi", clade.9 = "Gnathostomata",
     clade.10 = "Vertebrata", subphylum = "Craniata", phylum = "Chordata",
     clade.11 = "Deuterostomia", clade.12 = "Bilateria", clade.13 = "Eumetazoa",
     kingdom = "Metazoa", clade.14 = "Opisthokonta", domain = "Eukaryota",
     'no rank' = "cellular organisms")
)
normalizeTaxa(rawTaxa)
}
\seealso{
\code{\link{getRawTaxonomy}}
}
