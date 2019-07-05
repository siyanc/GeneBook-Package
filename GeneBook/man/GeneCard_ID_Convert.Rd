\name{GeneCard_ID_Convert}
\alias{GeneCard_ID_Convert}
\title{
GENE ID CONVERT
}
\description{
Function for transferring a single gene or multiple genes to the consistent gene symbols.
}
\usage{
GeneCard_ID_Convert(x)
}
\arguments{
  \item{x}{ An R string. Gene symbol or aliases for the gene
}
}
\details{
The gene conversion is based on the GeneCards Human Gene Database
}
\value{
Value return is a matrix including Previous ID, Symbol and Label. The label tells how accuracy the conversion is
}
\examples{
##  One Gene ID Convert
x = "AD2"
GeneCard_ID_Convert(x)
}


