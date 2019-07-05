\name{GeneCard_Symbol_Details}
\alias{GeneCard_Symbol_Details}
\title{
GET DETAILS OF GENE SYMBOLS
}
\description{
Function to get the detailed information of gene
}
\usage{
GeneCard_Symbol_Details(x)
}
\arguments{
  \item{x}{R string of converted gene by function GeneCard_ID_Convert
}
}
\details{
This function works only for the genes with consistent symbols. Please use function GeneCard_ID_Convert first and then apply function GeneCard_Symbol_Details to get the detailed information of the gene.
}
\examples{
## One Gene Details
x = "TRBV14"
GeneCard_Symbol_Details(x)
}
