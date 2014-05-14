\name{qmethod-package}
\alias{qmethod-package}
\alias{qmethod-package}
\docType{package}
\title{Package for Q methodology analysis}
\description{Overview of the \code{qmethod} package.
This package performs the analysis of Q methodology data using principal components analysis, varimax rotation (replaceable by other rotations allowed in \code{\link[psych]{principal}} from \pkg{psych}, and automatic flagging.

The following steps of the analysis correspond to separate functions: factor loadings for Q-sorts, automatic flagging of Q-sorts (manual flagging is optional), z-scores and rounded scores for statements, distinguishing and consensus statements, and general characteristics of the factors. The function \code{\link{qmethod}} wraps all.

The package includes a function to import data from PQMethod software, to export plain text data for interpretation in two flavours, and to plot Q method results. Use \code{help(package="qmethod")} for a list of all functions.}
\author{
  Aiora Zabala\cr
  \url{http://people.ds.cam.ac.uk/az296/}

Maintainer: Aiora Zabala \email{aiora.zabala@gmail.com}
}
\references{Brown, S. R., 1980 \emph{Political subjectivity: Applications of Q methodology in political science}, New Haven, CT: Yale University Press. Available from: \url{http://qmethod.org/papers/Brown-1980-PoliticalSubjectivity.pdf}.

Van Exel, J., and G. de Graaf, 2005. \emph{Q Methodology: A Sneak Preview} Available from: \url{http://qmethod.org/articles/vanExel.pdf}.

Watts, S., and and P. Stenner, 2012. \emph{Doing Q Methodological Research: Theory, Method & Interpretation}, London: Sage Publications Ltd.

\url{http://qmethod.org/}

\url{http://schmolck.userweb.mwn.de/qmethod/}
}
\examples{
data(lipset)
results <- qmethod(lipset, nfactors=3, nstat=33, 
           nqsorts=9, rotation="varimax")
summary(results)
results
}