\name{qfcharact}
\alias{qfcharact}
\title{Q methodology: factor characteristics}
\description{Calculates the general factor characteristics: number of flagged Q-sorts, composite reliability, standard errors of factor scores, and comparisons between factors.}
\usage{qfcharact(loa, flagged, nqsorts, zsc, nfactors, floa, av_rel_coef=0.800)}

\arguments{
  \item{loa}{a matrix or a data frame containing raw data, with statements as rows, Q-sorts as columns, and the scores of the columns in the distribution in each cell.}
  \item{nfactors}{number of factors to extract using PCA.}
  \item{flagged}{a data frame of type logical, indicating which Q-sorts are flagged for each factor. Provided manually or automatically using \code{\link{qflag}}.}
  \item{nqsorts}{number of Q-sorts in the study.}
  \item{zsc}{a data frame with the z-scores for statements, calculated using \code{\link{qzscores}}.}
  \item{floa}{a data frame with the factor loadings, calculated using PCA or Centroid factor analysis.}
  \item{av_rel_coef}{average reliability coefficient (the individual variability of a respondent), estimated by default as 0.8.}
}

\details{The default \code{\link{qmethod}} does automatic flagging and uses varimax rotation. The latter can be replaced by other methods for rotation allowed in \code{\link[psych]{principal}}.

If input data contains row names and variable names, these will be kept throughout the analysis. Input data is validated, and it will give an error either if there are non numerical values or if the number of statements and Q sorts introduced does not match with the input data.

This function works with any number of factors rotated, of statements, and of Q-sorts. The function returns the following factor characteristics:
\enumerate{
    \item{Average reliability coefficient.}
    \item{Number of loading Q-sorts.}
    \item{Eigenvalues.}
    \item{Percentage of explained variance.}
    \item{Composite reliability.}
    \item{Standard error of factor scores.}
    \item{Correlation coefficients between factors z-scores.}
    \item{Standard errors of differences.}
    }
}
\references{Brown, S. R., 1980 \emph{Political subjectivity: Applications of Q methodology in political science}, New Haven, CT: Yale University Press.

See further references on the methodology in \code{\link{qmethod-package}}.}

\note{This is a function used within \code{\link{qmethod}}. Rarely to be used independently.}

\author{Aiora Zabala}