\name{mlCopulaSelection-package}
\alias{mlCopulaSelection-package}
\alias{mlCopulaSelection}
\docType{package}
\title{
 Copula selection and fitting using maximum likelihood 
}
\description{
 Use numerical maximum likelihood to choose and fit a bivariate copula model (from a library of 40 models) to the data. The copula models in the library correspond to BB1, BB2,...,BB10 from Joe, H., (1997) and its 90, 180 and 270 degree rotations.
}
\details{
\tabular{ll}{
Package: \tab mlCopulaSelection\cr
Type: \tab Package\cr
Version: \tab 1.3\cr
Date: \tab 2006-08-12\cr
License: \tab GPL version 2. \cr
}

}
\author{
Jesus Garcia, IMECC-UNICAMP and 
Veronica Gonzalez-Lopez, IMECC-UNICAMP

Maintainer: Jesus Garcia <jg@ime.unicamp.br>

}
\references{  Joe, H., (1997). Multivariate Models and Dependence Concepts. 
Monogra. Stat. Appl. Probab. 73, London: Chapman and Hall.
}

\keyword{documentation}

\examples{

# The Data (the margins are uniform)
U <- c( 0.43, 0.1, 0.2, 0.33, 0.24, 0.29, 0.14, 0.4, 0.39, 0.8, 0.63, 0.16, 0.24, 0.14,0.71, 0.39, 0.48, 0.29, 0.38, 0.37)
V <- c(0.01, 0.26, 0.2, 0.36, 0.34, 0.43, 0.27, 0.61, 0.08, 0.25, 0.72, 0.15, 0.14, 0.12, 0.74, 0.18, 0.58, 0.15, 0.34, 0.13)
# find the maximun likelihood estimates
res<-mlcbbsel(U,V)
#the best fitting copula model:
res$copmax
#the parameters for the best fitting copula model:
res$parmax
#the log-likelihood of the best fitting copula model with those parameters:
res$llmax

}

