\name{llbb5}
\alias{llbb5}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ BB5's log-likelihood function}
\description{
 Calculate the log-likelihood for the BB5 density.}
\usage{
llbb5(param, u, v)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{param}{ bidimensional vector with parameters c(\code{theta},\code{delta}) (1<\code{theta} and 0<\code{delta}) }
  \item{u}{ vector with the first coordenate of the bivariate data }
  \item{v}{ vector with the second coordenate of the bivariate data (same size as\code{u})  }
}
\details{
  (u,v) margins must have Uniform(0,1) marginal distribution 
}
\value{
 BB5's log-likelihood function for the sample
}
\references{ Joe, H., (1997). Multivariate Models and Dependence Concepts. 
Monogra. Stat. Appl. Probab. 73, London: Chapman and Hall.}
\author{ Jesus Garcia, IMECC-UNICAMP and 
Veronica Gonzalez-Lopez, IMECC-UNICAMP
 }

\examples{
# The data:
u <- c( 0.43, 0.1, 0.2, 0.33, 0.24, 0.29, 0.14, 0.4, 0.39, 0.8, 0.63, 0.16, 0.24, 0.14,0.71, 0.39, 0.48, 0.29, 0.38, 0.37)
v <- c(0.01, 0.26, 0.2, 0.36, 0.34, 0.43, 0.27, 0.61, 0.08, 0.25, 0.72, 0.15, 0.14, 0.12, 0.74, 0.18, 0.58, 0.15, 0.34, 0.13)
# The log-likelihood 
r<-llbb5(c(1.5,1.5),u,v)

## The function is currently defined as
function(param,u,v)
{
n<-sum(u>=-1)
s<-0.
for(i in 1:n) { s<-s+log(dcbb5(param[1],param[2],u[i],v[i]));if(is.nan(s)) {break}; }
if(is.finite(s)) {res<-s} else {res<- -10**(64)}
  }
}
\keyword{ misc }
