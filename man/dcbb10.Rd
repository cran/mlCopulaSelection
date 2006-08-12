\name{dcbb10}
\alias{dcbb10}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ BB10 copula density function  }
\description{
  Calculate the value of the BB10 density.
}
\usage{
dcbb10(theta, delta, u, v)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{theta}{ Parameter \code{theta} of the BB10, (0<\code{theta}<1).  }
  \item{delta}{ Parameter \code{delta} of the BB10, (0<\code{delta}). }
  \item{u}{ First coordenate where de density will be evaluated. (0<\code{u}<1)}
  \item{v}{ Second coordenate where de density will be evaluated. (0<\code{v}<1)}
}

}
\value{
  value of de density BB10 for the parameters  \code{theta}  and  \code{delta} on ( \code{u} ,  \code{v} ) 
}
\references{ Joe, H., (1997). Multivariate Models and Dependence Concepts. 
Monogra. Stat. Appl. Probab. 73, London: Chapman and Hall.}
\author{ Jesus Garcia, IMECC-UNICAMP and 
Veronica Gonzalez-Lopez, IMECC-UNICAMP
 }

\examples{
res<-dcbb10(1.5,1.5,0.75,0.6)



## The function is currently defined as
function(theta,delta,u,v)
{S<-1-u^(1/delta);
T<-1-v^(1/delta);
W<-theta*S*T;
C<-u*v*(1-W)^(-delta);
DuS<-(-1/delta)*u^(1/delta-1);
DuW<-theta*T*DuS;
DvT<-(-1/delta)*v^(1/delta-1);
DvW<-theta*S*DvT;
DvuW<-theta*DuS*DvT;
densi<-(1-W)^(-delta)+v*(-delta)*(1-W)^(-delta-1)*(-1)*DvW+u*delta*(1-W)^(-delta-1)*DuW+u*v*delta*(-delta-1)*(1-W)^(-delta-2)*(-1)*DvW*DuW+u*v*delta*(1-W)^(-delta-1)*DvuW
  }
}
\keyword{ misc }
