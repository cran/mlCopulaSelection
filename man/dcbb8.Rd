\name{dcbb8}
\alias{dcbb8}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ BB8 copula density function  }
\description{
  Calculate the value of the BB8 density.
}
\usage{
dcbb8(theta, delta, u, v)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{theta}{ Parameter \code{theta} of the BB8, (1<\code{theta}).  }
  \item{delta}{ Parameter \code{delta} of the BB8, (0<\code{delta}<1). }
  \item{u}{ First coordenate where de density will be evaluated. (0<\code{u}<1)}
  \item{v}{ Second coordenate where de density will be evaluated. (0<\code{v}<1)}
}

}
\value{
  value of de density BB8 for the parameters  \code{theta}  and  \code{delta} on ( \code{u} ,  \code{v} ) 
}
\references{ Joe, H., (1997). Multivariate Models and Dependence Concepts. 
Monogra. Stat. Appl. Probab. 73, London: Chapman and Hall.}
\author{ Jesus Garcia, IMECC-UNICAMP and 
Veronica Gonzalez-Lopez, IMECC-UNICAMP
 }

\examples{
res<-dcbb8(1.5,0.5,0.75,0.6)


## The function is currently defined as
function(theta,delta,u,v)
{
S<-1-(1-delta*u)^(theta);
T<-1-(1-delta*v)^(theta);
K<-(1-(1-delta)^(theta))^(-1);
W<-1-K*S*T;
DuS<-theta*delta*(1-delta*u)^(theta-1);
DuW<--K*T*DuS;
DvT<-theta*delta*(1-delta*v)^(theta-1);
DvW<--K*S*DvT;
DvuW<--K*DuS*DvT;
densi<--delta^(-1)*(1/theta)*(1/theta-1)*W^(1/theta-2)*DvW*DuW-delta^(-1)*(1/theta)*W^(1/theta-1)*DvuW
  }
}
\keyword{ misc }
