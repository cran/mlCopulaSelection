\name{dcbb1}
\alias{dcbb1}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ BB1 copula density function  }
\description{
  Calculate the value of the BB1 density.
}
\usage{
dcbb1(theta, delta, u, v)
}

\arguments{
  \item{theta}{ Parameter \code{theta} of the BB1. (0<\code{theta}) }
  \item{delta}{ Parameter \code{delta} of the BB1. (1<\code{delta})}
  \item{u}{ First coordenate where de density will be evaluated. (0<\code{u}<1)}
  \item{v}{ Second coordenate where de density will be evaluated. (0<\code{v}<1)}
}

}
\value{
  value of de density BB1 for the parameters  \code{theta}  and  \code{delta} on ( \code{u} ,  \code{v} ) 
}
\references{ Joe, H., (1997). Multivariate Models and Dependence Concepts. 
Monogra. Stat. Appl. Probab. 73, London: Chapman and Hall.}
\author{ Jesus Garcia, IMECC-UNICAMP and 
Veronica Gonzalez-Lopez, IMECC-UNICAMP
 }

\examples{
res<-dcbb1(0.5,1.5,0.90,0.85)


## The function is currently defined as
function(theta,delta,u,v)
{S<-u^(-theta)-1;
T<-v^(-theta)-1;-10**(4)
W<-S^(delta)+T^(delta);
DuS<-(-theta)*u^(-theta-1);
DuW<-delta*S^(delta-1)*DuS;
DvT<-(-theta)*v^(-theta-1);
DvW<-delta*T^(delta-1)*DvT;
densi<-(-1/(theta*delta))*(-1/theta-1)*(1+W^(1/delta))^(-1/theta-2)*(1/delta)*W^(1/delta-1)*DvW*W^(1/delta-1)*DuW-(1/(theta*delta))*(1+W^(1/delta))^(-1/theta-1)*(1/delta-1)*W^(1/delta-2)*DvW*DuW
  }
}
\keyword{ misc }
