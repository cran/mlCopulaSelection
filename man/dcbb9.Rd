\name{dcbb9}
\alias{dcbb9}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ BB9 copula density function  }
\description{
  Calculate the value of the BB9 density.
}
\usage{
dcbb9(theta, delta, u, v)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{theta}{ Parameter \code{theta} of the BB9, (1<\code{theta}).  }
  \item{delta}{ Parameter \code{delta} of the BB9, (0<\code{delta}). }
  \item{u}{ First coordenate where de density will be evaluated. (0<\code{u}<1)}
  \item{v}{ Second coordenate where de density will be evaluated. (0<\code{v}<1)}
}

}
\value{
  value of de density BB9 for the parameters  \code{theta}  and  \code{delta} on ( \code{u} ,  \code{v} ) 
}
\references{ Joe, H., (1997). Multivariate Models and Dependence Concepts. 
Monogra. Stat. Appl. Probab. 73, London: Chapman and Hall.}
\author{ Jesus Garcia, IMECC-UNICAMP and 
Veronica Gonzalez-Lopez, IMECC-UNICAMP
 }

\examples{
res<-dcbb9(1.5,1.5,0.75,0.6)


## The function is currently defined as
function(theta,delta,u,v)
{S<-delta-log(u);
T<-delta-log(v);
W<-S^(theta)+T^(theta)-delta^(theta);
C<-exp(-W^(1/theta)+delta);
DuS<--1/u;
DuW<-theta*S^(theta-1)*DuS;
DvT<--1/v;
DvW<-theta*T^(theta-1)*DvT;
densi<-C*(1/theta^2)*(W^(1/theta-1))^2*DvW*DuW+C*(-1/theta)*(1/theta-1)*W^(1/theta-2)*DvW*DuW
  }
}
\keyword{ misc }
