\name{dcbb3}
\alias{dcbb3}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ BB3 copula density function  }
\description{
  Calculate the value of the BB3 density.
}
\usage{
dcbb3(theta, delta, u, v)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{theta}{ Parameter \code{theta} of the BB3, (0<\code{theta}).  }
  \item{delta}{ Parameter \code{delta} of the BB3, (1<\code{delta}). }
  \item{u}{ First coordenate where de density will be evaluated. (0<\code{u}<1)}
  \item{v}{ Second coordenate where de density will be evaluated. (0<\code{v}<1)}
}

}
\value{
  value of de density BB3 for the parameters  \code{theta}  and  \code{delta} on ( \code{u} ,  \code{v} ) 
}
\references{ Joe, H., (1997). Multivariate Models and Dependence Concepts. 
Monogra. Stat. Appl. Probab. 73, London: Chapman and Hall.}
\author{ Jesus Garcia, IMECC-UNICAMP and 
Veronica Gonzalez-Lopez, IMECC-UNICAMP
 }

\examples{
res<-dcbb3(1.5,1.5,0.75,0.6)


## The function is currently defined as
function(theta,delta,u,v){W<-exp(theta*(-log(u))^(delta));
P<-exp(theta*(-log(v))^(delta));
S<-W+P-1;
h<-(1/theta)*log(S);
F<-exp(-h^(1/delta));
duS<-exp((-log(u))^(delta)*theta)*(-theta*delta/u)*(-log(u))^(delta-1);
dvS<-exp((-log(v))^(delta)*theta)*(-theta*delta/v)*(-log(v))^(delta-1);
dvuh<-(-1/theta)/S^2*duS*dvS;
duh<-(1/theta)*duS/S;
dvh<-(1/theta)*dvS/S;
densi<-F*(1/delta^2)*(h^(1/delta-1))^2*dvh*duh+F*(-1/delta)*(1/delta-1)*(h)^(1/delta-2)*(dvh)*(duh)+F*(-1/delta)*(h)^(1/delta-1)*dvuh
  }
}
\keyword{ misc }

