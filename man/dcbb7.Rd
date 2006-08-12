\name{dcbb7}
\alias{dcbb7}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ BB7 copula density function  }
\description{
  Calculate the value of the BB7 density.
}
\usage{
dcbb7(theta, delta, u, v)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{theta}{ Parameter \code{theta} of the BB7, (1<\code{theta}).  }
  \item{delta}{ Parameter \code{delta} of the BB7, (0<\code{delta}). }
  \item{u}{ First coordenate where de density will be evaluated. (0<\code{u}<1)}
  \item{v}{ Second coordenate where de density will be evaluated. (0<\code{v}<1)}
}

}
\value{
  value of de density BB7 for the parameters  \code{theta}  and  \code{delta} on ( \code{u} ,  \code{v} ) 
}
\references{ Joe, H., (1997). Multivariate Models and Dependence Concepts. 
Monogra. Stat. Appl. Probab. 73, London: Chapman and Hall.}
\author{ Jesus Garcia, IMECC-UNICAMP and 
Veronica Gonzalez-Lopez, IMECC-UNICAMP
 }

\examples{
res<-dcbb7(1.5,1.5,0.75,0.6)



## The function is currently defined as
function(theta,delta,u,v)
{WU<-(1-(1-u)^(theta))^(-delta);
	WV<-(1-(1-v)^(theta))^(-delta);
	duWU<-(-delta*theta)*(1-(1-u)^(theta))^(-delta-1)*(1-u)^(theta-1);
	dvWV<-(-delta*theta)*(1-(1-v)^(theta))^(-delta-1)*(1-v)^(theta-1);
	K<-WU+WV;
	duK<-duWU;
	dvK<-dvWV;
	S<-K-1;
	duS<-duK;
	dvS<-dvK;
	h<-1-S^(-1/delta);
	duh<-(1/delta)*S^(-1/delta-1)*duS;
	dvh<-(1/delta)*S^(-1/delta-1)*dvS;
	dvuh<-(1/delta)*(-1/delta-1)*(S)^(-1/delta-2)*(dvS)*(duS);
	densi<-(-1/theta)*(1/theta-1)*h^(1/theta-2)*dvh*duh-(1/theta)*h^(1/theta-1)*dvuh
	}
}
\keyword{ misc }
