\name{dcbb2}
\alias{dcbb2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ BB2 copula density function  }
\description{
  Calculate the value of the BB2 density.
}
\usage{
dcbb2(theta, delta, u, v)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{theta}{ Parameter \code{theta} of the BB2, (0<\code{theta}).  }
  \item{delta}{ Parameter \code{delta} of the BB2, (0<\code{delta}). }
  \item{u}{ First coordenate where de density will be evaluated. (0<\code{u}<1)}
  \item{v}{ Second coordenate where de density will be evaluated. (0<\code{v}<1)}
}

}
\value{
  value of de density BB2 for the parameters  \code{theta}  and  \code{delta} on ( \code{u} ,  \code{v} ) 
}
\references{ Joe, H., (1997). Multivariate Models and Dependence Concepts. 
Monogra. Stat. Appl. Probab. 73, London: Chapman and Hall.}
\author{ Jesus Garcia, IMECC-UNICAMP and 
Veronica Gonzalez-Lopez, IMECC-UNICAMP
 }

\examples{
res<-dcbb2(0.5,0.5,0.75,0.6)


## The function is currently defined as
function(theta,delta,u,v)
{k<-exp(theta*(u^(-delta)-1));
t<-exp(theta*(v^(-delta)-1));
S<-k+t-1;
h<-(1/theta)*log(S);
duk<--k*(theta*delta)*u^(-delta-1);
dvt<--t*(theta*delta)*v^(-delta-1);
duS<-duk;
dvS<-dvt;
duh<-(1/theta)*duS/S;
dvh<-(1/theta)*dvS/S;
dvuh<-(-1/theta)*duS*dvS/S^2;
densi<-(1/delta)*(1+1/delta)*(1+h)^(-2-1/delta)*dvh*duh-(1/delta)*(1+h)^(-1-1/delta)*dvuh
	}
}
\keyword{ misc }
