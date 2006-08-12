\name{dcbb5}
\alias{dcbb5}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ BB5 copula density function  }
\description{
  Calculate the value of the BB5 density.
}
\usage{
dcbb5(theta, delta, u, v)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{theta}{ Parameter \code{theta} of the BB5, (1<\code{theta}).  }
  \item{delta}{ Parameter \code{delta} of the BB5, (0<\code{delta}). }
  \item{u}{ First coordenate where de density will be evaluated. (0<\code{u}<1)}
  \item{v}{ Second coordenate where de density will be evaluated. (0<\code{v}<1)}
}

}
\value{
  value of de density BB5 for the parameters  \code{theta}  and  \code{delta} on ( \code{u} ,  \code{v} ) 
}
\references{ Joe, H., (1997). Multivariate Models and Dependence Concepts. 
Monogra. Stat. Appl. Probab. 73, London: Chapman and Hall.}
\author{ Jesus Garcia, IMECC-UNICAMP and 
Veronica Gonzalez-Lopez, IMECC-UNICAMP
 }

\examples{
res<-dcbb5(1.5,1.5,0.75,0.6)


## The function is currently defined as
function(theta,delta,u,v)
{t<-(-log(u))^(-theta*delta)+(-log(v))^(-theta*delta);
	dut<-(theta*delta/u)*(-log(u))^(-theta*delta-1);
	dvt<-(theta*delta/v)*(-log(v))^(-theta*delta-1);
	S<-(-log(u))^(theta)+(-log(v))^(theta);
	duS<-(-theta/u)*(-log(u))^(theta-1);
	dvS<-(-theta/v)*(-log(v))^(theta-1);
	h<-S-t^(-1/delta);
	duh<-duS+(1/delta)*(t)^(-1/delta-1)*(dut);
	dvh<-dvS+(1/delta)*(t)^(-1/delta-1)*(dvt);
	dvuh<--1/delta*(1/delta+1)*t^(-1/delta-2)*dut*dvt;
	densi<-exp(-h^(1/theta))*(1/theta)^2*(h^(1/theta-1))^2*dvh*duh+exp(-h^(1/theta))*(-1/theta)*(1/theta-1)*h^(1/theta-2)*dvh*duh+exp(-h^(1/theta))*(-1/theta)*h^(1/theta-1)*dvuh
	}
}
\keyword{ misc }
