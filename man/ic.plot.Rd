\name{ic.plot}
\alias{ic.plot}

\title{
Plot confidence intervals of contrasts
}
\description{
Plot confidence intervals of contrasts
}
\usage{
ic.plot(data,col="dark green", cex=0.5, xlab="constrats",
pch=19,family="Times", bg="white",...)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
output object of ic (see examples)
}
 \item{col}{
colours of lines
}

\item{cex}{
size of points
}

\item{xlab}{
title of x-axis
}
\item{pch}{
type of points
}
\item{family}{
font of plot
}
 \item{bg}{
background color
}

\item{...}{
more plot parameters
}

}


\value{
Plot confidence intervals of contrasts
}

\references{
KAPS, M. and LAMBERSON, W. R. Biostatistics for Animal Science: an introductory text. 2nd Edition. CABI Publishing, Wallingford, Oxfordshire, UK, 2009. 504p.
}
\author{
Emmanuel Arnhold <emmanuelarnhold@yahoo.com.br>
}


\seealso{
ea1,ea2, box.plot, means.plot, means.plotfat, ic.plot, p.plot
}

\examples{

#data3: Kaps and Lamberson (2009): page 347
#Description:
##Latin square design for test four different treatments on hay intake of fattening steers

data(data3)

r<-ea1(data3, design=3)


#plot
#means
means=r[[2]]
means
ic(means, test=1, df=6) # tukey

#intervals
conf=ic(means, test=1, df=6)

#plot intervals
ic.plot(conf)

#more plot parameters
ic.plot(conf, las=2, bg="cornsilk");grid(10)



}

