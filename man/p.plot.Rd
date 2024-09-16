\name{p.plot}
\alias{p.plot}

\title{
Plot p values of the contrasts
}
\description{
Plot p values of the contrasts
}
\usage{
p.plot(data, ylab="", xlab="", col.lines="red", cex.axis=0.7, 
cex=0.9 , col.text="dark green",family="Times", bg="white",...)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
output object of ea1 or ea2 function (see examples)
}
\item{ylab}{
title of y-axis
}

\item{xlab}{
title of x-axis
}

\item{col.lines}{
colours of the lines
}

\item{cex.axis}{
font size in axis
}
 
\item{cex}{
size of points
}

\item{col.text}{
colours in letters and means
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
Plot p values of the contrasts
}

\references{
KAPS, M. and LAMBERSON, W. R. Biostatistics for Animal Science: an introductory text. 2nd Edition. CABI Publishing, Wallingford, Oxfordshire, UK, 2009. 504p.
}
\author{
Emmanuel Arnhold <emmanuelarnhold@yahoo.com.br>
}


\seealso{
ea1,ea2, box.plot, means.plot, means.plotfat, ic, ic.plot, m.plot
}

\examples{

#data3: Kaps and Lamberson (2009): page 347
#Description:
##Latin square design for test four different treatments on hay intake of fattening steers

data(data3)

r<-ea1(data3, design=3)


#plot
contrasts=r[[3]]
contrasts
p.plot(contrasts)

#direct
p.plot(r[3])

# more graphical parameters
p.plot(contrasts, bg="cornsilk", cex=1.5,cex.axis=1.5,
main="P-values of the tukey contrasts",family="sans");grid(10)

data('data7')
r<-ea2(data7,4)
p.plot(r[[5]], bg="cornsilk");grid(10)

par(mfrow=c(1,2))
p.plot(r[[9]][1], xlab="treatment 1", cex=0.5, bg="cornsilk");grid(10)
p.plot(r[[9]][2], xlab="treatment 2", cex=0.5, bg="cornsilk");grid(10)
}

