\name{m.plot}
\alias{m.plot}

\title{
Plot Means
}
\description{
Plot contrasts of means
}
\usage{
m.plot(data,  s="sd",test="tukey", family="Times", bg="white",  cex.text=0.7, 
cex=0.5,bar.order=2, decreasing=TRUE, xlab="treatments", ylab="",pch=19, ...)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
output object of ea1 or ea2 function (see examples)
}

  \item{s}{
s="sd" (defalt) plot standard deviation
s="sem" plot standard error of mean
}
  \item{test}{
Letters of the post-hoc test
test="tukey"  (default)
test="snk"
test="duncan"
test="t"
test="scott_knott"
}
 
 \item{family}{
font of plot
}
 \item{bg}{
background color
}
 
 \item{cex.text}{
font size in letters and means
}
\item{cex}{
font size in points
}
\item{bar.order}{
order of bar or means
1 = order of treatments names 
2 = order of the means (default)
}
\item{decreasing}{
decreasing bar order (TRUE or FALSE)
}

\item{xlab}{
title of x-axis
}

\item{ylab}{
title of y-axis
}

\item{pch}{
type of points
}
\item{...}{
more plot parameters
}
}


\value{
Returns plots of means
}

\references{
KAPS, M. and LAMBERSON, W. R. Biostatistics for Animal Science: an introductory text. 2nd Edition. CABI Publishing, Wallingford, Oxfordshire, UK, 2009. 504p.
}
\author{
Emmanuel Arnhold <emmanuelarnhold@yahoo.com.br>
}


\seealso{
ea1,ea2, box.plot, means.plot, means.plotfat, ic, ic.plot, p.plot
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
m.plot(means, col=gray.colors(4))

#direct
m.plot(r[[2]], col=gray.colors(4))

# more graphical parameters
m.plot(means, col=c(2,7,3,5), bg="white", las=1, cex.text=1,main="Tukey (0.05)", 
family="sans", bar.order=2, decreasing=FALSE);grid(10)

data('data7')
r<-ea2(data7,4)
m.plot(r[[4]], col=c(2,7,3,5), las=1, bg="cornsilk");grid(10)

par(mfrow=c(1,2))
m.plot(r[[8]][1], test="scott_knott",xlab="treatment 1",col=c(2,7,3,5), 
las=2, bg="cornsilk",bar.order=2, decreasing=FALSE);grid(10)
m.plot(r[[8]][2], test="scott_knott",xlab="treatment 2", col=c(2,7,3,5), 
las=2, bg="cornsilk", bar.order=2, decreasing=FALSE);grid(10)
}

