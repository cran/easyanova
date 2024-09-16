\name{means.plotfat}
\alias{means.plotfat}

\title{
Plot Means (interactions)
}
\description{
Plot contrasts of means
}
\usage{
means.plotfat(data, plot=1, s=1,test=1, legend=TRUE, letters=TRUE,
family="Times", bg="white",  cex.names=0.8, cex.text=0.7,
 cex.legend=1, bar.order=1,decreasing=TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
output object of ea2 function (see examples)
}

\item{plot}{
type of plot
1 = bar plot factor 1(default)
2 = bar plot factor 2 
3 = bar plot interactions (option 1)
4 = bar plot interactions (option 2)
5 = bar plot interactions (option 3)
6 = bar plot interactions (option 4)
}
  \item{s}{
s=1 (defalt) plot standard deviation
s=2 plot standard error of mean
}
  \item{test}{
Letters of the post-hoc test
1=Tukey
2=SNK
3=Duncan
4=t
5=Scott-Knott
}
  \item{legend}{
TRUE = plot p-value of F test
FALSE = not plot p-value
}
  \item{letters}{
TRUE = plot letters 
FALSE = not plot letters
}
 \item{family}{
font of plot
}
 \item{bg}{
background color
}
 \item{cex.names}{
font size in names of treatments (x-axis)
}
 \item{cex.text}{
font size in letters and means
}
\item{cex.legend}{
font size in legend
}
\item{bar.order}{
order of bar or means
1 = order of treatments names 
2 = order of the means (default)
}
\item{decreasing}{
decreasing bar order (TRUE or FALSE)
}
\item{...}{
more plot parameters
}
}


\value{
Returns bar plots 
}

\references{
KAPS, M. and LAMBERSON, W. R. Biostatistics for Animal Science: an introductory text. 2nd Edition. CABI Publishing, Wallingford, Oxfordshire, UK, 2009. 504p.
}
\author{
Emmanuel Arnhold <emmanuelarnhold@yahoo.com.br>
}


\seealso{
ea1,ea2, means.plot, box.plot
}

\examples{

#data3: Kaps and Lamberson (2009): page 347
#Description:
##Latin square design for test four different treatments on hay intake of fattening steers

data(data3)

r<-ea1(data3, design=3)


#plot
means.plot(r, col=gray.colors(4))

# more graphical parameters
means.plot(r, col=c(2,7,3,5), bg="cornsilk", las=1, cex.names=2, 
sub="treatments", family="sans");grid(10)

# plot = 2
means.plot(r, plot=2, col="dark green", bg="gray", las=1, cex.names=2, 
sub="Treatments", family="Times", ylab="Hay intake")

# plot = 2 decreasing =FALSE
means.plot(r, plot=2, las=1, cex.names=2, col="red",lty=2,pch=20,cex=1.1,
sub="Treatments", family="Times", ylab="Hay intake", decreasing=FALSE, legend=FALSE);grid(10)

# plot=3 
means.plot(r, plot=3, las=1, cex.names=2, 
sub="Contrasts (Tukey 0.05)", family="Times", ylab="")

# plot=3  alpha=0.10
means.plot(ea1(data3, design=3), plot=3, las=2, cex.names=2, 
sub="Contrasts (Tukey 0.10)", family="Times", ylab="", alpha=0.10, bg="cornsilk");grid(10)


}

