\name{box.plot}
\alias{box.plot}

\title{
Box plot
}
\description{
Plot quartis
}
\usage{
box.plot(data,test=1, xlab=NULL, ylab=NULL,legend=TRUE,
letters=TRUE, family="Times", bg="white",  cex.axis=0.7,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  
\item{data}{
data.frame with data (see examples)
}

\item{test}{
type of test
1 = Kruskall-Wallis
2 = Friedman
}
  \item{xlab}{
name of x-axis
}
  \item{ylab}{
name of y-axis
}
  \item{legend}{
TRUE = plot p-value of test
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
 \item{cex.axis}{
font size in the axis
}
 
\item{...}{
more plot parameters
}
}


\value{
Returns box plots and test of Kruskall-Wallis and Friedman
}

\references{
KAPS, M. and LAMBERSON, W. R. Biostatistics for Animal Science: an introductory text. 2nd Edition. CABI Publishing, Wallingford, Oxfordshire, UK, 2009. 504p.
}
\author{
Emmanuel Arnhold <emmanuelarnhold@yahoo.com.br>
}


\seealso{
ea1,ea2, means.plot
}

\examples{

#data3: Kaps and Lamberson (2009)
#Description:
##The experiment compared three diets for pigs in a completely randomized design
data(data1)

# Kruskal-Wallis test
box.plot(data1, test=1)

#Description
#Complete randomized block design to determine the average daily gain of steers
data(data2)
box.plot(data2, test=2)

#More plot parameters
box.plot(data2, test=2, col=c(2,7,3), col.axis="red",las=1, 
legend=FALSE, bg="cornsilk", sub="Treatments", cex=1.2);grid(10, lwd=1.5)


}

