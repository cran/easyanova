\name{ic}
\alias{ic}

\title{
Confidence intervals of contrasts
}
\description{
Estimate of confidence intervals of the contrasts
}
\usage{
ic(data, test=1, df=10, alpha=0.05)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
output object of ea1 or ea2 function (see examples)
}
 \item{test}{
Letters of the post-hoc test
1=Tukey
2=SNK
3=Duncan
4=t
5=Scott-Knott
}

\item{df}{
degree of freedom of residuals in anova
}

\item{alpha}{
significance level
}


}


\value{
Returns confidence intervals of the contrasts
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

# alpha = 0.10
ic(r[[2]], test=1, df=6, alpha=0.10) 


# split plot
data('data7')
r<-ea2(data7,4)

#plot
ic(r[2], df=15)

#split.plot
ic(r[4], df=45)


}

