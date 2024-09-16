\name{tab}
\alias{tab}

\title{
Table of results in the ea1 function
}
\description{
Summary of results in ea1 function
}
\usage{
tab(data, test=1)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
output object of ea1 function (see examples)
}
 \item{test}{
Letters of the post-hoc test
1=Tukey
2=SNK
3=Duncan
4=t
5=Scott-Knott
}


}


\value{
Summary of results in ea1 function
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

tab(r)

### multiple variables

t<-c('a','a','a','b','b','b','c','c','c')
r1<-c(10,12,12.8,4,6,8,14,15,16)
r2<-c(102,105,106,125,123,124,99,95,96)
r3<-c(560,589,590,658,678,629,369,389,378)


d<-data.frame(t,r1,r2,r3)

results=ea1(d, design=1, list=TRUE)

# scottknott test
tab(results,test=5)




}

