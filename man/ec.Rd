\name{ec}
\alias{ec}

\title{
Easy contrast 
}
\description{
Performs contrasts of means
}
\usage{
ec(mg1, mg2, sdg1, sdg2, df)
}
%- maybe also 'usage' for other objects documented here.
\arguments{

  \item{mg1}{
Means of the group 1
}

  \item{mg2}{
Means of the group 2
}

  \item{sdg1}{
Standard error of the group 1
}

  \item{sdg2}{
Standard error of the group 2
}

  \item{df}{
Degree of freedom from error
}
}


\value{
Returns t test for contrast
}
\references{
KAPS, M. and LAMBERSON, W. R. Biostatistics for Animal Science: an introductory text. 2nd Edition. CABI Publishing, Wallingford, Oxfordshire, UK, 2009. 504p.
}
\author{
Emmanuel Arnhold <emmanuelarnhold@yahoo.com.br>
}


\seealso{
ea1,ea2
}
\examples{
# Kaps and Lamberson(2009, pg 254)

data(data1)

r<-ea1(data1, design=1)
r[2]

# first contrast
mg1=312;mg2=c(278,280); sdg1=7.7028;sdg2=c(7.7028,7.7028); df=12
ec(mg1,mg2,sdg1,sdg2,df)

# second contrast
mg1=280;mg2=278; sdg1=7.7028;sdg2=7.7028; df=12
ec(mg1,mg2,sdg1,sdg2,df)

}

