\name{ea1}
\alias{ea1}

\title{
Analysis of variance in simple designs
}
\description{
Perform analysis of variance and other important complementary analyzes. The function are easy to use. Performs analysis in various simples designs, with balanced and unbalanced data.
}
\usage{
ea1(data)
}

\arguments{
  \item{data}{
data is a data.frame

data frame with two columns, treatment and response (completely randomized design)

data frame with three columns, treatment, blocks and response (randomized block design)

data frame with four columns, treatments, rows, cols and response (latin square design)

data frame with five columns, treatments, square, rows, cols and response (several latin squares)
}
}
\details{
The response variable must be numeric. Other variables can be numeric or factors.
}
\value{
Returns analysis of variance, normality test, test of homogeneity of variance, coefficient of variation, means (adjusted means), multiple comparison test.
}
\references{
KAPS, M. and LAMBERSON, W. R. Biostatistics for Animal Science: an introductory text. 2nd Edition. CABI Publishing, Wallingford, Oxfordshire, UK, 2009. 504p.
}
\author{
Emmanuel Arnhold <earnhold@pq.cnpq.br>
}

\seealso{
ea2, ea3, ea4, ea5, ea6, ea7, ea8, lme, glht
}
\examples{

# Kaps and Lamberson(2009)
data(data1)
data(data2)
data(data3)
data(data4)

# analysis in completely randomized design
r1<-ea1(data1)

names(r1)

r1

# analysis in randomized block design
r2<-ea1(data2)

# analysis in latin square design
r3<-ea1(data3)

# analysis in several latin squares design
r4<-ea1(data4)

r1[1]
r2[1]
r3[1]
r4[1]

# analysis in unbalanced randomized block design
response<-ifelse(data2$Gain>850, NA, data2$Gain)
ndata<-data.frame(data2[-3],response)
ndata

r5<-ea1(ndata)

r5['Adjusted means']

}

