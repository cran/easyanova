\name{ea2}
\alias{ea2}

\title{
Analysis of variance in double factorial designs
}
\description{
Perform analysis of variance and other important complementary analyzes in double factorial scheme, with balanced and unbalanced data.
}
\usage{
ea2(data)
}

\arguments{
  \item{data}{
data is a data.frame

data frame with three columns, factor 1, factor 2 and response (factorial in completely randomized design)

data frame with four columns, factor1, factor 2, blocks and response (factorial in randomized block design)

data frame with five columns, factor 1, factor 2, rows, cols and response (factorial in latin square design)
}
}
\details{
The response variable must be numeric. Other variables can be numeric or factors.
}
\value{
Returns analysis of variance, normality test, test of homogeneity of variance, coefficient of variation, means (adjusted means), multiple comparisons tests.
}
\references{

KAPS, M. and LAMBERSON, W. R. Biostatistics for Animal Science: an introductory text. 2nd Edition. CABI Publishing, Wallingford, Oxfordshire, UK, 2009. 504p.

PIMENTEL-GOMES, F. and GARCIA C.H. Estatistica aplicada a experimentos agronomicos e florestais: exposicao com exemplos e orientacoes para uso de aplicativos. Editora Fealq, v.11, 2002. 309p.

}
\author{
Emmanuel Arnhold <emmanuelarnhold@yahoo.com.br>
}

\seealso{
ea1, ea3, ea4, ea5, ea6, ea7, ea8, lme, glht
}
\examples{

# Kaps and Lamberson (2009)
data(data5)

# analysis in completely randomized design
r1<-ea2(data5)

names(r1)

r1["Multiple comparison test (factor 1)"]

r1[10]

r1[c(7,8,9)]

r1

# Pimentel Gomes and Garcia (2002)
data(data6)

# analysis in randomized block design
r2<-ea2(data6)

r2

r1[1];r2[1]

}

