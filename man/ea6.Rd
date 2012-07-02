\name{ea6}
\alias{ea6}

\title{
Analysis of variance with a covariate
}
\description{
Perform analysis of variance with a covariate and other important complementary analyzes. 
}
\usage{
ea6(data)
}

\arguments{
  \item{data}{
data is a data.frame

data frame with three columns, treatments, covariate and response (analysis in completely randomized design)

data frame with four columns, treatments, covariate, blocks and response (analysis in randomized block design)
}
}
\details{
The covariate and response variables must be numeric. Other variables can be numeric or factors.
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
ea1, ea2, ea3, ea4, ea5, ea7, ea8, lme, glht
}
\examples{
# Kaps and Lamberson (2009)
data(data10)

# analysis in completely randomized design
r1<-ea6(data10[-3])

# analysis in randomized block design
r2<-ea6(data10)

r1[1];r2[1]

r1[c(6,5)]
}

