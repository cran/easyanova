\name{ea3}
\alias{ea3}

\title{
Analysis of variance in split.plot designs (mixed model)
}
\description{
Perform analysis of variance and other important complementary analyzes in split.plot scheme, with balanced and unbalanced data.
}
\usage{
ea3(data, cov, design)
}

\arguments{
  \item{data}{
data is a data.frame

data frame with four columns, factor 1 (plot), repetitions or blocks, factor 2 (split.plot) and response (split.plot in completely randomized design or block design)

data frame with five columns, factor 1 (plot), cols, rows, factor 2 (split.plot), and response (split.plot in latin square design)
}
  \item{cov}{
Evaluated Structures

1 = Autoregressive

2 = Heterogenius Autoregressive

3 = Continuous Autoregressive Process

4 = Compound Symetry

5 = Unstructured
}
  \item{design}{
Design

1 = completely randomized design 

2 = randomized block design

3 = latin square design
}
}
\details{
The response variable must be numeric. Other variables can be numeric or factors.
}
\value{
Returns analysis of variance, parameters of model fitting, normality test, test of homogeneity of variance, means (adjusted means), multiple comparisons tests.
}
\references{
KAPS, M. and LAMBERSON, W. R. Biostatistics for Animal Science: an introductory text. 2nd Edition. CABI Publishing, Wallingford, Oxfordshire, UK, 2009. 504p.
}
\author{
Emmanuel Arnhold <earnhold@pq.cnpq.br>
}

\seealso{
ea1, ea2, ea4, ea5, ea6, ea7, ea8, lme, glht
}
\examples{
#Kaps and Lamberson (2009)
data(data7)
data(data8)

# analysis in completely randomized design (Autoregressive)
r1<-ea3(data7,1,1)

names(r1)

r1[15]

# analysis in randomized design (Compound Symetry)
r2<-ea3(data7,4,1)

r2["Adjusted Means (interaction)"]

# comparing models
r1[2];r2[c(2,15)]

# analysis in randomized block design (option 2, the third argument)
r3<-ea3(data8,1,2) # Autoregressive

names(r3)

r3["Evaluated Structures" ]

r4<-ea3(data8,4,2) # Compound Symetry 

r4[1]

r4[10]

r4[11]

}

