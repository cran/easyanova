\name{ea4}
\alias{ea4}

\title{
Analysis of variance in triple factorial designs
}
\description{
Perform analysis of variance and other important complementary analyzes in triple factorial scheme, with balanced and unbalanced data.
}
\usage{
ea4(data)
}

\arguments{
  \item{data}{
data is a data.frame

data frame with four columns, factor 1, factor 2, factor 3 and response (factorial in completely randomized design)

data frame with five columns, factor1, factor 2, factor 3, blocks and response (factorial in randomized block design)

data frame with six columns, factor 1, factor 2, factor3, rows, cols and response (factorial in latin square design)
}
}
\details{
The response variable must be numeric. Other variables can be numeric or factors.
}
\value{
Returns analysis of variance, normality test, coefficient of variation, means (adjusted means), multiple comparisons tests.
}
\references{
SAMPAIO, I. B. M. Estatistica aplicada a experimentacao animal. 3nd Edition. Belo Horizonte: Editora FEPMVZ, Fundacao de Ensino e Pesquisa em Medicina Veterinaria e Zootecnia, 2010. 264p.
}
\author{
Emmanuel Arnhold <earnhold@pq.cnpq.br>
}

\seealso{
ea1, ea2, ea3, ea5, ea6, ea7, ea8, lme, glht
}
\examples{
# Sampaio (2010)
data(data9)

# analysis in completely randomized design
ndata<-data9[-4]

r1<-ea4(ndata)

names(r1)


r1["Adjusted means (treatments_f1f2)"]

r1["Adjusted means (treatments_f1f2f3)"]

r1[c(4,5,6)]

r1


# analysis in randomized block design

r2<-ea4(data9)

r1[1];r2[1]

r2

}

