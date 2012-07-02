\name{ea5}
\alias{ea5}

\title{
Analysis of variance in factorial in split.plot designs (mixed model)
}
\description{
Perform analysis of variance and other important complementary analyzes in factorial in split.plot scheme, with balanced and unbalanced data.
}
\usage{
ea5(data, cov, design)
}

\arguments{
  \item{data}{
data is a data.frame

data frame with five columns, factor 1 (plot 1), factor 2 (plot 2), factor 3 (split.plot), repetitions or blocks, and response (split.plot in randomizad design or block design)
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
}
}
\details{
The response variable must be numeric. Other variables can be numeric or factors.
}
\value{
Returns analysis of variance, parameters of model fitting, normality test, means (adjusted means), multiple comparisons tests.
}
\references{
SAMPAIO, I. B. M. Estatistica aplicada a experimentacao animal. 3nd Edition. Belo Horizonte: Editora FEPMVZ, Fundacao de Ensino e Pesquisa em Medicina Veterinaria e Zootecnia, 2010. 264p.
}
\author{
Emmanuel Arnhold <earnhold@pq.cnpq.br>
}

\seealso{
ea1, ea2, ea3, ea4, ea6, ea7, ea8, lme, glht
}

\examples{

# Sampaio (2010)
data(data9)

# analysis in completely randomized design (Autoregressive)
r1<-ea5(data9,1,1)

names(r1)

r1

}

