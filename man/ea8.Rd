\name{ea8}
\alias{ea8}

\title{
Analysis of variance in switchback design
}
\description{
Perform analysis of variance in switchback design and other important complementary analyzes. 
}
\usage{
ea8(data)
}

\arguments{
  \item{data}{
data is a data.frame

data frame with four columns, treatment, period, animal, and response (completely randomized design)

data frame with five columns, treatment, blocks, period, animal and response (randomized block design)
}
}
\details{
The response and period must be numeric. Other variables can be numeric or factors.
}
\value{
Returns analysis of variance, normality test, coefficient of variation, means (adjusted means), multiple comparisons tests.
}
\references{
SAMPAIO, I. B. M. Estatistica aplicada a experimentacao animal. 3nd Edition. Belo Horizonte: Editora FEPMVZ, Fundacao de Ensino e Pesquisa em Medicina Veterinaria e Zootecnia, 2010. 264p.

SANDERS W.L. and GAYNOR, P.J. Analysis of switchback data using Statistical Analysis System, Inc. Software. Journal of Dairy Science, 70.2186-2191. 1987.
}
\author{
Emmanuel Arnhold <emmanuelarnhold@yahoo.com.br>
}

\seealso{
ea1, ea2, ea3, ea4, ea5, ea6, ea7, lme, glht
}
\examples{
# Sampaio (2010)
data(data16)
r1<-ea8(data16)
r1

# Sanders and Gaynor (1987)
data(data17)
r2<-ea8(data17)
r2

}

