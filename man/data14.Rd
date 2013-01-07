\name{data14}
\alias{data14}
\docType{data}
\title{
data14: Sampaio (2009): page173
}
\description{
Incomplete block design in animals
}
\usage{data(data14)}
\format{
  A data frame with 28 observations on the following 4 variables.
  \describe{
    \item{\code{treatment}}{a factor with levels \code{A} \code{B} \code{C} \code{D} \code{E} \code{F} \code{G}}
    \item{\code{animal}}{a factor with levels \code{A1} \code{A2} \code{A3} \code{A4} \code{A5} \code{A6} \code{A7}}
    \item{\code{period}}{a factor with levels \code{P1} \code{P2} \code{P3} \code{P4}}
    \item{\code{response}}{a numeric vector}
  }
}
\references{
SAMPAIO, I. B. M. Estatistica aplicada a experimentacao animal. 3nd Edition. Belo Horizonte: Editora FEPMVZ, Fundacao de Ensino e Pesquisa em Medicina Veterinaria e Zootecnia, 2010. 264p.
}
\examples{
data(data14)
summary(data14)
}

