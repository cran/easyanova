\name{data19}
\alias{data19}
\docType{data}
\title{
data19: Sampaio (2010): page 155
}

\description{
Repetition of latin square design
}
\usage{data(data19)}
\format{
  A data frame with 32 observations on the following 5 variables.
  \describe{
    \item{\code{treatments}}{a factor with levels \code{A} \code{B} \code{C} \code{D}}
    \item{\code{squares}}{a factor with levels \code{1} \code{2}}
    \item{\code{rows}}{a factor with levels \code{1} \code{2} \code{3} \code{4}}
    \item{\code{columns}}{a factor with levels \code{1} \code{2} \code{3} \code{4}}
    \item{\code{response}}{a numeric vector}
  }
}


\references{
SAMPAIO, I. B. M. Estatistica aplicada a experimentacao animal. 3nd Edition. Belo Horizonte: Editora FEPMVZ, Fundacao de Ensino e Pesquisa em Medicina Veterinaria e Zootecnia, 2010. 264p.
}

\examples{
data(data19)
summary(data19)
}

