\name{data9}
\alias{data9}
\docType{data}
\title{
data9: Sampaio (2010): page 67
}
\description{
Factorial design to evaluate egg quality according to the lineage of chicken, packaging and storage time.
}
\usage{data(data9)}
\format{
  A data frame with 120 observations on the following 5 variables.
  \describe{
    \item{\code{lineage}}{a factor with levels \code{A} \code{B}}
    \item{\code{packing}}{a factor with levels \code{Ce} \code{Co} \code{S}}
    \item{\code{time}}{a numeric vector}
    \item{\code{repetitions}}{a numeric vector}
    \item{\code{response}}{a numeric vector}
  }
}
\references{
SAMPAIO, I. B. M. Estatistica aplicada a experimentacao animal. 3nd Edition. Belo Horizonte: Editora FEPMVZ, Fundacao de Ensino e Pesquisa em Medicina Veterinaria e Zootecnia, 2010. 264p.
}
\examples{
data(data9)
summary(data9)
}

