\name{easyanova-package}
\alias{easyanova-package}
\alias{easyanova}
\docType{package}
\title{
Analysis of variance and other important complementary analyzes
}
\description{
Perform analysis of variance and other important complementary analyzes. The functions are easy to use. Performs analysis in various designs, with balanced and unbalanced data.
}
\details{
\tabular{ll}{
Package: \tab easyanova\cr
Type: \tab Package\cr
Version: \tab 1.1\cr
Date: \tab 2012-07-02\cr
License: \tab GPL-2\cr
}

}
\author{

Emmanuel Arnhold <emmanuelarnhold@yahoo.com.br>

}
\references{

CRUZ, C.D. and CARNEIRO, P.C.S. Modelos biometricos aplicados ao melhoramento genetico. 2nd Edition. Vicosa, UFV, v.2, 2006. 585p.

KAPS, M. and LAMBERSON, W. R. Biostatistics for Animal Science: an introductory text. 2nd Edition. CABI Publishing, Wallingford, Oxfordshire, UK, 2009. 504p.

SAMPAIO, I. B. M. Estatistica aplicada a experimentacao animal. 3nd Edition. Belo Horizonte: Editora FEPMVZ, Fundacao de Ensino e Pesquisa em Medicina Veterinaria e Zootecnia, 2010. 264p.

SANDERS W.L. and GAYNOR, P.J. Analysis of switchback data using Statistical Analysis System, Inc. Software. Journal of Dairy Science, 70.2186-2191. 1987.

PIMENTEL-GOMES, F. and GARCIA C.H. Estatistica aplicada a experimentos agronomicos e florestais: exposicao com exemplos e orientacoes para uso de aplicativos. Editora Fealq, v.11, 2002. 309p.

}


\seealso{ea1, ea2, ea3, ea4, ea5, ea6, ea7, ea8, lme, glht

}
\examples{

data(data1)
data(data2)
data(data3)

ea1(data1)
ea1(data2)
ea1(data3)

r<-ea1(data1)

names(r)

r[4]

}
