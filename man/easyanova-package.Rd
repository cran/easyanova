\name{easyanova-package}
\alias{easyanova-package}
\alias{easyanova}
\docType{package}
\title{
Analysis of Variance and Other Important Complementary Analyzes
}
\description{
Perform analysis of variance and other important complementary analyzes. The functions are easy to use. Performs analysis in various designs, with balanced and unbalanced data.
}
\details{
\tabular{ll}{
Package: \tab easyanova\cr
Type: \tab Package\cr
Version: \tab 7.0\cr
Date: \tab 2019-07-23\cr
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

RAMALHO, M. A. P.; FERREIRA, D. F. and OLIVEIRA, A. C. Experimentacao em Genetica e Melhoramento de Plantas. Editora UFLA, 2005, 322p.

}


\seealso{ea1, ea2, ec

}
\examples{

# Kaps and Lamberson(2009)
data(data1)
data(data2)
data(data3)
data(data4)

# analysis in completely randomized design
r1<-ea1(data1, design=1)

names(r1)

r1

# analysis in randomized block design
r2<-ea1(data2, design=2)

# analysis in latin square design
r3<-ea1(data3, design=3)

# analysis in several latin squares design
r4<-ea1(data4, design=4)

r1[1]
r2[1]
r3[1]
r4[1]

# analysis in unbalanced randomized block design
response<-ifelse(data2$Gain>850, NA, data2$Gain)
ndata<-data.frame(data2[-3],response)
ndata

r5<-ea1(ndata, design=2 )

r5

# multivariable response (list argument = TRUE)
t<-c('a','a','a','b','b','b','c','c','c')
r1<-c(10,12,12.8,4,6,8,14,15,16)
r2<-c(102,105,106,125,123,124,99,95,96)
r3<-c(560,589,590,658,678,629,369,389,378)


d<-data.frame(t,r1,r2,r3)

results=ea1(d, design=1, list=TRUE)
names(results)
results

results[1][[1]]

names(results[1][[1]])

}


