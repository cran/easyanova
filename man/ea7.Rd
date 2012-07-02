\name{ea7}
\alias{ea7}

\title{
Analysis of variance in incomplete blocks designs 
}
\description{
Perform analysis of variance in incomplete blocks designs and other important complementary analyzes. 
}
\usage{
ea7(data, design)
}

\arguments{
  \item{data}{
data is a data.frame

see the examples the forms input
}
  \item{design}{
1 = incomplete blocks type I and II

2 = incomplete blocks type III or augmented blocks
 
3 = incomplete blocks type III in animal experiments

4 = lattice (intra-block analysis) 

5 = lattice (inter-block analysis)
}
}
\details{
The response variable must be numeric. Other variables can be numeric or factors.
}
\value{
Returns analysis of variance, normality test, coefficient of variation, means (adjusted means), multiple comparisons tests.
}
\references{
CRUZ, C.D. and CARNEIRO, P.C.S. Modelos biometricos aplicados ao melhoramento genetico. 2nd Edition. Vicosa, UFV, v.2, 2006. 585p.

SAMPAIO, I. B. M. Estatistica aplicada a experimentacao animal. 3nd Edition. Belo Horizonte: Editora FEPMVZ, Fundacao de Ensino e Pesquisa em Medicina Veterinaria e Zootecnia, 2010. 264p.

PIMENTEL-GOMES, F. and GARCIA C.H. Estatistica aplicada a experimentos agronomicos e florestais: exposicao com exemplos e orientacoes para uso de aplicativos. Editora Fealq, v.11, 2002. 309p.
}
\author{
Emmanuel Arnhold <emmanuelarnhold@yahoo.com.br>
}

\seealso{
ea1, ea2, ea3, ea4, ea5, ea6, ea8, lme, glht
}
\examples{

# incomplete blocks type I and II
# Pimentel Gomes and Garcia (2002)
data(data11)
data(data12)

#r1<-ea7(data11,1)
#r2<-ea7(data12,1)

#r1;r2

#r1[1];r2[1]

#r1[4];r2[4]

# incomplete blocks type III or augmented blocks 
# Cruz and Carneiro (2006)
data(data13)

#r3<-ea7(data13, 2)
#r3

# see time: time<-system.time(ea7(data13, 2))

# incomplete blocks type III in animal experiments
# Sampaio (2010)
data(data14)

r4<-ea7(data14, 3)
r4

# lattice 
# Pimentel Gomes and Garcia (2002)
data(data15)

#r5<-ea7(data15, 4) # intra-block analysis 
#r6<-ea7(data15, 5) # inter-block analysis

#r5
#r6

}

