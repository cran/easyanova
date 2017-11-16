\name{ea1}
\alias{ea1}

\title{
Analysis of variance in simple designs
}
\description{
Perform analysis of variance and other important complementary analyzes. The function are easy to use. Performs analysis in various simples designs, with balanced and unbalanced data. Too performs analysis the kruskal-Wallis and Friedman (designs 14 and 15).
}

\usage{
ea1(data, design = 1, alpha = 0.05, list = FALSE, p.adjust=1, plot=2)
}

\arguments{
  \item{data}{
data is a data.frame

see how the input data in the examples

}
  \item{design}{
1 = completely randomized design

2 = randomized block design
 
3 = latin square design

4 = several latin squares

5 = analysis with a covariate (completely randomized design)

6 = analysis with a covariate (randomized block design)

7 = incomplete blocks type I and II

8 = incomplete blocks type III or augmented blocks
 
9 = incomplete blocks type III in animal experiments

10 = lattice (intra-block analysis) 

11 = lattice (inter-block analysis)

12 = switchback design 

13 = switchback design in blocks

14 = Kruskal-Wallis rank sum test

15 = Friedman rank sum test

}
  \item{alpha}{
significance level for multiple comparisons 
}
  \item{list}{
FALSE = a single response variable

TRUE = multivariable response
}

\item{p.adjust}{
1="none"; 2="holm"; 3="hochberg"; 4="hommel"; 5="bonferroni"; 6="BH", 7="BY"; 8="fdr"; for more details see function "p.adjust"}

\item{plot}{ 1 = box plot for residuals; 2 = standardized residuals vs sequence data; 3 = standardized residuals vs theoretical quantiles}
 
}
\details{
The response variable must be numeric. Other variables can be numeric or factors.
}
\value{
Returns analysis of variance, means (adjusted means), multiple comparison test (tukey, snk, duncan, t and scott knott) and residual analysis. Too returns analysis the kruskal-Wallis and Friedman (designs 14 and 15). 
}
\references{
CRUZ, C.D. and CARNEIRO, P.C.S. Modelos biometricos aplicados ao melhoramento genetico. 2nd Edition. Vicosa, UFV, v.2, 2006. 585p.

KAPS, M. and LAMBERSON, W. R. Biostatistics for Animal Science: an introductory text. 2nd Edition. CABI Publishing, Wallingford, Oxfordshire, UK, 2009. 504p.

SAMPAIO, I. B. M. Estatistica aplicada a experimentacao animal. 3nd Edition. Belo Horizonte: Editora FEPMVZ, Fundacao de Ensino e Pesquisa em Medicina Veterinaria e Zootecnia, 2010. 264p.

SANDERS W.L. and GAYNOR, P.J. Analysis of switchback data using Statistical Analysis System, Inc. Software. Journal of Dairy Science, 70.2186-2191. 1987.

PIMENTEL-GOMES, F. and GARCIA C.H. Estatistica aplicada a experimentos agronomicos e florestais: exposicao com exemplos e orientacoes para uso de aplicativos. Editora Fealq, v.11, 2002. 309p.


}
\author{
Emmanuel Arnhold <emmanuelarnhold@yahoo.com.br>
}

\seealso{
ea2, ec
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

# analysis with a covariate
# Kaps and Lamberson (2009)
# data(data10)

# analysis in completely randomized design
# r6<-ea1(data10[-3], design=5)

# r6


# incomplete blocks type I and II
# Pimentel Gomes and Garcia (2002)
# data(data11)
# data(data12)

#r7<-ea1(data11,design=7)
#r8<-ea1(data12,design=7)

#r7;r8


# incomplete blocks type III or augmented blocks 
# Cruz and Carneiro (2006)
# data(data13)

#r9<-ea1(data13, design=8)
#r9



# incomplete blocks type III in animal experiments
# Sampaio (2010)
# data(data14)

# r10<-ea1(data14, design=9)
# r10

# lattice 
# Pimentel Gomes and Garcia (2002)
# data(data15)

#r11<-ea1(data15, design=10) # intra-block analysis 
#r12<-ea1(data15, design=11) # inter-block analysis

#r11
#r12

# switchback design
# Sampaio (2010)
# data(data16)
# r13<-ea1(data16, design=12)
# r13

# switchback design in blocks
# Sanders and Gaynor (1987)
# data(data17)
# r14<-ea1(data17, design=13)
# r14

#Kruskal-Wallis Rank Sum Test
r15<-ea1(data1, design=14)
r15

#Friedman Rank Sum Test
r16<-ea1(data2, design=15)
r16


}

