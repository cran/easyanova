\name{ea2}
\alias{ea2}

\title{
Analysis of variance in factorial and split plot 
}
\description{
Perform analysis of variance and other important complementary analyzes in factorial and split plot scheme, with balanced and unbalanced data.
}
\usage{
ea2(data, design = 1, alpha = 0.05, cov = 4, list = FALSE, p.adjust=1, plot=2)
}

\arguments{
  \item{data}{
data is a data.frame

see how the input data in the examples
}
  \item{design}{

1 = double factorial in completely randomized design

2 = double factorial in randomized block design
 
3 = double factorial in latin square design

4 = split plot in completely randomized design

5 = split plot in randomized block design

6 = split plot in latin square design

7 = triple factorial in completely randomized design

8 = triple factorial in randomized block design

9 = double factorial in split plot (completely randomized)

10 = double factorial in split plot (randomized in block)

11 = joint analysis of experiments with hierarchical blocks

12 = joint analysis of repetitions of latin squares (hierarchical rows)

13 = joint analysis of repetitions of latin squares (hierarchical rows and columns)

 
}
  \item{alpha}{
significance level for multiple comparisons 
}
  \item{cov}{
for split plot designs

1 = Autoregressive

2 = Heterogenius Autoregressive

3 = Continuous Autoregressive Process

4 = Compound Symetry

5 = Unstructured
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
Returns analysis of variance, means (adjusted means), multiple comparison test (tukey, snk, duncan, t and scott knott) and residual analysis.
}
\references{
KAPS, M. and LAMBERSON, W. R. Biostatistics for Animal Science: an introductory text. 2nd Edition. CABI Publishing, Wallingford, Oxfordshire, UK, 2009. 504p.

SAMPAIO, I. B. M. Estatistica aplicada a experimentacao animal. 3nd Edition. Belo Horizonte: Editora FEPMVZ, Fundacao de Ensino e Pesquisa em Medicina Veterinaria e Zootecnia, 2010. 264p.

PIMENTEL-GOMES, F. and GARCIA C.H. Estatistica aplicada a experimentos agronomicos e florestais: exposicao com exemplos e orientacoes para uso de aplicativos. Editora Fealq, v.11, 2002. 309p.

RAMALHO, M. A. P.; FERREIRA, D. F. and OLIVEIRA, A. C. Experimentacao em Genetica e Melhoramento de Plantas. Editora UFLA, 2005, 322p.

}
\author{
Emmanuel Arnhold <emmanuelarnhold@yahoo.com.br>
}



\seealso{
ea1, ec
}
\examples{

# double factorial

# completely randomized design
data(data5)
r1=ea2(data5, design=1) 
r1  

# randomized block design
# data(data6)
# r2=ea2(data6, design=2) 
# r2      
 
# names(r1)

# names(r2)

# triple factorial

# completely randomized design
# data(data9)
# r3=ea2(data9[,-4], design=7) 
# r3[1]  


# split plot

# completely randomized design
# data(data7)
# r4=ea2(data7, design=4)
# r4

# randomized block design
# data(data8)
# r5=ea2(data8, design=5)
# r5

# hierarchical blocks
# Ramalho et al. (2005)
# data(data18)
# data18
# r6=ea2(data18, design=11)
# r6

# hierarchical latin squares 
# Sampaio (2010)
# data(data19)
# data19
# r7=ea2(data19, design=12)
# r8=ea2(data19, design=13)

# hierarchical rows
# r7

# hierarchical rows and columns
# r8
        
}

