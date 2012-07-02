ea2 <-
function(data){
kw <- function(expr) {
    localWarnings <- list()
    value <- withCallingHandlers(expr,
                     warning = function(w) {
                         localWarnings[[length(localWarnings)+1]] <<- w
                         invokeRestart("muffleWarning")
                     })
value=value
} 
res<- list("","", 
f1<-function(data){ 
names(data)=c("factor_1","factor_2","response")
data<-data.frame(factor_1=factor(data$factor_1), factor_2=factor(data$factor_2), response=data$response)
m<-aov(response~factor_1*factor_2,data=data, contrasts=list(factor_1=contr.sum, factor_2=contr.sum))
m1<-aov(response~-1+factor_1+factor_2+factor_1*factor_2,data=data, contrasts=list(factor_1=contr.sum, factor_2=contr.sum))
m2<-aov(response~-1+ factor_2+factor_1+factor_2*factor_1,data=data, contrasts=list(factor_1=contr.sum, factor_2=contr.sum))
a<-anova(m)
a2<-Anova(m, type=3) 
a3<-a2[-1,]
residuals<-resid(m)
s<-shapiro.test(residuals)
treatments=interaction(data$factor_1,data$factor_2)
data<-data.frame(data,treatments)
m3<-aov(response~-1+treatments,data=data, contrasts=list(treatments=contr.sum))
data2<-na.omit(data)
b1<- bartlett.test(residuals~factor_1, data=data2)
b2<- bartlett.test(residuals~factor_2, data=data2)
b3<- bartlett.test(residuals~treatments, data=data2)
cv<-function(x){sd=(deviance(x)/df.residual(x))^0.5
mm=mean(fitted(x))
r=100*sd/mm
return(round(r,2))}
cvf=cv(m)
Adjust.Means<-coef(m1)[1:nlevels(data$factor_1)]
Standart.Error<-sqrt(diag(vcov(m1)) [1:nlevels(data$factor_1)])
Factor_1<-levels(data$factor_1)
maf1=data.frame(Factor_1,Adjust.Means,Standart.Error)
rownames(maf1)=NULL
Adjust.Means<-coef(m2) [1:nlevels(data$factor_2)]
Standart.Error<-sqrt(diag(vcov(m2))) [1:nlevels(data$factor_2)]
Factor_2<-levels(data$factor_2)
maf2=data.frame(Factor_2,Adjust.Means,Standart.Error)
rownames(maf2)=NULL
Adjust.Means<-coef(m3)
Standart.Error<-sqrt(diag(vcov(m3)))
Treatments<-levels(data$t)
mat=data.frame(Treatments,Adjust.Means,Standart.Error)
rownames(mat)=NULL
test1<-summary(kw(glht (m1,  linfct = mcp(factor_1 = "Tukey"))))
cld(test1)
test2<-summary(kw(glht (m2,  linfct = mcp(factor_2 = "Tukey"))))
cld(test2)
test3<-summary(glht (m3,  linfct = mcp(treatments= "Tukey" )))
cld(test3)
test1<-list(test1,cld(test1))
test2<-list(test2,cld(test2))
test3<-list(test3,cld(test3))
test=glht (m3,  linfct = mcp(treatments= "Tukey" ))
mc=test$linfct
x=factor(data$factor_1)
y=factor(data$factor_2)
x=levels(x)
y=levels(y)
x=factor(x)
y=factor(y)
x=reorder(x,c(nlevels(x):1))
y=reorder(y, c(nlevels(y):1))
x1=lapply(levels(x), factor)
y1=lapply(levels(y), factor)
h1=lapply(x1, function(xi){interaction(xi,y)})
h2=lapply(y1, function(yi){interaction(x,yi)})
l1= lapply(h1, levels)
l2= lapply(h2, levels)
aux1<- lapply(l1, function(li){combn(li,2)})
aux2<- lapply(l2, function(li){combn(li,2)})
w1 <- lapply(aux1, function(auxi) {apply(auxi, 2, paste, collapse = " - ")})
w2 <- lapply(aux2, function(auxi) {apply(auxi, 2, paste, collapse = " - ")})
mc=data.frame(mc)
c1=lapply(w1, function(w1i) {        mc[w1i,]    })
c2=lapply(w2, function(w2i) {        mc[w2i,]    })
c1 <- lapply(c1, as.matrix, ncol=length(coef(m3)))
c2 <- lapply(c2, as.matrix, ncol=length(coef(m3)))
r1=lapply(c1, function(c1i) { summary(glht(m3,  linfct =c1i)) })
r2=lapply(c2, function(c2i) { summary(glht(m3,  linfct =c2i)) })
l<-list(a3,s,b1,b2,b3,cvf, maf1, maf2, mat, test1,test2,r2, r1,test3)
names(l)= list("Analysis of variance", "Normality test ", "Homogeneity of variances (factor 1)", "Homogeneity of variances (factor 2)", "Homogeneity of variances (treatments)", "Coefficient of variation (%)","Adjusted means (factor 1)", "Adjusted means (factor 2)", "Adjusted means (treatments)", "Multiple comparison test (factor 1)", "Multiple comparison test (factor 2)", "Multiple comparison test (factor 1 in levels of factor 2)", "Multiple comparison test (factor 2 in levels of factor 1)","Multiple comparison test (treatments)")
return(l)}
,
f2<-function(data){ 
names(data)=c("factor_1","factor_2","blocks","response")
data<-data.frame(factor_1=factor(data$factor_1), factor_2=factor(data$factor_2), blocks=factor(data$blocks), response=data$response)
m<-aov(response~factor_1*factor_2+blocks,data=data, contrasts=list(factor_1=contr.sum, factor_2=contr.sum, blocks=contr.sum))
m1<-aov(response~-1+factor_1+factor_2+factor_1*factor_2+blocks,data=data, contrasts=list(factor_1=contr.sum, factor_2=contr.sum, blocks=contr.sum))
m2<-aov(response~-1+ factor_2+factor_1+factor_2*factor_1+blocks,data=data, contrasts=list(factor_1=contr.sum, factor_2=contr.sum, blocks=contr.sum))
a<-anova(m)
a2<-Anova(m, type=3) 
a3<-a2[-1,]
residuals<-resid(m)
s<-shapiro.test(residuals)
treatments=interaction(data$factor_1,data$factor_2)
data<-data.frame(data,treatments)
m3<-aov(response~-1+treatments+blocks,data=data, contrasts=list(treatments=contr.sum, blocks=contr.sum))
data2<-na.omit(data)
b1<- bartlett.test(residuals~factor_1, data=data2)
b2<- bartlett.test(residuals~factor_2, data=data2)
b3<- bartlett.test(residuals~treatments, data=data2)
cv<-function(x){sd=(deviance(x)/df.residual(x))^0.5
mm=mean(fitted(x))
r=100*sd/mm
return(round(r,2))}
cvf=cv(m)
Adjust.Means<-coef(m1)[1:nlevels(data$factor_1)]
Standart.Error<-sqrt(diag(vcov(m1)) [1:nlevels(data$factor_1)])
Factor_1<-levels(data$factor_1)
maf1=data.frame(Factor_1,Adjust.Means,Standart.Error)
rownames(maf1)=NULL
Adjust.Means<-coef(m2) [1:nlevels(data$factor_2)]
Standart.Error<-sqrt(diag(vcov(m2))) [1:nlevels(data$factor_2)]
Factor_2<-levels(data$factor_2)
maf2=data.frame(Factor_2,Adjust.Means,Standart.Error)
rownames(maf2)=NULL
Adjust.Means<-coef(m3) [1:nlevels(data$treatments)]
Standart.Error<-sqrt(diag(vcov(m3))) [1:nlevels(data$treatments)]
Treatments<-levels(data$t)
mat=data.frame(Treatments,Adjust.Means,Standart.Error)
rownames(mat)=NULL
test1<-summary(kw(glht (m1,  linfct = mcp(factor_1 = "Tukey"))))
cld(test1)
test2<-summary(kw(glht (m2,  linfct = mcp(factor_2 = "Tukey"))))
cld(test2)
test3<-summary(glht (m3,  linfct = mcp(treatments= "Tukey" )))
cld(test3)
test1<-list(test1,cld(test1))
test2<-list(test2,cld(test2))
test3<-list(test3,cld(test3))
test=glht (m3,  linfct = mcp(treatments= "Tukey" ))
mc=test$linfct
x=factor(data$factor_1)
y=factor(data$factor_2)
x=levels(x)
y=levels(y)
x=factor(x)
y=factor(y)
x=reorder(x,c(nlevels(x):1))
y=reorder(y, c(nlevels(y):1))
x1=lapply(levels(x), factor)
y1=lapply(levels(y), factor)
h1=lapply(x1, function(xi){interaction(xi,y)})
h2=lapply(y1, function(yi){interaction(x,yi)})
l1= lapply(h1, levels)
l2= lapply(h2, levels)
aux1<- lapply(l1, function(li){combn(li,2)})
aux2<- lapply(l2, function(li){combn(li,2)})
w1 <- lapply(aux1, function(auxi) {apply(auxi, 2, paste, collapse = " - ")})
w2 <- lapply(aux2, function(auxi) {apply(auxi, 2, paste, collapse = " - ")})
mc=data.frame(mc)
c1=lapply(w1, function(w1i) {        mc[w1i,]    })
c2=lapply(w2, function(w2i) {        mc[w2i,]    })
c1 <- lapply(c1, as.matrix, ncol=length(coef(m3)))
c2 <- lapply(c2, as.matrix, ncol=length(coef(m3)))
r1=lapply(c1, function(c1i) { summary(glht(m3,  linfct =c1i)) })
r2=lapply(c2, function(c2i) { summary(glht(m3,  linfct =c2i)) })
l<-list(a3,s,b1,b2,b3,cvf, maf1, maf2, mat, test1,test2,r2, r1,test3)
names(l)= list("Analysis of variance", "Normality test ", "Homogeneity of variances (factor 1)", "Homogeneity of variances (factor 2)", "Homogeneity of variances (treatments)", "Coefficient of variation (%)","Adjusted means (factor 1)", "Adjusted means (factor 2)", "Adjusted means (treatments)", "Multiple comparison test (factor 1)", "Multiple comparison test (factor 2)", "Multiple comparison test (factor 1 in levels of factor 2)", "Multiple comparison test (factor 2 in levels of factor 1)","Multiple comparison test (treatments)")
return(l)
}
,
f3<-function(data){ 
names(data)=c("factor_1","factor_2","rows","cols","response")
data<-data.frame(factor_1=factor(data$factor_1), factor_2=factor(data$factor_2), rows=factor(data$rows), cols=factor(data$cols), response=data$response)
m<-aov(response~factor_1*factor_2+ rows+cols,data=data, contrasts=list(factor_1=contr.sum, factor_2=contr.sum, rows=contr.sum, cols=contr.sum))
m1<-aov(response~-1+factor_1+factor_2+factor_1*factor_2+ rows+cols,data=data, contrasts=list(factor_1=contr.sum, factor_2=contr.sum, rows=contr.sum, cols=contr.sum))
m2<-aov(response~-1+ factor_2+factor_1+factor_2*factor_1+ rows+cols,data=data, contrasts=list(factor_1=contr.sum, factor_2=contr.sum, rows=contr.sum, cols=contr.sum))
a<-anova(m)
a2<-Anova(m, type=3) 
a3<-a2[-1,]
residuals<-resid(m)
s<-shapiro.test(residuals)
treatments=interaction(data$factor_1,data$factor_2)
data<-data.frame(data,treatments)
m3<-aov(response~-1+treatments +rows+cols,data=data, contrasts=list(treatments=contr.sum, rows=contr.sum, cols=contr.sum))
data2<-na.omit(data)
b1<- bartlett.test(residuals~factor_1, data=data2)
b2<- bartlett.test(residuals~factor_2, data=data2)
b3<- bartlett.test(residuals~treatments, data=data2)
cv<-function(x){sd=(deviance(x)/df.residual(x))^0.5
mm=mean(fitted(x))
r=100*sd/mm
return(round(r,2))}
cvf=cv(m)
Adjust.Means<-coef(m1)[1:nlevels(data$factor_1)]
Standart.Error<-sqrt(diag(vcov(m1)) [1:nlevels(data$factor_1)])
Factor_1<-levels(data$factor_1)
maf1=data.frame(Factor_1,Adjust.Means,Standart.Error)
rownames(maf1)=NULL
Adjust.Means<-coef(m2) [1:nlevels(data$factor_2)]
Standart.Error<-sqrt(diag(vcov(m2))) [1:nlevels(data$factor_2)]
Factor_2<-levels(data$factor_2)
maf2=data.frame(Factor_2,Adjust.Means,Standart.Error)
rownames(maf2)=NULL
Adjust.Means<-coef(m3) [1:nlevels(data$treatments)]
Standart.Error<-sqrt(diag(vcov(m3))) [1:nlevels(data$treatments)]
Treatments<-levels(data$t)
mat=data.frame(Treatments,Adjust.Means,Standart.Error)
rownames(mat)=NULL
test1<-summary(kw(glht (m1,  linfct = mcp(factor_1 = "Tukey"))))
cld(test1)
test2<-summary(kw(glht (m2,  linfct = mcp(factor_2 = "Tukey"))))
cld(test2)
test3<-summary(glht (m3,  linfct = mcp(treatments= "Tukey" )))
cld(test3)
test1<-list(test1,cld(test1))
test2<-list(test2,cld(test2))
test3<-list(test3,cld(test3))
test=glht (m3,  linfct = mcp(treatments= "Tukey" ))
mc=test$linfct
x=factor(data$factor_1)
y=factor(data$factor_2)
x=levels(x)
y=levels(y)
x=factor(x)
y=factor(y)
x=reorder(x,c(nlevels(x):1))
y=reorder(y, c(nlevels(y):1))
x1=lapply(levels(x), factor)
y1=lapply(levels(y), factor)
h1=lapply(x1, function(xi){interaction(xi,y)})
h2=lapply(y1, function(yi){interaction(x,yi)})
l1= lapply(h1, levels)
l2= lapply(h2, levels)
aux1<- lapply(l1, function(li){combn(li,2)})
aux2<- lapply(l2, function(li){combn(li,2)})
w1 <- lapply(aux1, function(auxi) {apply(auxi, 2, paste, collapse = " - ")})
w2 <- lapply(aux2, function(auxi) {apply(auxi, 2, paste, collapse = " - ")})
mc=data.frame(mc)
c1=lapply(w1, function(w1i) {        mc[w1i,]    })
c2=lapply(w2, function(w2i) {        mc[w2i,]    })
c1 <- lapply(c1, as.matrix, ncol=length(coef(m3)))
c2 <- lapply(c2, as.matrix, ncol=length(coef(m3)))
r1=lapply(c1, function(c1i) { summary(glht(m3,  linfct =c1i)) })
r2=lapply(c2, function(c2i) { summary(glht(m3,  linfct =c2i)) })
l<-list(a3,s,b1,b2,b3,cvf, maf1, maf2, mat, test1,test2,r2, r1,test3)
names(l)= list("Analysis of variance", "Normality test ", "Homogeneity of variances (factor 1)", "Homogeneity of variances (factor 2)", "Homogeneity of variances (treatments)", "Coefficient of variation (%)","Adjusted means (factor 1)", "Adjusted means (factor 2)", "Adjusted means (treatments)", "Multiple comparison test (factor 1)", "Multiple comparison test (factor 2)", "Multiple comparison test (factor 1 in levels of factor 2)", "Multiple comparison test (factor 2 in levels of factor 1)","Multiple comparison test (treatments)")
return(l)
}
)
g=res[[length(data)]]
g(data)
}
