ea6 <-
function(data){
res=list("","",
f1<-function(data){ 
names(data)=c("treatments", "covariate", "response")
data<-data.frame(treatments=factor(data$treatments), covariate=as.numeric(data$covariate), response=data$response)
m<-lm(response~ covariate+treatments,data=data, contrasts=list(treatments=contr.sum))
m1<-lm(response~-1+treatments+covariate,data=data, contrasts=list(treatments=contr.sum))
a<-anova(m)
residuals<-resid(m)
s<-shapiro.test(residuals)
data2<-na.omit(data)
b<- bartlett.test(residuals~treatments, data=data2)
b1=coef(m)[[2]]
b1
ra=data$response-(b1*(data$covariate-mean(data$covariate)))
data=data.frame(data,ra)
m2=lm(ra~-1+treatments, data=data)
cv<-function(x){sd=(deviance(x)/df.residual(x))^0.5
 mm=mean(fitted(x))
 r=100*sd/mm
return(round(r,2))}
cvf=cv(m)
a2<-Anova(m, type=2) 
a3<-a2[-1,]
Adjust.Means<-coef(m2)[c(1:nlevels(data$treatments))]
Standart.Error<-sqrt(diag(vcov(m1)))
Standart.Error<- Standart.Error[1: nlevels(data$treatments)] 
Treatments<-levels(data$treatments)
ma=data.frame(Treatments,Adjust.Means,Standart.Error)
rownames(ma)=NULL
test1<-summary(glht (m,  linfct = mcp(treatments = "Tukey" )))
cld(test1)
test<-list(test1,cld(test1))
l<-list(a,s,b,cvf, ma, test)
names(l)= list("Analysis of variance", "Normality test ", "Homogeneity of variances", "Coefficient of variation (%)","Adjusted means", "Multiple comparison test")
return(l)}
,
f2<-function(data){ 
names(data)=c("treatments", "covariate", "blocks", "response")
data<-data.frame(treatments=factor(data$treatments), covariate=as.numeric(data$covariate), blocks=factor(data$blocks),response=data$response)
m<-lm(response~ covariate+treatments+blocks,data=data, contrasts=list(treatments=contr.sum, blocks=contr.sum))
m1<-lm(response~-1+treatments+covariate+blocks,data=data, contrasts=list(treatments=contr.sum, blocks=contr.sum))
a<-anova(m)
residuals<-resid(m)
s<-shapiro.test(residuals)
data2<-na.omit(data)
b<- bartlett.test(residuals~treatments, data=data2)
b1=coef(m)[[2]]
b1
ra=data$response-(b1*(data$covariate-mean(data$covariate)))
data=data.frame(data,ra)
m2=lm(ra~-1+treatments+blocks, data=data)
cv<-function(x){sd=(deviance(x)/df.residual(x))^0.5
 mm=mean(fitted(x))
 r=100*sd/mm
return(round(r,2))}
cvf=cv(m)
a2<-Anova(m, type=2) 
a3<-a2[-1,]
Adjust.Means<-coef(m2)[c(1:nlevels(data$treatments))]
Standart.Error<-sqrt(diag(vcov(m1)))
Standart.Error<- Standart.Error[1: nlevels(data$treatments)] 
Treatments<-levels(data$treatments)
ma=data.frame(Treatments,Adjust.Means,Standart.Error)
rownames(ma)=NULL
test1<-summary(glht (m,  linfct = mcp(treatments = "Tukey" )))
cld(test1)
test<-list(test1,cld(test1))
l<-list(a,s,b,cvf, ma, test)
names(l)= list("Analysis of variance", "Normality test ", "Homogeneity of variances", "Coefficient of variation (%)","Adjusted means", "Multiple comparison test")
return(l)}
)
g=res[[length(data)]]
g(data)
}
