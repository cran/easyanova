ea7 <-
function(data, design){ 
res=list(
f1<-function(data){
names(data)=c("treatments", "repetition","blocks","response")
data<-data.frame(treatments=factor(data$treatments), repetition=factor(data$repetition),  blocks=factor(data$blocks), response=data$response)
m<-lm(terms(response~ repetition/blocks +treatments, keep.order=TRUE),data=data, contrasts=list(treatments=contr.sum, repetition=contr.sum, blocks=contr.sum))
m1<-lm(terms(response~-1+treatments+repetition/blocks, keep.order=TRUE ),data=data, contrasts=list(treatments=contr.sum, repetition=contr.sum, blocks=contr.sum))
a<-anova(m)
residuals<-resid(m)
s<-shapiro.test(residuals)
cv<-function(x){sd=(deviance(x)/df.residual(x))^0.5
 mm=mean(fitted(x))
 r=100*sd/mm
return(round(r,2))}
cvf=cv(m)
Adjust.Means<-coef(m1)[c(1:nlevels(data$treatments))]
Standart.Error<-sqrt(diag(vcov(m1))) [c(1:nlevels(data$treatments))]
Treatments<-levels(data$treatments)
ma=data.frame(Treatments,Adjust.Means,Standart.Error)
rownames(ma)=NULL
test1<-summary(glht (m,  linfct = mcp(treatments = "Tukey" )))
cld(test1)
test<-list(test1,cld(test1))
l<-list(a,s,cvf, ma, test)
names(l)= list("Analysis of variance", "Normality test ",  "Coefficient of variation (%)","Adjusted means", "Multiple comparison test")
return(l)}
,
f2<-function(data){ 
names(data)=c("treatments", "blocks", "response")
data<-data.frame(treatments=factor(data$treatments), blocks=as.factor(data$blocks), response=data$response)
m<-lm(response~ blocks+treatments,data=data, contrasts=list(treatments=contr.sum, blocks=contr.sum))
m1<-lm(response~-1+treatments+blocks,data=data, contrasts=list(treatments=contr.sum, blocks=contr.sum))
a<-anova(m)
residuals<-resid(m)
s<-shapiro.test(residuals)
cv<-function(x){sd=(deviance(x)/df.residual(x))^0.5
 mm=mean(fitted(x))
 r=100*sd/mm
return(round(r,2))}
cvf=cv(m)
Adjust.Means<-coef(m1)[c(1:nlevels(data$treatments))]
Standart.Error<-sqrt(diag(vcov(m1))) [c(1:nlevels(data$treatments))]
Treatments<-levels(data$treatments)
ma=data.frame(Treatments,Adjust.Means,Standart.Error)
rownames(ma)=NULL
test1<-summary(glht (m,  linfct = mcp(treatments = "Tukey" )))
cld(test1)
test<-list(test1,cld(test1))
l<-list(a,s,cvf, ma, test)
names(l)= list("Analysis of variance", "Normality test ",  "Coefficient of variation (%)","Adjusted means", "Multiple comparison test")
return(l)}
,
f3<-function(data){ 
names(data)=c("treatments", "subject", "period", "response")
data<-data.frame(treatments=factor(data$treatments), subject=factor(data$subject), period=as.factor(data$period), response=data$response)
m<-lm(response~ treatments +subject+period,data=data, contrasts=list(treatments=contr.sum, subject=contr.sum, period=contr.sum))
m1<-lm(response~-1+treatments+ subject+period, data=data, contrasts=list(treatments=contr.sum, subject=contr.sum, period=contr.sum))
a<-anova(m)
residuals<-resid(m)
s<-shapiro.test(residuals)
cv<-function(x){sd=(deviance(x)/df.residual(x))^0.5
 mm=mean(fitted(x))
 r=100*sd/mm
return(round(r,2))}
cvf=cv(m)
Adjust.Means<-coef(m1)[c(1:nlevels(data$treatments))]
Standart.Error<-sqrt(diag(vcov(m1))) [c(1:nlevels(data$treatments))]
Treatments<-levels(data$treatments)
ma=data.frame(Treatments,Adjust.Means,Standart.Error)
rownames(ma)=NULL
test1<-summary(glht (m,  linfct = mcp(treatments = "Tukey" )))
cld(test1)
test<-list(test1,cld(test1))
l<-list(a,s,cvf, ma, test)
names(l)= list("Analysis of variance", "Normality test ",  "Coefficient of variation (%)","Adjusted means", "Multiple comparison test")
return(l)}
,
f4<-function(data){ 
names(data)=c("treatments","repetition", "blocks", "response")
data<-data.frame(treatments=factor(data$treatments), repetition=factor(data$repetition), blocks=factor(data$blocks), response=data$response)
m<-aov(response~repetition/blocks+treatments,data=data, contrasts=list(repetition=contr.sum, blocks=contr.sum, treatments=contr.sum))
m1<-aov(response~-1+treatments+repetition/blocks,data=data, contrasts=list(repetition=contr.sum, blocks=contr.sum, treatments=contr.sum))
a2<-Anova(m, type=3) 
a3<-a2[-1,]
residuals<-resid(m)
s<-shapiro.test(residuals)
data2<-na.omit(data)
b1<- bartlett.test(residuals~treatments, data=data2)
cv<-function(x){sd=(deviance(x)/df.residual(x))^0.5
mm=mean(fitted(x))
r=100*sd/mm
return(round(r,2))}
cvf=cv(m)
Adjust.Means<-coef(m1)[1:nlevels(data$treatments)]
Standart.Error<-sqrt(diag(vcov(m1)) [1:nlevels(data$treatments)])
Treatments<-levels(data$treatments)
ma=data.frame(Treatments,Adjust.Means,Standart.Error)
rownames(ma)=NULL
test1<-summary(glht (m,  linfct = mcp(treatments = "Tukey")))
c=cld(test1)
test1=list(test1, c)
QME=deviance(m)/df.residual(m)
m=nlevels(data$repetition) 
k=nlevels(data$blocks) 
vef=QME*(1+(m/((m-1)*(k+1))))
mb=lm(response~repetition+treatments, data=data)
QMEB=deviance(mb)/df.residual(mb)
Ef=100*QMEB/vef
l<-list(a3,s,b1,cvf, vef, Ef, ma,test1)
names(l)= list("Analysis of variance", "Normality test", "Homogeneity of variances", "Coefficient of variation (%)","Effective variance", "Efficiency of the design (%)","Adjusted means", "Multiple comparison test")
return(l)}
,
f5<-function(data){ 
names(data)=c("treatments","repetition", "blocks", "response")
data<-data.frame(treatments=factor(data$treatments), repetition=factor(data$repetition), blocks=factor(data$blocks), response=data$response)
block=interaction(data$repetition,data$blocks)
data=data.frame(data,block)
m<-lme(response~ repetition +treatments,random=~1|block, data=data, contrasts=list(repetition=contr.sum, treatments=contr.sum), na.action=na.omit)
m1<-lme(response~-1+treatments+repetition, random=~1|repetition/blocks,data=data, contrasts=list(repetition=contr.sum, blocks=contr.sum, treatments=contr.sum), na.action=na.omit)
a3<-anova(m, type="marginal")
a3<-a3[-1,]
residuals<-resid(m)
s<-shapiro.test(residuals)
data2<-na.omit(data)
b1<- bartlett.test(residuals~treatments, data=data2)
Adjust.Means<-fixef(m1)[1:nlevels(data$treatments)]
Standart.Error<-sqrt(diag(vcov(m1)) [1:nlevels(data$treatments)])
Treatments<-levels(data$treatments)
ma=data.frame(Treatments,Adjust.Means,Standart.Error)
rownames(ma)=NULL
test1<-summary(glht (m,  linfct = mcp(treatments = "Tukey")))
c=cld(test1)
test1=list(test1, c)
QME= m$sigma^2
m=nlevels(data$repetition) 
k=nlevels(data$blocks) 
mb=lm(response~repetition/blocks+treatments, data=data)
QMB= anova(mb)[[3]][3]
vef=QME*(1+(m/((m-1)*(k+1)))*(QMB-QME)/QMB)
mbb=lm(response~repetition+treatments, data=data)
Ef=100*(deviance(mbb)/df.residual(mbb))/vef
l<-list(a3,s,b1, vef, Ef, ma,test1)
names(l)= list("Analysis of variance", "Normality test", "Homogeneity of variances", "Effective variance", "Efficiency of the design (%)","Adjusted means", "Multiple comparison test")
return(l)}
)
g=res[[design]]
g(data)
}
