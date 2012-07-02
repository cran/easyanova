ea8 <-
function(data){
kp <- function(expr) {
    localWarnings <- list()
    value <- withCallingHandlers(expr,
                     warning = function(w) {
                         localWarnings[[length(localWarnings)+1]] <<- w
                         invokeRestart("muffleWarning")
                     })
value=value
}  
res<- list("","","", 
f1<-function(data){
names(data)=c("treatments", "period","animal","response")
data<-data.frame(treatments=factor(data$treatments), period=factor(data$period),  animal=factor(data$animal), response=data$response, p=as.numeric(data$period))
m=aov(response~p*animal-p+period+animal+treatments, contrasts=list(treatments=contr.sum, animal=contr.sum, period=contr.sum), data=data)
a<-anova(m)
residuals<-resid(m)
s<-shapiro.test(residuals)
cv<-function(x){sd=(deviance(x)/df.residual(x))^0.5
 mm=mean(fitted(x))
 r=100*sd/mm
return(round(r,2))}
cvf=cv(m)
c=coef(m)
names(c)
class(c)
c=as.list(c)
t=c[1:(nlevels(data$animal)+nlevels(data$period)+nlevels(data$treatments)-2)]
t1=t[-c(1:(nlevels(data$animal)+nlevels(data$period)-1))]
tf=-(sum(unlist(t1)))
ef=c(unlist(t1),tf)
Adjust.Means<- mean(data$response, na.rm = T)+as.numeric(ef)
Standart.Error<-sqrt(diag(vcov(m))) [1:(nlevels(data$animal)+nlevels(data$period)+nlevels(data$treatments)-2)]
Standart.Error= Standart.Error[-c(1:(nlevels(data$animal)+nlevels(data$period)-1))]
Standart.Error<-c(as.numeric(Standart.Error), as.numeric(Standart.Error)[1])
Treatments<-levels(data$treatments)
ma=data.frame(Treatments,Adjust.Means,Standart.Error)
rownames(ma)=NULL
test1<-summary(kp(glht (m,  linfct = mcp(treatments = "Tukey" ))))
cld(test1)
test<-list(test1,cld(test1))
l<-list(a,s,cvf, ma, test)
names(l)= list("Analysis of variance", "Normality test",  "Coefficient of variation (%)","Adjusted means", "Multiple comparison test")
return(l)}
,
f2<-function(data){
names(data)=c("treatments", "blocks", "period","animal","response")
data<-data.frame(treatments=factor(data$treatments), blocks=factor(data$blocks), period=factor(data$period),  animal=factor(data$animal), response=data$response, p=as.numeric(data$period))
m=aov(response~blocks+p*animal-p+animal+treatments+blocks/period, contrasts=list(treatments=contr.sum, animal=contr.sum, period=contr.sum, blocks=contr.sum), data=data)
a<-anova(m)
residuals<-resid(m)
s<-shapiro.test(residuals)
cv<-function(x){sd=(deviance(x)/df.residual(x))^0.5
 mm=mean(fitted(x))
 r=100*sd/mm
return(round(r,2))}
cvf=cv(m)
c=coef(m)
names(c)
class(c)
c=as.list(c)
t=c[1:(a[[1]][1]+ a[[1]][2]+a[[1]][3])+1]
t1=t[-c(1:(a[[1]][1]+a[[1]][2]))]
tf=-(sum(unlist(t1)))
ef=c(unlist(t1),tf)
Adjust.Means<- mean(data$response, na.rm = T)+as.numeric(ef)
Standart.Error<-sqrt(diag(vcov(m)))[1:(a[[1]][1]+ a[[1]][2]+a[[1]][3])+1]
Standart.Error= Standart.Error[-c(1:(a[[1]][1]+a[[1]][2]))]
Standart.Error<-c(as.numeric(Standart.Error), as.numeric(Standart.Error)[1])
Treatments<-levels(data$treatments)
ma=data.frame(Treatments,Adjust.Means,Standart.Error)
rownames(ma)=NULL
test1<-summary(kp(glht (m,  linfct = mcp(treatments = "Tukey" ))))
cld(test1)
test<-list(test1,cld(test1))
l<-list(a,s,cvf, ma, test)
names(l)= list("Analysis of variance", "Normality test",  "Coefficient of variation (%)","Adjusted means", "Multiple comparison test")
return(l)}
)
g=res[[length(data)]]
g(data)
}
