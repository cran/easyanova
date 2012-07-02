ea3 <-
function(data, cov, design){
gg=cov
kp <- function(expr) {
    localWarnings <- list()
    value <- withCallingHandlers(expr,
                     warning = function(w) {
                         localWarnings[[length(localWarnings)+1]] <<- w
                         invokeRestart("muffleWarning")
                     })
value=value
} 
res<- list( 
f1<-function(data, cov){
gg<-cov
names(data)<-c("plot","rep","split.plot","response")
data<-data.frame(plot=factor(data$plot), rep=factor(data$rep), split.plot=factor(data$split.plot), response=data$response)
subject<-interaction(data$plot,data$rep)
treatments<-interaction(data$plot,data$split.plot)
data<-data.frame(data, subject,treatments)
UN<-corSymm(form=~1|subject)
AR<-corAR1(form=~1|subject)
ARH<-corAR1(form=~1|subject)
CS<- corCompSymm (form=~1|subject) 
CAR<-corCAR1(form=~split.plot|subject)
o1<-list(AR,ARH, CAR, CS, UN)
cor<-o1[[gg]]
UN1<-varIdent(form=~1|split.plot)
AR1<-NULL
ARH1<- varIdent(form=~1|split.plot)
CS1<-NULL
CAR1<-NULL
o2<-list(AR1,ARH1, CAR1, CS1, UN1)
var<-o2[[gg]]
m<-lme(response~plot*split.plot, random=~1|subject, correlation=cor, weights=var, data<-data, na.action=na.omit, contrasts=list(plot=contr.sum, split.plot=contr.sum), control=lmeControl(maxIter =6000, msMaxIter=6000, niterEM=2000, opt="optim"))
b<-anova(m, type="marginal")[-1,]
m1<-lme(response~-1+ treatments, random=~1|subject, data=data, na.action=na.omit, contrasts=list(treatments=contr.sum), correlation=cor, weights=var, control=lmeControl(maxIter =6000, msMaxIter=6000, niterEM=2000, opt="optim"))
am<-fixef(m1)
c<-data.frame(levels(treatments), am, sqrt(diag(vcov(m1)))[1:nlevels(data$ treatments)])
colnames(c)<-c("plot*split.plot","Adjust Means", "Standart.Error")
rownames(c)<-NULL
df1<- anova(m) $"denDF"[2]
df2<- anova(m) $"denDF"[1]
residuals<-resid(m)
s<-shapiro.test(residuals)
data2<-na.omit(data)
b1<- bartlett.test(residuals~plot, data=data2)
b2<- bartlett.test(residuals~split.plot, data=data2)
b3<- bartlett.test(residuals~treatments, data=data2)
mm1<-lme(response~-1+plot*split.plot, random=~1|subject, data=data, na.action=na.omit, contrasts=list(plot=contr.sum, split.plot=contr.sum), correlation=cor, weights=var, control=lmeControl(maxIter =6000, msMaxIter=6000,  niterEM=2000, opt="optim"))
mm2<-lme(response~-1+split.plot*plot, random=~1|subject, data=data, na.action=na.omit, contrasts=list(split.plot=contr.sum, plot=contr.sum), correlation=cor, weights=var, control=lmeControl(maxIter =6000, msMaxIter=6000, niterEM=2000, opt="optim"))
a11<-data.frame(levels(data$plot), fixef(mm1)[1:nlevels(data$plot)], sqrt(diag(vcov(mm1)))[1:nlevels(data$plot)])
a22<-data.frame(levels(data$split.plot), fixef(mm2)[1:nlevels(data$split.plot)], sqrt(diag(vcov(mm2)))[1:nlevels(data$split.plot)])
colnames(a11)<-c("plot","Adjust Means", "Standart.Error")
rownames(a11)<-NULL
colnames(a22)<-c("split.plot","Adjust Means", "Standart.Error")
rownames(a22)<-NULL
test1<-summary(kp(glht (mm1,  linfct = mcp(plot = "Tukey"), df=df1)))
test2<-summary(kp(glht (mm2,  linfct = mcp(split.plot = "Tukey"), df=df2)))
ai<-as.numeric(AIC(m)); bi<-as.numeric(BIC(m)); log=as.numeric(logLik(m)); 
qa=data.frame(ai,bi,log)
colnames(qa)<-c("AIC","BIC", "LogLik")
rownames(qa)<-NULL
teste3<-glht (m1,  linfct = mcp(treatments="Tukey"))
mc<-teste3$linfct
x<-factor(data$plot)
y<-factor(data$split.plot)
x<-levels(x)
y<-levels(y)
x<-factor(x)
y<-factor(y)
x<-reorder(x,c(nlevels(x):1))
y<-reorder(y, c(nlevels(y):1))
x1<-lapply(levels(x), factor)
y1<-lapply(levels(y), factor)
h1<-lapply(x1, function(xi){interaction(xi,y)})
h2<-lapply(y1, function(yi){interaction(x,yi)})
l1<- lapply(h1, levels)

l2<- lapply(h2, levels)
aux1<- lapply(l1, function(li){combn(li,2)})
aux2<- lapply(l2, function(li){combn(li,2)})
w1 <- lapply(aux1, function(auxi) {apply(auxi, 2, paste, collapse = " - ")})
w2 <- lapply(aux2, function(auxi) {apply(auxi, 2, paste, collapse = " - ")})
mc<-data.frame(mc)
c1<-lapply(w1, function(w1i) {        mc[w1i,]    })
c2<-lapply(w2, function(w2i) {        mc[w2i,]    })
c1 <- lapply(c1, as.matrix, ncol=length(coef(m)))
c2 <- lapply(c2, as.matrix, ncol=length(coef(m)))
r1<-lapply(c2, function(c2i) { summary(glht(m1,  linfct =c2i, df=df2)) })
r2<-lapply(c1, function(c1i) { summary(glht(m1,  linfct =c1i, df=df2)) })
UN=corSymm(form=~1|subject)
AR=corAR1(form=~1|subject)
ARH=corAR1(form=~1|subject)
CS= corCompSymm (form=~1|subject) 
CAR<-corCAR1(form=~split.plot|subject)
UN1=varIdent(form=~1|split.plot)
ARH1= varIdent(form=~1|split.plot)
UN=lme(response~plot*split.plot, random=~1|subject, correlation=UN, weights=UN1, data=data, na.action=na.omit, contrasts=list(plot=contr.sum, split.plot=contr.sum), control=lmeControl(maxIter =6000, msMaxIter=6000, niterEM=2000, opt="optim"))
AR= update(UN, correlation=AR, weights=NULL)
ARH= update(UN, correlation=ARH, weights=ARH1)
CS= update(UN, correlation=CS, weights=NULL)
CAR=update(UN, correlation=CAR, weights=NULL)
bb=round(AIC(AR,ARH, CAR, CS,UN),4)
cc=round(BIC(AR,ARH, CAR, CS,UN),4)
ee=round(c(logLik(AR),logLik(ARH),logLik(CAR),logLik(CS),logLik(UN)),4)
LogLik=as.numeric(ee)
ee=data.frame(ee)
Structures=c("AR=Autoregressive","ARH=Heterogenius Autoregressive", "CAR=Continuous Autoregressive Process", "CS=Compound Symetry", "UN=Unstructured") 
Structures=data.frame(Structures)
f=data.frame(Structures, bb,cc[2],LogLik)
rownames(f)=NULL
test3<-summary(glht (m1,  linfct = mcp(treatments ="Tukey"), df=df2))
l=list(b, qa, s,b1,b2,b3,a11, a22, c, list(test1,cld(test1)), list(test2,cld(test2)), r1,r2, list(test3, cld(test3)),f)
names(l)=c("Marginal Anova = Type III Anova of SAS ", "Parameters of model fitting", "Normality test ", "Homogeneity of variances (plot)", "Homogeneity of variances (split.plot)", "Homogeneity of variances (treatments)","Adjusted Means (plot)", "Adjusted Means (split.plot)", "Adjusted Means (interaction)","Test plot","Test split.plot", "Test for plot in levels of the split.plot","Test for split.plot in levels of the plot", "Multiple comparison test (treatments)", "Evaluated Structures")
return(l)
}
,
f2<-function(data, cov){ 
gg<-cov
names(data)<-c("plot","block","split.plot","response")
data<-data.frame(plot=factor(data$plot), block=factor(data$block), split.plot=factor(data$split.plot), response=data$response)
subject<-interaction(data$plot,data$block)
treatments<-interaction(data$plot,data$split.plot)
data<-data.frame(data, subject,treatments)
UN<-corSymm(form=~1|subject)
AR<-corAR1(form=~1|subject)
ARH<-corAR1(form=~1|subject)
CS<- corCompSymm (form=~1|subject) 
CAR<-corCAR1(form=~split.plot|subject)
o1<-list(AR,ARH, CAR, CS, UN)
cor<-o1[[gg]]
UN1<-varIdent(form=~1|split.plot)
AR1<-NULL
ARH1<- varIdent(form=~1|split.plot)
CS1<-NULL
CAR1<-NULL

o2<-list(AR1,ARH1, CAR1, CS1, UN1)
var<-o2[[gg]]
m<-lme(response~plot*split.plot+block, random=~1|subject, correlation=cor, weights=var, data<-data, na.action=na.omit, contrasts=list(plot=contr.sum, split.plot=contr.sum, block=contr.sum), control=lmeControl(maxIter =6000, msMaxIter=6000, niterEM=2000, opt="optim"))
b<-anova(m, type="marginal")[-1,]
m1<-lme(response~-1+ treatments+block, random=~1|subject, data=data, na.action=na.omit, contrasts=list(treatments=contr.sum, block=contr.sum), correlation=cor, weights=var, control=lmeControl(maxIter =6000, msMaxIter=6000, niterEM=2000, opt="optim"))
am<-fixef(m1)[1:nlevels(data$ treatments)]
c<-data.frame(levels(treatments), am, sqrt(diag(vcov(m1)))[1:nlevels(data$ treatments)])
colnames(c)<-c("plot*split.plot","Adjust Means", "Standart.Error")
rownames(c)<-NULL
df1<- anova(m) $"denDF"[2]
df2<- anova(m) $"denDF"[1]
residuals<-resid(m)
s<-shapiro.test(residuals)
data2<-na.omit(data)
b1<- bartlett.test(residuals~plot, data=data2)
b2<- bartlett.test(residuals~split.plot, data=data2)
b3<- bartlett.test(residuals~treatments, data=data2)
mm1<-lme(response~-1+plot*split.plot+block, random=~1|subject, data=data, na.action=na.omit, contrasts=list(plot=contr.sum, split.plot=contr.sum, block=contr.sum), correlation=cor, weights=var, control=lmeControl(maxIter =6000, msMaxIter=6000, niterEM=2000, opt="optim"))
mm2<-lme(response~-1+split.plot*plot+block, random=~1|subject, data=data, na.action=na.omit, contrasts=list(split.plot=contr.sum, plot=contr.sum, block=contr.sum), correlation=cor, weights=var, control=lmeControl(maxIter =6000, msMaxIter=6000, niterEM=2000, opt="optim"))
a11<-data.frame(levels(data$plot), fixef(mm1)[1:nlevels(data$plot)], sqrt(diag(vcov(mm1)))[1:nlevels(data$plot)])
a22<-data.frame(levels(data$split.plot), fixef(mm2)[1:nlevels(data$split.plot)], sqrt(diag(vcov(mm2)))[1:nlevels(data$split.plot)])
colnames(a11)<-c("plot","Adjust Means", "Standart.Error")
rownames(a11)<-NULL
colnames(a22)<-c("split.plot","Adjust Means", "Standart.Error")
rownames(a22)<-NULL
test1<-summary(kp(glht (mm1,  linfct = mcp(plot = "Tukey"), df=df1)))
test2<-summary(kp(glht (mm2,  linfct = mcp(split.plot = "Tukey"), df=df2)))
ai<-as.numeric(AIC(m)); bi<-as.numeric(BIC(m)); log=as.numeric(logLik(m)); 
qa=data.frame(ai,bi,log)
colnames(qa)<-c("AIC","BIC", "LogLik")
rownames(qa)<-NULL
teste3<-glht (m1,  linfct = mcp(treatments="Tukey"))
mc<-teste3$linfct
x<-factor(data$plot)
y<-factor(data$split.plot)
x<-levels(x)
y<-levels(y)
x<-factor(x)
y<-factor(y)
x<-reorder(x,c(nlevels(x):1))
y<-reorder(y, c(nlevels(y):1))
x1<-lapply(levels(x), factor)
y1<-lapply(levels(y), factor)
h1<-lapply(x1, function(xi){interaction(xi,y)})
h2<-lapply(y1, function(yi){interaction(x,yi)})
l1<- lapply(h1, levels)
l2<- lapply(h2, levels)
aux1<- lapply(l1, function(li){combn(li,2)})
aux2<- lapply(l2, function(li){combn(li,2)})
w1 <- lapply(aux1, function(auxi) {apply(auxi, 2, paste, collapse = " - ")})
w2 <- lapply(aux2, function(auxi) {apply(auxi, 2, paste, collapse = " - ")})
mc<-data.frame(mc)
c1<-lapply(w1, function(w1i) {        mc[w1i,]    })
c2<-lapply(w2, function(w2i) {        mc[w2i,]    })
c1 <- lapply(c1, as.matrix, ncol=length(coef(m)))
c2 <- lapply(c2, as.matrix, ncol=length(coef(m)))
r1<-lapply(c2, function(c2i) { summary(glht(m1,  linfct =c2i, df=df2)) })
r2<-lapply(c1, function(c1i) { summary(glht(m1,  linfct =c1i, df=df2)) })
UN=corSymm(form=~1|subject)
AR=corAR1(form=~1|subject)
ARH=corAR1(form=~1|subject)
CS= corCompSymm (form=~1|subject) 
CAR<-corCAR1(form=~split.plot|subject)
UN1=varIdent(form=~1|split.plot)
ARH1= varIdent(form=~1|split.plot)
UN=lme(response~plot*split.plot+block, random=~1|subject, correlation=UN, weights=UN1, data=data, na.action=na.omit, contrasts=list(plot=contr.sum, split.plot=contr.sum, block=contr.sum), control=lmeControl(maxIter =6000, msMaxIter=6000, niterEM=2000, opt="optim"))
AR= update(UN, correlation=AR, weights=NULL)
ARH= update(UN, correlation=ARH, weights=ARH1)
CS= update(UN, correlation=CS, weights=NULL)
CAR=update(UN, correlation=CAR, weights=NULL)
bb=round(AIC(AR,ARH, CAR, CS,UN),4)
cc=round(BIC(AR,ARH, CAR, CS,UN),4)
ee=round(c(logLik(AR),logLik(ARH),logLik(CAR),logLik(CS),logLik(UN)),4)
LogLik=as.numeric(ee)
ee=data.frame(ee)
Structures=c("AR=Autoregressive","ARH=Heterogenius Autoregressive", "CAR=Continuous Autoregressive Process", "CS=Compound Symetry", "UN=Unstructured") 
Structures=data.frame(Structures)
f=data.frame(Structures, bb,cc[2],LogLik)
rownames(f)=NULL
test3<-summary(glht (m1,  linfct = mcp(treatments ="Tukey"), df=df2))
l=list(b, qa, s,b1,b2,b3,a11, a22, c, list(test1,cld(test1)), list(test2,cld(test2)), r1,r2, list(test3, cld(test3)),f)
names(l)=c("Marginal Anova = Type III Anova of SAS ", "Parameters of model fitting", "Normality test ", "Homogeneity of variances (plot)", "Homogeneity of variances (split.plot)", "Homogeneity of variances (treatments)","Adjusted Means (plot)", "Adjusted Means (split.plot)", "Adjusted Means (interaction)","Test plot","Test split.plot", "Test for plot in levels of the split.plot","Test for split.plot in levels of the plot", "Multiple comparison test (treatments)", "Evaluated Structures")
return(l)
}

,
f3<-function(data, cov){ 
gg<-cov
names(data)<-c("plot","col", "row", "split.plot","response")
data<-data.frame(plot=factor(data$plot), col=factor(data$col), row=factor(data$row), split.plot=factor(data$split.plot), response=data$response)
subject<-interaction(data$plot,data$col)
treatments<-interaction(data$plot,data$split.plot)
data<-data.frame(data, subject,treatments)
UN<-corSymm(form=~1|subject)
AR<-corAR1(form=~1|subject)
ARH<-corAR1(form=~1|subject)
CS<- corCompSymm (form=~1|subject) 
CAR<-corCAR1(form=~split.plot|subject)
o1<-list(AR,ARH, CAR, CS, UN)
cor<-o1[[gg]]
UN1<-varIdent(form=~1|split.plot)
AR1<-NULL
ARH1<- varIdent(form=~1|split.plot)
CS1<-NULL
CAR1<-NULL
o2<-list(AR1,ARH1, CAR1, CS1, UN1)
var<-o2[[gg]]
m<-lme(response~plot*split.plot+col+row, random=~1|subject, correlation=cor, weights=var, data<-data, na.action=na.omit, contrasts=list(plot=contr.sum, split.plot=contr.sum, col=contr.sum, row=contr.sum), control=lmeControl(maxIter =6000, msMaxIter=6000, niterEM=2000, opt="optim"))
b<-anova(m, type="marginal")[-1,]
m1<-lme(response~-1+ treatments+col+row, random=~1|subject, data=data, na.action=na.omit, contrasts=list(treatments=contr.sum, col=contr.sum, row=contr.sum), correlation=cor, weights=var, control=lmeControl(maxIter =6000, msMaxIter=6000, niterEM=2000,  opt="optim"))
am<-fixef(m1)[1:nlevels(data$ treatments)]
c<-data.frame(levels(treatments), am, sqrt(diag(vcov(m1)))[1:nlevels(data$ treatments)])
colnames(c)<-c("plot*split.plot","Adjust Means", "Standart.Error")
rownames(c)<-NULL
df1<- anova(m) $"denDF"[2]
df2<- anova(m) $"denDF"[1]
residuals<-resid(m)
s<-shapiro.test(residuals)
data2<-na.omit(data)
b1<- bartlett.test(residuals~plot, data=data2)
b2<- bartlett.test(residuals~split.plot, data=data2)
b3<- bartlett.test(residuals~treatments, data=data2)
mm1<-lme(response~-1+plot*split.plot+col+row, random=~1|subject, data=data, na.action=na.omit, contrasts=list(plot=contr.sum, split.plot=contr.sum, col=contr.sum, row=contr.sum), correlation=cor, weights=var, control=lmeControl(maxIter =6000, msMaxIter=6000, niterEM=2000, opt="optim"))
mm2<-lme(response~-1+split.plot*plot+col+row, random=~1|subject, data=data, na.action=na.omit, contrasts=list(split.plot=contr.sum, plot=contr.sum, col=contr.sum, row=contr.sum), correlation=cor, weights=var, control=lmeControl(maxIter =6000, msMaxIter=6000, niterEM=2000, opt="optim"))
a11<-data.frame(levels(data$plot), fixef(mm1)[1:nlevels(data$plot)], sqrt(diag(vcov(mm1)))[1:nlevels(data$plot)])
a22<-data.frame(levels(data$split.plot), fixef(mm2)[1:nlevels(data$split.plot)], sqrt(diag(vcov(mm2)))[1:nlevels(data$split.plot)])
colnames(a11)<-c("plot","Adjust Means", "Standart.Error")
rownames(a11)<-NULL
colnames(a22)<-c("split.plot","Adjust Means", "Standart.Error")
rownames(a22)<-NULL
test1<-summary(kp(glht (mm1,  linfct = mcp(plot = "Tukey"), df=df1)))
test2<-summary(kp(glht (mm2,  linfct = mcp(split.plot = "Tukey"), df=df2)))
ai<-as.numeric(AIC(m)); bi<-as.numeric(BIC(m)); log=as.numeric(logLik(m)); 
qa=data.frame(ai,bi,log)
colnames(qa)<-c("AIC","BIC", "LogLik")
rownames(qa)<-NULL
teste3<-glht (m1,  linfct = mcp(treatments="Tukey"))
mc<-teste3$linfct
x<-factor(data$plot)
y<-factor(data$split.plot)
x<-levels(x)
y<-levels(y)
x<-factor(x)
y<-factor(y)
x<-reorder(x,c(nlevels(x):1))
y<-reorder(y, c(nlevels(y):1))
x1<-lapply(levels(x), factor)
y1<-lapply(levels(y), factor)
h1<-lapply(x1, function(xi){interaction(xi,y)})
h2<-lapply(y1, function(yi){interaction(x,yi)})
l1<- lapply(h1, levels)
l2<- lapply(h2, levels)
aux1<- lapply(l1, function(li){combn(li,2)})
aux2<- lapply(l2, function(li){combn(li,2)})
w1 <- lapply(aux1, function(auxi) {apply(auxi, 2, paste, collapse = " - ")})
w2 <- lapply(aux2, function(auxi) {apply(auxi, 2, paste, collapse = " - ")})
mc<-data.frame(mc)
c1<-lapply(w1, function(w1i) {        mc[w1i,]    })
c2<-lapply(w2, function(w2i) {        mc[w2i,]    })
c1 <- lapply(c1, as.matrix, ncol=length(coef(m)))
c2 <- lapply(c2, as.matrix, ncol=length(coef(m)))
r1<-lapply(c2, function(c2i) { summary(glht(m1,  linfct =c2i, df=df2)) })
r2<-lapply(c1, function(c1i) { summary(glht(m1,  linfct =c1i, df=df2)) })
UN=corSymm(form=~1|subject)
AR=corAR1(form=~1|subject)
ARH=corAR1(form=~1|subject)
CS= corCompSymm (form=~1|subject) 
CAR<-corCAR1(form=~split.plot|subject)
UN1=varIdent(form=~1|split.plot)
ARH1= varIdent(form=~1|split.plot)
UN=lme(response~plot*split.plot+col+row, random=~1|subject, correlation=UN, weights=UN1, data=data, na.action=na.omit, contrasts=list(plot=contr.sum, split.plot=contr.sum, col=contr.sum, row=contr.sum), control=lmeControl(maxIter =6000, msMaxIter=6000, niterEM=2000, opt="optim"))
AR= update(UN, correlation=AR, weights=NULL)
ARH= update(UN, correlation=ARH, weights=ARH1)
CS= update(UN, correlation=CS, weights=NULL)
CAR=update(UN, correlation=CAR, weights=NULL)
bb=round(AIC(AR,ARH, CAR, CS,UN),4)
cc=round(BIC(AR,ARH, CAR, CS,UN),4)
ee=round(c(logLik(AR),logLik(ARH),logLik(CAR),logLik(CS),logLik(UN)),4)
LogLik=as.numeric(ee)
ee=data.frame(ee)
Structures=c("AR=Autoregressive","ARH=Heterogenius Autoregressive", "CAR=Continuous Autoregressive Process", "CS=Compound Symetry", "UN=Unstructured") 
Structures=data.frame(Structures)
f=data.frame(Structures, bb,cc[2],LogLik)
rownames(f)=NULL
test3<-summary(glht (m1,  linfct = mcp(treatments ="Tukey"), df=df2))
l=list(b, qa, s,b1,b2,b3,a11, a22, c, list(test1,cld(test1)), list(test2,cld(test2)), r1,r2, list(test3, cld(test3)),f)
names(l)=c("Marginal Anova = Type III Anova of SAS ", "Parameters of model fitting", "Normality test ", "Homogeneity of variances (plot)", "Homogeneity of variances (split.plot)", "Homogeneity of variances (treatments)","Adjusted Means (plot)", "Adjusted Means (split.plot)", "Adjusted Means (interaction)","Test plot","Test split.plot", "Test for plot in levels of the split.plot","Test for split.plot in levels of the plot", "Multiple comparison test (treatments)", "Evaluated Structures")
return(l)
}
)
g=res[[design]]
g(data,cov)
}
