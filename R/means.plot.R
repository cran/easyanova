means.plot=function(data, plot=1, s=1,test=1, legend=TRUE, letters=TRUE,
family="Times", bg="white",  cex.names=0.8, cex.text=0.7, cex.legend=1, 
bar.order=2, decreasing=TRUE, alpha=0.05,cex=0.5, pch=19,  ...)
{
dados=data

par(family=family, bg=bg)
# sd sem

ses=c(3,4)
s=ses[s]

#tukey snk duncan t scott knott
tes=c(7,8,9,10,11)
test=tes[test]

# bar.order 1=tratamentos 2=medias 


pb=function(dados)
 {
     

  
                   
                    ddd=as.numeric(dados[[1]][[5]][1])
                         pff=ifelse(is.na(ddd),000, ddd)
pf=paste("p-value (anova)=",format(round(pff,4),nsmall=4) )   
     p2=pf

   dados=dados[[2]][,c(1,2,s,test)]
   inn=suppressWarnings(ifelse(is.na(as.numeric(dados[,1])[1])==TRUE, 2,1))
   d2=dados
   dados[,1]=as.numeric(dados[,1])
	d1=dados
   ld=list(d1,d2)
   dados=ld[[inn]]
   no=c(1,2)
   no=no[bar.order]
   dados=dados[order(dados[,no], decreasing=decreasing),]
     
     l=dados[,1]
     m=round(dados[,2],2); m=signif(m)
     sd=dados[,3]
     let=dados[,4]

     
     md=m+sd
          mi=m-sd
     ml=max(md)*1.5
     
	     mf=format(m)
	     pp=paste(mf,let)

	     
	     le=c("",p2)
	     pp=list("",pp)
	     
nu=ifelse(legend==TRUE,2,1)
nuu=ifelse(letters==TRUE,2,1)
     
     barras=barplot(m, ylim=c(0,ml), cex.names=cex.names, names=l,space=0.1, ...)
     arrows(barras,mi, barras, md, angle=90, code=3, length = 0.10)
     text(barras, md, pp[[nuu]],cex=cex.text,pos=3, col="black")
       legend(
  'topright', ncol = 1, title =NULL,
  legend = le[nu], cex=cex.legend, bty="n")
     
     
}


pb1=function(dados)
 {
     

  
                   
                    ddd=as.numeric(dados[[1]][[5]][1])
                         pff=ifelse(is.na(ddd),000, ddd)
pf=paste("p-value (anova)=",format(round(pff,4),nsmall=4) )   
     p2=pf

   dados=dados[[2]][,c(1,2,s,test)]
   inn=suppressWarnings(ifelse(is.na(as.numeric(dados[,1])[1])==TRUE, 2,1))
   d2=dados
   dados[,1]=as.numeric(dados[,1])
	d1=dados
   ld=list(d1,d2)
   dados=ld[[inn]]
   no=c(1,2)
   no=no[bar.order]
   dados=dados[order(dados[,no], decreasing=decreasing),]
     
     l=dados[,1]
     m=round(dados[,2],2); m=signif(m)
     sd=dados[,3]
     let=dados[,4]

     
     md=m+sd
          mi=m-sd
          mli=min(mi)-(mean(sd)/4)
     ml=max(md)+(mean(sd)/4)
     
	     mf=format(m)
	     pp=paste(mf,let)

	     
	     le=c("",p2)
	     pp=list("",pp)
	     
nu=ifelse(legend==TRUE,2,1)
nuu=ifelse(letters==TRUE,2,1)

d=1:length(m)
d1=(d[1]-1)
d2=(max(d)+1)

ra=(ml-mli)/10

sl=seq(mli,ml, by=ra)
me=mean(m);esp=ifelse(me>9,1,2);esp=ifelse(me<0,3,esp);esp=ifelse(me>99,0,esp)
sl=round(sl,esp)

plot(m~d,ylim=c(mli,ml),xlab="", cex=cex, pch=pch, cex.names=cex.names, names=l,space=0.1, xlim=c(d1,d2), bty="n", axes=FALSE, ...)
arrows(d,mi, d, md, angle=90, code=0, length = 0.10,...) 
text(d, md, pp[[nuu]],cex=cex.text,pos=3, col="black")
       legend(  'topright', ncol = 1, title =NULL,
  legend = le[nu], cex=cex.legend, bty="n") 
  axis(1, d,labels=dados[,1], lwd=0, las=2, cex.axis=0.8, ...)  
  axis(2, sl, las=2, cex.axis=0.8, ...)    
    

   
          
}


pconf=function(dados)
{

 fm=function(dados){
 ma=dados[[2]]
        ma=data.frame(ma,co=ma[,1])
        ma=ma[order(ma[,2], decreasing=TRUE),]
        jj=ma[,2]
        auxj <- combn(jj, 2)
        yi=qtukey((1-alpha),length(ma[,2]),dados[[1]][length(dados[[1]][,1]),1])
        jjj=ma$sem^2
        auxjj <- combn(jjj, 2)
        si=sqrt((auxjj[1,]+auxjj[2,])/2)
        fs=function(ns){s=2:ns;return(s)}
        ns=length(ma[,2]):2
        se=sapply(ns, fs)
        ns=unlist(se)
                yii=qtukey((1-alpha),ns,dados[[1]][length(dados[[1]][,1]),1])
          yiii=qtukey((1-alpha)^(ns-1), ns, dados[[1]][length(dados[[1]][,1]),1])
yiiii=qt((1-alpha),dados[[1]][length(dados[[1]][,1]),1])
        yt=yi*si
        ysnk=yii*si
        yd=yiii*si
        ytt=yiiii*(si*sqrt(2))
        confs=data.frame(yt,ysnk,yd,ytt)
        return(confs)}
      
       
        q=fm(dados)
        
        
set=q[test-6]
dados=dados[[3]][,c(1,2)]
mi=round(dados$contrast-set,2);mi=mi[,1]
md=round(dados$contrast+set,2);md=md[,1]
dados=data.frame(d=1:length(dados[,1]), dados)

mdm=(max(md)+(max(md)*0.2))
mim=(min(mi))
ra=(mdm-mim)/10

sl=seq(mim,mdm, by=ra)
me=mean(dados[,3]);me=abs(me);esp=ifelse(me>9,1,2);esp=ifelse(me<0,3,esp);esp=ifelse(me>99,0,esp)
sl=round(sl,esp)

plot(dados$contrast~dados$d, axes=F, cex=cex, pch=pch,ylim=c(min(mi),(max(md)+(max(md)*0.2))), xlim=c(0,(length(dados$contrast)+1)), xlab="", ...)
axis(1, dados$d,dados$pair, lwd=0, ...)
axis(2, sl, las=2, ...)
arrows(dados$d,mi, dados$d, md, angle=90, code=0, length = 0.10,...)
abline(h=0,lty=2)

dat=data.frame(dados$pair,dados$contrast,mi,md);names(dat)=c("pair","contrast", "ICinf","ICsup")
return(dat)

}

##### list ###### 


fpb1=function(dados)
{
pb=function(i)
 {
        
 n=names(dados)
 n=n[[i]]
     d=dados[[i]]
        d2=dados[[i]][[3]]
         dados=d  
              ddd=as.numeric(dados[[1]][[5]][1])
                         pff=ifelse(is.na(ddd),000, ddd)
pf=paste("p-value (anova)=",format(round(pff,4),nsmall=4) )   
     p2=pf

   dados=dados[[2]][,c(1,2,s,test)]
   inn=suppressWarnings(ifelse(is.na(as.numeric(dados[,1])[1])==TRUE, 2,1))
   d2=dados
   dados[,1]=as.numeric(dados[,1])
	d1=dados
   ld=list(d1,d2)
   dados=ld[[inn]]
   no=c(1,2)
   no=no[bar.order]
   dados=dados[order(dados[,no], decreasing=decreasing),]
     
     l=dados[,1]
     m=round(dados[,2],2); m=signif(m)
     sd=dados[,3]
     let=dados[,4]

     
     md=m+sd
          mi=m-sd
          mli=min(mi)-(mean(sd)/4)
     ml=max(md)+(mean(sd)/4)
     
	     mf=format(m)
	     pp=paste(mf,let)

	     
	     le=c("",p2)
	     pp=list("",pp)
	     
nu=ifelse(legend==TRUE,2,1)
nuu=ifelse(letters==TRUE,2,1)

d=1:length(m)
d1=(d[1]-1)
d2=(max(d)+1)

ra=(ml-mli)/10

sl=seq(mli,ml, by=ra)

me=mean(m);esp=ifelse(me>9,1,2);esp=ifelse(me<0,3,esp);esp=ifelse(me>99,0,esp)
sl=round(sl,esp)

plot(m~d,ylim=c(mli,ml),xlab="", cex=cex, pch=pch, ylab=n,cex.names=cex.names, names=l,space=0.1, xlim=c(d1,d2), bty="n", axes=FALSE, ...)
arrows(d,mi, d, md, angle=90, code=0, length = 0.10, ...) 
text(d, md, pp[[nuu]],cex=cex.text,pos=3, col="black")
       legend(  'topright', ncol = 1, title =NULL,
  legend = le[nu], cex=cex.legend, bty="n") 
  axis(1, d,labels=dados[,1], lwd=0, las=2, cex.axis=0.8, ...)  
  axis(2, sl, las=2, cex.axis=0.8, ...)    
    
               
   
}

u=ifelse(class(dados[[1]])=="data.frame",1,length(dados))
i=1:u


pdf("Rplot means.pdf", family=family, bg=bg)
lapply(i, pb)
 dev.off()
 
 }
 
 fpb=function(dados)
{
pb=function(i)
 {
 
        
 n=names(dados)
 n=n[[i]]
     d=dados[[i]]
        d2=dados[[i]][[3]]
           
               
     ddd=as.numeric(d[[1]][[5]][1])
                         pff=ifelse(is.na(ddd),000, ddd)
pf=paste("p-value (anova)=",format(round(pff,4),nsmall=4) )   

     p2=pf
    

     
   d=d[[2]][,c(1,2,s,test)]
   inn=suppressWarnings(ifelse(is.na(as.numeric(d[,1])[1])==TRUE, 2,1))
   d2=d
   d[,1]=as.numeric(d[,1])
	d1=d
   ld=list(d1,d2)
   d=ld[[inn]]
   no=c(1,2)
   no=no[bar.order]
   d=d[order(d[,no], decreasing=decreasing),]
   
     l=d[,1]
     m=round(d[,2],2)
     sd=d[,3]
     let=d[,4]
     md=m+sd
          mi=m-sd
     ml=max(md)*1.5
     mf=format(m)
	     pp=paste(mf,let)

	     le=c("",p2)
	     pp=list("",pp)
	     
nu=ifelse(legend==TRUE,2,1)
nuu=ifelse(letters==TRUE,2,1)  
  
     
     barras=barplot(m, ylim=c(0,ml), ylab=n, cex.names=cex.names, names=l, space=0.1,  xlim=c(0,(length(m)+1)), ...)
     arrows(barras,mi, barras, md, angle=90, code=3, length = 0.10)
     text(barras, md, pp[[nuu]],cex=cex.text,pos=3, col="black")
       legend(
  'topright', ncol = 1, title =NULL,
  legend = le[nu], cex=cex.legend, bty="n")

     
     
}

u=ifelse(class(dados[[1]])=="data.frame",1,length(dados))
i=1:u


pdf("Rplot means.pdf", family=family, bg=bg)
lapply(i, pb)
 dev.off()
 
 }
 
 
 pconfl=function(dados)
{


pic=function(i){
n=names(dados)
 n=n[[i]]
dados=dados[[i]]
 fm=function(dados){
 ma=dados[[2]]
        ma=data.frame(ma,co=ma[,1])
        ma=ma[order(ma[,2], decreasing=TRUE),]
        jj=ma[,2]
        auxj <- combn(jj, 2)
        yi=qtukey((1-alpha),length(ma[,2]),dados[[1]][length(dados[[1]][,1]),1])
        jjj=ma$sem^2
        auxjj <- combn(jjj, 2)
        si=sqrt((auxjj[1,]+auxjj[2,])/2)
        fs=function(ns){s=2:ns;return(s)}
        ns=length(ma[,2]):2
        se=sapply(ns, fs)
        ns=unlist(se)
                yii=qtukey((1-alpha),ns,dados[[1]][length(dados[[1]][,1]),1])
          yiii=qtukey((1-alpha)^(ns-1), ns, dados[[1]][length(dados[[1]][,1]),1])
yiiii=qt((1-alpha),dados[[1]][length(dados[[1]][,1]),1])
        yt=yi*si
        ysnk=yii*si
        yd=yiii*si
        ytt=yiiii*(si*sqrt(2))
        confs=data.frame(yt,ysnk,yd,ytt)
        return(confs)}
      
       
        q=fm(dados)
        
        
set=q[test-6]
dados=dados[[3]][,c(1,2)]
mi=round(dados$contrast-set,2);mi=mi[,1]
md=round(dados$contrast+set,2);md=md[,1]
dados=data.frame(d=1:length(dados[,1]), dados)

mdm=(max(md)+(max(md)*0.2))
mim=(min(mi))
ra=(mdm-mim)/10

sl=seq(mim,mdm, by=ra)
me=mean(dados[,3]);me=abs(me);esp=ifelse(me>9,1,2);esp=ifelse(me<0,3,esp);esp=ifelse(me>99,0,esp)
sl=round(sl,esp)


plot(dados$contrast~dados$d, pch=pch,cex=cex,axes=F, ylim=c(min(mi),(max(md)+(max(md)*0.2))), xlim=c(0,(length(dados$contrast)+1)),ylab=n, xlab="", ...)
axis(1, dados$d,dados$pair, lwd=0, ...)
axis(2, sl, las=2,...)
arrows(dados$d,mi, dados$d, md, angle=90, code=0, length = 0.10,...)
abline(h=0,lty=2)

dat=data.frame(dados$pair,dados$contrast,mi,md);names(dat)=c("pair","contrast", "ICinf","ICsup")
return(dat)

}

ui=ifelse(class(dados[[1]])=="data.frame",1,length(dados))
i=1:ui
pdf("Rplot confint.pdf", family=family, bg=bg)
lapply(i, pic)
 dev.off()
 
 l=lapply(i, pic); names(l)=names(dados);return(l)

}


 
 


u=ifelse(class(dados[[1]])=="data.frame",1,2)
uc=plot

lf=list(list(pb,pb1,pconf), list(fpb,fpb1,pconfl))

suppressWarnings(lf[[u]][[uc]](dados))

}






