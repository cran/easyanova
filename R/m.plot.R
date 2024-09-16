m.plot=function(data,  s="sd",test="tukey", family="Times", bg="white",  cex.text=0.7, 
cex=0.5,bar.order=2, decreasing=TRUE, xlab="treatments", ylab="",pch=19, ...)
{
dados=data
par(family=family, bg=bg)
                 
d=if (is.data.frame(dados)){dados} else {dados$"M"} 
d=if (is.data.frame(d)){d} else {dados$"A"} 
dados=if (is.data.frame(d)){d} else {dados$"s"} 

s=if(names(dados)[3]=="sd"){s} else {"standard.error"} 

   dados=data.frame(dados[,c(1,2)],dados[s],dados[test])
   inn=suppressWarnings(ifelse(is.na(as.numeric(dados[,1])[1])==TRUE, 2,1))
   d2=dados
   dados[,1]=suppressWarnings(as.numeric(dados[,1]))
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


d=1:length(m)
d1=(d[1]-1)
d2=(max(d)+1)

ra=(ml-mli)/10

sl=seq(mli,ml, by=ra)
me=mean(m);esp=ifelse(me>9,1,2);esp=ifelse(me<0,3,esp);esp=ifelse(me>99,0,esp)
sl=round(sl,esp)

plot(m~d,ylim=c(mli,ml),xlab=xlab, ylab=ylab, cex=cex, pch=pch, xlim=c(d1,d2), bty="n", axes=FALSE, ...)
arrows(d,mi, d, md, angle=90, code=0, length = 0.10,...) 
text(d, md, pp,cex=cex.text,pos=3, col="black")
  axis(1, d,labels=dados[,1], lwd=0, las=2, cex.axis=0.8, ...)  
  axis(2, sl, las=2, cex.axis=0.8, ...)    
    


}





