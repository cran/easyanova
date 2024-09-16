ic.plot=function(data,col="dark green", cex=0.5, xlab="constrats",pch=19,family="Times", bg="white",...)

{
dados=data
par(family=family, bg=bg)
md=dados[,4];mi=dados[,3]
  mdm=(max(md)+(max(md)*0.2))
mim=(min(mi))
ra=(mdm-mim)/10

sl=seq(mim,mdm, by=ra)
me=mean(dados[,2]);me=abs(me);esp=ifelse(me>9,1,2);esp=ifelse(me<0,3,esp);esp=ifelse(me>99,0,esp)
sl=round(sl,esp)
d=1:length(dados[,2])

plot(dados$contrast~d, xlab=xlab,ylab="",col=col,cex=cex, pch=pch,ylim=c(min(mi),(max(md)+(max(md)*0.2))), xlim=c(0,(length(dados$contrast)+1)),axes=F,  ...)
axis(1, d,dados$pair, lwd=0, ...)
axis(2, sl, las=2, ...)
arrows(d,mi, d, md, angle=90, code=0, length = 0.10, col=col,...)
abline(h=0,lty=2)
}
	




