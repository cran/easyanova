p.plot=function(data, ylab="", xlab="", col.lines="red", cex.axis=0.7,cex=0.9 ,col.text="dark green",family="Times", bg="white",...)
{
dados=data
par(family=family, bg=bg)

  d=if (is.data.frame(dados)){dados} else {dados$"M"}
  d=if (is.data.frame(d)){d} else {dados$"f"}
    dados=if (is.data.frame(d)){d} else {dados$"s"}
       d2=dados
      
        ro=round(d2[[3]],3)
        ro=format(ro)
     p1=paste('p=',ro )
     p2=paste(d2[[1]])
                        
     ll=length(p1)+1
    x=d2[[3]];y=1:length(d2[[3]])
    plot(x,y,axes=F, ylim=c(0,(max(y)+1)), xlim=c(-0.25,1) ,pch="", xlab=xlab, ylab=ylab, ...)
    segments(x0=rep(0,length(d2[[3]])),y0=y,x1=x, col=col.lines) 
axis(1,c(0.0,0.05,0.10,0.2,0.4,0.6,0.8,1.0),las=2, srt=45,  ...)
    abline(v=0.05, lty=3); abline(v=0.10, lty=2); abline(v=0, lty=1)
text(-0.05,y, p2, cex=cex, col=col.text, adj=1 ,...)


}





