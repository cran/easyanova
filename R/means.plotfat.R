means.plotfat=function(data, plot=1, s=1,test=1, legend=TRUE, letters=TRUE,family="Times", bg="white",  cex.names=0.8, cex.text=0.7, cex.legend=1,  bar.order=1,decreasing=TRUE, ...)

{
dados=data
fat=plot

par(family=family, bg=bg)
# sd sem

ses=c(3,4)
s=ses[s]

#tukey snk duncan t scott knott
tes=c(5,6,7,8,9)
test=tes[test]

# bar.order 1=tratamentos 2=medias 


pb1=function(dados)
 {
     
da=as.data.frame(dados[[1]])
  
                   
                    ddd=as.numeric(da[[length(da)]][fat])
                         pff=ifelse(is.na(ddd),000, ddd)
pf=paste("p-value (anova)=",format(round(pff,4),nsmall=4) )   
     p2=pf
     
     fc=c(2,4,6,8)
     fc=fc[fat]

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
	     #"#E6E6E6"
	     
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

pb2=function(dados)
 {
  da=as.data.frame(dados[[1]])
  
                   
                    ddd=as.numeric(da[[length(da)]][fat])
                         pff=ifelse(is.na(ddd),000, ddd)
pf=paste("p-value (anova)=",format(round(pff,4),nsmall=4) )   
     p2=pf
     
     fc=c(2,4,6,8)
     fc=fc[fat]

   dados=dados[[fc]][,c(1,2,s,test)]
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
	     #"#E6E6E6"
	     
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

pb3=function(dados)
 {
     
da=as.data.frame(dados[[1]])
  
           nn=rownames(da)
           nn=nn[3] 
           nn=if (nn=="row"){5} else {nn}  
                      nn=if (nn=="rows"){5} else {nn}  
                      nn=if (nn=="block"){4} else {nn} 
                                                                  nn=if (nn=="blocks"){4} else {nn}  
                                            nn=if (is.numeric(nn)){nn} else {3}                   
                    ddd=as.numeric(da[[length(da)]][nn])
                         pff=ifelse(is.na(ddd),000, ddd)
pf=paste("p-value (anova)=",format(round(pff,4),nsmall=4) )   
     p2=pf
     
     fc=c(2,4,6,8)
     fc=fc[fat]

       
        
     
d=dados

f=function(i){dff=d[[6]][[i]][,2];return(dff)}
i=1:length(d[[6]])
lapply(i,f)
l=lapply(i,f)
l=matrix(unlist(l), ncol=length(d[[6]]))

ff=function(i){dff=d[[6]][[i]][,1];return(dff)}
i=1:length(d[[6]])
lapply(i,ff)
ll=lapply(i,ff)
ll=unlist(ll)

ff=function(i){dff=d[[6]][[i]][,test];return(dff)}
i=1:length(d[[6]])
lapply(i,ff)
let=lapply(i,ff)
let=unlist(let)

ff=function(i){dff=d[[6]][[i]][,2];return(dff)}
i=1:length(d[[6]])
m=lapply(i,ff)
m=unlist(m)
m=round(m,2); m=signif(m)

ff=function(i){dff=d[[6]][[i]][,s];return(dff)}
i=1:length(d[[6]])
sd=lapply(i,ff)
sd=unlist(sd)
    
     md=m+sd
          mi=m-sd
     ml=max(md)*1.5
     
	     mf=format(m)
	     pp=paste(mf,let)
	     #"#E6E6E6"
	     
	     le=c("",p2)
	     pp=list("",pp)
	     
nu=ifelse(legend==TRUE,2,1)
nuu=ifelse(letters==TRUE,2,1)



barras=barplot(l, besid=T, names=ll, ylim=c(0,ml), ...)
     arrows(barras,mi, barras, md, angle=90, code=3, length = 0.10)
     text(barras, md, pp[[nuu]],cex=cex.text,pos=3, col="black")
       legend('topright', ncol = 1, title =NULL,
  legend = le[nu], cex=cex.legend, bty="n")
  
  }
  
  pb4=function(dados)
 {
     
da=as.data.frame(dados[[1]])
  
           nn=rownames(da)
           nn=nn[3] 
           nn=if (nn=="row"){5} else {nn}  
                                 nn=if (nn=="rows"){5} else {nn}  
                      nn=if (nn=="block"){4} else {nn} 
                                                                  nn=if (nn=="blocks"){4} else {nn}  
                                            nn=if (is.numeric(nn)){nn} else {3}                   
                    ddd=as.numeric(da[[length(da)]][nn])
                                             pff=ifelse(is.na(ddd),000, ddd)
pf=paste("p-value (anova)=",format(round(pff,4),nsmall=4) )   
     p2=pf
     
     fc=c(2,4,6,8)
     fc=fc[fat]

       
        
     
d=dados

f=function(i){dff=d[[8]][[i]][,2];return(dff)}
i=1:length(d[[8]])
lapply(i,f)
l=lapply(i,f)
l=matrix(unlist(l), ncol=length(d[[8]]))

ff=function(i){dff=d[[8]][[i]][,1];return(dff)}
i=1:length(d[[8]])
lapply(i,ff)
ll=lapply(i,ff)
ll=unlist(ll)

ff=function(i){dff=d[[8]][[i]][,test];return(dff)}
i=1:length(d[[8]])
lapply(i,ff)
let=lapply(i,ff)
let=unlist(let)

ff=function(i){dff=d[[8]][[i]][,2];return(dff)}
i=1:length(d[[8]])
m=lapply(i,ff)
m=unlist(m)
m=round(m,2); m=signif(m)

ff=function(i){dff=d[[8]][[i]][,s];return(dff)}
i=1:length(d[[8]])
sd=lapply(i,ff)
sd=unlist(sd)
    
     md=m+sd
          mi=m-sd
     ml=max(md)*1.5
     
	     mf=format(m)
	     pp=paste(mf,let)
	     #"#E6E6E6"
	     
	     le=c("",p2)
	     pp=list("",pp)
	     
nu=ifelse(legend==TRUE,2,1)
nuu=ifelse(letters==TRUE,2,1)


barras=barplot(l, besid=T, names=ll, ylim=c(0,ml), ...)
     arrows(barras,mi, barras, md, angle=90, code=3, length = 0.10)
     text(barras, md, pp[[nuu]],cex=cex.text,pos=3, col="black")
       legend('topright', ncol = 1, title =NULL,
  legend = le[nu], cex=cex.legend, bty="n")
  
  }
     

pb5=function(dados)
 {
     

  da=as.data.frame(dados[[1]])
  
           nn=rownames(da)
           nn=nn[3] 
           nn=if (nn=="row"){5} else {nn} 
                                 nn=if (nn=="rows"){5} else {nn}   
                      nn=if (nn=="block"){4} else {nn} 
                                                                  nn=if (nn=="blocks"){4} else {nn}  
                                            nn=if (is.numeric(nn)){nn} else {3}                   
                    ddd=as.numeric(da[[length(da)]][nn])
                                             pff=ifelse(is.na(ddd),000, ddd)
pf=paste("p-value (anova)=",format(round(pff,4),nsmall=4) )   
     p2=pf
     
     fc=c(2,4,6,8)
     fc=fc[fat]

       
        
     
d=dados
r=dados

fletr=function(i){
ob=r[[6]][[i]][,1]
letr=unlist(strsplit(ob,"[.]"))
letr=matrix(letr, ncol=length(r[[8]]))
ma=data.frame(l=letr[1,],r[[6]][[i]][,-1])
 ma=ma[order(ma[,1], decreasing=FALSE),]
return(ma)
}
i=1:length(r[[6]])
la=lapply(i,fletr)

flet=function(i){
ob=r[[6]][[i]][,1]
letr=unlist(strsplit(ob,"[.]"))
letr=matrix(letr, ncol=length(r[[8]]))
ma=data.frame(l=letr[2,1])
 return(ma)
}
i=1:length(r[[6]])
lac=lapply(i,flet);lac=unlist(lac)

f=function(i){dff=la[[i]][,2];return(dff)}
i=1:length(la)
lapply(i,f)
l=lapply(i,f)
l=matrix(unlist(l), ncol=length(la))
rownames(l)=la[[1]][,1]
colnames(l)=lac

ff=function(i){dff=la[[i]][,1];return(dff)}
i=1:length(la)
lapply(i,ff)
ll=lapply(i,ff)
ll=unlist(ll)

ff=function(i){dff=la[[i]][,test];return(dff)}
i=1:length(la)
lapply(i,ff)
let=lapply(i,ff)
let=unlist(let)

ff=function(i){dff=la[[i]][,2];return(dff)}
i=1:length(la)
m=lapply(i,ff)
m=unlist(m)
m=round(m,2); m=signif(m)

ff=function(i){dff=la[[i]][,s];return(dff)}
i=1:length(la)
sd=lapply(i,ff)
sd=unlist(sd)
    
     md=m+sd
          mi=m-sd
     ml=max(md)*1.5
     
	     mf=format(m)
	     pp=paste(mf,let)
	     #"#E6E6E6"
	     
	     le=c("",p2)
	     pp=list("",pp)
	     
nu=ifelse(legend==TRUE,2,1)
nuu=ifelse(letters==TRUE,2,1)


barras=barplot(l, besid=T, legend.text =TRUE, ylim=c(0,ml),...)
     arrows(barras,mi, barras, md, angle=90, code=3, length = 0.10)
     text(barras, md, pp[[nuu]],cex=cex.text,pos=3, col="black")
       legend('topleft', ncol = 1, title =NULL,
  legend = le[nu], cex=cex.legend, bty="n")
  
  }
     
pb6=function(dados)
 {
     

  da=as.data.frame(dados[[1]])
  
           nn=rownames(da)
           nn=nn[3] 
           nn=if (nn=="row"){5} else {nn} 
                                 nn=if (nn=="rows"){5} else {nn}   
                      nn=if (nn=="block"){4} else {nn} 
                                                                                        nn=if (nn=="blocks"){4} else {nn}  
                                            nn=if (is.numeric(nn)){nn} else {3}                   
                    ddd=as.numeric(da[[length(da)]][nn])
                                             pff=ifelse(is.na(ddd),000, ddd)
pf=paste("p-value (anova)=",format(round(pff,4),nsmall=4) )   
     p2=pf
     
       
        
     
d=dados
r=dados

fletr=function(i){
ob=r[[8]][[i]][,1]
letr=unlist(strsplit(ob,"[.]"))
letr=matrix(letr, ncol=length(r[[6]]))
ma=data.frame(l=letr[2,],r[[8]][[i]][,-1])
 ma=ma[order(ma[,1], decreasing=FALSE),]
return(ma)
}
i=1:length(r[[8]])
la=lapply(i,fletr)

flet=function(i){
ob=r[[8]][[i]][,1]
letr=unlist(strsplit(ob,"[.]"))
letr=matrix(letr, ncol=length(r[[6]]))
ma=data.frame(l=letr[1,1])
 return(ma)
}
i=1:length(r[[8]])
lac=lapply(i,flet);lac=unlist(lac)

f=function(i){dff=la[[i]][,2];return(dff)}
i=1:length(la)
lapply(i,f)
l=lapply(i,f)
l=matrix(unlist(l), ncol=length(la))
rownames(l)=la[[1]][,1]
colnames(l)=lac

ff=function(i){dff=la[[i]][,1];return(dff)}
i=1:length(la)
lapply(i,ff)
ll=lapply(i,ff)
ll=unlist(ll)

ff=function(i){dff=la[[i]][,test];return(dff)}
i=1:length(la)
lapply(i,ff)
let=lapply(i,ff)
let=unlist(let)

ff=function(i){dff=la[[i]][,2];return(dff)}
i=1:length(la)
m=lapply(i,ff)
m=unlist(m)
m=round(m,2); m=signif(m)

ff=function(i){dff=la[[i]][,s];return(dff)}
i=1:length(la)
sd=lapply(i,ff)
sd=unlist(sd)
    
     md=m+sd
          mi=m-sd
     ml=max(md)*1.5
     
	     mf=format(m)
	     pp=paste(mf,let)
	     #"#E6E6E6"
	     
	     le=c("",p2)
	     pp=list("",pp)
	     
nu=ifelse(legend==TRUE,2,1)
nuu=ifelse(letters==TRUE,2,1)


barras=barplot(l, besid=T, legend.text =TRUE, ylim=c(0,ml),...)
     arrows(barras,mi, barras, md, angle=90, code=3, length = 0.10)
     text(barras, md, pp[[nuu]],cex=cex.text,pos=3, col="black")
       legend('topleft', ncol = 1, title =NULL,
  legend = le[nu], cex=cex.legend, bty="n")
  
  }





pb11=function(dados)
 {
 pb=function(i)
 {
 
        
 n=names(dados)
 n=n[[i]]
     dados=dados[[i]]
     
da=as.data.frame(dados[[1]])
  
                            
                    ddd=as.numeric(da[[length(da)]][1])
                         pff=ifelse(is.na(ddd),000, ddd)
pf=paste("p-value (anova)=",format(round(pff,4),nsmall=4) )   
     p2=pf
     
     fc=c(2,4,6,8)
     fc=fc[fat]

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
	     #"#E6E6E6"
	     
	     le=c("",p2)
	     pp=list("",pp)
	     
nu=ifelse(legend==TRUE,2,1)
nuu=ifelse(letters==TRUE,2,1)
     
     barras=barplot(m, ylim=c(0,ml),ylab=n, cex.names=cex.names, names=l,space=0.1, ...)
     arrows(barras,mi, barras, md, angle=90, code=3, length = 0.10)
     text(barras, md, pp[[nuu]],cex=cex.text,pos=3, col="black")
       legend(
  'topright', ncol = 1, title =NULL,
  legend = le[nu], cex=cex.legend, bty="n")
        
}

   u=ifelse(class(dados[[1]])=="data.frame",1,length(dados))
i=1:u


pdf("Barplot.pdf", family=family, bg=bg)
lapply(i, pb)
 dev.off()
 
 }

pb22=function(dados)
 {
   pb=function(i)
 {
  n=names(dados)
 n=n[[i]]
     dados=dados[[i]]
     
da=as.data.frame(dados[[1]])
  
        
                   
                    ddd=as.numeric(da[[length(da)]][2])
                   

                         pff=ifelse(is.na(ddd),000, ddd)
pf=paste("p-value (anova)=",format(round(pff,4),nsmall=4) )   
     p2=pf
     
     fc=c(2,4,6,8)
     fc=fc[fat]

   dados=dados[[fat]][,c(1,2,s,test)]
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
	     #"#E6E6E6"
	     
	     le=c("",p2)
	     pp=list("",pp)
	     
nu=ifelse(legend==TRUE,2,1)
nuu=ifelse(letters==TRUE,2,1)
     
     barras=barplot(m, ylim=c(0,ml),ylab=n, cex.names=cex.names, names=l,space=0.1, ...)
     arrows(barras,mi, barras, md, angle=90, code=3, length = 0.10)
     text(barras, md, pp[[nuu]],cex=cex.text,pos=3, col="black")
       legend(
  'topright', ncol = 1, title =NULL,
  legend = le[nu], cex=cex.legend, bty="n")
     
  
}
   u=ifelse(class(dados[[1]])=="data.frame",1,length(dados))
i=1:u


pdf("Barplot.pdf", family=family, bg=bg)
lapply(i, pb)
 dev.off()
 
 }


pb33=function(dados)
 {
     
pb=function(i)
 {
 
        
 n=names(dados)
 n=n[[i]]
     dados=dados[[i]]
  da=as.data.frame(dados[[1]])
  
           nn=rownames(da)
           nn=nn[3] 
           nn=if (nn=="row"){5} else {nn} 
                                 nn=if (nn=="rows"){5} else {nn}   
                      nn=if (nn=="block"){4} else {nn} 
                                                                                        nn=if (nn=="blocks"){4} else {nn}  
                                            nn=if (is.numeric(nn)){nn} else {3}                   
                    ddd=as.numeric(da[[length(da)]][nn])
                   
                         pff=ifelse(is.na(ddd),000, ddd)
pf=paste("p-value (anova)=",format(round(pff,4),nsmall=4) )   
     p2=pf
     
     fc=c(2,4,6,8)
     fc=fc[fat]

       
        
     
d=dados

f=function(i){dff=d[[6]][[i]][,2];return(dff)}
i=1:length(d[[6]])
lapply(i,f)
l=lapply(i,f)
l=matrix(unlist(l), ncol=length(d[[6]]))

ff=function(i){dff=d[[6]][[i]][,1];return(dff)}
i=1:length(d[[6]])
lapply(i,ff)
ll=lapply(i,ff)
ll=unlist(ll)

ff=function(i){dff=d[[6]][[i]][,test];return(dff)}
i=1:length(d[[6]])
lapply(i,ff)
let=lapply(i,ff)
let=unlist(let)

ff=function(i){dff=d[[6]][[i]][,2];return(dff)}
i=1:length(d[[6]])
m=lapply(i,ff)
m=unlist(m)
m=round(m,2); m=signif(m)

ff=function(i){dff=d[[6]][[i]][,s];return(dff)}
i=1:length(d[[6]])
sd=lapply(i,ff)
sd=unlist(sd)
    
     md=m+sd
          mi=m-sd
     ml=max(md)*1.5
     
	     mf=format(m)
	     pp=paste(mf,let)
	     #"#E6E6E6"
	     
	     le=c("",p2)
	     pp=list("",pp)
	     
nu=ifelse(legend==TRUE,2,1)
nuu=ifelse(letters==TRUE,2,1)



barras=barplot(l, besid=T, names=ll, ylim=c(0,ml), ...)
     arrows(barras,mi, barras, md, angle=90, code=3, length = 0.10)
     text(barras, md, pp[[nuu]],cex=cex.text,pos=3, col="black")
       legend('topright', ncol = 1, title =NULL,
  legend = le[nu], cex=cex.legend, bty="n")
  
  }
  
     u=ifelse(class(dados[[1]])=="data.frame",1,length(dados))
i=1:u


pdf("Barplot.pdf", family=family, bg=bg)
lapply(i, pb)
 dev.off()
 
 }
  
  pb44=function(dados)
 {
     
pb=function(i)
 {
 
        
 n=names(dados)
 n=n[[i]]
     dados=dados[[i]]
  
                da=as.data.frame(dados[[1]])
  
           nn=rownames(da)
           nn=nn[3] 
           nn=if (nn=="row"){5} else {nn}
                                 nn=if (nn=="rows"){5} else {nn}    
                      nn=if (nn=="block"){4} else {nn}
                                                                                        nn=if (nn=="blocks"){4} else {nn}   
                                            nn=if (is.numeric(nn)){nn} else {3}                   
                    ddd=as.numeric(da[[length(da)]][nn])
                   
                         pff=ifelse(is.na(ddd),000, ddd)
pf=paste("p-value (anova)=",format(round(pff,4),nsmall=4) )   
     p2=pf
     
     fc=c(2,4,6,8)
     fc=fc[fat]

       
        
     
d=dados

f=function(i){dff=d[[8]][[i]][,2];return(dff)}
i=1:length(d[[8]])
lapply(i,f)
l=lapply(i,f)
l=matrix(unlist(l), ncol=length(d[[8]]))

ff=function(i){dff=d[[8]][[i]][,1];return(dff)}
i=1:length(d[[8]])
lapply(i,ff)
ll=lapply(i,ff)
ll=unlist(ll)

ff=function(i){dff=d[[8]][[i]][,test];return(dff)}
i=1:length(d[[8]])
lapply(i,ff)
let=lapply(i,ff)
let=unlist(let)

ff=function(i){dff=d[[8]][[i]][,2];return(dff)}
i=1:length(d[[8]])
m=lapply(i,ff)
m=unlist(m)
m=round(m,2); m=signif(m)

ff=function(i){dff=d[[8]][[i]][,s];return(dff)}
i=1:length(d[[8]])
sd=lapply(i,ff)
sd=unlist(sd)
    
     md=m+sd
          mi=m-sd
     ml=max(md)*1.5
     
	     mf=format(m)
	     pp=paste(mf,let)
	     #"#E6E6E6"
	     
	     le=c("",p2)
	     pp=list("",pp)
	     
nu=ifelse(legend==TRUE,2,1)
nuu=ifelse(letters==TRUE,2,1)


barras=barplot(l, besid=T,ylab=n, names=ll, ylim=c(0,ml), ...)
     arrows(barras,mi, barras, md, angle=90, code=3, length = 0.10)
     text(barras, md, pp[[nuu]],cex=cex.text,pos=3, col="black")
       legend('topright', ncol = 1, title =NULL,
  legend = le[nu], cex=cex.legend, bty="n")
  
  }
  
  u=ifelse(class(dados[[1]])=="data.frame",1,length(dados))
i=1:u


pdf("Barplot.pdf", family=family, bg=bg)
lapply(i, pb)
 dev.off()
 
 }
     

pb55=function(dados)
 {
     
pb=function(i)
 {
 
        
 n=names(dados)
 n=n[[i]]
     dados=dados[[i]]
  
                 da=as.data.frame(dados[[1]])
  
           nn=rownames(da)
           nn=nn[3] 
           nn=if (nn=="row"){5} else {nn}  
                                 nn=if (nn=="rows"){5} else {nn}  
                      nn=if (nn=="block"){4} else {nn} 
                                                                                        nn=if (nn=="blocks"){4} else {nn}  
                                            nn=if (is.numeric(nn)){nn} else {3}                   
                    ddd=as.numeric(da[[length(da)]][nn])
                   
                         pff=ifelse(is.na(ddd),000, ddd)
pf=paste("p-value (anova)=",format(round(pff,4),nsmall=4) )   
     p2=pf
     
     fc=c(2,4,6,8)
     fc=fc[fat]

       
        
     
d=dados
r=dados

fletr=function(i){
ob=r[[6]][[i]][,1]
letr=unlist(strsplit(ob,"[.]"))
letr=matrix(letr, ncol=length(r[[8]]))
ma=data.frame(l=letr[1,],r[[6]][[i]][,-1])
 ma=ma[order(ma[,1], decreasing=FALSE),]
return(ma)
}
i=1:length(r[[6]])
la=lapply(i,fletr)

flet=function(i){
ob=r[[6]][[i]][,1]
letr=unlist(strsplit(ob,"[.]"))
letr=matrix(letr, ncol=length(r[[8]]))
ma=data.frame(l=letr[2,1])
 return(ma)
}
i=1:length(r[[6]])
lac=lapply(i,flet);lac=unlist(lac)

f=function(i){dff=la[[i]][,2];return(dff)}
i=1:length(la)
lapply(i,f)
l=lapply(i,f)
l=matrix(unlist(l), ncol=length(la))
rownames(l)=la[[1]][,1]
colnames(l)=lac

ff=function(i){dff=la[[i]][,1];return(dff)}
i=1:length(la)
lapply(i,ff)
ll=lapply(i,ff)
ll=unlist(ll)

ff=function(i){dff=la[[i]][,test];return(dff)}
i=1:length(la)
lapply(i,ff)
let=lapply(i,ff)
let=unlist(let)

ff=function(i){dff=la[[i]][,2];return(dff)}
i=1:length(la)
m=lapply(i,ff)
m=unlist(m)
m=round(m,2); m=signif(m)

ff=function(i){dff=la[[i]][,s];return(dff)}
i=1:length(la)
sd=lapply(i,ff)
sd=unlist(sd)
    
     md=m+sd
          mi=m-sd
     ml=max(md)*1.5
     
	     mf=format(m)
	     pp=paste(mf,let)
	     #"#E6E6E6"
	     
	     le=c("",p2)
	     pp=list("",pp)
	     
nu=ifelse(legend==TRUE,2,1)
nuu=ifelse(letters==TRUE,2,1)


barras=barplot(l, besid=T,ylab=n, legend.text =TRUE, ylim=c(0,ml),...)
     arrows(barras,mi, barras, md, angle=90, code=3, length = 0.10)
     text(barras, md, pp[[nuu]],cex=cex.text,pos=3, col="black")
       legend('topleft', ncol = 1, title =NULL,
  legend = le[nu], cex=cex.legend, bty="n")
  
  }
  
   u=ifelse(class(dados[[1]])=="data.frame",1,length(dados))
i=1:u


pdf("Barplot.pdf", family=family, bg=bg)
lapply(i, pb)
 dev.off()
 
 }
     
pb66=function(dados)
 {
 
 pb=function(i)
 {
 
        
 n=names(dados)
 n=n[[i]]
     dados=dados[[i]]
     

   da=as.data.frame(dados[[1]])
  
           nn=rownames(da)
           nn=nn[3] 
           nn=if (nn=="row"){5} else {nn}  
                                 nn=if (nn=="rows"){5} else {nn}  
                      nn=if (nn=="block"){4} else {nn}
                                            nn=if (nn=="blocks"){4} else {nn}  
                                            nn=if (is.numeric(nn)){nn} else {3}                   
                    ddd=as.numeric(da[[length(da)]][nn])
                   
                         pff=ifelse(is.na(ddd),000, ddd)
pf=paste("p-value (anova)=",format(round(pff,4),nsmall=4) )   
     p2=pf
     
       
        
     
d=dados
r=dados

fletr=function(i){
ob=r[[8]][[i]][,1]
letr=unlist(strsplit(ob,"[.]"))
letr=matrix(letr, ncol=length(r[[6]]))
ma=data.frame(l=letr[2,],r[[8]][[i]][,-1])
 ma=ma[order(ma[,1], decreasing=FALSE),]
return(ma)
}
i=1:length(r[[8]])
la=lapply(i,fletr)

flet=function(i){
ob=r[[8]][[i]][,1]
letr=unlist(strsplit(ob,"[.]"))
letr=matrix(letr, ncol=length(r[[6]]))
ma=data.frame(l=letr[1,1])
 return(ma)
}
i=1:length(r[[8]])
lac=lapply(i,flet);lac=unlist(lac)

f=function(i){dff=la[[i]][,2];return(dff)}
i=1:length(la)
lapply(i,f)
l=lapply(i,f)
l=matrix(unlist(l), ncol=length(la))
rownames(l)=la[[1]][,1]
colnames(l)=lac

ff=function(i){dff=la[[i]][,1];return(dff)}
i=1:length(la)
lapply(i,ff)
ll=lapply(i,ff)
ll=unlist(ll)

ff=function(i){dff=la[[i]][,test];return(dff)}
i=1:length(la)
lapply(i,ff)
let=lapply(i,ff)
let=unlist(let)

ff=function(i){dff=la[[i]][,2];return(dff)}
i=1:length(la)
m=lapply(i,ff)
m=unlist(m)
m=round(m,2); m=signif(m)

ff=function(i){dff=la[[i]][,s];return(dff)}
i=1:length(la)
sd=lapply(i,ff)
sd=unlist(sd)
    
     md=m+sd
          mi=m-sd
     ml=max(md)*1.5
     
	     mf=format(m)
	     pp=paste(mf,let)
	     #"#E6E6E6"
	     
	     le=c("",p2)
	     pp=list("",pp)
	     
nu=ifelse(legend==TRUE,2,1)
nuu=ifelse(letters==TRUE,2,1)


barras=barplot(l, besid=T,ylab=n, legend.text =TRUE, ylim=c(0,ml),...)
     arrows(barras,mi, barras, md, angle=90, code=3, length = 0.10)
     text(barras, md, pp[[nuu]],cex=cex.text,pos=3, col="black")
       legend('topleft', ncol = 1, title =NULL,
  legend = le[nu], cex=cex.legend, bty="n")
  
  }
  
  u=ifelse(class(dados[[1]])=="data.frame",1,length(dados))
i=1:u


pdf("Barplot.pdf", family=family, bg=bg)
lapply(i, pb)
 dev.off()
 
 }
  
  



 
 u=ifelse(class(dados[[1]])=="data.frame",1,2)

lf1=list(pb1,pb2,pb3,pb4,pb5,pb6)
lf2=list(pb11,pb22,pb33,pb44,pb55,pb66)
lff=list(lf1,lf2)
lf=lff[[u]]
suppressWarnings(lf[[fat]](dados))

}



########







