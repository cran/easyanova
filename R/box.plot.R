box.plot=function(data,test=1, xlab=NULL, ylab=NULL,legend=TRUE, letters=TRUE, family="Times", bg="white",  cex.axis=0.7,...)
{
y=data
name=c("p-value (Kruskal-Wallis)=","p-value (Friedman)=")
name=name[test]
design=if(test==1) 14 else 15
ui=if(ncol(data)>3) TRUE else FALSE
x=ea1(y,design=design, list=ui)
y=list(y, y[,-2])
y=y[[test]]


par(family=family, bg=bg)

pb=function(x, y)
 {
 y=y[,c(1,2)]
 
 y[,1]=as.factor(y[,1])
 
 y[,1]=factor(y[,1], levels=x[[2]][[1]])
     

           d2=x[[2]]
           d3=x[[3]]
           
             ddd=as.numeric(x[[1]][2,])
    pff=ifelse(is.na(ddd),000, ddd)
    pf=paste(name,format(round(pff,4),nsmall=4) ) 
    
              
         let=d2[[10]]
md=d2[[4]]
       
                   # pppp=paste(format(round(md,3),nsmall=3), let)
                    # pppp=paste("=",pppp)
                    # pppp=paste(d2[[1]],pppp)
        


    n=names(y)[2]
    te=1:length(let)
        lx=(length(d2[[1]])+1)
     
     mini=min(y[,2],na.rm = TRUE)-sd(y[,2],na.rm = TRUE)
          mini2=min(y[,2],na.rm = TRUE)-(sd(y[,2], na.rm = TRUE)/1.5)
          
             maxi=max(y[,2],na.rm = TRUE)
             
             posi=rep(mini2,length(md))
             
    	     g=c("",pf) 
    	     	     let=list("",let)      
 nu=ifelse(legend==TRUE,2,1)
 nuu=ifelse(letters==TRUE,2,1)       
    
    boxplot(y[,2]~y[,1], xlim=c(0,lx), ylim=c(mini, maxi),xlab=xlab,ylab=ylab, cex.axis=cex.axis,...) 
     stripchart(y[,2]~y[,1], vertical = TRUE,  
           method = "jitter", add = TRUE, pch=20, cex=0.7)
            legend(
  'topright', ncol = 1, title =NULL,
  legend = g[nu], cex=1, bty="n")
text(te, posi, let[[nuu]],cex=1.2, pos=3, col="black")

}    
    

fpb=function(x, y)
{
h=x

pb=function(i)
 {
         
 n=names(x)
 n=n[[i]]
     x=x[[i]]
     
             y=y[,c(1,(i+1))]
        

        
        y[,1]=as.factor(y[,1])
  y[,1]=factor(y[,1], levels=x[[2]][[1]])
           
            d2=x[[2]]
           d3=x[[3]]
           
                
    p3=paste(d2[[1]],"=",format(round(d2[[2]],2)),d2[[8]])
    
     ddd=as.numeric(x[[1]][2,])
    pff=ifelse(is.na(ddd),000, ddd)
    pf=paste(name,format(round(pff,4),nsmall=4) ) 
    
   #### g=c("Kruskal-Wallis",pf,"","Contrasts",p2,"", "", "Ranks", p3)
    
        let=d2[[10]]
        md=d2[[4]]
       
                    #pppp=paste(format(round(md,3),nsmall=3), let)
                    #pppp=paste("=",pppp)
                    #pppp=paste(d2[[1]],pppp)
        
    g=c("",pf)
    
    te=1:length(let)
    
        te2=0:(length(let)+1)
        
    lx=(length(d2[[1]])+1)
     mini=min(y[,2],na.rm = TRUE)-sd(y[,2],na.rm = TRUE)
          mini2=min(y[,2],na.rm = TRUE)-(sd(y[,2],na.rm = TRUE)/1.5)
          
             maxi=max(y[,2],na.rm = TRUE)
             
             posi=rep(mini2,length(md))
        
    	     g=c("",pf)
    	     let=list("",let)      
 nu=ifelse(legend==TRUE,2,1)
 nuu=ifelse(letters==TRUE,2,1)       
    
     ylab=n
    boxplot(y[,2]~y[,1], xlim=c(0,lx), ylim=c(mini, maxi),xlab=xlab,ylab=ylab,cex.axis=cex.axis,...) 
     stripchart(y[,2]~y[,1], vertical = TRUE,  
           method = "jitter", add = TRUE, pch=1, cex=0.7)
            legend(
  'topright', ncol = 1, title =NULL,
  legend = g[nu], cex=1, bty="n")
text(te, posi, let[[nuu]],cex=1.2, pos=3, col="black")

  
     
        
}

u=ifelse(class(h[[1]])=="data.frame",1,length(h))
i=1:u


     pdf("Boxplot.pdf",family=family, bg=bg)
lapply(i, pb)
 dev.off()
 
 }


u=ifelse(class(x[[1]])=="data.frame",1,2)

lf=list(pb, fpb)

lf[[u]](x,y)

}


########



