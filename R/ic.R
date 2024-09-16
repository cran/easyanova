ic=function(data, test=1, df=10, alpha=0.05)

{
dados=data
d=if (is.data.frame(dados)){dados} else {dados$"M"} 
d=if (is.data.frame(d)){d} else {dados$"A"} 
dados=if (is.data.frame(d)){d} else {dados$"s"} 
ma=dados
  ma=data.frame(ma,co=ma[,1])
        ma=ma[order(ma[,2], decreasing=TRUE),]
        jj=ma[,2]
        auxj <- combn(jj, 2)
        yi=qtukey((1-alpha),length(ma[,2]),df)
        jjj=ma$sem^2
        auxjj <- combn(jjj, 2)
        si=sqrt((auxjj[1,]+auxjj[2,])/2)
        fs=function(ns){s=2:ns;return(s)}
        ns=length(ma[,2]):2
        se=sapply(ns, fs)
        ns=unlist(se)
                yii=qtukey((1-alpha),ns,df)
          yiii=qtukey((1-alpha)^(ns-1), ns, df)
yiiii=qt((1-alpha),df)
        yt=yi*si
        ysnk=yii*si
        yd=yiii*si
        ytt=yiiii*(si*sqrt(2))
        confs=data.frame(yt,ysnk,yd,ytt)
             
       
        q=confs
        
        
set=q[,test]
m1=combn(ma[,2],2);m1=m1[1,]-m1[2,]
m2=combn(ma[,1],2);m2=paste(m2[1,],m2[2,], sep="-")
m3=data.frame(m2,m1)
mi=round(m3$m1-set,2)
md=round(m3$m1+set,2)

dat=data.frame(pair=m2,contrast=m1, ICinf=mi, ICsup=md)
return(dat)

}





