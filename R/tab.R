tab=function(data, test=1)
{
dados=data
r=data
tes=c(7,8,9,10,11)
test=tes[test]

f1=function(dados){
r=dados
op=if(names(r[1])=='Kruskal-Wallis Rank Sum Test') 2 else 1
op=if(names(r[1])=="Friedman Rank Sum Test") 3 else op


fa=function(dados){
b=if(rownames(r[[1]])[1]=='covariate') 2 else 1
p=r[[1]][b,][[5]]
da=r[[2]]
treatment=da[,1];mean=paste(format(da[,2],4),da[,test])
sem=format(mean(da$sem),4);cv=format(r[[4]][[1]][3],2);sw=r[[4]][[1]][1];max=format(max(da$max),4)
min=format(min(da$min),4)
tab=data.frame(treatment,mean)
tab=rbind(tab,c("p(test F)",p))
tab=rbind(tab,c("p(Shapiro-Wilk)",sw))
tab=rbind(tab, c("sem",sem))
tab=rbind(tab, c("cv(%)",cv))
tab=rbind(tab, c("max",max))
tab=rbind(tab, c("min",min))
print.data.frame(tab, right=F)
}

fk=function(dados){
p=r[[1]][,1][[2]]
da=r[[2]]
treatment=da[,1];median=paste(format(da[,4],4),da[,10]); median=paste(median,"(",da[,5],":",da[,6],")", sep="")
q1=format(da$q1,4)
q3=format(da$q1,4)
tab=data.frame(treatment,median)
names(tab)=c("treatment","median (q1:q3)")
tab=rbind(tab,c("p(Kruskall-Wallis)",p))
print.data.frame(tab, right=F)
}

ff=function(dados){
p=r[[1]][,1][[2]]
da=r[[2]]
treatment=da[,1];median=paste(format(da[,4],4),da[,10]); median=paste(median,"(",da[,5],":",da[,6],")", sep="")
q1=format(da$q1,4)
q3=format(da$q1,4)
tab=data.frame(treatment,median)
names(tab)=c("treatment","median (q1:q3)")
tab=rbind(tab,c("p(Friedman)",p))
print.data.frame(tab, right=F)
}

l=list(fa,fk,ff)
l[[op]](dados)

}

f2=function(dados){
ff=function(i){
r=dados[[i]]

op=if(names(r[1])=='Kruskal-Wallis Rank Sum Test') 2 else 1
op=if(names(r[1])=="Friedman Rank Sum Test") 3 else op



fa=function(dados){
b=if(rownames(r[[1]])[1]=='covariate') 2 else 1
p=r[[1]][b,][[5]]
da=r[[2]]
treatment=da[,1];mean=paste(format(da[,2],4),da[,test])
sem=format(mean(da$sem),4);cv=format(r[[4]][[1]][3],2);sw=r[[4]][[1]][1]
max=format(max(da$max),4)
min=format(min(da$min),4)
tab=data.frame(treatment,mean);tab=tab[order(tab[,1]),]
tab=rbind(tab,c("p(F)",p))
tab=rbind(tab,c("p(Shapiro-Wilk)",sw))
tab=rbind(tab, c("sem",sem))
tab=rbind(tab, c("cv(%)",cv))
tab=rbind(tab, c("max",max))
tab=rbind(tab, c("min",min))
tab=tab[,-1]
return(tab)
}

fk=function(dados){
p=r[[1]][,1][[2]]
da=r[[2]]
treatment=da[,1];median=paste(format(da[,4],4),da[,10]); median=paste(median,"(",da[,5],":",da[,6],")", sep="")
q1=format(da$q1,4)
q3=format(da$q1,4)
tab=data.frame(treatment,median)
names(tab)=c("treatment","median (q1:q3)")
tab=rbind(tab,c("p(Kruskall-Wallis)",p))
tab=tab[,-1]
return(tab)
}

ffi=function(r){
p=r[[1]][,1][[2]]
da=r[[2]]
treatment=da[,1];median=paste(format(da[,4],4),da[,10]); median=paste(median,"(",da[,5],":",da[,6],")", sep="")
q1=format(da$q1,4)
q3=format(da$q1,4)
tab=data.frame(treatment,median)
names(tab)=c("treatment","median (q1:q3)")
tab=rbind(tab,c("p(Friedman)",p))
tab=tab[,-1]
return(tab)
}

l=list(fa,fk,ffi)
l[[op]](dados)


}



i=1:length(dados)
op=if(names(dados[[1]][1])=='Kruskal-Wallis Rank Sum Test') 2 else 1
op=if(names(dados[[1]][1])=="Friedman Rank Sum Test") 3 else op

l=lapply(i,ff);l=data.frame(l);colnames(l)=NULL
rn=data.frame(sort(dados[[1]][[2]][,1]))
rn1=rbind(rn,"p(F)","p(Shapiro-Wilk)","sem","cv(%)","max","min")
rn2=rbind(rn,"p(Kruskal-Wallis)")
rn3=rbind(rn,"p(Friedman)")
rn=list(rn1,rn2,rn3)[[op]]
rn=as.factor(rn[,1])
rownames(l)=rn
colnames(l)=c(names(dados))
return(l)
}


nnn=if (is.data.frame(dados[[1]])){1} else {2} 
f=list(f1,f2)
f[[nnn]](dados)
}



