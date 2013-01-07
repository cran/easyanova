ea2 <-
function(data, design=1, alpha=0.05, cov=4, list=FALSE)
    {
        list=ifelse(list==FALSE,1,2)
        
        cv <- function(x) {
            sd = (deviance(x)/df.residual(x))^0.5
            mm = mean(fitted(x))
            r = 100 * sd/mm
            return(round(r, 2))
        }
        
        fr=function(m,data){
            r=resid(m)
            s <- shapiro.test(r)
            b1<- bartlett.test(r~factor_1, data=data)
            b2<- bartlett.test(r~factor_2, data=data)
            b3<- bartlett.test(r~treatments, data=data)
            cvf=cv(m)
            rd=as.data.frame((sort(sqrt(r^2),decreasing=TRUE)))
            rl=as.list(rownames(rd))
            r1=rl[[1]];r2=rl[[2]];r3=rl[[3]]
            d=data.frame(round(s$"p.value",4),round(b1$"p.value",4),round(b2$"p.value",4),round(b3$"p.value",4), round(cvf,2),as.numeric(r1),as.numeric(r2),as.numeric(r3)); d=t(d)
            rownames(d)=c("p.value Shapiro-Wilk test","p.value Bartlett test (factor_1)","p.value Bartlett test (factor_2)","p.value Bartlett test (treatments)","coefficient of variation (%)", "first value most discrepant","second value most discrepant","third value most discrepant")
            colnames(d)="values"
            return(d)}
        
        fr2=function(m,data){
            data=data.frame(data,nr=c(1:length(data$response)))
            data2<-na.omit(data)
            r<-resid(m)
            s<-shapiro.test(r)
            b1<- bartlett.test(r~plot, data=data2)
            b2<- bartlett.test(r~split.plot, data=data2)
            b3<- bartlett.test(r~treatments, data=data2)
            a1=AIC(m);a1=as.numeric(a1);a1=round(a1,4)
            b11=BIC(m);b11=as.numeric(b11);b11=round(b11,4)
            names(r)=data2$nr
            rd=as.data.frame((sort(sqrt(r^2),decreasing=TRUE)))
            rl=as.list(rownames(rd))
            r1=rl[[1]];r2=rl[[2]];r3=rl[[3]]
            d=data.frame(round(s$"p.value",4),round(b1$"p.value",4),round(b2$"p.value",4),round(b3$"p.value",4), a1,b11,as.numeric(r1),as.numeric(r2),as.numeric(r3)); d=t(d)
            rownames(d)=c("p.value Shapiro-Wilk test","p.value Bartlett test (plot)","p.value Bartlett test (split.plot)","p.value Bartlett test (plot*split.plot)","AIC","BIC", "first value most discrepant","second value most discrepant","third value most discrepant")
            colnames(d)="values"
            return(d)}
        
        fr3=function(m,data){
            r=resid(m)
            s <- shapiro.test(r)
            b1<- bartlett.test(r~factor_1, data=data)
            b2<- bartlett.test(r~factor_2, data=data)
            b3<- bartlett.test(r~factor_3, data=data)
            cvf=cv(m)
            rd=as.data.frame((sort(sqrt(r^2),decreasing=TRUE)))
            rl=as.list(rownames(rd))
            r1=rl[[1]];r2=rl[[2]];r3=rl[[3]]
            d=data.frame(round(s$"p.value",4),round(b1$"p.value",4),round(b2$"p.value",4),round(b3$"p.value",4), round(cvf,2),as.numeric(r1),as.numeric(r2),as.numeric(r3)); d=t(d)
            rownames(d)=c("p.value Shapiro-Wilk test","p.value Bartlett test (factor_1)","p.value Bartlett test (factor_2)","p.value Bartlett test (factor_3)","coefficient of variation (%)", "first value most discrepant","second value most discrepant","third value most discrepant")
            colnames(d)="values"
            return(d)}
        
        fr4=function(m,data){
            data=data.frame(data,nr=c(1:length(data$response)))
            data2<-na.omit(data)
            r<-resid(m)
            s<-shapiro.test(r)
            b1<- bartlett.test(r~factor_1, data=data2)
            b2<- bartlett.test(r~factor_2, data=data2)
            b3<- bartlett.test(r~factor_3, data=data2)
            a1=AIC(m);a1=as.numeric(a1);a1=round(a1,4)
            b11=BIC(m);b11=as.numeric(b11);b11=round(b11,4)
            names(r)=data2$nr
            rd=as.data.frame((sort(sqrt(r^2),decreasing=TRUE)))
            rl=as.list(rownames(rd))
            r1=rl[[1]];r2=rl[[2]];r3=rl[[3]]
            d=data.frame(round(s$"p.value",4),round(b1$"p.value",4),round(b2$"p.value",4),round(b3$"p.value",4), a1,b11,as.numeric(r1),as.numeric(r2),as.numeric(r3)); d=t(d)
            rownames(d)=c("p.value Shapiro-Wilk test","p.value Bartlett test (factor_1)","p.value Bartlett test (factor_2)","p.value Bartlett test (factor_3)","AIC","BIC", "first value most discrepant","second value most discrepant","third value most discrepant")
            colnames(d)="values"
            return(d)}
        
        
        fa2=function(a){
            res=a; d=data.frame(res); d=data.frame(d[,2],d[,1],d[,1]/d[,2],d[,3],d[,4])
            d=round(d,4); d1=d[,5]; d2=ifelse(d1<0.001, "<0.001", d1); 
            d2=d2[-length(d2)];d2=c(d2,"-"); d=d[,-5];d=data.frame(d,d2);d[is.na(d)] <- "-"
            names(d)=c("df", "type III SS", "mean square", "F value", "p>F"); rownames(d)=rownames(res)
            return(d)
        }
        
        fm=function(ma,dff){
            ma=data.frame(ma,co=ma[,1])
            ma=ma[order(ma[,2], decreasing=TRUE),]
            j=ma[,1];j=as.character(j)
            aux <- combn(j, 2)
            w <- apply(aux, 2, paste, collapse = " - ")
            jj=ma[,2]
            auxj <- combn(jj, 2)
            yi=auxj[1,]-auxj[2,]
            jjj=ma$standart.error^2
            auxjj <- combn(jjj, 2)
            si=sqrt((auxjj[1,]+auxjj[2,])/2)
            yx=yi/si; yx=yx^2; yx=sqrt(yx)
            nmeans=length(ma[,1])
            ft=function(yx, nmeans){1-ptukey(yx,nmeans, dff)}
            st=ft(yx,nmeans)
            st=round(st,4)
            fs=function(ns){s=2:ns;return(s)}
            ns=nmeans:2
            se=sapply(ns, fs)
            ns=unlist(se)
            ssnk=ft(yx, ns)
            ssnk=round(ssnk,4)
            sd=1-ptukey(yx,ns, dff)^(1/(ns-1))
            sd=round(sd,4)
            yxx=yi/(si*sqrt(2))
            vt=1-pt(yxx,dff); vt=vt*2
            vt=round(vt,4)
            ggs=data.frame(w,round(yi,4),st, ssnk, sd, vt)
            colnames(ggs)=c("pair", "contrast","p(tukey)", "p(snk)", "p(duncan)", "p(t)")
            return(ggs)
        }
        ft=function(test, alpha=0.05){
            level=alpha
            tes1=test[,3]
            tes2=test[,4]
            tes3=test[,5]
            tes4=test[,6]
            names(tes1)=test$pair
            names(tes2)=test$pair
            names(tes3)=test$pair
            names(tes4)=test$pair
            tes1=ifelse(tes1<=level,TRUE,FALSE)
            tes2=ifelse(tes2<=level,TRUE,FALSE)
            tes3=ifelse(tes3<=level,TRUE,FALSE)
            tes4=ifelse(tes4<=level,TRUE,FALSE)
            x1=tes1;x2=tes2;x3=tes3;x4=tes4
            inab <- function(x, Letters=c(letters, LETTERS), separator=".", decreasing = decreasing){
                obj_x <- deparse(substitute(x))
                namx <- names(x)
                namx <- gsub(" ", "", names(x))
                if(length(namx) != length(x))
                    stop("Names required for ", obj_x)
                split_names <- strsplit(namx, "-")
                stopifnot( sapply(split_names, length) == 2 )
                comps <- t(as.matrix(as.data.frame(split_names)))
                rownames(comps) <- names(x)
                lvls <- unique(as.vector(comps))
                n <- length(lvls)
                lmat <- array(TRUE, dim=c(n,1), dimnames=list(lvls, NULL) )
                if( sum(x) == 0 ){                                                       
                    ltrs <- rep(get_letters(1, Letters=Letters, separator=separator), length(lvls) )
                    names(ltrs) <- lvls
                    colnames(lmat) <- ltrs[1]
                    msl <- ltrs
                    ret <- list(Letters=ltrs, monospacedLetters=msl, LetterMatrix=lmat)
                    return(ret)
                }
                else{
                    signifs <- comps[x,,drop=FALSE]
                    absorb <- function(m){
                        for(j in 1:(ncol(m)-1)){
                            for(k in (j+1):ncol(m)){
                                if( all(m[which(m[,k]),k] & m[which(m[,k]),j]) ){               
                                    m <- m[,-k, drop=FALSE]
                                    return(absorb(m))
                                }
                                else if( all(m[which(m[,j]),k] & m[which(m[,j]),j]) ){           
                                    m <- m[,-j, drop=FALSE]
                                    return(absorb(m))
                                }
                            }
                        }
                        return(m)
                    }
                    for( i in 1:nrow(signifs) ){                                           
                        tmpcomp <- signifs[i,]
                        wassert <- which(lmat[tmpcomp[1],] & lmat[tmpcomp[2],])               
                        if(any(wassert)){
                            tmpcols <- lmat[,wassert,drop=FALSE]
                            tmpcols[tmpcomp[2],] <- FALSE
                            lmat[tmpcomp[1],wassert] <- FALSE
                            lmat <- cbind(lmat, tmpcols)
                            colnames(lmat) <- get_letters( ncol(lmat), Letters=Letters,
                                                           separator=separator)
                            if(ncol(lmat) > 1){                                               
                                lmat <- absorb(lmat)
                                colnames(lmat) <- get_letters( ncol(lmat),  Letters=Letters,
                                                               separator=separator )
                            }
                        }
                    }
                }
                lmat <- lmat[,order(apply(lmat, 2, sum))]
                lmat <- sweepLetters(lmat)                                                                  
                lmat <- lmat[,names(sort(apply(lmat,2, function(x) return(min(which(x))))))]                
                colnames(lmat) <- get_letters( ncol(lmat),  Letters=Letters,
                                               separator=separator)
                lmat <- lmat[,order(apply(lmat, 2, sum))]                                                  
                lmat <- sweepLetters(lmat)
                lmat <- lmat[,names(sort(apply(lmat,2, function(x) return(min(which(x)))), 
                                         decreasing = decreasing))]                
                colnames(lmat) <- get_letters( ncol(lmat),  Letters=Letters,
                                               separator=separator)
                ltrs <- apply(lmat,1,function(x) return(paste(names(x)[which(x)], sep="", collapse="") ) )
                msl <- matrix(ncol=ncol(lmat), nrow=nrow(lmat))                                           
                for( i in 1:nrow(lmat) ){
                    msl[i,which(lmat[i,])] <- colnames(lmat)[which(lmat[i,])]
                    absent <- which(!lmat[i,])
                    if( length(absent) < 2 ){
                        if( length(absent) == 0 )
                            next
                        else{
                            msl[i,absent] <- paste( rep(" ", nchar(colnames(lmat)[absent])), collapse="" )
                        }
                    }
                    else{
                        msl[i,absent] <- unlist( lapply( sapply( nchar(colnames(lmat)[absent]),
                                                                 function(x) return(rep( " ",x)) ),
                                                         paste, collapse="") )
                    }
                }
                msl <- apply(msl, 1, paste, collapse="")
                names(msl) <- rownames(lmat)
                ret <- list(Letters=ltrs)
                return(ret)
            }
            
            sweepLetters <- function(mat, start.col=1, Letters=c(letters, LETTERS), separator="."){
                stopifnot( all(start.col %in% 1:ncol(mat)) )
                locked <- matrix(rep(0,ncol(mat)*nrow(mat)), ncol=ncol(mat))         
                cols <- 1:ncol(mat)
                cols <- cols[c( start.col, cols[-start.col] )]
                if( any(is.na(cols) ) )
                    cols <- cols[-which(is.na(cols))]
                for( i in cols){
                    tmp <- matrix(rep(0,ncol(mat)*nrow(mat)), ncol=ncol(mat))
                    tmp[which(mat[,i]),] <- mat[which(mat[,i]),]                        
                    one <- which(tmp[,i]==1)
                    if( all(apply(tmp[,-i,drop=FALSE], 1, function(x) return( any(x==1) ))) ){    
                    }
                    for( j in one ){                                                    
                        if( locked[j,i] == 1 ){                                           
                            next
                        }
                        chck <- 0
                        lck <- list()
                        for( k in one ){
                            if( j==k ){
                                next
                            }
                            else{                                                           
                                rows <- tmp[c(j,k),]
                                dbl <- rows[1,] & rows[2,]
                                hit <- which(dbl)
                                hit <- hit[-which(hit==i)]
                                dbl <- rows[1,-i,drop=FALSE] & rows[2,-i,drop=FALSE]
                                if( any(dbl) ){
                                    chck <- chck + 1
                                    lck[[chck]] <- list(c(j,hit[length(hit)]), c(k,hit[length(hit)]))     
                                }
                            }
                        }
                        if( (chck == (length(one)-1)) && chck != 0 ){                    
                            for( k in 1:length(lck) ){                                     
                                locked[ lck[[k]][[1]][1], lck[[k]][[1]][2] ] <- 1
                                locked[ lck[[k]][[2]][1], lck[[k]][[2]][2] ] <- 1
                            }
                            mat[j,i] <- FALSE                                             
                        }
                    }
                    if(all(mat[,i]==FALSE)){                                          
                        mat <- mat[,-i,drop=FALSE]
                        colnames(mat) <- get_letters( ncol(mat), Letters=Letters, separator=separator)
                        return(sweepLetters(mat, Letters=Letters, separator=separator))
                    }
                }
                onlyF <- apply(mat, 2, function(x) return(all(!x)))
                if( any(onlyF) ){                                                     
                    mat <- mat[,-which(onlyF),drop=FALSE]
                    colnames(mat) <- get_letters( ncol(mat), Letters=Letters, separator=separator)
                }
                return( mat )
            }
            
            get_letters <- function( n, Letters=c(letters, LETTERS), separator="." ){
                n.complete <- floor(n / length(Letters))       
                n.partial <- n %% length(Letters)              
                lett <- character()
                separ=""
                if( n.complete > 0 ){
                    for( i in 1:n.complete ){
                        lett <- c(lett, paste(separ, Letters, sep="") )
                        separ <- paste( separ, separator, sep="" )
                    }
                }
                if(n.partial > 0 )
                    lett <- c(lett, paste(separ, Letters[1:n.partial], sep="") )
                return(lett)
            }
            decreasing=FALSE;jjj1=inab(x1, decreasing = decreasing,); jjj1=jjj1[[1]]
            jjj2=inab(x2, decreasing = decreasing,); jjj2=jjj2[[1]]
            jjj3=inab(x3, decreasing = decreasing,); jjj3=jjj3[[1]]
            jjj4=inab(x4, decreasing = decreasing,); jjj4=jjj4[[1]]
            hgy=data.frame(jjj1,jjj2,jjj3,jjj4); names(hgy)=c("tukey","snk","duncan","t")
            return(hgy)
        }
        
        
        f1<-function(data,cov){ 
            names(data)=c("factor_1","factor_2","response")
            data<-data.frame(factor_1=factor(data$factor_1), factor_2=factor(data$factor_2), response=data$response)
            m<-aov(response~factor_1*factor_2,data=data, contrasts=list(factor_1=contr.sum, factor_2=contr.sum))
            m1<-aov(response~-1+factor_1+factor_2+factor_1*factor_2,data=data, contrasts=list(factor_1=contr.sum, factor_2=contr.sum))
            m2<-aov(response~-1+ factor_2+factor_1+factor_2*factor_1,data=data, contrasts=list(factor_1=contr.sum, factor_2=contr.sum))
            a<-anova(m)
            a2<-Anova(m, type=3) 
            a3<-a2[-1,]
            a3<-fa2(a3)
            treatments=interaction(data$factor_1,data$factor_2)
            data<-data.frame(data,treatments)
            m3<-aov(response~-1+treatments,data=data, contrasts=list(treatments=contr.sum))
            data2<-na.omit(data)
            res=fr(m,data2)
            adjusted.mean<-round(coef(m1)[1:nlevels(data$factor_1)],4)
            standart.error<-round(sqrt(diag(vcov(m1)) [1:nlevels(data$factor_1)]),4)
            factor_1<-levels(data$factor_1)
            maf1=data.frame(factor_1,adjusted.mean,standart.error)
            rownames(maf1)=NULL
            adjusted.mean<-round(coef(m2)[1:nlevels(data$factor_2)],4)
            standart.error<-round(sqrt(diag(vcov(m2))) [1:nlevels(data$factor_2)],4)
            factor_2<-levels(data$factor_2)
            maf2=data.frame(factor_2,adjusted.mean,standart.error)
            rownames(maf2)=NULL
            adjusted.mean<-round(coef(m3),4)
            standart.error<-round(sqrt(diag(vcov(m3))),4)
            treatment<-levels(data$t)
            mat=data.frame(treatment,adjusted.mean,standart.error)
            rownames(mat)=NULL
            dff=df.residual(m)
            test1=fm(maf1,dff)
            test2=fm(maf2,dff)
            groups1=ft(test1, alpha); maf1=maf1[order(maf1[,2], decreasing=TRUE),]
            mf1=data.frame(maf1,groups1)
            rownames(mf1) = NULL
            groups2=ft(test2, alpha); maf2=maf2[order(maf2[,2], decreasing=TRUE),]
            mf2=data.frame(maf2,groups2)
            rownames(mf2) = NULL
            c1=rep(1:nlevels(data$factor_2), each=nlevels(data$factor_1))
            cs1=split(mat, c1)
            test3=lapply(cs1, fm, dff=dff); n1=rep("factor_1 in", nlevels(data$factor_2));n11=data.frame(n1,levels(data$factor_2)) 
            n11 <- apply(t(n11), 2, paste, collapse = "  ")
            names(test3)=n11
            c2=rep(1:nlevels(data$factor_1), nlevels(data$factor_2))
            cs2=split(mat, c2)
            test4=lapply(cs2, fm, dff=dff); n2=rep("factor_2 in", nlevels(data$factor_1));n22=data.frame(n2,levels(data$factor_1)) 
            n22 <- apply(t(n22), 2, paste, collapse = "  ")
            names(test4)=n22
            csf=function(x){a=x[order(x[,2], decreasing=TRUE),];return(a)}
            groups3=lapply(test3, ft, alpha)
            mft1=lapply(cs1,csf);
            dd<-function(x){l=data.frame(mft1[[x]],groups3[[x]]); return(l)}; nf1=as.list(c(1:nlevels(data$factor_2)))
            mft1=lapply(nf1,dd)
            names(mft1)=n11
            groups4=lapply(test4, ft, alpha)
            mft2=lapply(cs2,csf);
            ddd<-function(x){l=data.frame(mft2[[x]],groups4[[x]]); return(l)}; nf2=as.list(c(1:nlevels(data$factor_1)))
            mft2=lapply(nf2,ddd)
            names(mft2)=n22
            l<-list(a3,mf1, test1, mf2,test2,mft1,test3, mft2, test4, res)
            names(l)= list("Analysis of variance", "Adjusted means (factor 1)", "Multiple comparison test (factor 1)","Adjusted means (factor 2)", "Multiple comparison test (factor 2)", "Adjusted means (factor 1 in levels of factor 2)", "Multiple comparison test (factor 1 in levels of factor 2)", "Adjusted means (factor 2 in levels of factor 1)", "Multiple comparison test (factor 2 in levels of factor 1)","Residual analysis")
            return(l)}
        
        
        f2<-function(data, cov){ 
            names(data)=c("factor_1","factor_2","blocks","response")
            data<-data.frame(factor_1=factor(data$factor_1), factor_2=factor(data$factor_2), blocks=factor(data$blocks), response=data$response)
            m<-aov(response~factor_1*factor_2+blocks,data=data, contrasts=list(factor_1=contr.sum, factor_2=contr.sum, blocks=contr.sum))
            m1<-aov(response~-1+factor_1+factor_2+factor_1*factor_2+blocks,data=data, contrasts=list(factor_1=contr.sum, factor_2=contr.sum, blocks=contr.sum))
            m2<-aov(response~-1+ factor_2+factor_1+factor_2*factor_1+blocks,data=data, contrasts=list(factor_1=contr.sum, factor_2=contr.sum, blocks=contr.sum))
            a<-anova(m)
            a2<-Anova(m, type=3) 
            a3<-a2[-1,]
            a3<-fa2(a3)
            treatments=interaction(data$factor_1,data$factor_2)
            data<-data.frame(data,treatments)
            m3<-aov(response~-1+treatments+blocks,data=data, contrasts=list(treatments=contr.sum, blocks=contr.sum))
            data2<-na.omit(data)
            res=fr(m,data2)
            adjusted.mean<-round(coef(m1)[1:nlevels(data$factor_1)],4)
            standart.error<-round(sqrt(diag(vcov(m1)) [1:nlevels(data$factor_1)]),4)
            factor_1<-levels(data$factor_1)
            maf1=data.frame(factor_1,adjusted.mean,standart.error)
            rownames(maf1)=NULL
            adjusted.mean<-coef(m2)[1:nlevels(data$factor_2)]
            standart.error<-round(sqrt(diag(vcov(m2)))[1:nlevels(data$factor_2)],4)
            factor_2<-levels(data$factor_2)
            maf2=data.frame(factor_2,adjusted.mean,standart.error)
            rownames(maf2)=NULL
            adjusted.mean<-round(coef(m3)[1:nlevels(data$treatments)],4)
            standart.error<-round(sqrt(diag(vcov(m3)))[1:nlevels(data$factor_2)],4)
            treatment<-levels(data$t)
            mat=data.frame(treatment,adjusted.mean,standart.error)
            rownames(mat)=NULL
            dff=df.residual(m)
            test1=fm(maf1,dff)
            test2=fm(maf2,dff)
            groups1=ft(test1, alpha); maf1=maf1[order(maf1[,2], decreasing=TRUE),]
            mf1=data.frame(maf1,groups1)
            rownames(mf1) = NULL
            groups2=ft(test2, alpha); maf2=maf2[order(maf2[,2], decreasing=TRUE),]
            mf2=data.frame(maf2,groups2)
            rownames(mf2) = NULL
            c1=rep(1:nlevels(data$factor_2), each=nlevels(data$factor_1))
            cs1=split(mat, c1)
            test3=lapply(cs1, fm, dff=dff); n1=rep("factor_1 in", nlevels(data$factor_2));n11=data.frame(n1,levels(data$factor_2)) 
            n11 <- apply(t(n11), 2, paste, collapse = "  ")
            names(test3)=n11
            c2=rep(1:nlevels(data$factor_1), nlevels(data$factor_2))
            cs2=split(mat, c2)
            test4=lapply(cs2, fm, dff=dff); n2=rep("factor_2 in", nlevels(data$factor_1));n22=data.frame(n2,levels(data$factor_1)) 
            n22 <- apply(t(n22), 2, paste, collapse = "  ")
            names(test4)=n22
            csf=function(x){a=x[order(x[,2], decreasing=TRUE),];return(a)}
            groups3=lapply(test3, ft, alpha)
            mft1=lapply(cs1,csf);
            dd<-function(x){l=data.frame(mft1[[x]],groups3[[x]]); return(l)}; nf1=as.list(c(1:nlevels(data$factor_2)))
            mft1=lapply(nf1,dd)
            names(mft1)=n11
            groups4=lapply(test4, ft, alpha)
            mft2=lapply(cs2,csf);
            ddd<-function(x){l=data.frame(mft2[[x]],groups4[[x]]); return(l)}; nf2=as.list(c(1:nlevels(data$factor_1)))
            mft2=lapply(nf2,ddd)
            names(mft2)=n22
            l<-list(a3,mf1, test1, mf2,test2,mft1,test3, mft2, test4, res)
            names(l)= list("Analysis of variance", "Adjusted means (factor 1)", "Multiple comparison test (factor 1)","Adjusted means (factor 2)", "Multiple comparison test (factor 2)", "Adjusted means (factor 1 in levels of factor 2)", "Multiple comparison test (factor 1 in levels of factor 2)", "Adjusted means (factor 2 in levels of factor 1)", "Multiple comparison test (factor 2 in levels of factor 1)","Residual analysis")
            return(l)
        }
        
        f3<-function(data, cov){ 
            names(data)=c("factor_1","factor_2","rows","cols","response")
            data<-data.frame(factor_1=factor(data$factor_1), factor_2=factor(data$factor_2), rows=factor(data$rows), cols=factor(data$cols), response=data$response)
            m<-aov(response~factor_1*factor_2+ rows+cols,data=data, contrasts=list(factor_1=contr.sum, factor_2=contr.sum, rows=contr.sum, cols=contr.sum))
            m1<-aov(response~-1+factor_1+factor_2+factor_1*factor_2+ rows+cols,data=data, contrasts=list(factor_1=contr.sum, factor_2=contr.sum, rows=contr.sum, cols=contr.sum))
            m2<-aov(response~-1+ factor_2+factor_1+factor_2*factor_1+ rows+cols,data=data, contrasts=list(factor_1=contr.sum, factor_2=contr.sum, rows=contr.sum, cols=contr.sum))
            a<-anova(m)
            a2<-Anova(m, type=3) 
            a3<-a2[-1,]
            a3<-fa2(a3)
            treatments=interaction(data$factor_1,data$factor_2)
            data<-data.frame(data,treatments)
            m3<-aov(response~-1+treatments +rows+cols,data=data, contrasts=list(treatments=contr.sum, rows=contr.sum, cols=contr.sum))
	    data2<-na.omit(data)
            res=fr(m,data2)
            adjusted.mean<-round(coef(m1)[1:nlevels(data$factor_1)],4)
            standart.error<-round(sqrt(diag(vcov(m1)) [1:nlevels(data$factor_1)]),4)
            factor_1<-levels(data$factor_1)
            maf1=data.frame(factor_1,adjusted.mean,standart.error)
            rownames(maf1)=NULL
            adjusted.mean<-coef(m2)[1:nlevels(data$factor_2)]
            standart.error<-round(sqrt(diag(vcov(m2)))[1:nlevels(data$factor_2)],4)
            factor_2<-levels(data$factor_2)
            maf2=data.frame(factor_2,adjusted.mean,standart.error)
            rownames(maf2)=NULL
            adjusted.mean<-round(coef(m3)[1:nlevels(data$treatments)],4)
            standart.error<-round(sqrt(diag(vcov(m3)))[1:nlevels(data$factor_2)],4)
            treatment<-levels(data$t)
            mat=data.frame(treatment,adjusted.mean,standart.error)
            rownames(mat)=NULL
            dff=df.residual(m)
            test1=fm(maf1,dff)
            test2=fm(maf2,dff)
            groups1=ft(test1, alpha); maf1=maf1[order(maf1[,2], decreasing=TRUE),]
            mf1=data.frame(maf1,groups1)
            rownames(mf1) = NULL
            groups2=ft(test2, alpha); maf2=maf2[order(maf2[,2], decreasing=TRUE),]
            mf2=data.frame(maf2,groups2)
            rownames(mf2) = NULL
            c1=rep(1:nlevels(data$factor_2), each=nlevels(data$factor_1))
            cs1=split(mat, c1)
            test3=lapply(cs1, fm, dff=dff); n1=rep("factor_1 in", nlevels(data$factor_2));n11=data.frame(n1,levels(data$factor_2)) 
            n11 <- apply(t(n11), 2, paste, collapse = "  ")
            names(test3)=n11
            c2=rep(1:nlevels(data$factor_1), nlevels(data$factor_2))
            cs2=split(mat, c2)
            test4=lapply(cs2, fm, dff=dff); n2=rep("factor_2 in", nlevels(data$factor_1));n22=data.frame(n2,levels(data$factor_1)) 
            n22 <- apply(t(n22), 2, paste, collapse = "  ")
            names(test4)=n22
            csf=function(x){a=x[order(x[,2], decreasing=TRUE),];return(a)}
            groups3=lapply(test3, ft, alpha)
            mft1=lapply(cs1,csf);
            dd<-function(x){l=data.frame(mft1[[x]],groups3[[x]]); return(l)}; nf1=as.list(c(1:nlevels(data$factor_2)))
            mft1=lapply(nf1,dd)
            names(mft1)=n11
            groups4=lapply(test4, ft, alpha)
            mft2=lapply(cs2,csf);
            ddd<-function(x){l=data.frame(mft2[[x]],groups4[[x]]); return(l)}; nf2=as.list(c(1:nlevels(data$factor_1)))
            mft2=lapply(nf2,ddd)
            names(mft2)=n22
            l<-list(a3,mf1, test1, mf2,test2,mft1,test3, mft2, test4, res)
            names(l)= list("Analysis of variance", "Adjusted means (factor 1)", "Multiple comparison test (factor 1)","Adjusted means (factor 2)", "Multiple comparison test (factor 2)", "Adjusted means (factor 1 in levels of factor 2)", "Multiple comparison test (factor 1 in levels of factor 2)", "Adjusted means (factor 2 in levels of factor 1)", "Multiple comparison test (factor 2 in levels of factor 1)","Residual analysis")
            return(l)
        }
        
        f4<-function(data, cov){
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
            am<-fixef(m1)[1:nlevels(data$treatments)]
            c<-data.frame(levels(treatments), round(am,4), round(sqrt(diag(vcov(m1)))[1:nlevels(data$treatments)],4))
            colnames(c)<-c("plot*split.plot","adjusted.means", "standart.error")
            rownames(c)<-NULL
            df1<- anova(m) $"denDF"[2]
            df2<- anova(m) $"denDF"[1]
            
            mm1<-lme(response~-1+plot*split.plot, random=~1|subject, data=data, na.action=na.omit, contrasts=list(plot=contr.sum, split.plot=contr.sum), correlation=cor, weights=var, control=lmeControl(maxIter =6000, msMaxIter=6000,  niterEM=2000, opt="optim"))
            mm2<-lme(response~-1+split.plot*plot, random=~1|subject, data=data, na.action=na.omit, contrasts=list(split.plot=contr.sum, plot=contr.sum), correlation=cor, weights=var, control=lmeControl(maxIter =6000, msMaxIter=6000, niterEM=2000, opt="optim"))
            a11<-data.frame(levels(data$plot), round(fixef(mm1)[1:nlevels(data$plot)],4), round(sqrt(diag(vcov(mm1)))[1:nlevels(data$plot)],4))
            a22<-data.frame(levels(data$split.plot), round(fixef(mm2)[1:nlevels(data$split.plot)],2), round(sqrt(diag(vcov(mm2)))[1:nlevels(data$split.plot)],4))
            colnames(a11)<-c("plot","adjusted.mean", "standart.error")
            rownames(a11)<-NULL
            colnames(a22)<-c("split.plot","adjusted.mean", "standart.error")
            rownames(a22)<-NULL
            
            res=fr2(m,data)
            test1=fm(a11,df1)
            test2=fm(a22,df2)
            groups1=ft(test1, alpha); a11=a11[order(a11[,2], decreasing=TRUE),]
            mf1=data.frame(a11,groups1);rownames(mf1) = NULL
            groups2=ft(test2, alpha); a22=a22[order(a22[,2], decreasing=TRUE),]
            mf2=data.frame(a22,groups2);rownames(mf2) = NULL
            c1=rep(1:nlevels(data$split.plot), each=nlevels(data$plot))
            cs1=split(c, c1)
            test3=lapply(cs1, fm, dff=df1); n1=rep("plot in", nlevels(data$split.plot));n11=data.frame(n1,levels(data$split.plot)) 
            n11 <- apply(t(n11), 2, paste, collapse = "  ")
            names(test3)=n11
            c2=rep(1:nlevels(data$plot), nlevels(data$split.plot))
            cs2=split(c, c2)
            test4=lapply(cs2, fm, dff=df2); n2=rep("split.plot in", nlevels(data$plot));n22=data.frame(n2,levels(data$plot)) 
            n22 <- apply(t(n22), 2, paste, collapse = "  ")
            names(test4)=n22
            csf=function(x){a=x[order(x[,2], decreasing=TRUE),];return(a)}
            groups3=lapply(test3, ft, alpha)
            mft1=lapply(cs1,csf);
            dd<-function(x){l=data.frame(mft1[[x]],groups3[[x]]); return(l)}; nf1=as.list(c(1:nlevels(data$split.plot)))
            mft1=lapply(nf1,dd)
            names(mft1)=n11
            groups4=lapply(test4, ft, alpha)
            mft2=lapply(cs2,csf);
            ddd<-function(x){l=data.frame(mft2[[x]],groups4[[x]]); return(l)}; nf2=as.list(c(1:nlevels(data$plot)))
            mft2=lapply(nf2,ddd)
            names(mft2)=n22
                    
                     
            
            l=list(b,mf1,test1,mf2,test2,mft1,test3,mft2,test4, res)
            names(l)=c("Marginal anova (Type III Sum of Squares)","Adjusted means (plot)", "Multiple comparison test (plot)","Adjusted means (split.plot)", "Multiple comparison test (split.plot)", "Adjusted means (plot in levels of split.plot)", "Multiple comparison test (plot in levels of split.plot)", "Adjusted means (split.plot in levels of plot)", "Multiple comparison test (split.plot in levels of plot)","Residual analysis")
            return(l)
        }
        
        f5<-function(data, cov){ 
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
            c<-data.frame(levels(treatments), round(am,4), round(sqrt(diag(vcov(m1)))[1:nlevels(data$ treatments)],4))
            colnames(c)<-c("plot*split.plot","adjusted.mean", "standart.error")
            rownames(c)<-NULL
            df1<- anova(m) $"denDF"[2]
            df2<- anova(m) $"denDF"[1]
            mm1<-lme(response~-1+plot*split.plot+block, random=~1|subject, data=data, na.action=na.omit, contrasts=list(plot=contr.sum, split.plot=contr.sum, block=contr.sum), correlation=cor, weights=var, control=lmeControl(maxIter =6000, msMaxIter=6000, niterEM=2000, opt="optim"))
            mm2<-lme(response~-1+split.plot*plot+block, random=~1|subject, data=data, na.action=na.omit, contrasts=list(split.plot=contr.sum, plot=contr.sum, block=contr.sum), correlation=cor, weights=var, control=lmeControl(maxIter =6000, msMaxIter=6000, niterEM=2000, opt="optim"))
            a11<-data.frame(levels(data$plot), round(fixef(mm1)[1:nlevels(data$plot)],4), round(sqrt(diag(vcov(mm1)))[1:nlevels(data$plot)],4))
            a22<-data.frame(levels(data$split.plot), round(fixef(mm2)[1:nlevels(data$split.plot)],4), round(sqrt(diag(vcov(mm2)))[1:nlevels(data$split.plot)],4))
            colnames(a11)<-c("plot","adjusted.mean", "standart.error")
            rownames(a11)<-NULL
            colnames(a22)<-c("split.plot","adjusted.mean", "standart.error")
            rownames(a22)<-NULL
            
            res=fr2(m,data)
            test1=fm(a11,df1)
            test2=fm(a22,df2)
            groups1=ft(test1, alpha); a11=a11[order(a11[,2], decreasing=TRUE),]
            mf1=data.frame(a11,groups1);rownames(mf1) = NULL
            groups2=ft(test2, alpha); a22=a22[order(a22[,2], decreasing=TRUE),]
            mf2=data.frame(a22,groups2);rownames(mf2) = NULL
            c1=rep(1:nlevels(data$split.plot), each=nlevels(data$plot))
            cs1=split(c, c1)
            test3=lapply(cs1, fm, dff=df1); n1=rep("plot in", nlevels(data$split.plot));n11=data.frame(n1,levels(data$split.plot)) 
            n11 <- apply(t(n11), 2, paste, collapse = "  ")
            names(test3)=n11
            c2=rep(1:nlevels(data$plot), nlevels(data$split.plot))
            cs2=split(c, c2)
            test4=lapply(cs2, fm, dff=df2); n2=rep("split.plot in", nlevels(data$plot));n22=data.frame(n2,levels(data$plot)) 
            n22 <- apply(t(n22), 2, paste, collapse = "  ")
            names(test4)=n22
            csf=function(x){a=x[order(x[,2], decreasing=TRUE),];return(a)}
            groups3=lapply(test3, ft, alpha)
            mft1=lapply(cs1,csf);
            dd<-function(x){l=data.frame(mft1[[x]],groups3[[x]]); return(l)}; nf1=as.list(c(1:nlevels(data$split.plot)))
            mft1=lapply(nf1,dd)
            names(mft1)=n11
            groups4=lapply(test4, ft, alpha)
            mft2=lapply(cs2,csf);
            ddd<-function(x){l=data.frame(mft2[[x]],groups4[[x]]); return(l)}; nf2=as.list(c(1:nlevels(data$plot)))
            mft2=lapply(nf2,ddd)
            names(mft2)=n22
            
                      
            l=list(b,mf1,test1,mf2,test2,mft1,test3,mft2,test4, res)
            names(l)=c("Marginal anova (Type III Sum of Squares)","Adjusted means (plot)", "Multiple comparison test (plot)","Adjusted means (split.plot)", "Multiple comparison test (split.plot)", "Adjusted means (plot in levels of split.plot)", "Multiple comparison test (plot in levels of split.plot)", "Adjusted means (split.plot in levels of plot)", "Multiple comparison test (split.plot in levels of plot)","Residual analysis")
            return(l)
        }
        
        f6<-function(data, cov){ 
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
            c<-data.frame(levels(treatments), round(am,4), round(sqrt(diag(vcov(m1)))[1:nlevels(data$ treatments)],4))
            colnames(c)<-c("plot*split.plot","adjusted.mean", "standart.error")
            rownames(c)<-NULL
            df1<- anova(m) $"denDF"[2]
            df2<- anova(m) $"denDF"[1]
            
            mm1<-lme(response~-1+plot*split.plot+col+row, random=~1|subject, data=data, na.action=na.omit, contrasts=list(plot=contr.sum, split.plot=contr.sum, col=contr.sum, row=contr.sum), correlation=cor, weights=var, control=lmeControl(maxIter =6000, msMaxIter=6000, niterEM=2000, opt="optim"))
            mm2<-lme(response~-1+split.plot*plot+col+row, random=~1|subject, data=data, na.action=na.omit, contrasts=list(split.plot=contr.sum, plot=contr.sum, col=contr.sum, row=contr.sum), correlation=cor, weights=var, control=lmeControl(maxIter =6000, msMaxIter=6000, niterEM=2000, opt="optim"))
            a11<-data.frame(levels(data$plot), round(fixef(mm1)[1:nlevels(data$plot)],4), round(sqrt(diag(vcov(mm1)))[1:nlevels(data$plot)]),4)
            a22<-data.frame(levels(data$split.plot), round(fixef(mm2)[1:nlevels(data$split.plot)],4), round(sqrt(diag(vcov(mm2)))[1:nlevels(data$split.plot)],4))
            colnames(a11)<-c("plot","adjusted.mean", "standart.error")
            rownames(a11)<-NULL
            colnames(a22)<-c("split.plot","adjusted.mean", "standart.error")
            rownames(a22)<-NULL	
            
            res=fr2(m,data)
            test1=fm(a11,df1)
            test2=fm(a22,df2)
            groups1=ft(test1, alpha); a11=a11[order(a11[,2], decreasing=TRUE),]
            mf1=data.frame(a11,groups1);rownames(mf1) = NULL
            groups2=ft(test2, alpha); a22=a22[order(a22[,2], decreasing=TRUE),]
            mf2=data.frame(a22,groups2);rownames(mf2) = NULL
            c1=rep(1:nlevels(data$split.plot), each=nlevels(data$plot))
            cs1=split(c, c1)
            test3=lapply(cs1, fm, dff=df1); n1=rep("plot in", nlevels(data$split.plot));n11=data.frame(n1,levels(data$split.plot)) 
            n11 <- apply(t(n11), 2, paste, collapse = "  ")
            names(test3)=n11
            c2=rep(1:nlevels(data$plot), nlevels(data$split.plot))
            cs2=split(c, c2)
            test4=lapply(cs2, fm, dff=df2); n2=rep("split.plot in", nlevels(data$plot));n22=data.frame(n2,levels(data$plot)) 
            n22 <- apply(t(n22), 2, paste, collapse = "  ")
            names(test4)=n22
            csf=function(x){a=x[order(x[,2], decreasing=TRUE),];return(a)}
            groups3=lapply(test3, ft, alpha)
            mft1=lapply(cs1,csf);
            dd<-function(x){l=data.frame(mft1[[x]],groups3[[x]]); return(l)}; nf1=as.list(c(1:nlevels(data$split.plot)))
            mft1=lapply(nf1,dd)
            names(mft1)=n11
            groups4=lapply(test4, ft, alpha)
            mft2=lapply(cs2,csf);
            ddd<-function(x){l=data.frame(mft2[[x]],groups4[[x]]); return(l)}; nf2=as.list(c(1:nlevels(data$plot)))
            mft2=lapply(nf2,ddd)
            names(mft2)=n22
            
            l=list(b,mf1,test1,mf2,test2,mft1,test3,mft2,test4, res)
            names(l)=c("Marginal anova (Type III Sum of Squares)","Adjusted means (plot)", "Multiple comparison test (plot)","Adjusted means (split.plot)", "Multiple comparison test (split.plot)", "Adjusted means (plot in levels of split.plot)", "Multiple comparison test (plot in levels of split.plot)", "Adjusted means (split.plot in levels of plot)", "Multiple comparison test (split.plot in levels of plot)","Residual analysis")
            return(l)
        }
        
        
        f7<-function(data, cov){ 
            names(data)=c("factor_1","factor_2", "factor_3", "response")
            data<-data.frame(factor_1=factor(data$factor_1), factor_2=factor(data$factor_2), factor_3=factor(data$factor_3), response=data$response)
            m<-aov(response~factor_1*factor_2*factor_3,data=data, contrasts=list(factor_1=contr.sum, factor_2=contr.sum, factor_3=contr.sum))
            m1<-aov(response~-1+ factor_1*factor_2*factor_3,data=data, contrasts=list(factor_1=contr.sum, factor_2=contr.sum, factor_3=contr.sum))
            m2<-aov(response~-1+ factor_2*factor_1*factor_3,data=data, contrasts=list(factor_1=contr.sum, factor_2=contr.sum, factor_3=contr.sum))
            m3<-aov(response~-1+ factor_3*factor_1*factor_2,data=data, contrasts=list(factor_1=contr.sum, factor_2=contr.sum, factor_3=contr.sum))
            a2<-Anova(m, type=3) 
            a3<-a2[-1,]
            a3<-fa2(a3)
            data2<-na.omit(data)
            res=fr3(m,data2)
            
            treatments_f1f2=interaction(data$factor_1,data$factor_2)
            treatments_f1f3=interaction(data$factor_1,data$factor_3)
            treatments_f2f3=interaction(data$factor_2,data$factor_3)
            treatments_f1f2f3=interaction(data$factor_1,data$factor_2, data$factor_3)
            treatments_f2f1f3=interaction(data$factor_2,data$factor_1, data$factor_3)
            treatments_f3f1f2=interaction(data$factor_3,data$factor_1, data$factor_2)
            data<-data.frame(data,treatments_f1f2, treatments_f1f3, treatments_f2f3, treatments_f1f2f3, treatments_f2f1f3, treatments_f3f1f2)
            m4<-aov(response~-1+treatments_f1f2+factor_3*treatments_f1f2,data=data, contrasts=list(treatments_f1f2=contr.sum, factor_3=contr.sum))
            m5<-aov(response~-1+treatments_f1f3+factor_2*treatments_f1f3,data=data, contrasts=list(treatments_f1f3=contr.sum, factor_2=contr.sum))
            m6<-aov(response~-1+treatments_f2f3+factor_1*treatments_f2f3,data=data, contrasts=list(treatments_f2f3=contr.sum, factor_1=contr.sum))
            m7<-aov(response~-1+treatments_f1f2f3,data=data, contrasts=list(treatments_f1f2f3=contr.sum))
            m8<-aov(response~-1+treatments_f2f1f3,data=data, contrasts=list(treatments_f2f1f3=contr.sum))
            m9<-aov(response~-1+treatments_f3f1f2,data=data, contrasts=list(treatments_f3f1f2=contr.sum))
            
            adjusted.mean<-round(coef(m1)[1:nlevels(data$factor_1)],4)
            standart.error<-round(sqrt(diag(vcov(m1)) [1:nlevels(data$factor_1)]),4)
            factor_1<-levels(data$factor_1)
            maf1=data.frame(factor_1,adjusted.mean,standart.error)
            rownames(maf1)=NULL
            adjusted.mean<-round(coef(m2) [1:nlevels(data$factor_2)],4)
            standart.error<-round(sqrt(diag(vcov(m2))) [1:nlevels(data$factor_2)],4)
            factor_2<-levels(data$factor_2)
            maf2=data.frame(factor_2,adjusted.mean,standart.error)
            rownames(maf2)=NULL
            adjusted.mean<-round(coef(m3) [1:nlevels(data$factor_3)],4)
            standart.error<-round(sqrt(diag(vcov(m3))) [1:nlevels(data$factor_3)],4)
            factor_3<-levels(data$factor_3)
            maf3=data.frame(factor_3,adjusted.mean,standart.error)
            rownames(maf3)=NULL
            adjusted.mean<-round(coef(m4)[1:nlevels(data$treatments_f1f2)],4)
            standart.error<-round(sqrt(diag(vcov(m4)))[1:nlevels(data$treatments_f1f2)],4)
            treatments_f1f2<-levels(data$treatments_f1f2)
            mat1=data.frame(treatments_f1f2,adjusted.mean,standart.error)
            rownames(mat1)=NULL
            adjusted.mean<-round(coef(m5)[1:nlevels(data$treatments_f1f3)],4)
            standart.error<-round(sqrt(diag(vcov(m5)))[1:nlevels(data$treatments_f1f3)],4)
            treatments_f1f3<-levels(data$treatments_f1f3)
            mat2=data.frame(treatments_f1f3,adjusted.mean,standart.error)
            rownames(mat2)=NULL
            adjusted.mean<-round(coef(m6)[1:nlevels(data$treatments_f2f3)],4)
            standart.error<-round(sqrt(diag(vcov(m6)))[1:nlevels(data$treatments_f2f3)],4)
            treatments_f2f3<-levels(data$treatments_f2f3)
            mat3=data.frame(treatments_f2f3,adjusted.mean,standart.error)
            rownames(mat3)=NULL
            adjusted.mean<-round(coef(m7)[1:nlevels(data$treatments_f1f2f3)],4)
            standart.error<-round(sqrt(diag(vcov(m7)))[1:nlevels(data$treatments_f1f2f3)],4)
            treatments_f1f2f3<-levels(data$treatments_f1f2f3)
            mat4=data.frame(treatments_f1f2f3,adjusted.mean,standart.error)
            rownames(mat4)=NULL
            dff=df.residual(m)
            test1=fm(maf1,dff)
            test2=fm(maf2,dff)
            test3=fm(maf3,dff)
            groups1=ft(test1, alpha); maf1=maf1[order(maf1[,2], decreasing=TRUE),]
            mf1=data.frame(maf1,groups1)
            rownames(mf1) = NULL
            groups2=ft(test2, alpha); maf2=maf2[order(maf2[,2], decreasing=TRUE),]
            mf2=data.frame(maf2,groups2)
            rownames(mf2) = NULL
            groups3=ft(test3, alpha); maf3=maf3[order(maf3[,2], decreasing=TRUE),]
            mf3=data.frame(maf3,groups3)
            rownames(mf3) = NULL
            c1=rep(1:nlevels(data$factor_2), each=nlevels(data$factor_1))
            cs1=split(mat1, c1)
            test4=lapply(cs1, fm, dff=dff); n1=rep("factor_1 in", nlevels(data$factor_2));n11=data.frame(n1,levels(data$factor_2)) 
            n11 <- apply(t(n11), 2, paste, collapse = "  ")
            names(test4)=n11
            c2=rep(1:nlevels(data$factor_1), nlevels(data$factor_2))
            cs2=split(mat1, c2)
            test5=lapply(cs2, fm, dff=dff); n2=rep("factor_2 in", nlevels(data$factor_1));n22=data.frame(n2,levels(data$factor_1)) 
            n22 <- apply(t(n22), 2, paste, collapse = "  ")
            names(test5)=n22
            csf=function(x){a=x[order(x[,2], decreasing=TRUE),];return(a)}
            groups4=lapply(test4, ft, alpha)
            mft1=lapply(cs1,csf);
            dd<-function(x){l=data.frame(mft1[[x]],groups4[[x]]); return(l)}; nf1=as.list(c(1:nlevels(data$factor_2)))
            mft1=lapply(nf1,dd)
            names(mft1)=n11
            groups5=lapply(test5, ft, alpha)
            mft2=lapply(cs2,csf);
            ddd<-function(x){l=data.frame(mft2[[x]],groups5[[x]]); return(l)}; nf2=as.list(c(1:nlevels(data$factor_1)))
            mft2=lapply(nf2,ddd)
            names(mft2)=n22
            c1=rep(1:nlevels(data$factor_3), each=nlevels(data$factor_1))
            cs1=split(mat2, c1)
            test6=lapply(cs1, fm, dff=dff); n1=rep("factor_1 in", nlevels(data$factor_3));n11=data.frame(n1,levels(data$factor_3)) 
            n11 <- apply(t(n11), 2, paste, collapse = "  ")
            names(test6)=n11
            c2=rep(1:nlevels(data$factor_1), nlevels(data$factor_3))
            cs2=split(mat2, c2)
            test7=lapply(cs2, fm, dff=dff); n2=rep("factor_3 in", nlevels(data$factor_1));n22=data.frame(n2,levels(data$factor_1)) 
            n22 <- apply(t(n22), 2, paste, collapse = "  ")
            names(test7)=n22
            csf=function(x){a=x[order(x[,2], decreasing=TRUE),];return(a)}
            groups6=lapply(test6, ft, alpha)
            mft3=lapply(cs1,csf);
            dd<-function(x){l=data.frame(mft3[[x]],groups6[[x]]); return(l)}; nf1=as.list(c(1:nlevels(data$factor_3)))
            mft3=lapply(nf1,dd)
            names(mft3)=n11
            groups7=lapply(test7, ft, alpha)
            mft4=lapply(cs2,csf);
            ddd<-function(x){l=data.frame(mft4[[x]],groups7[[x]]); return(l)}; nf2=as.list(c(1:nlevels(data$factor_1)))
            mft4=lapply(nf2,ddd)
            names(mft4)=n22
            c1=rep(1:nlevels(data$factor_3), each=nlevels(data$factor_2))
            cs1=split(mat3, c1)
            test8=lapply(cs1, fm, dff=dff); n1=rep("factor_2 in", nlevels(data$factor_3));n11=data.frame(n1,levels(data$factor_3)) 
            n11 <- apply(t(n11), 2, paste, collapse = "  ")
            names(test8)=n11
            c2=rep(1:nlevels(data$factor_2), nlevels(data$factor_3))
            cs2=split(mat3, c2)
            test9=lapply(cs2, fm, dff=dff); n2=rep("factor_3 in", nlevels(data$factor_2));n22=data.frame(n2,levels(data$factor_2)) 
            n22 <- apply(t(n22), 2, paste, collapse = "  ")
            names(test9)=n22
            csf=function(x){a=x[order(x[,2], decreasing=TRUE),];return(a)}
            groups8=lapply(test8, ft, alpha)
            mft5=lapply(cs1,csf);
            dd<-function(x){l=data.frame(mft5[[x]],groups8[[x]]); return(l)}; nf1=as.list(c(1:nlevels(data$factor_3)))
            mft5=lapply(nf1,dd)
            names(mft5)=n11
            groups9=lapply(test9, ft, alpha)
            mft6=lapply(cs2,csf);
            ddd<-function(x){l=data.frame(mft6[[x]],groups9[[x]]); return(l)}; nf2=as.list(c(1:nlevels(data$factor_2)))
            mft6=lapply(nf2,ddd)
            names(mft6)=n22
            c1=rep(1:nlevels(data$treatments_f2f3), each=nlevels(data$factor_1))
            cs1=split(mat4, c1)
            test10=lapply(cs1, fm, dff=dff); n1=rep("factor_1 in", nlevels(data$treatments_f2f3));n11=data.frame(n1,levels(data$treatments_f2f3)) 
            n11 <- apply(t(n11), 2, paste, collapse = "  ")
            names(test10)=n11
            csf=function(x){a=x[order(x[,2], decreasing=TRUE),];return(a)}
            groups10=lapply(test10, ft, alpha)
            mft7=lapply(cs1,csf);
            dd<-function(x){l=data.frame(mft7[[x]],groups10[[x]]); return(l)}; nf1=as.list(c(1:nlevels(data$treatments_f2f3)))
            mft7=lapply(nf1,dd)
            names(mft7)=n11
            c1=rep(1:nlevels(data$factor_1), nlevels(data$factor_2));c1=rep(c1,nlevels(data$factor_3))
            s=rep(0:(nlevels(data$factor_3)-1), each=nlevels(data$treatments_f1f2)); s=2*s; c1=c1+s
            cs1=split(mat4, c1)
            test11=lapply(cs1, fm, dff=dff); n1=rep("factor_2 in", nlevels(data$treatments_f1f3));n11=data.frame(n1,levels(data$treatments_f1f3)) 
            n11 <- apply(t(n11), 2, paste, collapse = "  ")
            names(test11)=n11
            csf=function(x){a=x[order(x[,2], decreasing=TRUE),];return(a)}
            groups11=lapply(test11, ft, alpha)
            mft8=lapply(cs1,csf);
            dd<-function(x){l=data.frame(mft8[[x]],groups11[[x]]); return(l)}; nf1=as.list(c(1:nlevels(data$treatments_f1f3)))
            mft8=lapply(nf1,dd)
            names(mft8)=n11
            c1=rep(1:nlevels(data$treatments_f1f2), nlevels(data$factor_3))
            cs1=split(mat4, c1)
            test12=lapply(cs1, fm, dff=dff); n1=rep("factor_3 in", nlevels(data$treatments_f1f2));n11=data.frame(n1,levels(data$treatments_f1f2)) 
            n11 <- apply(t(n11), 2, paste, collapse = "  ")
            names(test12)=n11
            csf=function(x){a=x[order(x[,2], decreasing=TRUE),];return(a)}
            groups12=lapply(test12, ft, alpha)
            mft9=lapply(cs1,csf);
            dd<-function(x){l=data.frame(mft9[[x]],groups12[[x]]); return(l)}; nf1=as.list(c(1:nlevels(data$treatments_f1f2)))
            mft9=lapply(nf1,dd)
            names(mft9)=n11
            l<-list(a3,  mf1, test1, mf2,test2,mf3,test3,   mft1,test4,mft2,test5,  mft3,test6,mft4,test7,  mft5,test8,mft6,test9,  mft7,test10, mft8,test11, mft9,test12, res)
            names(l)= list("Analysis of variance", "Adjusted means (factor 1)", "Multiple comparison test (factor 1)","Adjusted means (factor 2)", "Multiple comparison test (factor 2)", "Adjusted means (factor 3)", "Multiple comparison test (factor 3)", "Adjusted means (factor 1 in levels of factor 2)", "Multiple comparison test (factor 1 in levels of factor 2)", "Adjusted means (factor 2 in levels of factor 1)", "Multiple comparison test (factor 2 in levels of factor 1)","Adjusted means (factor 1 in levels of factor 3)", "Multiple comparison test (factor 1 in levels of factor 3)", "Adjusted means (factor 3 in levels of factor 1)", "Multiple comparison test (factor 3 in levels of factor 1)","Adjusted means (factor 2 in levels of factor 3)", "Multiple comparison test (factor 2 in levels of factor 3)", "Adjusted means (factor 3 in levels of factor 2)", "Multiple comparison test (factor 3 in levels of factor 2)","Adjusted means (factor 1 in levels of treatments factor2*factor3)", "Multiple comparison test (factor 1 in levels of treatments factor2*factor3)","Adjusted means (factor 2 in levels of treatments factor1*factor3)", "Multiple comparison test (factor 2 in levels of treatments factor1*factor3)", "Adjusted means (factor 3 in levels of treatments factor1*factor2)","Multiple comparison test (factor 3 in levels of treatments factor1*factor2)","Residual analysis")
            return(l)}
        
        
        f8<-function(data, cov){ 
            names(data)=c("factor_1","factor_2", "factor_3", "blocks","response")
            data<-data.frame(factor_1=factor(data$factor_1), factor_2=factor(data$factor_2), factor_3=factor(data$factor_3), blocks=factor(data$blocks), response=data$response)
            m<-aov(response~factor_1*factor_2*factor_3+ blocks,data=data, contrasts=list(factor_1=contr.sum, factor_2=contr.sum, factor_3=contr.sum, blocks=contr.sum))
            m1<-aov(response~-1+ factor_1*factor_2*factor_3+ blocks,data=data, contrasts=list(factor_1=contr.sum, factor_2=contr.sum, factor_3=contr.sum, blocks=contr.sum))
            m2<-aov(response~-1+ factor_2*factor_1*factor_3+ blocks,data=data, contrasts=list(factor_1=contr.sum, factor_2=contr.sum, factor_3=contr.sum, blocks=contr.sum))
            m3<-aov(response~-1+ factor_3*factor_1*factor_2+ blocks,data=data, contrasts=list(factor_1=contr.sum, factor_2=contr.sum, factor_3=contr.sum, blocks=contr.sum))
            a2<-Anova(m, type=3) 
            a3<-a2[-1,]
            a3<-fa2(a3)
            data2<-na.omit(data)
            res=fr3(m,data2)
            
            treatments_f1f2=interaction(data$factor_1,data$factor_2)
            treatments_f1f3=interaction(data$factor_1,data$factor_3)
            treatments_f2f3=interaction(data$factor_2,data$factor_3)
            treatments_f1f2f3=interaction(data$factor_1,data$factor_2, data$factor_3)
            treatments_f2f1f3=interaction(data$factor_2,data$factor_1, data$factor_3)
            treatments_f3f1f2=interaction(data$factor_3,data$factor_1, data$factor_2)
            data<-data.frame(data,treatments_f1f2, treatments_f1f3, treatments_f2f3, treatments_f1f2f3, treatments_f2f1f3, treatments_f3f1f2)
            m4<-aov(response~-1+treatments_f1f2+factor_3*treatments_f1f2+ blocks,data=data, contrasts=list(treatments_f1f2=contr.sum, factor_3=contr.sum, blocks=contr.sum))
            m5<-aov(response~-1+treatments_f1f3+factor_2*treatments_f1f3+ blocks,data=data, contrasts=list(treatments_f1f3=contr.sum, factor_2=contr.sum, blocks=contr.sum))
            m6<-aov(response~-1+treatments_f2f3+factor_1*treatments_f2f3+blocks,data=data, contrasts=list(treatments_f2f3=contr.sum, factor_1=contr.sum, blocks=contr.sum))
            m7<-aov(response~-1+treatments_f1f2f3+ blocks,data=data, contrasts=list(treatments_f1f2f3=contr.sum, blocks=contr.sum))
            m8<-aov(response~-1+treatments_f2f1f3+ blocks,data=data, contrasts=list(treatments_f2f1f3=contr.sum, blocks=contr.sum))
            m9<-aov(response~-1+treatments_f3f1f2+ blocks,data=data, contrasts=list(treatments_f3f1f2=contr.sum, blocks=contr.sum))
            
            adjusted.mean<-round(coef(m1)[1:nlevels(data$factor_1)],4)
            standart.error<-round(sqrt(diag(vcov(m1)) [1:nlevels(data$factor_1)]),4)
            factor_1<-levels(data$factor_1)
            maf1=data.frame(factor_1,adjusted.mean,standart.error)
            rownames(maf1)=NULL
            adjusted.mean<-round(coef(m2) [1:nlevels(data$factor_2)],4)
            standart.error<-round(sqrt(diag(vcov(m2))) [1:nlevels(data$factor_2)],4)
            factor_2<-levels(data$factor_2)
            maf2=data.frame(factor_2,adjusted.mean,standart.error)
            rownames(maf2)=NULL
            adjusted.mean<-round(coef(m3) [1:nlevels(data$factor_3)],4)
            standart.error<-round(sqrt(diag(vcov(m3))) [1:nlevels(data$factor_3)],4)
            factor_3<-levels(data$factor_3)
            maf3=data.frame(factor_3,adjusted.mean,standart.error)
            rownames(maf3)=NULL
            adjusted.mean<-round(coef(m4)[1:nlevels(data$treatments_f1f2)],4)
            standart.error<-round(sqrt(diag(vcov(m4)))[1:nlevels(data$treatments_f1f2)],4)
            treatments_f1f2<-levels(data$treatments_f1f2)
            mat1=data.frame(treatments_f1f2,adjusted.mean,standart.error)
            rownames(mat1)=NULL
            adjusted.mean<-round(coef(m5)[1:nlevels(data$treatments_f1f3)],4)
            standart.error<-round(sqrt(diag(vcov(m5)))[1:nlevels(data$treatments_f1f3)],4)
            treatments_f1f3<-levels(data$treatments_f1f3)
            mat2=data.frame(treatments_f1f3,adjusted.mean,standart.error)
            rownames(mat2)=NULL
            adjusted.mean<-round(coef(m6)[1:nlevels(data$treatments_f2f3)],4)
            standart.error<-round(sqrt(diag(vcov(m6)))[1:nlevels(data$treatments_f2f3)],4)
            treatments_f2f3<-levels(data$treatments_f2f3)
            mat3=data.frame(treatments_f2f3,adjusted.mean,standart.error)
            rownames(mat3)=NULL
            adjusted.mean<-round(coef(m7)[1:nlevels(data$treatments_f1f2f3)],4)
            standart.error<-round(sqrt(diag(vcov(m7)))[1:nlevels(data$treatments_f1f2f3)],4)
            treatments_f1f2f3<-levels(data$treatments_f1f2f3)
            mat4=data.frame(treatments_f1f2f3,adjusted.mean,standart.error)
            rownames(mat4)=NULL
            dff=df.residual(m)
            test1=fm(maf1,dff)
            test2=fm(maf2,dff)
            test3=fm(maf3,dff)
            groups1=ft(test1, alpha); maf1=maf1[order(maf1[,2], decreasing=TRUE),]
            mf1=data.frame(maf1,groups1)
            rownames(mf1) = NULL
            groups2=ft(test2, alpha); maf2=maf2[order(maf2[,2], decreasing=TRUE),]
            mf2=data.frame(maf2,groups2)
            rownames(mf2) = NULL
            groups3=ft(test3, alpha); maf3=maf3[order(maf3[,2], decreasing=TRUE),]
            mf3=data.frame(maf3,groups3)
            rownames(mf3) = NULL
            c1=rep(1:nlevels(data$factor_2), each=nlevels(data$factor_1))
            cs1=split(mat1, c1)
            test4=lapply(cs1, fm, dff=dff); n1=rep("factor_1 in", nlevels(data$factor_2));n11=data.frame(n1,levels(data$factor_2)) 
            n11 <- apply(t(n11), 2, paste, collapse = "  ")
            names(test4)=n11
            c2=rep(1:nlevels(data$factor_1), nlevels(data$factor_2))
            cs2=split(mat1, c2)
            test5=lapply(cs2, fm, dff=dff); n2=rep("factor_2 in", nlevels(data$factor_1));n22=data.frame(n2,levels(data$factor_1)) 
            n22 <- apply(t(n22), 2, paste, collapse = "  ")
            names(test5)=n22
            csf=function(x){a=x[order(x[,2], decreasing=TRUE),];return(a)}
            groups4=lapply(test4, ft, alpha)
            mft1=lapply(cs1,csf);
            dd<-function(x){l=data.frame(mft1[[x]],groups4[[x]]); return(l)}; nf1=as.list(c(1:nlevels(data$factor_2)))
            mft1=lapply(nf1,dd)
            names(mft1)=n11
            groups5=lapply(test5, ft, alpha)
            mft2=lapply(cs2,csf);
            ddd<-function(x){l=data.frame(mft2[[x]],groups5[[x]]); return(l)}; nf2=as.list(c(1:nlevels(data$factor_1)))
            mft2=lapply(nf2,ddd)
            names(mft2)=n22
            c1=rep(1:nlevels(data$factor_3), each=nlevels(data$factor_1))
            cs1=split(mat2, c1)
            test6=lapply(cs1, fm, dff=dff); n1=rep("factor_1 in", nlevels(data$factor_3));n11=data.frame(n1,levels(data$factor_3)) 
            n11 <- apply(t(n11), 2, paste, collapse = "  ")
            names(test6)=n11
            c2=rep(1:nlevels(data$factor_1), nlevels(data$factor_3))
            cs2=split(mat2, c2)
            test7=lapply(cs2, fm, dff=dff); n2=rep("factor_3 in", nlevels(data$factor_1));n22=data.frame(n2,levels(data$factor_1)) 
            n22 <- apply(t(n22), 2, paste, collapse = "  ")
            names(test7)=n22
            csf=function(x){a=x[order(x[,2], decreasing=TRUE),];return(a)}
            groups6=lapply(test6, ft, alpha)
            mft3=lapply(cs1,csf);
            dd<-function(x){l=data.frame(mft3[[x]],groups6[[x]]); return(l)}; nf1=as.list(c(1:nlevels(data$factor_3)))
            mft3=lapply(nf1,dd)
            names(mft3)=n11
            groups7=lapply(test7, ft, alpha)
            mft4=lapply(cs2,csf);
            ddd<-function(x){l=data.frame(mft4[[x]],groups7[[x]]); return(l)}; nf2=as.list(c(1:nlevels(data$factor_1)))
            mft4=lapply(nf2,ddd)
            names(mft4)=n22
            c1=rep(1:nlevels(data$factor_3), each=nlevels(data$factor_2))
            cs1=split(mat3, c1)
            test8=lapply(cs1, fm, dff=dff); n1=rep("factor_2 in", nlevels(data$factor_3));n11=data.frame(n1,levels(data$factor_3)) 
            n11 <- apply(t(n11), 2, paste, collapse = "  ")
            names(test8)=n11
            c2=rep(1:nlevels(data$factor_2), nlevels(data$factor_3))
            cs2=split(mat3, c2)
            test9=lapply(cs2, fm, dff=dff); n2=rep("factor_3 in", nlevels(data$factor_2));n22=data.frame(n2,levels(data$factor_2)) 
            n22 <- apply(t(n22), 2, paste, collapse = "  ")
            names(test9)=n22
            csf=function(x){a=x[order(x[,2], decreasing=TRUE),];return(a)}
            groups8=lapply(test8, ft, alpha)
            mft5=lapply(cs1,csf);
            dd<-function(x){l=data.frame(mft5[[x]],groups8[[x]]); return(l)}; nf1=as.list(c(1:nlevels(data$factor_3)))
            mft5=lapply(nf1,dd)
            names(mft5)=n11
            groups9=lapply(test9, ft, alpha)
            mft6=lapply(cs2,csf);
            ddd<-function(x){l=data.frame(mft6[[x]],groups9[[x]]); return(l)}; nf2=as.list(c(1:nlevels(data$factor_2)))
            mft6=lapply(nf2,ddd)
            names(mft6)=n22
            c1=rep(1:nlevels(data$treatments_f2f3), each=nlevels(data$factor_1))
            cs1=split(mat4, c1)
            test10=lapply(cs1, fm, dff=dff); n1=rep("factor_1 in", nlevels(data$treatments_f2f3));n11=data.frame(n1,levels(data$treatments_f2f3)) 
            n11 <- apply(t(n11), 2, paste, collapse = "  ")
            names(test10)=n11
            csf=function(x){a=x[order(x[,2], decreasing=TRUE),];return(a)}
            groups10=lapply(test10, ft, alpha)
            mft7=lapply(cs1,csf);
            dd<-function(x){l=data.frame(mft7[[x]],groups10[[x]]); return(l)}; nf1=as.list(c(1:nlevels(data$treatments_f2f3)))
            mft7=lapply(nf1,dd)
            names(mft7)=n11
            c1=rep(1:nlevels(data$factor_1), nlevels(data$factor_2));c1=rep(c1,nlevels(data$factor_3))
            s=rep(0:(nlevels(data$factor_3)-1), each=nlevels(data$treatments_f1f2)); s=2*s; c1=c1+s
            cs1=split(mat4, c1)
            test11=lapply(cs1, fm, dff=dff); n1=rep("factor_2 in", nlevels(data$treatments_f1f3));n11=data.frame(n1,levels(data$treatments_f1f3)) 
            n11 <- apply(t(n11), 2, paste, collapse = "  ")
            names(test11)=n11
            csf=function(x){a=x[order(x[,2], decreasing=TRUE),];return(a)}
            groups11=lapply(test11, ft, alpha)
            mft8=lapply(cs1,csf);
            dd<-function(x){l=data.frame(mft8[[x]],groups11[[x]]); return(l)}; nf1=as.list(c(1:nlevels(data$treatments_f1f3)))
            mft8=lapply(nf1,dd)
            names(mft8)=n11
            c1=rep(1:nlevels(data$treatments_f1f2), nlevels(data$factor_3))
            cs1=split(mat4, c1)
            test12=lapply(cs1, fm, dff=dff); n1=rep("factor_3 in", nlevels(data$treatments_f1f2));n11=data.frame(n1,levels(data$treatments_f1f2)) 
            n11 <- apply(t(n11), 2, paste, collapse = "  ")
            names(test12)=n11
            csf=function(x){a=x[order(x[,2], decreasing=TRUE),];return(a)}
            groups12=lapply(test12, ft, alpha)
            mft9=lapply(cs1,csf);
            dd<-function(x){l=data.frame(mft9[[x]],groups12[[x]]); return(l)}; nf1=as.list(c(1:nlevels(data$treatments_f1f2)))
            mft9=lapply(nf1,dd)
            names(mft9)=n11
            l<-list(a3,  mf1, test1, mf2,test2,mf3,test3,   mft1,test4,mft2,test5,  mft3,test6,mft4,test7,  mft5,test8,mft6,test9,  mft7,test10, mft8,test11, mft9,test12, res)
            names(l)= list("Analysis of variance", "Adjusted means (factor 1)", "Multiple comparison test (factor 1)","Adjusted means (factor 2)", "Multiple comparison test (factor 2)", "Adjusted means (factor 3)", "Multiple comparison test (factor 3)", "Adjusted means (factor 1 in levels of factor 2)", "Multiple comparison test (factor 1 in levels of factor 2)", "Adjusted means (factor 2 in levels of factor 1)", "Multiple comparison test (factor 2 in levels of factor 1)","Adjusted means (factor 1 in levels of factor 3)", "Multiple comparison test (factor 1 in levels of factor 3)", "Adjusted means (factor 3 in levels of factor 1)", "Multiple comparison test (factor 3 in levels of factor 1)","Adjusted means (factor 2 in levels of factor 3)", "Multiple comparison test (factor 2 in levels of factor 3)", "Adjusted means (factor 3 in levels of factor 2)", "Multiple comparison test (factor 3 in levels of factor 2)","Adjusted means (factor 1 in levels of treatments factor2*factor3)", "Multiple comparison test (factor 1 in levels of treatments factor2*factor3)","Adjusted means (factor 2 in levels of treatments factor1*factor3)", "Multiple comparison test (factor 2 in levels of treatments factor1*factor3)", "Adjusted means (factor 3 in levels of treatments factor1*factor2)","Multiple comparison test (factor 3 in levels of treatments factor1*factor2)","Residual analysis")
            return(l)
        }
        
        f9<-function(data, cov){ 
            gg=cov
            names(data)=c("factor_1","factor_2", "factor_3","rep","response")
            data<-data.frame(factor_1=factor(data$factor_1), factor_2=factor(data$factor_2), factor_3=factor(data$factor_3),  rep=factor(data$rep), response=data$response)
            subject<-interaction(data$factor_1,data$factor_2,data$rep)
            data<-data.frame(data,subject)
            UN<-corSymm(form=~1|subject)
            AR<-corAR1(form=~1|subject)
            ARH<-corAR1(form=~1|subject)
            CS<- corCompSymm (form=~1|subject) 
            CAR<-corCAR1(form=~factor_3|subject)
            o1<-list(AR,ARH, CAR, CS, UN)
            cor<-o1[[gg]]
            UN1<-varIdent(form=~1|factor_3)
            AR1<-NULL
            ARH1<- varIdent(form=~1|factor_3)
            CS1<-NULL
            CAR1<-NULL
            o2<-list(AR1,ARH1, CAR1, CS1, UN1)
            var<-o2[[gg]]
            m<- lme(response~factor_1*factor_2*factor_3, random=~1|subject, correlation=cor, weights=var , data=data, contrasts=list(factor_1=contr.sum, factor_2=contr.sum, factor_3=contr.sum), control=lmeControl(maxIter =6000, msMaxIter=6000, niterEM=2000, opt="optim"),na.action=na.omit)
            m1<-lme(response~-1+ factor_1*factor_2*factor_3, random=~1|subject, correlation=cor, weights=var ,data=data, contrasts=list(factor_1=contr.sum, factor_2=contr.sum, factor_3=contr.sum), control=lmeControl(maxIter =6000, msMaxIter=6000, niterEM=2000, opt="optim"),na.action=na.omit)
            m2<-lme(response~-1+ factor_2*factor_1*factor_3, random=~1|subject, correlation=cor, weights=var ,data=data, contrasts=list(factor_1=contr.sum, factor_2=contr.sum, factor_3=contr.sum), control=lmeControl(maxIter =6000, msMaxIter=6000, niterEM=2000, opt="optim"),na.action=na.omit)
            m3<-lme(response~-1+ factor_3*factor_1*factor_2, random=~1|subject, correlation=cor, weights=var , data=data, contrasts=list(factor_1=contr.sum, factor_2=contr.sum, factor_3=contr.sum), control=lmeControl(maxIter =6000, msMaxIter=6000, niterEM=2000, opt="optim"),na.action=na.omit)
            a3<-anova(m, type="marginal")[-1,]
            
            df1<- anova(m) $"denDF"[2]
            df2<- anova(m) $"denDF"[1]
            treatments_f1f2=interaction(data$factor_1,data$factor_2)
            treatments_f1f3=interaction(data$factor_1,data$factor_3)
            treatments_f2f3=interaction(data$factor_2,data$factor_3)
            treatments_f1f2f3=interaction(data$factor_1,data$factor_2, data$factor_3)
            treatments_f2f1f3=interaction(data$factor_2,data$factor_1, data$factor_3)
            treatments_f3f1f2=interaction(data$factor_3,data$factor_1, data$factor_2)
            data<-data.frame(data,treatments_f1f2, treatments_f1f3, treatments_f2f3, treatments_f1f2f3, treatments_f2f1f3, treatments_f3f1f2)
            m4<-lme(response~-1+treatments_f1f2+factor_3*treatments_f1f2, random=~1|subject, correlation=cor, weights=var ,data=data, contrasts=list(treatments_f1f2=contr.sum, factor_3=contr.sum), control=lmeControl(maxIter =6000, msMaxIter=6000, niterEM=2000, opt="optim"),na.action=na.omit)
            m5<-lme(response~-1+treatments_f1f3+factor_2*treatments_f1f3, random=~1|subject, correlation=cor, weights=var ,data=data, contrasts=list(treatments_f1f3=contr.sum, factor_2=contr.sum), control=lmeControl(maxIter =6000, msMaxIter=6000, niterEM=2000, opt="optim"),na.action=na.omit)
            m6<-lme(response~-1+treatments_f2f3+factor_1*treatments_f2f3, random=~1|subject, correlation=cor, weights=var ,data=data, contrasts=list(treatments_f2f3=contr.sum, factor_1=contr.sum), control=lmeControl(maxIter =6000, msMaxIter=6000, niterEM=2000, opt="optim"),na.action=na.omit)
            m7<-lme(response~-1+treatments_f1f2f3, random=~1|subject, correlation=cor, weights=var ,data=data, contrasts=list(treatments_f1f2f3=contr.sum), control=lmeControl(maxIter =6000, msMaxIter=6000, niterEM=2000, opt="optim"),na.action=na.omit)
            m8<-lme(response~-1+treatments_f2f1f3, random=~1|subject, correlation=cor, weights=var ,data=data, contrasts=list(treatments_f2f1f3=contr.sum), control=lmeControl(maxIter =6000, msMaxIter=6000, niterEM=2000, opt="optim"),na.action=na.omit)
            m9<-lme(response~-1+treatments_f3f1f2, random=~1|subject, correlation=cor, weights=var ,data=data, contrasts=list(treatments_f3f1f2=contr.sum), control=lmeControl(maxIter =6000, msMaxIter=6000, niterEM=2000, opt="optim"),na.action=na.omit)
            
            res=fr4(m,data)
            adjusted.mean<-round(fixef(m1)[1:nlevels(data$factor_1)],4)
            standart.error<-round(sqrt(diag(vcov(m1)) [1:nlevels(data$factor_1)]),4)
            factor_1<-levels(data$factor_1)
            maf1=data.frame(factor_1,adjusted.mean,standart.error)
            rownames(maf1)=NULL
            adjusted.mean<-round(fixef(m2) [1:nlevels(data$factor_2)],4)
            standart.error<-round(sqrt(diag(vcov(m2))) [1:nlevels(data$factor_2)],4)
            factor_2<-levels(data$factor_2)
            maf2=data.frame(factor_2,adjusted.mean,standart.error)
            rownames(maf2)=NULL
            adjusted.mean<-round(fixef(m3) [1:nlevels(data$factor_3)],4)
            standart.error<-round(sqrt(diag(vcov(m3))) [1:nlevels(data$factor_3)],4)
            factor_3<-levels(data$factor_3)
            maf3=data.frame(factor_3,adjusted.mean,standart.error)
            rownames(maf3)=NULL
            adjusted.mean<-round(fixef(m4)[1:nlevels(data$treatments_f1f2)],4)
            standart.error<-round(sqrt(diag(vcov(m4)))[1:nlevels(data$treatments_f1f2)],4)
            treatments_f1f2<-levels(data$treatments_f1f2)
            mat1=data.frame(treatments_f1f2,adjusted.mean,standart.error)
            rownames(mat1)=NULL
            adjusted.mean<-round(fixef(m5)[1:nlevels(data$treatments_f1f3)],4)
            standart.error<-round(sqrt(diag(vcov(m5)))[1:nlevels(data$treatments_f1f3)],4)
            treatments_f1f3<-levels(data$treatments_f1f3)
            mat2=data.frame(treatments_f1f3,adjusted.mean,standart.error)
            rownames(mat2)=NULL
            adjusted.mean<-round(fixef(m6)[1:nlevels(data$treatments_f2f3)],2)
            standart.error<-round(sqrt(diag(vcov(m6)))[1:nlevels(data$treatments_f2f3)],2)
            treatments_f2f3<-levels(data$treatments_f2f3)
            mat3=data.frame(treatments_f2f3,adjusted.mean,standart.error)
            rownames(mat3)=NULL
            adjusted.mean<-round(fixef(m7)[1:nlevels(data$treatments_f1f2f3)],4)
            standart.error<-round(sqrt(diag(vcov(m7)))[1:nlevels(data$treatments_f1f2f3)],4)
            treatments_f1f2f3<-levels(data$treatments_f1f2f3)
            mat4=data.frame(treatments_f1f2f3,adjusted.mean,standart.error)
            rownames(mat4)=NULL
            test1=fm(maf1,df1)
            test2=fm(maf2,df1)
            test3=fm(maf3,df2)
            groups1=ft(test1, alpha); maf1=maf1[order(maf1[,2], decreasing=TRUE),]
            mf1=data.frame(maf1,groups1)
            rownames(mf1) = NULL
            groups2=ft(test2, alpha); maf2=maf2[order(maf2[,2], decreasing=TRUE),]
            mf2=data.frame(maf2,groups2)
            rownames(mf2) = NULL
            groups3=ft(test3, alpha); maf3=maf3[order(maf3[,2], decreasing=TRUE),]
            mf3=data.frame(maf3,groups3)
            rownames(mf3) = NULL
            c1=rep(1:nlevels(data$factor_2), each=nlevels(data$factor_1))
            cs1=split(mat1, c1)
            
            test4=lapply(cs1, fm, dff=df1); n1=rep("factor_1 in", nlevels(data$factor_2));n11=data.frame(n1,levels(data$factor_2)) 
            n11 <- apply(t(n11), 2, paste, collapse = "  ")
            names(test4)=n11
            c2=rep(1:nlevels(data$factor_1), nlevels(data$factor_2))
            cs2=split(mat1, c2)
            test5=lapply(cs2, fm, dff=df1); n2=rep("factor_2 in", nlevels(data$factor_1));n22=data.frame(n2,levels(data$factor_1)) 
            n22 <- apply(t(n22), 2, paste, collapse = "  ")
            names(test5)=n22
            csf=function(x){a=x[order(x[,2], decreasing=TRUE),];return(a)}
            groups4=lapply(test4, ft, alpha)
            mft1=lapply(cs1,csf);
            dd<-function(x){l=data.frame(mft1[[x]],groups4[[x]]); return(l)}; nf1=as.list(c(1:nlevels(data$factor_2)))
            mft1=lapply(nf1,dd)
            names(mft1)=n11
            groups5=lapply(test5, ft, alpha)
            mft2=lapply(cs2,csf);
            ddd<-function(x){l=data.frame(mft2[[x]],groups5[[x]]); return(l)}; nf2=as.list(c(1:nlevels(data$factor_1)))
            mft2=lapply(nf2,ddd)
            names(mft2)=n22
            c1=rep(1:nlevels(data$factor_3), each=nlevels(data$factor_1))
            cs1=split(mat2, c1)
            test6=lapply(cs1, fm, dff=df2); n1=rep("factor_1 in", nlevels(data$factor_3));n11=data.frame(n1,levels(data$factor_3)) 
            n11 <- apply(t(n11), 2, paste, collapse = "  ")
            names(test6)=n11
            c2=rep(1:nlevels(data$factor_1), nlevels(data$factor_3))
            cs2=split(mat2, c2)
            test7=lapply(cs2, fm, dff=df2); n2=rep("factor_3 in", nlevels(data$factor_1));n22=data.frame(n2,levels(data$factor_1)) 
            n22 <- apply(t(n22), 2, paste, collapse = "  ")
            names(test7)=n22
            csf=function(x){a=x[order(x[,2], decreasing=TRUE),];return(a)}
            groups6=lapply(test6, ft, alpha)
            mft3=lapply(cs1,csf);
            dd<-function(x){l=data.frame(mft3[[x]],groups6[[x]]); return(l)}; nf1=as.list(c(1:nlevels(data$factor_3)))
            mft3=lapply(nf1,dd)
            names(mft3)=n11
            groups7=lapply(test7, ft, alpha)
            mft4=lapply(cs2,csf);
            ddd<-function(x){l=data.frame(mft4[[x]],groups7[[x]]); return(l)}; nf2=as.list(c(1:nlevels(data$factor_1)))
            mft4=lapply(nf2,ddd)
            names(mft4)=n22
            c1=rep(1:nlevels(data$factor_3), each=nlevels(data$factor_2))
            cs1=split(mat3, c1)
            test8=lapply(cs1, fm, dff=df2); n1=rep("factor_2 in", nlevels(data$factor_3));n11=data.frame(n1,levels(data$factor_3)) 
            n11 <- apply(t(n11), 2, paste, collapse = "  ")
            names(test8)=n11
            c2=rep(1:nlevels(data$factor_2), nlevels(data$factor_3))
            cs2=split(mat3, c2)
            test9=lapply(cs2, fm, dff=df2); n2=rep("factor_3 in", nlevels(data$factor_2));n22=data.frame(n2,levels(data$factor_2)) 
            n22 <- apply(t(n22), 2, paste, collapse = "  ")
            names(test9)=n22
            csf=function(x){a=x[order(x[,2], decreasing=TRUE),];return(a)}
            groups8=lapply(test8, ft, alpha)
            mft5=lapply(cs1,csf);
            dd<-function(x){l=data.frame(mft5[[x]],groups8[[x]]); return(l)}; nf1=as.list(c(1:nlevels(data$factor_3)))
            mft5=lapply(nf1,dd)
            names(mft5)=n11
            groups9=lapply(test9, ft, alpha)
            mft6=lapply(cs2,csf);
            ddd<-function(x){l=data.frame(mft6[[x]],groups9[[x]]); return(l)}; nf2=as.list(c(1:nlevels(data$factor_2)))
            mft6=lapply(nf2,ddd)
            names(mft6)=n22
            c1=rep(1:nlevels(data$treatments_f2f3), each=nlevels(data$factor_1))
            cs1=split(mat4, c1)
            test10=lapply(cs1, fm, dff=df2); n1=rep("factor_1 in", nlevels(data$treatments_f2f3));n11=data.frame(n1,levels(data$treatments_f2f3)) 
            n11 <- apply(t(n11), 2, paste, collapse = "  ")
            names(test10)=n11
            csf=function(x){a=x[order(x[,2], decreasing=TRUE),];return(a)}
            groups10=lapply(test10, ft, alpha)
            mft7=lapply(cs1,csf);
            dd<-function(x){l=data.frame(mft7[[x]],groups10[[x]]); return(l)}; nf1=as.list(c(1:nlevels(data$treatments_f2f3)))
            mft7=lapply(nf1,dd)
            names(mft7)=n11
            c1=rep(1:nlevels(data$factor_1), nlevels(data$factor_2));c1=rep(c1,nlevels(data$factor_3))
            s=rep(0:(nlevels(data$factor_3)-1), each=nlevels(data$treatments_f1f2)); s=2*s; c1=c1+s
            cs1=split(mat4, c1)
            test11=lapply(cs1, fm, dff=df2); n1=rep("factor_2 in", nlevels(data$treatments_f1f3));n11=data.frame(n1,levels(data$treatments_f1f3)) 
            n11 <- apply(t(n11), 2, paste, collapse = "  ")
            names(test11)=n11
            csf=function(x){a=x[order(x[,2], decreasing=TRUE),];return(a)}
            groups11=lapply(test11, ft, alpha)
            mft8=lapply(cs1,csf);
            dd<-function(x){l=data.frame(mft8[[x]],groups11[[x]]); return(l)}; nf1=as.list(c(1:nlevels(data$treatments_f1f3)))
            mft8=lapply(nf1,dd)
            names(mft8)=n11
            c1=rep(1:nlevels(data$treatments_f1f2), nlevels(data$factor_3))
            cs1=split(mat4, c1)
            test12=lapply(cs1, fm, dff=df2); n1=rep("factor_3 in", nlevels(data$treatments_f1f2));n11=data.frame(n1,levels(data$treatments_f1f2)) 
            n11 <- apply(t(n11), 2, paste, collapse = "  ")
            names(test12)=n11
            csf=function(x){a=x[order(x[,2], decreasing=TRUE),];return(a)}
            groups12=lapply(test12, ft, alpha)
            mft9=lapply(cs1,csf);
            dd<-function(x){l=data.frame(mft9[[x]],groups12[[x]]); return(l)}; nf1=as.list(c(1:nlevels(data$treatments_f1f2)))
            mft9=lapply(nf1,dd)
            names(mft9)=n11
            l<-list(a3,  mf1, test1, mf2,test2,mf3,test3,   mft1,test4,mft2,test5,  mft3,test6,mft4,test7,  mft5,test8,mft6,test9,  mft7,test10, mft8,test11, mft9,test12, res)
            names(l)= list("Marginal anova (Type III Sum of Squares)", "Adjusted means (factor 1)", "Multiple comparison test (factor 1)","Adjusted means (factor 2)", "Multiple comparison test (factor 2)", "Adjusted means (factor 3)", "Multiple comparison test (factor 3)", "Adjusted means (factor 1 in levels of factor 2)", "Multiple comparison test (factor 1 in levels of factor 2)", "Adjusted means (factor 2 in levels of factor 1)", "Multiple comparison test (factor 2 in levels of factor 1)","Adjusted means (factor 1 in levels of factor 3)", "Multiple comparison test (factor 1 in levels of factor 3)", "Adjusted means (factor 3 in levels of factor 1)", "Multiple comparison test (factor 3 in levels of factor 1)","Adjusted means (factor 2 in levels of factor 3)", "Multiple comparison test (factor 2 in levels of factor 3)", "Adjusted means (factor 3 in levels of factor 2)", "Multiple comparison test (factor 3 in levels of factor 2)","Adjusted means (factor 1 in levels of treatments factor2*factor3)", "Multiple comparison test (factor 1 in levels of treatments factor2*factor3)","Adjusted means (factor 2 in levels of treatments factor1*factor3)", "Multiple comparison test (factor 2 in levels of treatments factor1*factor3)", "Adjusted means (factor 3 in levels of treatments factor1*factor2)","Multiple comparison test (factor 3 in levels of treatments factor1*factor2)","Residual analysis")
            return(l)
        }
        
        f10<-function(data, cov){ 
            gg=cov
            names(data)=c("factor_1","factor_2", "factor_3","blocks","response")
            data<-data.frame(factor_1=factor(data$factor_1), factor_2=factor(data$factor_2), factor_3=factor(data$factor_3),  blocks=factor(data$blocks), response=data$response)
            subject<-interaction(data$factor_1,data$factor_2,data$blocks)
            data<-data.frame(data,subject)
            UN<-corSymm(form=~1|subject)
            AR<-corAR1(form=~1|subject)
            ARH<-corAR1(form=~1|subject)
            CS<- corCompSymm (form=~1|subject) 
            CAR<-corCAR1(form=~factor_3|subject)
            o1<-list(AR,ARH, CAR, CS, UN)
            cor<-o1[[gg]]
            UN1<-varIdent(form=~1|factor_3)
            AR1<-NULL
            ARH1<- varIdent(form=~1|factor_3)
            CS1<-NULL
            CAR1<-NULL
            o2<-list(AR1,ARH1, CAR1, CS1, UN1)
            var<-o2[[gg]]
            m<- lme(response~factor_1*factor_2*factor_3+blocks, random=~1|subject, correlation=cor, weights=var , data=data, contrasts=list(factor_1=contr.sum, factor_2=contr.sum, factor_3=contr.sum, blocks=contr.sum), control=lmeControl(maxIter =6000, msMaxIter=6000, niterEM=2000, opt="optim"),na.action=na.omit)
            m1<-lme(response~-1+ factor_1*factor_2*factor_3+blocks, random=~1|subject, correlation=cor, weights=var ,data=data, contrasts=list(factor_1=contr.sum, factor_2=contr.sum, factor_3=contr.sum, blocks=contr.sum), control=lmeControl(maxIter =6000, msMaxIter=6000, niterEM=2000, opt="optim"),na.action=na.omit)
            m2<-lme(response~-1+ factor_2*factor_1*factor_3+blocks, random=~1|subject, correlation=cor, weights=var ,data=data, contrasts=list(factor_1=contr.sum, factor_2=contr.sum, factor_3=contr.sum, blocks=contr.sum), control=lmeControl(maxIter =6000, msMaxIter=6000, niterEM=2000, opt="optim"),na.action=na.omit)
            m3<-lme(response~-1+ factor_3*factor_1*factor_2+blocks, random=~1|subject, correlation=cor, weights=var , data=data, contrasts=list(factor_1=contr.sum, factor_2=contr.sum, factor_3=contr.sum, blocks=contr.sum), control=lmeControl(maxIter =6000, msMaxIter=6000, niterEM=2000, opt="optim"),na.action=na.omit)
            a3<-anova(m, type="marginal")[-1,]
            df1<- anova(m) $"denDF"[2]
            df2<- anova(m) $"denDF"[1]
            treatments_f1f2=interaction(data$factor_1,data$factor_2)
            treatments_f1f3=interaction(data$factor_1,data$factor_3)
            treatments_f2f3=interaction(data$factor_2,data$factor_3)
            treatments_f1f2f3=interaction(data$factor_1,data$factor_2, data$factor_3)
            treatments_f2f1f3=interaction(data$factor_2,data$factor_1, data$factor_3)
            treatments_f3f1f2=interaction(data$factor_3,data$factor_1, data$factor_2)
            data<-data.frame(data,treatments_f1f2, treatments_f1f3, treatments_f2f3, treatments_f1f2f3, treatments_f2f1f3, treatments_f3f1f2)
            m4<-lme(response~-1+treatments_f1f2+factor_3*treatments_f1f2+blocks, random=~1|subject, correlation=cor, weights=var ,data=data, contrasts=list(treatments_f1f2=contr.sum, factor_3=contr.sum, blocks=contr.sum), control=lmeControl(maxIter =6000, msMaxIter=6000, niterEM=2000, opt="optim"),na.action=na.omit)
            m5<-lme(response~-1+treatments_f1f3+factor_2*treatments_f1f3+blocks, random=~1|subject, correlation=cor, weights=var ,data=data, contrasts=list(treatments_f1f3=contr.sum, factor_2=contr.sum, blocks=contr.sum), control=lmeControl(maxIter =6000, msMaxIter=6000, niterEM=2000, opt="optim"),na.action=na.omit)
            m6<-lme(response~-1+treatments_f2f3+factor_1*treatments_f2f3+blocks, random=~1|subject, correlation=cor, weights=var ,data=data, contrasts=list(treatments_f2f3=contr.sum, factor_1=contr.sum, blocks=contr.sum), control=lmeControl(maxIter =6000, msMaxIter=6000, niterEM=2000, opt="optim"),na.action=na.omit)
            m7<-lme(response~-1+treatments_f1f2f3+blocks, random=~1|subject, correlation=cor, weights=var ,data=data, contrasts=list(treatments_f1f2f3=contr.sum, blocks=contr.sum), control=lmeControl(maxIter =6000, msMaxIter=6000, niterEM=2000, opt="optim"),na.action=na.omit)
            m8<-lme(response~-1+treatments_f2f1f3+blocks, random=~1|subject, correlation=cor, weights=var ,data=data, contrasts=list(treatments_f2f1f3=contr.sum, blocks=contr.sum), control=lmeControl(maxIter =6000, msMaxIter=6000, niterEM=2000, opt="optim"),na.action=na.omit)
            m9<-lme(response~-1+treatments_f3f1f2+blocks, random=~1|subject, correlation=cor, weights=var ,data=data, contrasts=list(treatments_f3f1f2=contr.sum, blocks=contr.sum), control=lmeControl(maxIter =6000, msMaxIter=6000, niterEM=2000, opt="optim"),na.action=na.omit)
            
            res=fr4(m,data)
            adjusted.mean<-round(fixef(m1)[1:nlevels(data$factor_1)],4)
            standart.error<-round(sqrt(diag(vcov(m1)) [1:nlevels(data$factor_1)]),4)
            factor_1<-levels(data$factor_1)
            maf1=data.frame(factor_1,adjusted.mean,standart.error)
            rownames(maf1)=NULL
            adjusted.mean<-round(fixef(m2) [1:nlevels(data$factor_2)],4)
            standart.error<-round(sqrt(diag(vcov(m2))) [1:nlevels(data$factor_2)],4)
            factor_2<-levels(data$factor_2)
            maf2=data.frame(factor_2,adjusted.mean,standart.error)
            rownames(maf2)=NULL
            adjusted.mean<-round(fixef(m3) [1:nlevels(data$factor_3)],4)
            standart.error<-round(sqrt(diag(vcov(m3))) [1:nlevels(data$factor_3)],4)
            factor_3<-levels(data$factor_3)
            maf3=data.frame(factor_3,adjusted.mean,standart.error)
            rownames(maf3)=NULL
            adjusted.mean<-round(fixef(m4)[1:nlevels(data$treatments_f1f2)],4)
            standart.error<-round(sqrt(diag(vcov(m4)))[1:nlevels(data$treatments_f1f2)],4)
            treatments_f1f2<-levels(data$treatments_f1f2)
            mat1=data.frame(treatments_f1f2,adjusted.mean,standart.error)
            rownames(mat1)=NULL
            adjusted.mean<-round(fixef(m5)[1:nlevels(data$treatments_f1f3)],4)
            standart.error<-round(sqrt(diag(vcov(m5)))[1:nlevels(data$treatments_f1f3)],4)
            treatments_f1f3<-levels(data$treatments_f1f3)
            mat2=data.frame(treatments_f1f3,adjusted.mean,standart.error)
            rownames(mat2)=NULL
            adjusted.mean<-round(fixef(m6)[1:nlevels(data$treatments_f2f3)],2)
            standart.error<-round(sqrt(diag(vcov(m6)))[1:nlevels(data$treatments_f2f3)],2)
            treatments_f2f3<-levels(data$treatments_f2f3)
            mat3=data.frame(treatments_f2f3,adjusted.mean,standart.error)
            rownames(mat3)=NULL
            adjusted.mean<-round(fixef(m7)[1:nlevels(data$treatments_f1f2f3)],4)
            standart.error<-round(sqrt(diag(vcov(m7)))[1:nlevels(data$treatments_f1f2f3)],4)
            treatments_f1f2f3<-levels(data$treatments_f1f2f3)
            mat4=data.frame(treatments_f1f2f3,adjusted.mean,standart.error)
            rownames(mat4)=NULL
            test1=fm(maf1,df1)
            test2=fm(maf2,df1)
            test3=fm(maf3,df2)
            groups1=ft(test1, alpha); maf1=maf1[order(maf1[,2], decreasing=TRUE),]
            mf1=data.frame(maf1,groups1)
            rownames(mf1) = NULL
            groups2=ft(test2, alpha); maf2=maf2[order(maf2[,2], decreasing=TRUE),]
            mf2=data.frame(maf2,groups2)
            rownames(mf2) = NULL
            groups3=ft(test3, alpha); maf3=maf3[order(maf3[,2], decreasing=TRUE),]
            mf3=data.frame(maf3,groups3)
            rownames(mf3) = NULL
            c1=rep(1:nlevels(data$factor_2), each=nlevels(data$factor_1))
            cs1=split(mat1, c1)
            test4=lapply(cs1, fm, dff=df1); n1=rep("factor_1 in", nlevels(data$factor_2));n11=data.frame(n1,levels(data$factor_2)) 
            n11 <- apply(t(n11), 2, paste, collapse = "  ")
            names(test4)=n11
            c2=rep(1:nlevels(data$factor_1), nlevels(data$factor_2))
            cs2=split(mat1, c2)
            test5=lapply(cs2, fm, dff=df1); n2=rep("factor_2 in", nlevels(data$factor_1));n22=data.frame(n2,levels(data$factor_1)) 
            n22 <- apply(t(n22), 2, paste, collapse = "  ")
            names(test5)=n22
            csf=function(x){a=x[order(x[,2], decreasing=TRUE),];return(a)}
            groups4=lapply(test4, ft, alpha)
            mft1=lapply(cs1,csf);
            dd<-function(x){l=data.frame(mft1[[x]],groups4[[x]]); return(l)}; nf1=as.list(c(1:nlevels(data$factor_2)))
            mft1=lapply(nf1,dd)
            names(mft1)=n11
            groups5=lapply(test5, ft, alpha)
            mft2=lapply(cs2,csf);
            ddd<-function(x){l=data.frame(mft2[[x]],groups5[[x]]); return(l)}; nf2=as.list(c(1:nlevels(data$factor_1)))
            mft2=lapply(nf2,ddd)
            names(mft2)=n22
            c1=rep(1:nlevels(data$factor_3), each=nlevels(data$factor_1))
            cs1=split(mat2, c1)
            test6=lapply(cs1, fm, dff=df2); n1=rep("factor_1 in", nlevels(data$factor_3));n11=data.frame(n1,levels(data$factor_3)) 
            n11 <- apply(t(n11), 2, paste, collapse = "  ")
            names(test6)=n11
            c2=rep(1:nlevels(data$factor_1), nlevels(data$factor_3))
            cs2=split(mat2, c2)
            test7=lapply(cs2, fm, dff=df2); n2=rep("factor_3 in", nlevels(data$factor_1));n22=data.frame(n2,levels(data$factor_1)) 
            n22 <- apply(t(n22), 2, paste, collapse = "  ")
            names(test7)=n22
            csf=function(x){a=x[order(x[,2], decreasing=TRUE),];return(a)}
            groups6=lapply(test6, ft, alpha)
            mft3=lapply(cs1,csf);
            dd<-function(x){l=data.frame(mft3[[x]],groups6[[x]]); return(l)}; nf1=as.list(c(1:nlevels(data$factor_3)))
            mft3=lapply(nf1,dd)
            names(mft3)=n11
            groups7=lapply(test7, ft, alpha)
            mft4=lapply(cs2,csf);
            ddd<-function(x){l=data.frame(mft4[[x]],groups7[[x]]); return(l)}; nf2=as.list(c(1:nlevels(data$factor_1)))
            mft4=lapply(nf2,ddd)
            names(mft4)=n22
            c1=rep(1:nlevels(data$factor_3), each=nlevels(data$factor_2))
            cs1=split(mat3, c1)
            test8=lapply(cs1, fm, dff=df2); n1=rep("factor_2 in", nlevels(data$factor_3));n11=data.frame(n1,levels(data$factor_3)) 
            n11 <- apply(t(n11), 2, paste, collapse = "  ")
            names(test8)=n11
            c2=rep(1:nlevels(data$factor_2), nlevels(data$factor_3))
            cs2=split(mat3, c2)
            test9=lapply(cs2, fm, dff=df2); n2=rep("factor_3 in", nlevels(data$factor_2));n22=data.frame(n2,levels(data$factor_2)) 
            n22 <- apply(t(n22), 2, paste, collapse = "  ")
            names(test9)=n22
            csf=function(x){a=x[order(x[,2], decreasing=TRUE),];return(a)}
            groups8=lapply(test8, ft, alpha)
            mft5=lapply(cs1,csf);
            dd<-function(x){l=data.frame(mft5[[x]],groups8[[x]]); return(l)}; nf1=as.list(c(1:nlevels(data$factor_3)))
            mft5=lapply(nf1,dd)
            names(mft5)=n11
            groups9=lapply(test9, ft, alpha)
            mft6=lapply(cs2,csf);
            ddd<-function(x){l=data.frame(mft6[[x]],groups9[[x]]); return(l)}; nf2=as.list(c(1:nlevels(data$factor_2)))
            mft6=lapply(nf2,ddd)
            names(mft6)=n22
            c1=rep(1:nlevels(data$treatments_f2f3), each=nlevels(data$factor_1))
            cs1=split(mat4, c1)
            test10=lapply(cs1, fm, dff=df2); n1=rep("factor_1 in", nlevels(data$treatments_f2f3));n11=data.frame(n1,levels(data$treatments_f2f3)) 
            n11 <- apply(t(n11), 2, paste, collapse = "  ")
            names(test10)=n11
            csf=function(x){a=x[order(x[,2], decreasing=TRUE),];return(a)}
            groups10=lapply(test10, ft, alpha)
            mft7=lapply(cs1,csf);
            dd<-function(x){l=data.frame(mft7[[x]],groups10[[x]]); return(l)}; nf1=as.list(c(1:nlevels(data$treatments_f2f3)))
            mft7=lapply(nf1,dd)
            names(mft7)=n11
            c1=rep(1:nlevels(data$factor_1), nlevels(data$factor_2));c1=rep(c1,nlevels(data$factor_3))
            s=rep(0:(nlevels(data$factor_3)-1), each=nlevels(data$treatments_f1f2)); s=2*s; c1=c1+s
            cs1=split(mat4, c1)
            test11=lapply(cs1, fm, dff=df2); n1=rep("factor_2 in", nlevels(data$treatments_f1f3));n11=data.frame(n1,levels(data$treatments_f1f3)) 
            n11 <- apply(t(n11), 2, paste, collapse = "  ")
            names(test11)=n11
            csf=function(x){a=x[order(x[,2], decreasing=TRUE),];return(a)}
            groups11=lapply(test11, ft, alpha)
            mft8=lapply(cs1,csf);
            dd<-function(x){l=data.frame(mft8[[x]],groups11[[x]]); return(l)}; nf1=as.list(c(1:nlevels(data$treatments_f1f3)))
            mft8=lapply(nf1,dd)
            names(mft8)=n11
            c1=rep(1:nlevels(data$treatments_f1f2), nlevels(data$factor_3))
            cs1=split(mat4, c1)
            test12=lapply(cs1, fm, dff=df2); n1=rep("factor_3 in", nlevels(data$treatments_f1f2));n11=data.frame(n1,levels(data$treatments_f1f2)) 
            n11 <- apply(t(n11), 2, paste, collapse = "  ")
            names(test12)=n11
            csf=function(x){a=x[order(x[,2], decreasing=TRUE),];return(a)}
            groups12=lapply(test12, ft, alpha)
            mft9=lapply(cs1,csf);
            dd<-function(x){l=data.frame(mft9[[x]],groups12[[x]]); return(l)}; nf1=as.list(c(1:nlevels(data$treatments_f1f2)))
            mft9=lapply(nf1,dd)
            names(mft9)=n11
            l<-list(a3,  mf1, test1, mf2,test2,mf3,test3,   mft1,test4,mft2,test5,  mft3,test6,mft4,test7,  mft5,test8,mft6,test9,  mft7,test10, mft8,test11, mft9,test12, res)
            names(l)= list("Marginal anova (Type III Sum of Squares)", "Adjusted means (factor 1)", "Multiple comparison test (factor 1)","Adjusted means (factor 2)", "Multiple comparison test (factor 2)", "Adjusted means (factor 3)", "Multiple comparison test (factor 3)", "Adjusted means (factor 1 in levels of factor 2)", "Multiple comparison test (factor 1 in levels of factor 2)", "Adjusted means (factor 2 in levels of factor 1)", "Multiple comparison test (factor 2 in levels of factor 1)","Adjusted means (factor 1 in levels of factor 3)", "Multiple comparison test (factor 1 in levels of factor 3)", "Adjusted means (factor 3 in levels of factor 1)", "Multiple comparison test (factor 3 in levels of factor 1)","Adjusted means (factor 2 in levels of factor 3)", "Multiple comparison test (factor 2 in levels of factor 3)", "Adjusted means (factor 3 in levels of factor 2)", "Multiple comparison test (factor 3 in levels of factor 2)","Adjusted means (factor 1 in levels of treatments factor2*factor3)", "Multiple comparison test (factor 1 in levels of treatments factor2*factor3)","Adjusted means (factor 2 in levels of treatments factor1*factor3)", "Multiple comparison test (factor 2 in levels of treatments factor1*factor3)", "Adjusted means (factor 3 in levels of treatments factor1*factor2)","Multiple comparison test (factor 3 in levels of treatments factor1*factor2)","Residual analysis")
            return(l)
        }
        
        de1=c(1,2); de2=c(1,2,3);de3=c(1,2,3,4); de4=c(1,2,3);de5=c(1,2,3);de6=c(1,2,3,4);de7=c(1,2,3);de8=c(1,2,3,4) 
        de9=c(1,2,3,4);de10=c(1,2,3,4)
        de=list(de1,de2,de3,de4,de5,de6, de7,de8,de9,de10)
        de=de[[design]]
        d=as.list(data)
        d1=d[de]
        d2=d[-de]
        f=function(h){data.frame(d1,d2[h])}
        h=length(d2)
        h=1:h
        l=lapply(h, f)
        l2=list(f1,f2,f3,f4,f5,f6,f7,f8,f9,f10)
        fun=l2[[design]]
        li1=lapply(l, fun, cov)
        names(li1)=names(d2)
        li=list(fun(data, cov),li1)
        li=li[[list]]
        return(li)
    }
