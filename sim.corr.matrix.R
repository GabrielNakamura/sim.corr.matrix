sim.corr.matrix<- function(x, low.corr.level, up.corr.level){
    if(is.matrix(x)==FALSE){
        stop("\n x must be a matrix\n")
    }
    if(is.numeric(low.corr.level)==FALSE){
        stop("\n low.corr.level must be a valid numeric value\n")
    }
    if(is.numeric(up.corr.level)==FALSE){
        stop("\n up.corr.level must be a valid numeric value\n")
    }
    dim.matrix<- dim(x)[2]
    mat.corr<- diag(x = dim.matrix)
    off.diag.sup<-c(sample(seq(low.corr.level,up.corr.level,0.1), dim.matrix*(dim.matrix-1)/2, TRUE))
    off.diag.inf<-off.diag.sup
    mat.corr[upper.tri(mat.corr)]<- off.diag.sup
    mat.corr[lower.tri(mat.corr)]<- t(mat.corr)[lower.tri(t(mat.corr))]
    if((dim.matrix<=3)==TRUE){
        chol.mat<-t(chol(mat.corr))
        corr.structure<-t(chol.mat%*%t(x))
        colnames(corr.structure)<- colnames(x)
        return(corr.structure)
    }
    if((dim.matrix>3)==TRUE){
        chol.mat<-t(chol(mat.corr, pivot= TRUE))
        corr.structure<-t(chol.mat%*%t(x))
        colnames(corr.structure)<- colnames(x)
        return(corr.structure)
    }
    set.seed(NULL, normal.kind = "default")    
}
