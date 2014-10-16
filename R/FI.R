FI <- function(mirt_item, Theta){
    cls <- class(mirt_item)
    
    if(cls == 'dich'){
        dP <- mirt:::DerivTheta(mirt_item, Theta)$grad[[2L]][1L,]
        PQ <- prod(mirt:::ProbTrace(mirt_item, Theta))
        ret <- outer(dP, dP) / PQ
    } else if(cls == 'graded'){
        P <- mirt:::ProbTrace(mirt_item, Theta, itemexp=FALSE)
        P <- P[,-c(1, ncol(P)), drop=FALSE]
        PQ <- P * (1 - P)
        a <- mirt:::ExtractLambdas(mirt_item)
        ret <- outer(a, a) * sum(PQ)
    } else {
        stop('Fisher-information matrix not currently supported for supplied classes')
    }

    return(ret)
}
