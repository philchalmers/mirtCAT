FI <- function(mirt_item, Theta){
    P <- mirt:::ProbTrace(mirt_item, Theta)
    Pstar <- cumsum(P[,ncol(P):1L])[(ncol(P)-1L):1L]
    PQ <- Pstar * (1 - Pstar)
    a <- mirt:::ExtractLambdas(mirt_item)
    ret <- outer(a, a) * sum(PQ)
    return(ret)
}
