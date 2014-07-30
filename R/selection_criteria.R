# each function returns a numeric vector of values, length == nrow(possible_patterns)

MI <- function(which_not_answered, possible_patterns, person, test, row_loc){
    infos <- lapply(which_not_answered, function(x)
        iteminfo(extract.item(test$mirt_object, x), Theta=person$thetas))
    crit <- do.call(c, infos)
    crit
}

MEI <- function(which_not_answered, possible_patterns, person, test, row_loc){
    P <- numeric(nrow(possible_patterns))
    for(i in which_not_answered){
        ii <- extract.item(MCE$test$mirt_object, i)
        p <- probtrace(ii, MCE$person$thetas)
        P[row_loc == i] <- p
    }
    infostmp <- lapply(which_not_answered, function(x)
        iteminfo(extract.item(test$mirt_object, x), Theta=person$thetas, total.info = FALSE))
    infostmp <- as.list(do.call(c, infostmp))
    infos <- weighted_mat(P=P, mat=infostmp, row_loc=row_loc, which_not_answered=which_not_answered)
    crit <- do.call(c, infos)
    crit
}

MEPV <- function(which_not_answered, possible_patterns, person, test, row_loc){
    P <- numeric(nrow(possible_patterns))
    for(i in which_not_answered){
        ii <- extract.item(MCE$test$mirt_object, i)
        p <- probtrace(ii, MCE$person$thetas)
        P[row_loc == i] <- p
    }
    pp2 <- possible_patterns
    pp2[ ,which(possible_patterns[1L, ] == possible_patterns[2L,])] <- NA
    acovstmp <- getAcovs(pp2, method = 'MAP')
    acovs <- weighted_mat(P=P, mat=acovstmp, row_loc=row_loc, which_not_answered=which_not_answered)
    crit <- do.call(c, acovs)
    crit
}

MLWI <- function(which_not_answered, possible_patterns, person, test, row_loc){
    Theta <- MCE$test$ThetaGrid
    density <- MCE$test$density
    LL <- vector('list', nrow(possible_patterns))
    ll <- log(mirt:::computeItemtrace(pars = MCE$test$mirt_object@pars,
                                 Theta=Theta, 
                                 itemloc = MCE$test$mirt_object@itemloc,
                                 CUSTOM.IND=list()))
    for(i in 1L:nrow(possible_patterns)){
        pick <- !is.na(possible_patterns[i,])
        tmp <- MCE$test$itemloc2[pick] + possible_patterns[i, pick]
        LL[[i]] <- exp(rowSums(ll[,tmp]))
    }
    infostmp <- lapply(which_not_answered, function(x)
        iteminfo(extract.item(test$mirt_object, x), Theta=Theta))
    uniq <- unique(row_loc)
    count <- 1L
    for(i in uniq){
        LL[i == row_loc] <- lapply(LL[i == row_loc], function(x, C)
            return(x * C), C=infostmp[[count]])
        count <- count + 1L
    }
    infos <- weighted_mat(mat=LL, row_loc=row_loc, which_not_answered=which_not_answered)
    crit <- do.call(c, lapply(infos, function(y, x) integrate.xy(x, y), x=Theta))
    crit
}

MPWI <- function(which_not_answered, possible_patterns, person, test, row_loc){
    Theta <- MCE$test$ThetaGrid
    density <- MCE$test$density
    LL <- vector('list', nrow(possible_patterns))
    ll <- log(mirt:::computeItemtrace(pars = MCE$test$mirt_object@pars,
                                      Theta=Theta, 
                                      itemloc = MCE$test$mirt_object@itemloc,
                                      CUSTOM.IND=list()))
    for(i in 1L:nrow(possible_patterns)){
        pick <- !is.na(possible_patterns[i,])
        tmp <- MCE$test$itemloc2[pick] + possible_patterns[i, pick]
        LL[[i]] <- exp(rowSums(ll[,tmp]))
    }
    infostmp <- lapply(which_not_answered, function(x)
        iteminfo(extract.item(test$mirt_object, x), Theta=Theta))
    uniq <- unique(row_loc)
    count <- 1L
    for(i in uniq){
        LL[i == row_loc] <- lapply(LL[i == row_loc], function(x, C, dd)
            return(x * C * dd), C=infostmp[[count]], dd=MCE$test$density)
        count <- count + 1L
    }
    infos <- weighted_mat(mat=LL, row_loc=row_loc, which_not_answered=which_not_answered)
    crit <- do.call(c, lapply(infos, function(y, x) integrate.xy(x, y), x=Theta))
    crit
}

Drule <- function(which_not_answered, possible_patterns, person, test, row_loc, method){
    acovs <- getAcovs(possible_patterns, method)
    crit <- do.call(c, lapply(acovs, det))
    crit
}

Erule <- function(which_not_answered, possible_patterns, person, test, row_loc, method){
    acovs <- getAcovs(possible_patterns, method)
    crit <- do.call(c, lapply(acovs, function(x) eigen(x)$values[1L]))
    crit
}

Trule <- function(which_not_answered, possible_patterns, person, test, row_loc, method){
    acovs <- getAcovs(possible_patterns, method)
    infos <- lapply(acovs, function(x){
        ret <- try(solve(x), TRUE)
        if(is(ret, 'try-error'))
            ret <- matrix(0, nrow(x), ncol(x))
        ret
    })
    crit <- do.call(c, lapply(infos, function(x, w) sum(diag(x) * w), 
                              w=MCE$design$Wrule_weights[!MCE$design$met_SEM]))
    crit
}

Wrule <- function(which_not_answered, possible_patterns, person, test, row_loc, method){
    acovs <- getAcovs(possible_patterns, method)
    infos <- lapply(acovs, function(x){
        ret <- try(solve(x), TRUE)
        if(is(ret, 'try-error'))
            ret <- matrix(0, nrow(x), ncol(x))
        ret
    })
    crit <- do.call(c, lapply(infos, function(x, w) w %*% x %*% w, 
                              w=MCE$design$Wrule_weights[!MCE$design$met_SEM]))
    crit
}

KL <- function(which_not_answered, possible_patterns, person, test, row_loc, delta,
               thetas = NULL){
    info <- numeric(length(which_not_answered))
    if(is.null(thetas)){
        thetas <- MCE$person$thetas
        for(i in 1L:length(which_not_answered)){
            ii <- extract.item(MCE$test$mirt_object, which_not_answered[i])
            p0 <- probtrace(ii, thetas - delta)
            p1 <- probtrace(ii, thetas + delta)
            info[i] <- sum(p1 * (log(p1) - log(p0)))
        }
    } else {
        info <- matrix(0, nrow(thetas), length(which_not_answered))
        for(i in 1L:length(which_not_answered)){
            ii <- extract.item(MCE$test$mirt_object, which_not_answered[i])
            p0 <- probtrace(ii, thetas)
            p1 <- probtrace(ii, person$thetas)
            info[,i] <- rowSums(t(p1[1L,] * t(matrix(log(p1), nrow(thetas), length(p1), byrow=TRUE)
                                          - log(p0))))
        }
    }    
    return(info)
}

IKL <- function(which_not_answered, possible_patterns, person, test, row_loc, delta,
                den=FALSE){
    Theta <- matrix(seq(person$thetas-delta, person$thetas+delta, length.out=test$quadpts))
    LL <- vector('list', nrow(possible_patterns))
    ll <- log(mirt:::computeItemtrace(pars = MCE$test$mirt_object@pars,
                                      Theta=Theta, 
                                      itemloc = MCE$test$mirt_object@itemloc,
                                      CUSTOM.IND=list()))
    for(i in 1L:nrow(possible_patterns)){
        pick <- !is.na(possible_patterns[i,])
        tmp <- MCE$test$itemloc2[pick] + possible_patterns[i, pick]
        LL[[i]] <- exp(rowSums(ll[,tmp]))
    }
    KLcrit <- KL(which_not_answered=which_not_answered, possible_patterns=possible_patterns,
               person=person, test=test, row_loc=row_loc, thetas=Theta,
               delta=NA)
    uniq <- unique(row_loc)
    count <- 1L
    dd <- if(den){
        dd <- mirt:::mirt_dmvnorm(Theta, test$gp$gmeans, test$gp$gcov) 
    } else 1
    for(i in uniq){
        LL[i == row_loc] <- lapply(LL[i == row_loc], function(x, C, dd)
            return(x * C * dd), C=KLcrit[[count]], dd=dd)
        count <- count + 1L
    }
    infos <- weighted_mat(mat=LL, row_loc=row_loc, which_not_answered=which_not_answered)
    crit <- do.call(c, lapply(infos, function(y, x) integrate.xy(x, y), x=Theta))
    return(crit)
}