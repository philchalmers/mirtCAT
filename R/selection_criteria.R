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
        ii <- extract.item(test$mirt_object, i)
        p <- probtrace(ii, person$thetas)
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
        ii <- extract.item(test$mirt_object, i)
        p <- probtrace(ii, person$thetas)
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
    Theta <- test$ThetaGrid
    density <- test$density
    LL <- vector('list', nrow(possible_patterns))
    ll <- log(mirt:::computeItemtrace(pars = test$mirt_object@pars,
                                 Theta=Theta, 
                                 itemloc = test$mirt_object@itemloc,
                                 CUSTOM.IND=list()))
    for(i in 1L:nrow(possible_patterns)){
        pick <- !is.na(possible_patterns[i,])
        tmp <- test$itemloc2[pick] + possible_patterns[i, pick]
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
    Theta <- test$ThetaGrid
    density <- test$density
    LL <- vector('list', nrow(possible_patterns))
    ll <- log(mirt:::computeItemtrace(pars = test$mirt_object@pars,
                                      Theta=Theta, 
                                      itemloc = test$mirt_object@itemloc,
                                      CUSTOM.IND=list()))
    for(i in 1L:nrow(possible_patterns)){
        pick <- !is.na(possible_patterns[i,])
        tmp <- test$itemloc2[pick] + possible_patterns[i, pick]
        LL[[i]] <- exp(rowSums(ll[,tmp]))
    }
    infostmp <- lapply(which_not_answered, function(x)
        iteminfo(extract.item(test$mirt_object, x), Theta=Theta))
    uniq <- unique(row_loc)
    count <- 1L
    for(i in uniq){
        LL[i == row_loc] <- lapply(LL[i == row_loc], function(x, C, dd)
            return(x * C * dd), C=infostmp[[count]], dd=test$density)
        count <- count + 1L
    }
    infos <- weighted_mat(mat=LL, row_loc=row_loc, which_not_answered=which_not_answered)
    crit <- do.call(c, lapply(infos, function(y, x) integrate.xy(x, y), x=Theta))
    crit
}

Drule <- function(which_not_answered, possible_patterns, person, test, row_loc, method, 
                  prior = FALSE){
    infos <- lapply(which_not_answered, function(x)
        FI(extract.item(test$mirt_object, x), Theta=person$thetas))
    crit <- lapply(infos, function(x, person) det(x + person$info_thetas), person=person)
    crit <- do.call(c, crit)
    crit
}

Erule <- function(which_not_answered, possible_patterns, person, test, row_loc, method){
    infos <- lapply(which_not_answered, function(x)
        FI(extract.item(test$mirt_object, x), Theta=person$thetas))
    crit <- lapply(infos, function(x, person){
        ev <- eigen(x + person$info_thetas)$values
        return(min(ev))
        }, person=person)
    crit <- do.call(c, crit)
    crit
}

Trule <- function(which_not_answered, possible_patterns, person, test, row_loc, method, design){
    infos <- lapply(which_not_answered, function(x)
        FI(extract.item(test$mirt_object, x), Theta=person$thetas))
    crit <- lapply(infos, function(x, person, w) sum(diag(x + person$info_thetas) * w),
        person=person, w=design$Wrule_weights)
    crit <- do.call(c, crit)
    crit
}

Arule <- function(which_not_answered, possible_patterns, person, test, row_loc, method, design){
    infos <- lapply(which_not_answered, function(x)
        FI(extract.item(test$mirt_object, x), Theta=person$thetas))
    acovs <- lapply(infos, function(x, person){
        ret <- try(solve(x + person$info_thetas), TRUE)
        if(is(ret, 'try-error')) ret <- diag(ncol(x)) * 1e10
        ret
    }, person=person)
    crit <- lapply(acovs, function(x, w) sum(diag(x) * w), w=design$Wrule_weights)
    crit <- do.call(c, crit)
    crit
}

Wrule <- function(which_not_answered, possible_patterns, person, test, row_loc, method, design){
    infos <- lapply(which_not_answered, function(x)
        FI(extract.item(test$mirt_object, x), Theta=person$thetas))
    crit <- lapply(infos, function(x, person, w) w %*% (x + person$info_thetas) %*% w,
                   person=person, w=design$Wrule_weights)
    crit <- do.call(c, crit)
    crit
}

Drule2 <- function(which_not_answered, possible_patterns, person, test, row_loc, method){
    acovs <- getAcovs(possible_patterns, method)
    crit <- do.call(c, lapply(acovs, det))
    crit
}

Erule2 <- function(which_not_answered, possible_patterns, person, test, row_loc, method){
    acovs <- getAcovs(possible_patterns, method)
    crit <- do.call(c, lapply(acovs, function(x) eigen(x)$values[1L]))
    crit
}

Trule2 <- function(which_not_answered, possible_patterns, person, test, row_loc, method, design){
    acovs <- getAcovs(possible_patterns, method)
    infos <- lapply(acovs, function(x){
        ret <- try(solve(x), TRUE)
        if(is(ret, 'try-error'))
            ret <- matrix(0, nrow(x), ncol(x))
        ret
    })
    crit <- do.call(c, lapply(infos, function(x, w) sum(diag(x) * w), 
                              w=design$Wrule_weights[!design$met_SEM]))
    crit
}

Wrule2 <- function(which_not_answered, possible_patterns, person, test, row_loc, method, design){
    acovs <- getAcovs(possible_patterns, method)
    infos <- lapply(acovs, function(x){
        ret <- try(solve(x), TRUE)
        if(is(ret, 'try-error'))
            ret <- matrix(0, nrow(x), ncol(x))
        ret
    })
    crit <- do.call(c, lapply(infos, function(x, w) w %*% x %*% w, 
                              w=design$Wrule_weights[!design$met_SEM]))
    crit
}

KL <- function(which_not_answered, possible_patterns, person, test, row_loc, delta,
               thetas = NULL){
    info <- numeric(length(which_not_answered))
    if(is.null(thetas)){
        thetas <- person$thetas
        for(i in 1L:length(which_not_answered)){
            ii <- extract.item(test$mirt_object, which_not_answered[i])
            p0 <- probtrace(ii, thetas - delta)
            p1 <- probtrace(ii, thetas + delta)
            info[i] <- sum(p1 * (log(p1) - log(p0)))
        }
    } else {
        info <- matrix(0, nrow(thetas), length(which_not_answered))
        for(i in 1L:length(which_not_answered)){
            ii <- extract.item(test$mirt_object, which_not_answered[i])
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
    ll <- log(mirt:::computeItemtrace(pars = test$mirt_object@pars,
                                      Theta=Theta, 
                                      itemloc = test$mirt_object@itemloc,
                                      CUSTOM.IND=list()))
    for(i in 1L:nrow(possible_patterns)){
        pick <- !is.na(possible_patterns[i,])
        tmp <- test$itemloc2[pick] + possible_patterns[i, pick]
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