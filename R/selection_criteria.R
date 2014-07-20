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
    acovstmp <- getAcovs(pp2)
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

Drule <- function(which_not_answered, possible_patterns, person, test, row_loc){
    acovstmp <- getAcovs(possible_patterns)
    infostmp <- lapply(acovstmp, function(x){
        ret <- try(solve(x), TRUE)
        if(is(ret, 'try-error'))
            ret <- matrix(0, nrow(x), ncol(x))
        ret
    })
    infos <- weighted_mat(mat=infostmp, row_loc=row_loc, which_not_answered=which_not_answered)
    crit <- do.call(c, lapply(infos, det))
    crit
}

Trule <- function(which_not_answered, possible_patterns, person, test, row_loc){
    acovstmp <- getAcovs(possible_patterns)
    infostmp <- lapply(acovstmp, function(x){
        ret <- try(solve(x), TRUE)
        if(is(ret, 'try-error'))
            ret <- matrix(0, nrow(x), ncol(x))
        ret
    })
    infos <- weighted_mat(mat=infostmp, row_loc=row_loc, which_not_answered=which_not_answered)
    crit <- do.call(c, lapply(infos, function(x) sum(diag(x))))
    crit
}

Wrule <- function(which_not_answered, possible_patterns, person, test, row_loc){
    acovstmp <- getAcovs(possible_patterns)
    infostmp <- lapply(acovstmp, function(x){
        ret <- try(solve(x), TRUE)
        if(is(ret, 'try-error'))
            ret <- matrix(0, nrow(x), ncol(x))
        ret
    })
    infos <- weighted_mat(mat=infostmp, row_loc=row_loc, which_not_answered=which_not_answered)
    crit <- do.call(c, lapply(infos, function(x, w) w %*% x %*% w, 
                              w=MCE$design$Wrule_weights))
    crit
}

KL <- function(which_not_answered, possible_patterns, person, test, row_loc, delta){
    info <- numeric(length(which_not_answered))
    for(i in 1L:length(which_not_answered)){
        ii <- extract.item(MCE$test$mirt_object, which_not_answered[i])
        p0 <- probtrace(ii, MCE$person$thetas - delta)
        p1 <- probtrace(ii, MCE$person$thetas + delta)
        info[i] <- sum(p1 * (log(p1) - log(p0)))
    }
    info
}