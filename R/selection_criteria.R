# each function returns a numeric vector of values, length == nrow(possible_patterns)

MI <- function(which_not_answered, person, test, thetas){
    .Call('ComputeCriteria', test@EIs, person$thetas, which_not_answered, 
          1, 0, person$info_thetas)
}

MEI <- function(which_not_answered, possible_patterns, person, test, row_loc, thetas){
    P <- numeric(nrow(possible_patterns))
    for(i in which_not_answered){
        p <- probtrace(test@EIs[[i]], person$thetas)
        P[row_loc == i] <- p
    }
    infostmp <- lapply(which_not_answered, function(x)
        mirt:::ItemInfo2(test@EIs[[x]], Theta=person$thetas, total.info=FALSE))
    infostmp <- as.list(do.call(c, infostmp))
    infos <- weighted_mat(P=P, mat=infostmp, row_loc=row_loc, 
                          which_not_answered=which_not_answered)
    crit <- do.call(c, infos)
    crit
}

MEPV <- function(which_not_answered, possible_patterns, person, test, design, row_loc, thetas){
    P <- numeric(nrow(possible_patterns))
    for(i in which_not_answered){
        p <- probtrace(test@EIs[[i]], person$thetas)
        P[row_loc == i] <- p
    }
    pp2 <- possible_patterns
    pp2[ ,which(possible_patterns[1L, ] == possible_patterns[2L,])] <- NA
    acovstmp <- getAcovs(pp2, method = 'MAP', test=test, design=design)
    acovs <- weighted_mat(P=P, mat=acovstmp, row_loc=row_loc, 
                          which_not_answered=which_not_answered)
    crit <- do.call(c, acovs)
    crit
}

MLWI <- function(which_not_answered, possible_patterns, person, test, row_loc, thetas){
    Theta <- test@ThetaGrid
    density <- test@density
    LL <- vector('list', nrow(possible_patterns))
    ll <- log(mirt:::computeItemtrace(pars = test@mo@pars,
                                 Theta=Theta, 
                                 itemloc = test@mo@itemloc,
                                 CUSTOM.IND=list()))
    for(i in 1L:nrow(possible_patterns)){
        pick <- !is.na(possible_patterns[i,])
        tmp <- test@itemloc2[pick] + possible_patterns[i, pick]
        LL[[i]] <- exp(rowSums(ll[,tmp, drop=FALSE]))
    }
    infostmp <- as.list(.Call('ComputeCriteria', test@EIs, person$thetas, 
                              which_not_answered, 1, 0, person$info_thetas))
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

MPWI <- function(which_not_answered, possible_patterns, person, test, row_loc, thetas){
    Theta <- test@ThetaGrid
    density <- test@density
    LL <- vector('list', nrow(possible_patterns))
    ll <- log(mirt:::computeItemtrace(pars = test@mo@pars,
                                      Theta=Theta, 
                                      itemloc = test@mo@itemloc,
                                      CUSTOM.IND=list()))
    for(i in 1L:nrow(possible_patterns)){
        pick <- !is.na(possible_patterns[i,])
        tmp <- test@itemloc2[pick] + possible_patterns[i, pick]
        LL[[i]] <- exp(rowSums(ll[,tmp, drop=FALSE]))
    }
    infostmp <- as.list(.Call('ComputeCriteria', test@EIs, person$thetas, which_not_answered, 
                      1, 0, person$info_thetas))
    uniq <- unique(row_loc)
    count <- 1L
    for(i in uniq){
        LL[i == row_loc] <- lapply(LL[i == row_loc], function(x, C, dd)
            return(x * C * dd), C=infostmp[[count]], dd=test@density)
        count <- count + 1L
    }
    infos <- weighted_mat(mat=LL, row_loc=row_loc, which_not_answered=which_not_answered)
    crit <- do.call(c, lapply(infos, function(y, x) integrate.xy(x, y), x=Theta))
    crit
}

Drule <- function(which_not_answered, person, test, thetas){
    .Call('ComputeCriteria', test@EIs, person$thetas, which_not_answered, 
                              2, 0, person$info_thetas)
}

Trule <- function(which_not_answered, person, test, design, thetas){
    .Call('ComputeCriteria', test@EIs, person$thetas, which_not_answered, 
          3, design@weights, person$info_thetas)
}

Arule <- function(which_not_answered, person, test, design, thetas){
    .Call('ComputeCriteria', test@EIs, person$thetas, which_not_answered, 
          4, design@weights, person$info_thetas)
}

Wrule <- function(which_not_answered, person, test, design, thetas){
    .Call('ComputeCriteria', test@EIs, person$thetas, which_not_answered, 
          5, design@weights, person$info_thetas)
}

Erule <- function(which_not_answered, person, test, thetas){
    .Call('ComputeCriteria', test@EIs, person$thetas, which_not_answered, 
          6, 0, person$info_thetas)
}

KL <- function(which_not_answered, person, test, delta, thetas, thetas2 = NULL){
    info <- numeric(length(which_not_answered))
    if(is.null(thetas2)){
        for(i in 1L:length(which_not_answered)){
            ii <- test@EIs[[which_not_answered[i]]]
            p0 <- probtrace(ii, thetas - delta)
            p1 <- probtrace(ii, thetas + delta)
            info[i] <- sum(p1 * (log(p1) - log(p0)))
        }
    } else {
        info <- matrix(0, nrow(thetas2), length(which_not_answered))
        for(i in 1L:length(which_not_answered)){
            ii <- test@EIs[[which_not_answered[i]]]
            p0 <- probtrace(ii, thetas2)
            p1 <- probtrace(ii, thetas)
            info[,i] <- rowSums(t(p1[1L,] * t(matrix(log(p1), nrow(thetas2), length(p1), byrow=TRUE)
                                          - log(p0))))
        }
    }    
    return(info)
}

IKL <- function(which_not_answered, possible_patterns, person, test, row_loc, delta,
                den=FALSE, thetas){
    Theta <- matrix(seq(person$thetas-delta, person$thetas+delta, length.out=test@quadpts))
    LL <- vector('list', nrow(possible_patterns))
    ll <- log(mirt:::computeItemtrace(pars = test@mo@pars,
                                      Theta=Theta, 
                                      itemloc = test@mo@itemloc,
                                      CUSTOM.IND=list()))
    for(i in 1L:nrow(possible_patterns)){
        pick <- !is.na(possible_patterns[i,])
        tmp <- test@itemloc2[pick] + possible_patterns[i, pick]
        LL[[i]] <- exp(rowSums(ll[,tmp]))
    }
    KLcrit <- KL(which_not_answered=which_not_answered, person=person, 
                 test=test, thetas=thetas, thetas2=Theta, delta=NA)
    uniq <- unique(row_loc)
    count <- 1L
    dd <- if(den){
        mirt:::mirt_dmvnorm(Theta, test@gp$gmeans, test@gp$gcov)
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