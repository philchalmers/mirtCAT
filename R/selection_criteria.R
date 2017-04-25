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
    new_thetas <- possible_pattern_thetas(possible_patterns=possible_patterns, test=test)[ ,'F1', drop=FALSE]
    infostmp <- lapply(1:length(which_not_answered), function(x, wna, nt)
        mirt:::ItemInfo2(test@EIs[[ wna[x] ]], Theta=nt[x, , drop=FALSE], total.info=FALSE), 
        wna=which_not_answered, nt=new_thetas)
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
    acovstmp <- getAcovs(possible_patterns, method = 'EAP', test=test, design=design)
    acovs <- weighted_mat(P=P, mat=acovstmp, row_loc=row_loc, 
                          which_not_answered=which_not_answered)
    crit <- do.call(c, acovs)
    crit
}

MLWI <- function(which_not_answered, person, test, thetas, prior = FALSE){
    Theta <- test@ThetaGrid
    pick1 <- na.omit(person$items_answered)
    pars <- test@mo@ParObjects$pars[c(pick1, test@length + 1)]
    itemloc <- c(0, cumsum(test@mo@Data$K[pick1])) + 1L
    if(length(pick1)){
        ll <- log(mirt:::computeItemtrace(pars=pars, Theta=Theta, itemloc = itemloc, CUSTOM.IND=list()))
        pick2 <- itemloc[-length(itemloc)] + person$responses[pick1]
        LL <- rowSums(ll[ ,pick2, drop=FALSE])
    } else LL <- rep(1, nrow(Theta))
    Is <- matrix(NA, nrow(Theta), length(which_not_answered))
    for(i in seq_len(nrow(Theta)))
        Is[i, ] <- .Call('ComputeCriteria', test@EIs, Theta[i, ,drop=FALSE], which_not_answered, 
                    1, 0, person$info_thetas)
    Is <- log(Is) + LL
    if(prior) Is <- Is + log(test@density)
    crit <- apply(exp(Is), 2, function(y, x) integrate.xy(x, y), x = Theta)
    crit
}

InfoMats <- function(which_not_answered, person, test, thetas){
    .Call('ComputeCriteriaMats', test@EIs, person$thetas, which_not_answered, 
          person$info_thetas)
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
        for(i in seq_len(length(which_not_answered))){
            ii <- test@EIs[[which_not_answered[i]]]
            p0 <- probtrace(ii, thetas - delta)
            p1 <- probtrace(ii, thetas + delta)
            info[i] <- sum(p1 * (log(p1) - log(p0)))
        }
    } else {
        info <- matrix(0, nrow(thetas2), length(which_not_answered))
        for(i in seq_len(length(which_not_answered))){
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
    Theta <- matrix(seq(as.vector(person$thetas-delta), 
                        as.vector(person$thetas+delta), length.out=test@quadpts))
    LL <- vector('list', nrow(possible_patterns))
    ll <- log(mirt:::computeItemtrace(pars = test@mo@ParObjects$pars,
                                      Theta=Theta, 
                                      itemloc = test@mo@Model$itemloc,
                                      CUSTOM.IND=list()))
    for(i in seq_len(nrow(possible_patterns))){
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