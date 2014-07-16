# each function returns a numeric vector of values, length == nrow(possible_patterns)

MI <- function(which_not_answered, possible_patterns, person, test){
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
    acovs <- getAcovs(possible_patterns)
    infotmp <- lapply(acovs, solve)
    infos <- weighted_mat(P=P, mat=infotmp, row_loc=row_loc, which_not_answered=which_not_answered)
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
    acovstmp <- getAcovs(possible_patterns)
    acovs <- weighted_mat(P=P, mat=acovstmp, row_loc=row_loc, which_not_answered=which_not_answered)
    crit <- do.call(c, acovs)
    crit
    
}

MLWI <- function(which_not_answered, possible_patterns, person, test){
    browser()
    
    
}

MPWI <- function(which_not_answered, possible_patterns, person, test){
    browser()
    
    
}

Drule <- function(which_not_answered, possible_patterns, person, test){
    browser()
    acovs <- getAcovs(possible_patterns)
    crit <- do.call(c, lapply(acovs, det))
    crit
}

Trule <- function(which_not_answered, possible_patterns, person, test){
    browser()
    acovs <- getAcovs(possible_patterns)
    crit <- do.call(c, lapply(acovs, function(x) sum(diag(x))))
    crit
}

Wrule <- function(which_not_answered, possible_patterns, person, test){
    browser()
    acovs <- getAcovs(possible_patterns)
    crit <- do.call(c, lapply(acovs, function(x, w) w %*% x %*% w, 
                              w=MCE$design$Trule_weights))
    crit
}

KL <- function(which_not_answered, possible_patterns, person, test){
    browser()
    
    
}