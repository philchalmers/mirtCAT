findNextCATItem <- function(person, test, lastitem, criteria){
    
    #heavy lifty CAT stuff just to find new item
    if(all(is.na(person$responses)))
        return(MCE$design$start_item)
    not_answered <- is.na(person$responses)
    which_not_answered <- which(not_answered)
    K <- test$mirt_object@Data$K
    possible_patterns <- matrix(person$responses, sum(K[not_answered]), 
                                length(not_answered), byrow=TRUE)
    row <- 1L
    row_loc <- numeric(nrow(possible_patterns))
    for(ii in which(not_answered)){
        resp <- 0L:(K[ii] - 1L)
        row_loc[row:(row+length(resp)-1L)] <- ii
        for(j in 1L:length(resp)){
            possible_patterns[row, ii] <- resp[j]
            row <- row + 1L   
        }
    }
    method <- MCE$design$criteria_estimator
    #saftey features
    if(length(unique(na.omit(person$responses))) < 2L) method <- 'MAP'
    if(sum(!is.na(person$responses)) < 5L) method <- 'MAP'
    
    if(criteria == 'seq'){
        return(as.integer(lastitem + 1L))
    } else if(criteria == 'random'){
        if(length(which_not_answered) == 1L) item <- which_not_answered
        else item <- sample(which_not_answered, 1L)
        return(as.integer(item))
    } else if(criteria == 'KL'){
        crit <- KL(which_not_answered=which_not_answered, possible_patterns=possible_patterns,
                   person=person, test=test, row_loc=row_loc, delta=MCE$design$KL_delta)
        index <- which_not_answered
    } else if(criteria == 'KLn'){
            crit <- KL(which_not_answered=which_not_answered, possible_patterns=possible_patterns,
                       person=person, test=test, row_loc=row_loc, 
                       delta=MCE$design$KL_delta*sqrt(sum(!is.na(person$responses))))
            index <- which_not_answered
    } else if(criteria == 'IKL'){
        crit <- IKL(which_not_answered=which_not_answered, possible_patterns=possible_patterns,
                   person=person, test=test, row_loc=row_loc, delta=MCE$design$KL_delta)
        index <- which_not_answered
    } else if(criteria == 'IKLP'){
            crit <- IKL(which_not_answered=which_not_answered, possible_patterns=possible_patterns,
                        person=person, test=test, row_loc=row_loc, delta=MCE$design$KL_delta,
                        den=TRUE)
            index <- which_not_answered
    } else if(criteria == 'IKLn'){
        crit <- IKL(which_not_answered=which_not_answered, possible_patterns=possible_patterns,
                   person=person, test=test, row_loc=row_loc, 
                   delta=MCE$design$KL_delta*sqrt(sum(!is.na(person$responses))))
        index <- which_not_answered
    } else if(criteria == 'IKLPn'){
        crit <- IKL(which_not_answered=which_not_answered, possible_patterns=possible_patterns,
                    person=person, test=test, row_loc=row_loc, 
                    delta=MCE$design$KL_delta*sqrt(sum(!is.na(person$responses))))
        index <- which_not_answered
    } else if(criteria == 'MI'){
        crit <- MI(which_not_answered=which_not_answered, possible_patterns=possible_patterns,
                   person=person, test=test, row_loc=row_loc)
        index <- which_not_answered
    } else if(criteria == 'MEI'){
        crit <- MEI(which_not_answered=which_not_answered, possible_patterns=possible_patterns,
                    person=person, test=test, row_loc=row_loc)
        index <- which_not_answered
    } else if(criteria == 'MEPV'){
        crit <- -MEPV(which_not_answered=which_not_answered, possible_patterns=possible_patterns,
                    person=person, test=test, row_loc=row_loc)
        index <- which_not_answered
    } else if(criteria == 'MLWI'){
        crit <- MLWI(which_not_answered=which_not_answered, possible_patterns=possible_patterns,
                     person=person, test=test, row_loc=row_loc)
        index <- which_not_answered
    } else if(criteria == 'MPWI'){
        crit <- MPWI(which_not_answered=which_not_answered, possible_patterns=possible_patterns,
                     person=person, test=test, row_loc=row_loc)
        index <- which_not_answered
    } else if(criteria == 'Drule' || criteria == 'DPrule'){
        crit <- -Drule(which_not_answered=which_not_answered, possible_patterns=possible_patterns,
                      person=person, test=test, row_loc=row_loc, method=method)
        index <- row_loc
    } else if(criteria == 'Erule' || criteria == 'EPrule'){
        crit <- -Erule(which_not_answered=which_not_answered, possible_patterns=possible_patterns,
                      person=person, test=test, row_loc=row_loc, method=method)
        index <- row_loc
    } else if(criteria == 'Trule' || criteria == 'TPrule'){
        crit <- Trule(which_not_answered=which_not_answered, possible_patterns=possible_patterns,
                      person=person, test=test, row_loc=row_loc, method=method)
        index <- row_loc
    } else if(criteria == 'Wrule' || criteria == 'WPrule'){
        crit <- Wrule(which_not_answered=which_not_answered, possible_patterns=possible_patterns,
                      person=person, test=test, row_loc=row_loc, method=method)
        index <- row_loc
    } else {
        stop('Selection criteria does not exist')
    }
    
    if(MCE$design$exposure[lastitem+1L] == 1L){
        item <- index[which(max(crit) == crit)][1L]
    } else {
        rnk <- rank(crit, ties.method = 'random')
        pick <- which(rnk %in% 1L:MCE$design$exposure[lastitem+1L])
        item <- index[sample(pick, 1L)]
    }
    return(as.integer(item))
}