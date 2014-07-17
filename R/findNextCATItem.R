findNextCATItem <- function(person, test){
    
    #heavy lifty CAT stuff just to find new item
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
    
    if(MCE$design$criteria == 'random'){
        item <- sample(which_not_answered, 1)
    } else if(MCE$design$criteria == 'KL'){
        crit <- KL(which_not_answered=which_not_answered, possible_patterns=possible_patterns,
                   person=person, test=test, row_loc=row_loc)
        item <- which_not_answered[max(crit) == crit]
        browser()
    } else if(MCE$design$criteria == 'MI'){
        crit <- MI(which_not_answered=which_not_answered, possible_patterns=possible_patterns,
                   person=person, test=test, row_loc=row_loc)
        item <- which_not_answered[max(crit) == crit]
    } else if(MCE$design$criteria == 'MEI'){
        crit <- MEI(which_not_answered=which_not_answered, possible_patterns=possible_patterns,
                    person=person, test=test, row_loc=row_loc)
        item <- which_not_answered[max(crit) == crit]
    } else if(MCE$design$criteria == 'MEPV'){
        crit <- MEPV(which_not_answered=which_not_answered, possible_patterns=possible_patterns,
                    person=person, test=test, row_loc=row_loc)
        item <- which_not_answered[min(crit) == crit]
    } else if(MCE$design$criteria == 'MLWI'){
        crit <- MLWI(which_not_answered=which_not_answered, possible_patterns=possible_patterns,
                     person=person, test=test, row_loc=row_loc)
        item <- which_not_answered[max(crit) == crit]
    } else if(MCE$design$criteria == 'MPWI'){
        crit <- MPWI(which_not_answered=which_not_answered, possible_patterns=possible_patterns,
                     person=person, test=test, row_loc=row_loc)
        item <- which_not_answered[max(crit) == crit]
    } else if(MCE$design$criteria == 'Drule'){
        crit <- Drule(which_not_answered=which_not_answered, possible_patterns=possible_patterns,
                      person=person, test=test, row_loc=row_loc)
        item <- which_not_answered[which(max(crit) == crit)]            
    } else if(MCE$design$criteria == 'Trule'){
        crit <- Trule(which_not_answered=which_not_answered, possible_patterns=possible_patterns,
                      person=person, test=test, row_loc=row_loc)
        item <- which_not_answered[which(max(crit) == crit)]
    } else if(MCE$design$criteria == 'Wrule'){
        crit <- Wrule(which_not_answered=which_not_answered, possible_patterns=possible_patterns,
                      person=person, test=test, row_loc=row_loc)
        item <- which_not_answered[which(min(crit) == crit)]
    }
    
    return(as.integer(item[1L]))
}