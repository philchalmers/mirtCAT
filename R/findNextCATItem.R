findNextCATItem <- function(person, test){
    
    #heavy lifty CAT stuff just to find new item
    not_answered <- is.na(person$responses)
    K <- test$mirt_object@Data$K
    positive_patterns <- matrix(person$responses, sum(K-1L)-1L, 
                                length(not_answered), byrow=TRUE)
    row <- 1L
    row_loc <- numeric(nrow(positive_patterns))
    for(ii in which(not_answered)){
        resp <- 1L:(K[ii] - 1L)
        row_loc[row:(row+length(resp)-1L)] <- ii
        for(j in 1L:length(resp)){
            positive_patterns[row, ii] <- resp[j]
            row <- row + 1L   
        }
    }
    
    if(MCE$design$criteria == 'random'){
        item <- sample(which(not_answered), 1)
        return(item)
    } else if(MCE$design$criteria == 'KL' || MCE$design$criteria == 'KLP'){
        browser()
        item <- 1
        return(item)
    }
    
    # TODO might fail
    acovs <- try(fscores(test$mirt_object, return.acov = TRUE, method = MCE$design$method, 
                         response.pattern = positive_patterns), silent=TRUE)
    
    if(MCE$test$nfact == 1L){
        
        if(MCE$design$criteria == ''){}
        
        
    } else {
        
        if(MCE$design$criteria == 'Drule'){
            crit <- do.call(c, lapply(avovs, det))
            item <- row_loc[which(max(crit) == crit)]            
        } else if(MCE$design$criteria == 'Trule'){
            crit <- do.call(c, lapply(avovs, function(x) sum(diag(x))))
            item <- row_loc[which(min(crit) == crit)]
        } else if(MCE$design$criteria == 'Wrule'){
            crit <- do.call(c, lapply(avovs, function(x, w) w %*% x %*% w, 
                                      w=MCE$design$Trule_weights))
            item <- row_loc[which(min(crit) == crit)]
        } 
    }
    
    return(item)
    
    browser()
    
    
}