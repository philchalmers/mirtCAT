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
        item <- sample(which(not_answered), 1)
    } else if(MCE$design$criteria == 'KL'){
        for(i in which_not_answered){
            pick <- possible_patterns[i == which_not_answered, , drop=FALSE]
        }
        browser()
        
    } else {
                
        # category probabilities given current thetas
        P <- numeric(nrow(possible_patterns))
        for(i in which_not_answered){
            ii <- extract.item(mirt_object, i)
            p <- probtrace(ii, MCE$person$thetas)
            P[row_loc == i] <- p
        }
        
        # TODO might fail. Proposed thetas and their acovs/information            
        thetas <- try(fscores(test$mirt_object, method = MCE$design$method, 
                              response.pattern = possible_patterns), silent=TRUE)
        thetas <- thetas[,paste0('F', 1L:MCE$test$nfact), drop=FALSE]        
        acovstmp <- try(fscores(test$mirt_object, return.acov = TRUE, method = MCE$design$method, 
                             response.pattern = possible_patterns), silent=TRUE)
        infotmp <- lapply(acovstmp, function(x) try(solve(x), silent = TRUE))
        for(i in 1L:length(P)) infotmp[[i]] <- infotmp[[i]] * P[i]
        infos <- vector('list', length(which_not_answered))
        for(i in 1L:length(infos)){
            pick <- which(row_loc == which_not_answered[i])
            infos[[i]] <- do.call(`+`, infotmp[pick])
        }
        
        if(MCE$test$nfact == 1L){
            
            if(MCE$design$criteria == 'MI'){
                crit <- do.call(c, infos)
                item <- which_not_answered[max(crit) == crit]
            } else if(MCE$design$criteria == 'MEI'){
                browser()
            }
            
            
        } else {
            
            acovs <- lapply(infos, function(x) try(solve(x), silent = TRUE))
            
            if(MCE$design$criteria == 'Drule'){
                crit <- do.call(c, lapply(avovs, det))
                item <- which_not_answered[which(min(crit) == crit)]            
            } else if(MCE$design$criteria == 'Trule'){
                crit <- do.call(c, lapply(avovs, function(x) sum(diag(x))))
                item <- which_not_answered[which(min(crit) == crit)]
            } else if(MCE$design$criteria == 'Wrule'){
                crit <- do.call(c, lapply(avovs, function(x, w) w %*% x %*% w, 
                                          w=MCE$design$Trule_weights))
                item <- which_not_answered[which(min(crit) == crit)]
            } 
        }
    }
    
    return(as.integer(item[1L]))
}