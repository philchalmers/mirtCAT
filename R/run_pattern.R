run_local <- function(responses){
    
    MCE$person$items_answered[1L] <- 1L
    
    for(i in 2L:(length(responses)+1L)){
        pick <- MCE$person$items_answered[i-1]
        name <- MCE$test$itemnames[pick]
        ip <- responses[pick]
        MCE$person$raw_responses[pick] <- MCE$person$responses[pick] <- 
            which(MCE$test$item_options[[pick]] %in% ip) - 1L
        if(!is.na(MCE$test$item_answers[[pick]]))
            MCE$person$responses[pick] <- as.integer(ip == MCE$test$item_answers[[pick]])
        
        #update Thetas
        MCE$person$Update.thetas()
        MCE$design$Update.stop_now()
        if(MCE$design$stop_now) break
        
        item <- findNextCATItem(person=MCE$person, test=MCE$test, lastitem=i-1L,
                                criteria=MCE$design$criteria)
        MCE$person$items_answered[i] <- item
    }
    
    return(MCE$person)
}