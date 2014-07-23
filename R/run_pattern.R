run_local <- function(responses){
    
    item <- findNextCATItem(person=MCE$person, test=MCE$test, lastitem=0L,
                            criteria=MCE$design$criteria)
    MCE$person$items_answered[1L] <- item
    
    for(i in 2L:(length(responses)+1L)){
        pick <- MCE$person$items_answered[i-1]
        name <- MCE$test$itemnames[pick]
        ip <- responses[pick]
        MCE$person$raw_responses[pick] <- MCE$person$responses[pick] <- 
            which(MCE$test$item_options[[pick]] %in% ip) - 1L
        if(!is.na(MCE$test$item_answers[[pick]]) && MCE$test$item_class[pick] != 'nestlogit')
            MCE$person$responses[pick] <- as.integer(ip == MCE$test$item_answers[[pick]])
        
        #update Thetas
        MCE$person$Update.thetas()
        MCE$design$Update.stop_now()
        if(i > MCE$design$preCAT_nitems)
            if(MCE$design$stop_now) 
                break
        
        MCE$design$Next.stage(item=i)
        
        item <- findNextCATItem(person=MCE$person, test=MCE$test, lastitem=i-1L,
                                criteria=MCE$design$criteria)
        MCE$person$items_answered[i] <- item
    }
    
    return(MCE$person)
}