run_local <- function(responses, nfact, start_item, nitems, thetas.start_in, 
                      score, verbose = FALSE, ...){
    
    ret <- vector('list', nrow(responses))
    
    for(n in 1L:nrow(responses)){
        
        MCE$design$stop_now <- FALSE
        if(is.na(start_item)) MCE$design$start_item <- sample(1L:ncol(responses), 1L)
        person <- Person$new(nfact=nfact, nitems=nitems, 
                             thetas.start_in=thetas.start_in, score=score)
        
        item <- findNextCATItem(person=person, test=MCE$test, design=MCE$design)
        person$items_answered[1L] <- item
        
        for(i in 2L:(ncol(responses)+1L)){
            if(verbose) cat(sprintf('\rRow: %i; Item: %i; Thetas: %.3f; SE(Thetas): %.3f', n, i, person$thetas,
                                    person$thetas_SE_history[nrow(person$thetas_SE_history), ]))
            pick <- person$items_answered[i-1]
            name <- MCE$test$itemnames[pick]
            ip <- responses[n, pick]
            person$raw_responses[pick] <- person$responses[pick] <- 
                which(MCE$test$item_options[[pick]] %in% ip) - 1L
            if(!is.na(MCE$test$item_answers[[pick]]) && MCE$test$item_class[pick] != 'nestlogit')
                person$responses[pick] <- as.integer(ip == MCE$test$item_answers[[pick]])
            
            #update Thetas
            person$Update.thetas()
            MCE$design$Update.stop_now(person)
            if(i > MCE$design$preCAT_nitems)
                if(MCE$design$stop_now) 
                    break
            
            MCE$design$Next.stage(item=i)
            
            item <- findNextCATItem(person=person, test=MCE$test, design=MCE$design)
            person$items_answered[i] <- item
        }
        MCE$item_time <- numeric(0)
        ret[[n]] <- person
    }
    if(verbose) cat('\n')
    ret
}