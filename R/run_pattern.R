run_local <- function(responses, nfact, start_item, nitems, thetas.start_in, 
                      score, design, test, verbose = FALSE, cl = NULL){
    
    fn <- function(n, responses, nfact, start_item, nitems, thetas.start_in, 
                   score, verbose, design, test){
        if(is.na(start_item)) design@start_item <- sample(1L:ncol(responses), 1L)
        person <- Person$new(nfact=nfact, nitems=nitems, theta_SEs=sqrt(diag(test@gp$gcov)),
                             thetas.start_in=thetas.start_in, score=score)
        item <- findNextCATItem(person=person, test=test, design=design, criteria=design@criteria)
        if(is.na(item)){
            design@stop_now <- TRUE
            return(person)
        }
        person$items_answered[1L] <- item
        
        for(i in 2L:(ncol(responses)+1L)){
            if(verbose) cat(sprintf('\rRow: %i; Item: %i; Thetas: %.3f; SE(Thetas): %.3f', n, i, person$thetas,
                                    person$thetas_SE_history[nrow(person$thetas_SE_history), ]))
            pick <- person$items_answered[i-1]
            name <- test@itemnames[pick]
            ip <- responses[n, pick]
            person$responses[pick] <- which(test@item_options[[pick]] %in% ip) - 1L
            person$raw_responses[pick] <- as.character(person$responses[pick] + 1L)
            if(!is.na(test@item_answers[[pick]]) && test@item_class[pick] != 'nestlogit')
                person$responses[pick] <- as.integer(ip == test@item_answers[[pick]])
            
            #update Thetas
            person$Update.thetas(design, test)
            design <- Update.stop_now(design, person)
            if(design@stop_now) break
            
            design <- Next.stage(design, person=person, test=test, item=i)
            
            item <- findNextCATItem(person=person, test=test, design=design, 
                                    criteria=design@criteria)
            if(is.na(item)){
                design@stop_now <- TRUE
                break
            } 
            person$items_answered[i] <- item
        }
        return(person)
    }
    if(is.null(cl) || nrow(responses) == 1L){
        ret <- lapply(1L:nrow(responses), fn, responses=responses, nfact=nfact, start_item=start_item,
                      nitems=nitems, thetas.start_in=thetas.start_in, score=score, verbose=verbose, 
                      design=design, test=test)
    } else {
        ret <- parallel::parLapply(cl=cl, X=1L:nrow(responses), fun=fn, responses=responses, 
                                   nfact=nfact, start_item=start_item, design=design, test=test,
                                   nitems=nitems, thetas.start_in=thetas.start_in, score=score,
                                   verbose=verbose)
    }
    if(verbose) cat('\n')
    ret
}