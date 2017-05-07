run_local <- function(responses, nfact, start_item, nitems, thetas.start_in, 
                      score, design, test, progress, 
                      verbose = FALSE, cl = NULL, primeCluster = TRUE){
    
    fn <- function(n, responses, nfact, start_item, nitems, thetas.start_in, 
                   score, verbose, design, test, Info_thetas_cov){
        person <- Person$new(nfact=nfact, nitems=nitems, theta_SEs=sqrt(diag(test@gp$gcov)),
                             thetas.start_in=thetas.start_in, score=score, Info_thetas_cov=Info_thetas_cov, ID=n)
        item <- design@start_item[n]
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
            design@Update.thetas(design=design, person=person, test=test)
            person$Update.info_mats(design=design, test=test)
            design <- Update.stop_now(design, person=person)
            if(design@stop_now) break
            
            design <- Next.stage(design, person=person, test=test, item=i)
            
            item <- findNextCATItem(person=person, test=test, design=design)
            if(is.na(item)){
                design@stop_now <- TRUE
                break
            }
            if(!is.null(attr(item, 'design'))) design <- attr(item, 'design')
            person$items_answered[i] <- as.integer(item)
        }
        return(person)
    }
    if(length(design@person_properties)){
        if(nrow(design@person_properties) != nrow(responses))
            stop('person_properties does not have the same number of rows as the local_pattern input',
                 call.=FALSE)
    }
    Info_thetas_cov <- solve(test@gp$gcov)
    if(is.null(cl) || nrow(responses) == 1L){
        ret <- if(progress){
            pbapply::pblapply(1L:nrow(responses), fn, responses=responses, nfact=nfact, start_item=start_item,
                              nitems=nitems, thetas.start_in=thetas.start_in, score=score, verbose=verbose, 
                              design=design, test=test, Info_thetas_cov=Info_thetas_cov)
        } else {
            lapply(1L:nrow(responses), fn, responses=responses, nfact=nfact, start_item=start_item,
                   nitems=nitems, thetas.start_in=thetas.start_in, score=score, verbose=verbose, 
                   design=design, test=test, Info_thetas_cov=Info_thetas_cov)
        }    
    } else {
        parallel::clusterEvalQ(cl, library("mirtCAT"))
        if(primeCluster) parallel::parLapply(cl=cl, X=1L:(length(cl)*2), function(x) invisible())
        ret <- if(progress){
            pbapply::pblapply(cl=cl, X=1L:nrow(responses), FUN=fn, responses=responses,
                              nfact=nfact, start_item=start_item, design=design, test=test,
                              nitems=nitems, thetas.start_in=thetas.start_in, score=score,
                              verbose=verbose, Info_thetas_cov=Info_thetas_cov)
        } else {
            parallel::parLapply(cl=cl, X=1L:nrow(responses), fun=fn, responses=responses,
                                nfact=nfact, start_item=start_item, design=design, test=test,
                                nitems=nitems, thetas.start_in=thetas.start_in, score=score,
                                verbose=verbose, Info_thetas_cov=Info_thetas_cov)
        }
    }
    if(verbose) cat('\n')
    ret
}