Person <- setRefClass("Person", 
                      
                      fields = list(raw_responses = 'integer',
                                    responses = 'integer',
                                    items_answered = 'integer',
                                    thetas = 'matrix',
                                    thetas_history = 'matrix',
                                    thetas_acov = 'matrix',
                                    thetas_SE_history = 'matrix',
                                    demographics = 'data.frame'),
                      
                      methods = list(
                         initialize = function(nfact, nitems, thetas.start_in){
                             raw_responses <<- as.integer(rep(NA, nitems))
                             responses <<- as.integer(rep(NA, nitems))
                             items_answered <<- as.integer(rep(NA, nitems))
                             thetas <<- matrix(numeric(nfact))
                             thetas_SE_history <<- matrix(1, 1L, nfact)
                             if(!is.null(thetas.start_in))
                                thetas <<- thetas.start_in
                             thetas_history <<- matrix(thetas, 1L, nfact)
                         })
                      
)

Person$methods(
    
    # Update thetas
    Update.thetas = function(){
        tmp <- try(fscores(MCE$test$mirt_object, 
                            method = MCE$design$method, response.pattern = responses), silent=TRUE)
        thetas <<- tmp[,paste0('F', 1L:MCE$test$nfact), drop=FALSE]
        thetas_history <<- rbind(thetas_history, thetas)
        thetas_SE_history <<- rbind(thetas_SE_history, 
                                    tmp[,paste0('SE_F', 1L:MCE$test$nfact), drop=FALSE])
        if(MCE$test$nfact > 1L && !MCE$design$conjunctive){
            tmp2 <- try(fscores(MCE$test$mirt_object, return.acov = TRUE,
                                method = MCE$design$method, response.pattern = responses), silent=TRUE)
            thetas_acov <<- tmp2[[1L]]
        }
    }
)
