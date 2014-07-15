Person <- setRefClass("Person", 
                      
                      fields = list(raw_responses = 'integer',
                                    responses = 'integer',
                                    items_answered = 'integer',
                                    thetas = 'matrix',
                                    theta_acov = 'matrix',
                                    thetas_history = 'matrix',
                                    thetas_SE_history = 'matrix',
                                    max_change = 'numeric',
                                    stop_now = 'logical',
                                    CAT_values = 'numeric',
                                    demographics = 'data.frame'),
                      
                      methods = list(
                         initialize = function(nfact, nitems, thetas.start = NULL){
                             raw_responses <<- as.integer(rep(NA, nitems))
                             responses <<- as.integer(rep(NA, nitems))
                             items_answered <<- as.integer(rep(NA, nitems))
                             if(is.null(thetas.start)){
                                thetas <<- matrix(numeric(nfact))
                             } else {
                                 thetas <<- thetas.start
                             }
                             thetas_history <<- matrix(thetas, 1L, nfact)
                             thetas_SE_history <<- matrix(NA, 1L, nfact)
                             max_change <<- rep(.3, nfact)
                             stop_now <<- FALSE
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
        if(MCE$design$adaptive){
            theta_acov <<- try(fscores(MCE$test$mirt_object, return.acov = TRUE,
                            method = MCE$design$method, response.pattern = responses), silent=TRUE)[[1L]]
        }
    },
    
    # Check whether to stop adaptive test
    Update.stop_now = function(){
        diff <- thetas_SE_history[nrow(thetas_SE_history)]
        if(all(diff < max_change))
            stop_now <<- TRUE
    }
    
)