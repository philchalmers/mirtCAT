Person <- setRefClass("Person", 
                      
                      fields = list(responses = 'integer',
                                    items_answered = 'integer',
                                    thetas = 'matrix',
                                    thetas_history = 'matrix',
                                    max_change = 'numeric',
                                    stop_now = 'logical',
                                    demographics = 'data.frame'),
                      
                      methods = list(
                         initialize = function(nfact, nitems, thetas.start = NULL){
                             responses <<- as.integer(rep(NA, nitems))
                             items_answered <<- integer(0L)
                             if(is.null(thetas.start)){
                                thetas <<- matrix(numeric(nfact))
                             } else {
                                 thetas <<- thetas.start
                             }
                             thetas_history <<- matrix(thetas, 1L, nfact)
                             max_change <<- rep(.1, nfact)
                             stop_now <<- FALSE
                         })
                      
)

Person$methods(
    
    # Update thetas
    Update.thetas = function(response, item, Test){
        newthetas <- rnorm(length(thetas)) #TODO placeholder
        thetas <<- newthetas
        thetas_history <<- rbind(thetas_history, newthetas)        
    },
    
    # Check whether to stop adaptive test
    Check.stop_now = function(){
        diff <- abs(thetas_history[nrow(thetas_history), ] - 
                        thetas_history[nrow(thetas_history) - 1L, ])
        if(all(diff < max_change)){
            return(TRUE)
        } else {
            return(FALSE)
        }
    }
    
)