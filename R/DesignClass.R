Design <- setRefClass("Design", 
                    
                    fields = list(method = 'character',
                                  criteria = 'character',
                                  adaptive = 'logical',
                                  conjunctive = 'logical',
                                  max_SEM = 'numeric',
                                  stop_now = 'logical',
                                  Wrule_weights = 'numeric'),
                    
                    methods = list(
                        initialize = function(method, criteria, adaptive, nfact, design_list){
                            method <<- method
                            criteria <<- criteria
                            adaptive <<- adaptive
                            conjunctive <<- TRUE
                            max_SEM <<- .3
                            Wrule_weights <<- rep(1/nfact, nfact)
                            stop_now <<- FALSE
                            if(length(design_list)){
                                if(!is.null(design_list$conjunctive)) 
                                    conjunctive <<- design_list$conjunctive
                                if(!is.null(design_list$Wrule_weights)) 
                                    Wrule_weights <<- design_list$Wrule_weights
                                if(!is.null(design_list$max_SEM))
                                    max_SEM <<- design_list$max_SEM
                            }
                        })
                    
)

Design$methods(

    # Check whether to stop adaptive test
    Update.stop_now = function(){
        if(!conjunctive && MCE$test$nfact > 1L){
            info <- try(solve(MCE$person$thetas_acov), TRUE)
            diff <- if(is(info, 'try-error')) max_SEM + 1
            else sqrt(1/abs(MCE$design$Wrule_weights %*% info %*% MCE$design$Wrule_weights))
        } else {
            diff <- MCE$person$thetas_SE_history[nrow(MCE$person$thetas_SE_history), ]
        }        
        if(all(diff < max_SEM)) stop_now <<- TRUE
    }
)