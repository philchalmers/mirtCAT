Design <- setRefClass("Design", 
                    
                    fields = list(method = 'character',
                                  criteria = 'character',
                                  adaptive = 'logical',
                                  conjunctive = 'logical',
                                  min_SEM = 'numeric',
                                  min_items = 'integer',
                                  max_items = 'integer',
                                  stop_now = 'logical',
                                  Wrule_weights = 'numeric'),
                    
                    methods = list(
                        initialize = function(method, criteria, adaptive, nfact, design_list){
                            method <<- method
                            criteria <<- criteria
                            if(criteria == 'MI' && nfact > 1L)
                                criteria <<- 'Drule'
                            adaptive <<- adaptive
                            conjunctive <<- TRUE
                            min_SEM <<- .3
                            Wrule_weights <<- rep(1/nfact, nfact)
                            min_items <<- 1L
                            max_items <<- as.integer(1e8)
                            stop_now <<- FALSE
                            if(length(design_list)){
                                if(!is.null(design_list$conjunctive)) 
                                    conjunctive <<- design_list$conjunctive
                                if(!is.null(design_list$Wrule_weights)) 
                                    Wrule_weights <<- design_list$Wrule_weights
                                if(!is.null(design_list$min_SEM))
                                    min_SEM <<- design_list$min_SEM
                                if(!is.null(design_list$min_items))
                                    min_items <<- as.integer(design_list$min_items)
                                if(!is.null(design_list$max_items))
                                    max_items <<- as.integer(design_list$max_items)
                            }
                        })
                    
)

Design$methods(

    # Check whether to stop adaptive test
    Update.stop_now = function(){
        nanswered <- sum(!is.na(MCE$person$responses))
        if(nanswered >= min_items){
            if(!conjunctive && MCE$test$nfact > 1L){
                info <- try(solve(MCE$person$thetas_acov), TRUE)
                diff <- if(is(info, 'try-error')) min_SEM + 1
                else sqrt(1/abs(MCE$design$Wrule_weights %*% info %*% MCE$design$Wrule_weights))
            } else {
                diff <- MCE$person$thetas_SE_history[nrow(MCE$person$thetas_SE_history), ]
            }        
            if(all(diff < min_SEM)) stop_now <<- TRUE
        }
        if(nanswered >= max_items) stop_now <<- TRUE
    }
)