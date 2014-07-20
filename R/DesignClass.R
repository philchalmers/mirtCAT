Design <- setRefClass("Design", 
                    
                    fields = list(method = 'character',
                                  criteria = 'character',
                                  conjunctive = 'logical',
                                  min_SEM = 'numeric',
                                  min_items = 'integer',
                                  max_items = 'integer',
                                  stop_now = 'logical',
                                  Wrule_weights = 'numeric',
                                  KL_delta = 'numeric',
                                  preCAT_nitems = 'integer',
                                  preCAT_criteria = 'character',
                                  preCAT_method = 'character'),
                    
                    methods = list(
                        initialize = function(method, criteria, nfact, design_list,
                                              preCAT_list, nitems){
                            method <<- method
                            criteria <<- criteria
                            if(nfact > 1L && 
                                   !any(criteria %in% c('Drule', 'Trule', 'Wrule', 'seq', 'random')))
                                stop('Selected criteria not valid for multidimensional tests')
                            conjunctive <<- TRUE
                            min_SEM <<- .3
                            Wrule_weights <<- rep(1/nfact, nfact)
                            min_items <<- 1L
                            max_items <<- nitems
                            stop_now <<- FALSE
                            preCAT_nitems <<- 0L
                            KL_delta <<- 0.1
                            if(length(design_list)){
                                if(!is.null(design_list$KL_delta))
                                    KL_delta <<- design_list$KL_delta
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
                            if(!mirt:::closeEnough(sum(Wrule_weights)-1, -1e-6, 1e-6))
                                stop('Wrule_weights does not sum to 1')
                            if(length(preCAT_list)){
                                if(is.null(preCAT_list$nitems))
                                    stop('preCAT_list nitems must be specified')
                                else preCAT_nitems <<- preCAT_list$nitems
                                if(is.null(preCAT_list$method))
                                    preCAT_method <<- 'MAP'
                                else preCAT_method <<- preCAT_list$method
                                if(is.null(preCAT_list$criteria))
                                    preCAT_criteria <<- 'random'
                                else preCAT_criteria <<- preCAT_list$criteria
                            }
                        })
                    
)

Design$methods(

    # Check whether to stop adaptive test
    Update.stop_now = function(){
        nanswered <- sum(!is.na(MCE$person$responses))
        if(MCE$person$score){
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
        }
        if(nanswered == max_items) stop_now <<- TRUE
    },
    
    Next.stage = function(){
        
    }
)