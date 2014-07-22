Design <- setRefClass("Design", 
                    
                    fields = list(method = 'character',
                                  criteria = 'character',
                                  min_SEM = 'numeric',
                                  min_items = 'integer',
                                  max_items = 'integer',
                                  stop_now = 'logical',
                                  exposure = 'integer',
                                  Wrule_weights = 'numeric',
                                  KL_delta = 'numeric',
                                  random.start = 'logical',
                                  preCAT_nitems = 'integer',
                                  preCAT_criteria = 'character',
                                  preCAT_method = 'character',
                                  CAT_criteria = 'character',
                                  CAT_method = 'character'),
                    
                    methods = list(
                        initialize = function(method, criteria, nfact, design_list,
                                              random.start, preCAT_list, nitems, exposure){
                            method <<- method
                            criteria <<- criteria
                            CAT_criteria <<- criteria
                            CAT_method <<- method
                            random.start <<- random.start
                            if(length(exposure) == nitems)
                                exposure <<- as.integer(exposure)
                            else stop('exposure input is not the correct length')
                            if(any(exposure[2L:(length(exposure)-1L)] > (nitems-2L):1L))
                                stop('exposure input contains more sampling than possible options')
                            if(random.start && criteria == 'seq')
                                stop('random.start with sequantially criteria is invalid')
                            if(nfact > 1L && 
                                   !any(criteria %in% c('Drule', 'Trule', 'Wrule', 'KL', 'KLn',
                                                        'Erule', 'seq', 'random')))
                                stop('Selected criteria not valid for multidimensional tests')
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
                            if(length(min_SEM) != 1L && length(min_SEM) != nfact)
                                stop('min_SEM criteria is not a suitable length')
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
                                criteria <<- preCAT_criteria
                                method <<- preCAT_method
                                if(random.start && criteria == 'seq')
                                    stop('random.start with sequantially criteria is invalid')
                            }
                        })
                    
)

Design$methods(

    # Check whether to stop adaptive test
    Update.stop_now = function(){
        nanswered <- sum(!is.na(MCE$person$responses))
        if(MCE$person$score){
            if(nanswered >= min_items){
                diff <- MCE$person$thetas_SE_history[nrow(MCE$person$thetas_SE_history), ]
                if(all(diff < min_SEM)) stop_now <<- TRUE
            }
        }
        if(nanswered == max_items) stop_now <<- TRUE
    },
    
    Next.stage = function(item){
        if(item == preCAT_nitems){
            criteria <<- CAT_criteria
            method <<- CAT_method
        }
    }
)