Design <- setRefClass("Design", 
                    
                    fields = list(method = 'character',
                                  criteria = 'character',
                                  criteria_estimator = 'character',
                                  classify = 'numeric',
                                  classify_alpha = 'numeric',
                                  min_SEM = 'numeric',
                                  met_SEM = 'logical',
                                  min_items = 'integer',
                                  max_items = 'integer',
                                  stop_now = 'logical',
                                  exposure = 'integer',
                                  Wrule_weights = 'numeric',
                                  KL_delta = 'numeric',
                                  start_item = 'integer',
                                  preCAT_nitems = 'integer',
                                  preCAT_criteria = 'character',
                                  preCAT_method = 'character',
                                  CAT_criteria = 'character',
                                  CAT_method = 'character',
                                  max_time = 'numeric',
                                  use_content = 'logical',
                                  content = 'factor',
                                  content_prop = 'numeric',
                                  content_prop_empirical = 'numeric',
                                  numerical_info = 'logical'),
                    
                    methods = list(
                        initialize = function(method, criteria, nfact, design,
                                              start_item, preCAT, nitems, exposure){
                            numerical_info <<- FALSE
                            method <<- method
                            criteria <<- criteria
                            criteria_estimator <<- 'MAP'
                            if(criteria %in% c('Drule', 'Trule', 'Erule', 'Wrule', 'Arule')){
                                criteria_estimator <<- 'ML'
                            } else if(criteria %in% c('DPrule', 'TPrule', 'EPrule', 'WPrule',
                                                      'MEPV', 'APrule')){
                                criteria_estimator <<- 'MAP'
                            }
                            CAT_criteria <<- criteria
                            CAT_method <<- method
                            start_item <<- as.integer(start_item)
                            if(!length(exposure) == nitems)
                                stop('exposure input is not the correct length')
                            pick <- exposure > nitems:1L
                            tmp <- exposure
                            tmp[pick] <- (nitems:1L)[pick]
                            exposure <<- as.integer(tmp)
                            if(start_item != 1 && criteria == 'seq')
                                stop('start_item must equal 1 with seq criteria')
                            if(nfact > 1L && 
                                   !any(criteria %in% c('Drule', 'Trule', 'Wrule', 'KL', 'KLn',
                                                        'Erule', 'seq', 'random', 'Arule', 'APrule',
                                                        'DPrule', 'TPrule', 'EPrule', 'WPrule')))
                                stop('Selected criteria not valid for multidimensional tests')
                            min_SEM <<- .3
                            met_SEM <<- rep(FALSE, nfact)
                            Wrule_weights <<- rep(1/nfact, nfact)
                            min_items <<- 1L
                            max_items <<- nitems
                            stop_now <<- FALSE
                            preCAT_nitems <<- 0L
                            KL_delta <<- 0.1
                            max_time <<- Inf
                            use_content <<- FALSE
                            content_prop_empirical <<- 1
                            classify <<- NaN
                            classify_alpha <<- .05
                            if(length(design)){
                                if(!is.null(design$content)){
                                    use_content <<- TRUE
                                    content <<- factor(design$content)
                                    if(!mirt:::closeEnough(sum(design$content_prop)-1, -1e-6, 1e-6))
                                        stop('content_prop does not sum to 1')
                                    tmp <- design$content_prop
                                    tmp <- tmp[match(names(table(content)), names(tmp))]
                                    content_prop <<- tmp
                                    tmp[1L:length(tmp)] <- 0
                                    content_prop_empirical <<- tmp
                                }
                                if(!is.null(design$max_time))
                                    max_time <<- design$max_time
                                if(!is.null(design$KL_delta))
                                    KL_delta <<- design$KL_delta
                                if(!is.null(design$Wrule_weights)) 
                                    Wrule_weights <<- design$Wrule_weights
                                if(!is.null(design$min_SEM))
                                    min_SEM <<- design$min_SEM
                                if(!is.null(design$min_items))
                                    min_items <<- as.integer(design$min_items)
                                if(!is.null(design$max_items))
                                    max_items <<- as.integer(design$max_items)
                                if(!is.null(design$numerical_info))
                                    numerical_info <<- design$numerical_info
                                if(!is.null(design$classify))
                                    classify <<- design$classify
                                if(!is.null(design$classify_CI)){
                                    if(design$classify_CI > 1 || design$classify_CI < 0)
                                        stop('classify_CI criteria must be between 0 and 1')
                                    classify_alpha <<- (1 - design$classify_CI)/2
                                }
                            }
                            if(use_content && criteria == 'seq')
                                stop('content designs are not supported for seq criteria')
                            if(!mirt:::closeEnough(sum(Wrule_weights)-1, -1e-6, 1e-6))
                                stop('Wrule_weights does not sum to 1')
                            if(length(min_SEM) != 1L && length(min_SEM) != nfact)
                                stop('min_SEM criteria is not a suitable length')
                            if(length(preCAT)){
                                if(is.null(preCAT$nitems))
                                    stop('preCAT nitems must be specified')
                                else preCAT_nitems <<- as.integer(preCAT$nitems)
                                if(is.null(preCAT$method))
                                    preCAT_method <<- 'MAP'
                                else preCAT_method <<- preCAT$method
                                if(is.null(preCAT$criteria))
                                    preCAT_criteria <<- 'random'
                                else preCAT_criteria <<- preCAT$criteria
                                criteria <<- preCAT_criteria
                                method <<- preCAT_method
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
                if(!is.nan(classify[1L])){
                    z <- -abs(MCE$person$thetas - classify) / diff
                    if(all(z < qnorm(classify_alpha))) stop_now <<- TRUE
                } else {
                    met_SEM <<- diff < min_SEM
                    if(all(met_SEM)) stop_now <<- TRUE
                }
            }
        }
        if(nanswered == max_items) stop_now <<- TRUE
        if(max_time <= sum(MCE$person$item_time)) stop_now <<- TRUE
    },
    
    Next.stage = function(item){
        if(item == preCAT_nitems){
            criteria <<- CAT_criteria
            method <<- CAT_method
        }
    }
)