Person <- setRefClass("Person", 
                      
                      fields = list(raw_responses = 'integer',
                                    responses = 'integer',
                                    items_answered = 'integer',
                                    thetas = 'matrix',
                                    thetas_history = 'matrix',
                                    thetas_SE_history = 'matrix',
                                    info_thetas = 'matrix',
                                    demographics = 'data.frame',
                                    item_time = 'numeric',
                                    score = 'logical'),
                      
                      methods = list(
                         initialize = function(nfact, nitems, thetas.start_in, score){
                             raw_responses <<- as.integer(rep(NA, nitems))
                             responses <<- as.integer(rep(NA, nitems))
                             items_answered <<- as.integer(rep(NA, nitems))
                             thetas <<- matrix(numeric(nfact))
                             thetas_SE_history <<- matrix(1, 1L, nfact)
                             score <<- score
                             item_time <<- numeric(nitems)
                             if(!is.null(thetas.start_in))
                                thetas <<- thetas.start_in
                             thetas_history <<- matrix(thetas, 1L, nfact)
                             info_thetas <<- matrix(0, nfact, nfact)
                         })
                      
)

Person$methods(
    
    # Update thetas
    Update.thetas = function(){
        if(score){
            method <- MCE$design$method
            if(method == 'ML'){
                if(length(unique(na.omit(responses))) < 2L) method <- 'MAP'
            }
            tmp <- try(fscores(MCE$test$mirt_object, method=method, response.pattern=responses,
                               rotate=MCE$test$fscores_args$rotate, theta_lim=MCE$test$fscores_args$theta_lim,
                               MI = MCE$test$fscores_args$MI, quadpts = MCE$test$quadpts, 
                               mean = MCE$test$fscores_args$mean, cov = MCE$test$fscores_args$cov), 
                       silent=TRUE)
            if(!is(tmp, 'try-error'))
                thetas <<- tmp[,paste0('F', 1L:MCE$test$nfact), drop=FALSE]
            thetas_history <<- rbind(thetas_history, thetas)
            thetas_SE_history <<- rbind(thetas_SE_history, 
                                        tmp[,paste0('SE_F', 1L:MCE$test$nfact), drop=FALSE])
            set <- c('Drule', 'Trule', 'Erule', 'Wrule', 
                     'DPrule', 'TPrule', 'EPrule', 'WPrule')
            if(!MCE$design$numerical_info && MCE$test$nfact > 1L && 
                   MCE$design$criteria %in% set){
                pick <- which(!is.na(responses))
                infos <- lapply(pick, function(x, thetas)
                    FI(extract.item(MCE$test$mirt_object, x), Theta=thetas), thetas=thetas)
                tmp <- matrix(0, nrow(infos[[1L]]), ncol(infos[[1L]]))
                for(i in 1L:length(infos))
                    tmp <- tmp + infos[[i]]
                if(MCE$design$criteria %in% c('DPrule', 'TPrule', 'EPrule', 'WPrule'))
                    tmp <- tmp + solve(MCE$test$gp$gcov)
                info_thetas <<- tmp
            }
        }
    }
)
