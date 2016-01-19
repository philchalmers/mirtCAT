Person <- setRefClass("Person", 
                      
                      fields = list(raw_responses = 'character',
                                    responses = 'integer',
                                    items_answered = 'integer',
                                    thetas = 'matrix',
                                    thetas_history = 'matrix',
                                    thetas_SE_history = 'matrix',
                                    info_thetas = 'matrix',
                                    demographics = 'data.frame',
                                    item_time = 'numeric',
                                    valid_item = 'logical',
                                    score = 'logical'),
                      
                      methods = list(
                         initialize = function(nfact, nitems, thetas.start_in, score,
                                               theta_SEs){
                             'Initialize the person object given background information'
                             raw_responses <<- as.character(rep(NA, nitems))
                             responses <<- as.integer(rep(NA, nitems))
                             valid_item <<- rep(TRUE, nitems)
                             items_answered <<- as.integer(rep(NA, nitems))
                             thetas <<- matrix(numeric(nfact), nrow=1L)
                             thetas_SE_history <<- matrix(theta_SEs, 1L)
                             score <<- score
                             item_time <<- numeric(nitems)
                             if(!is.null(thetas.start_in))
                                thetas <<- matrix(thetas.start_in, nrow=1L)
                             thetas_history <<- matrix(thetas, 1L, nfact)
                             info_thetas <<- matrix(0, nfact, nfact)
                         })
                      
)

Person$methods(
    
    # Update thetas
    Update.thetas = function(design, test){
        'Update the latent trait (theta) values using information 
        from the design and test objects'
        responses2 <- responses
        responses2[design@items_not_scored] <- NA
        if(score){
            method <- design@method
            if(last_item(items_answered) %in% design@items_not_scored)
                method <- 'fixed'
            if(method == 'ML'){
                if(length(unique(na.omit(responses2))) < 2L) method <- 'MAP'
            }
            if(method != 'fixed'){
                suppressWarnings(tmp <- fscores(test@mo, method=method, response.pattern=responses2,
                                   theta_lim=test@fscores_args$theta_lim,
                                   MI = test@fscores_args$MI, quadpts = test@quadpts, 
                                   mean = test@fscores_args$mean, cov = test@fscores_args$cov,
                                   QMC=test@fscores_args$QMC, custom_den=test@fscores_args$custom_den))
                thetas <<- tmp[,paste0('F', 1L:test@nfact), drop=FALSE]
                thetas_SE_history <<- rbind(thetas_SE_history, 
                                            tmp[,paste0('SE_F', 1L:test@nfact), drop=FALSE])
            } else {
                thetas_SE_history <<- rbind(thetas_SE_history, 
                                            thetas_SE_history[nrow(thetas_SE_history),])
            }
            thetas_history <<- rbind(thetas_history, thetas)
            set <- c('Drule', 'Trule', 'Erule', 'Wrule', 'Arule', 'APrule',
                     'DPrule', 'TPrule', 'EPrule', 'WPrule')
            if(test@nfact > 1L && design@criteria %in% set){
                pick <- which(!is.na(responses2))
                infos <- lapply(pick, function(x, thetas)
                    FI(extract.item(test@mo, x), Theta=thetas), thetas=thetas)
                tmp <- matrix(0, nrow(infos[[1L]]), ncol(infos[[1L]]))
                for(i in 1L:length(infos))
                    tmp <- tmp + infos[[i]]
                if(design@criteria %in% c('DPrule', 'TPrule', 'EPrule', 'WPrule', 'APrule'))
                    tmp <- tmp + solve(test@gp$gcov)
                info_thetas <<- tmp
            }
        }
    }
)
