Person <- setRefClass("Person", 
                      
                      fields = list(ID = 'integer',
                                    raw_responses = 'character',
                                    responses = 'integer',
                                    items_answered = 'integer',
                                    thetas = 'matrix',
                                    thetas_history = 'matrix',
                                    thetas_SE_history = 'matrix',
                                    info_thetas = 'matrix',
                                    demographics = 'data.frame',
                                    item_time = 'numeric',
                                    valid_item = 'logical',
                                    state = 'list',
                                    login_name = 'character',
                                    score = 'logical',
                                    true_thetas = 'numeric',
                                    info_thetas_cov = 'matrix',
                                    terminated_sucessfully = 'logical'),
                      
                      methods = list(
                         initialize = function(nfact, nitems, thetas.start_in, score,
                                               theta_SEs, CustomUpdateThetas, Info_thetas_cov, ID = 0L){
                             'Initialize the person object given background information'
                             ID <<- ID
                             true_thetas <<- numeric(0L)
                             raw_responses <<- as.character(rep(NA, nitems))
                             responses <<- as.integer(rep(NA, nitems))
                             valid_item <<- rep(TRUE, nitems)
                             items_answered <<- as.integer(rep(NA, nitems))
                             thetas <<- matrix(numeric(nfact), nrow=1L)
                             thetas_SE_history <<- matrix(theta_SEs, 1L)
                             score <<- score
                             item_time <<- numeric(nitems)
                             login_name <<- character(0L)
                             if(!is.null(thetas.start_in))
                                thetas <<- matrix(thetas.start_in, nrow=1L)
                             thetas_history <<- matrix(thetas, 1L, nfact)
                             info_thetas <<- matrix(0, nfact, nfact)
                             info_thetas_cov <<- Info_thetas_cov 
                             terminated_sucessfully <<- FALSE
                         })
                      
)

Person$methods(
    
    # Update thetas
    Update.info_mats = function(design, test){
        'Update the information matricies for previous answered multidimensional IRT models'
        set <- c('Drule', 'Trule', 'Erule', 'Wrule', 'Arule', 'APrule',
                 'DPrule', 'TPrule', 'EPrule', 'WPrule', 'custom')
        if(test@nfact > 1L && design@criteria %in% set){
            responses2 <- responses
            responses2[design@items_not_scored] <- NA
            pick <- which(!is.na(responses2))
            infos <- lapply(pick, function(x, thetas)
                FI(extract.item(test@mo, x), Theta=thetas), thetas=thetas)
            tmp <- matrix(0, nrow(infos[[1L]]), ncol(infos[[1L]]))
            for(i in seq_len(length(infos)))
                tmp <- tmp + infos[[i]]
            info_thetas <<- tmp
        }
    },
    
    Update_thetas = function(theta, theta_SE){
        if(!is.matrix(theta)) theta <- matrix(theta, 1L)
        thetas <<- theta
        thetas_SE_history <<- rbind(thetas_SE_history, theta_SE)
        thetas_history <<- rbind(thetas_history, thetas)
    }
)
