MCE <- new.env()

calcLL <- function(thetas){
    LL <- 0
    for(i in MCE$person$responses){
        if(!is.na(MCE$person$responses)){
            item <- extract.item(MCE$test$mirt_object, i)
            P <- probtrace(item, thetas)
            LL <- LL + log(P[ ,MCE$person$responses])
        }
    }
    LL
}

getAcovs <- function(possible_patterns){
    ret <- try(fscores(MCE$test$mirt_object, return.acov = TRUE, method = MCE$design$method, 
                response.pattern = possible_patterns), silent = TRUE)
    ret    
} 

weighted_mat <- function(mat, row_loc, which_not_answered, P = rep(1, length(row_loc))){
    for(i in 1L:length(P)) mat[[i]] <- mat[[i]] * P[i]
    mat2 <- vector('list', length(unique(row_loc)))
    for(i in 1L:length(mat2)){
        pick <- which(row_loc == which_not_answered[i])
        mat2[[i]] <- do.call(`+`, mat[pick])
    }
    mat2
}