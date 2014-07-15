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

KL <- function(positive_patterns){
    browser()
    
    
}
