nextItem <- function(){    
    if(MCE$test$adaptive){
        browser() #FIXME
        
    } else {
        return(max(1L, MCE$person$items_answered[length(MCE$person$items_answered)] + 1L))
    }
}