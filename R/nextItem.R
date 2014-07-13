nextItem <- function(){
    if(MCE$Next == sum(!is.na(MCE$person$responses))){
        MCE$Next <- MCE$Next + 1L
        if(MCE$test$adaptive){
            browser() #FIXME
            
        } else {
            return(MCE$Next)
        }
    } else return(MCE$person$items_answered[min(which(is.na(MCE$person$responses)))])
}