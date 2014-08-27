#' Return the cached results from an interupted GUI session
#' 
#' If the mirtCAT GUI session was interupted, thee function will return the cached 
#' results if they are available. If no cached results can be found, a message is printed and
#' a \code{NULL} object is returned. 
#' 
#' Returns a list object containing the raw responses, items answered, demographic information,
#' and item response time.
#' 
#' @export partial_result
#' @author Phil Chalmers \email{rphilip.chalmers@@gmail.com}
#' @seealso \code{\link{mirtCAT}}
#' 
#' @examples
#' \dontrun{
#' 
#' partial_result()
#' 
#' # interupt a mirtCAT session, and run again to obtain partial information
#' # mirtCAT(...)
#' partial_result() 
#' 
#' }

partial_result <- function(){
    if(MCE$complete){
        message('No partial results are available.')
        return(invisible())
    } else {
        person <- MCE$person
        ret <- list(raw_responses=person$raw_responses + 1L, 
                    items_answered=person$items_answered,
                    item_time=person$item_time,
                    demographics=person$demographics)
        return(ret)    
    }
}