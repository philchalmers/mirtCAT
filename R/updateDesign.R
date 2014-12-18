#' Update design elements
#' 
#' A function that will update the object returned from \code{\link{findNextItem}}.
#' 
#' @param x an object of class 'mirtCAT_design' returned from the \code{\link{mirtCAT}} function
#'   when passing \code{design_elements = TRUE}
#' @param items a numeric vector indicating which items to select
#' @param responses a numeric vector indiciating the responses the the selected items
#' @param Theta (optional) vector indicating the value of Theta/latent traits to be set
#' 
#' @seealso \code{\link{mirtCAT}}, \code{\link{findNextItem}}
#' @export updateDesign
#' @author Phil Chalmers \email{rphilip.chalmers@@gmail.com}     
#' @return returns returns an object of class 'mirtCAT_design' with updated elements
#' @examples
#' \dontrun{
#' # test defined in mirtCAT help file, first example
#' CATdesign <- mirtCAT(df, mod, criteria = 'MI', design_elements = TRUE)
#' 
#' # returns number 1 in this case, since that's the starting item
#' findNextItem(CATdesign) 
#' 
#' # detemine next item if item 1 and item 10 were answered correctly, and Theta = 0.5
#' CATdesign <- updateDesign(CATdesign, items = c(1, 10), responses = c(1, 1), Theta = 0.5)
#' findNextItem(CATdesign) 
#' }
#' 
updateDesign <- function(x, items, responses, Theta=NULL){
    if(missing(x) || missing(items) || missing(responses))
        stop('require inputs have not been supplied')
    if(!is.null(Theta)){
        Theta <- matrix(Theta, nrow = 1L)
        x$person$thetas <- Theta
    }
    x$person$responses[items] <- as.integer(responses)
    pick <- min(which(is.na(x$person$items_answered)))
    x$person$items_answered[pick:(length(responses)+pick-1L)] <- as.integer(items)
    return(x)
}