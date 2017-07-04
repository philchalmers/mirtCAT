#' Update design elements
#' 
#' A function that will update the object returned from \code{\link{findNextItem}}.
#' 
#' @param x an object of class 'mirtCAT_design' returned from the \code{\link{mirtCAT}} function
#'   when passing \code{design_elements = TRUE}
#' @param items a numeric vector indicating which items to select
#' @param responses a numeric vector indicating the responses the the selected items
#' @param Theta (optional) vector indicating the value of Theta/latent traits to be set
#' 
#' @seealso \code{\link{mirtCAT}}, \code{\link{findNextItem}}
#' @export updateDesign
#' @author Phil Chalmers \email{rphilip.chalmers@@gmail.com}
#' @references 
#' 
#' Chalmers, R., P. (2012). mirt: A Multidimensional Item Response Theory
#' Package for the R Environment. \emph{Journal of Statistical Software, 48}(6), 1-29.
#' \doi{10.18637/jss.v048.i06}
#' 
#' Chalmers, R. P. (2016). Generating Adaptive and Non-Adaptive Test Interfaces for 
#' Multidimensional Item Response Theory Applications. \emph{Journal of Statistical Software, 71}(5), 
#' 1-39. \doi{10.18637/jss.v071.i05}     
#' @return returns an object of class 'mirtCAT_design' with updated elements.
#' @examples
#' \dontrun{
#' # test defined in mirtCAT help file, first example
#' CATdesign <- mirtCAT(df, mod, criteria = 'MI', design_elements = TRUE)
#' 
#' # returns number 1 in this case, since that's the starting item
#' findNextItem(CATdesign) 
#' 
#' # determine next item if item 1 and item 10 were answered correctly, and Theta = 0.5
#' CATdesign <- updateDesign(CATdesign, items = c(1, 10), responses = c(1, 1), Theta = 0.5)
#' findNextItem(CATdesign) 
#' 
#' # alternatively, update the Theta using the Update.thetas definition in design
#' CATdesign$design@Update.thetas(CATdesign$design, CATdesign$person, CATdesign$test) 
#' findNextItem(CATdesign)
#' }
#' 
updateDesign <- function(x, items, responses, Theta=NULL){
    if(missing(x) || missing(items) || missing(responses))
        stop('require inputs have not been supplied', call.=FALSE)
    if(!is.null(Theta)){
        Theta <- matrix(Theta, nrow = 1L)
        x$person$thetas <- Theta
    }
    if(any(items > length(x$person$responses)))
        stop('Items locations are larger than the length of the test.', call.=FALSE)
    x$person$responses[items] <- as.integer(responses)
    x$person$raw_responses[items] <- as.character(x$person$responses[items] + 1L)
    pick <- min(which(is.na(x$person$items_answered)))
    x$person$items_answered[pick:(length(responses)+pick-1L)] <- as.integer(items)
    return(x)
}