#' Update design elements
#' 
#' A function that will update the object returned from \code{\link{findNextItem}}. This can 
#' be used to run the CAT session manually in a set-by-step manner. 
#' 
#' @param x an object of class 'mirtCAT_design' returned from the \code{\link{mirtCAT}} function
#'   when passing \code{design_elements = TRUE}
#' @param new_item a numeric vector indicating which items to select
#' @param new_response a numeric vector indicating the responses the the selected items
#' @param updateTheta logical; update the internal ability terms after the new item response has been
#'  added to the internal objects? 
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
#' 
#' set.seed(1)
#' nitems <- 100
#' itemnames <- paste0('Item.', 1:nitems)
#' a <- matrix(rlnorm(nitems, .2, .3))
#' d <- matrix(rnorm(nitems))
#' dat <- simdata(a, d, 500, itemtype = 'dich')
#' colnames(dat) <- itemnames
#' mod <- mirt(dat, 1, verbose = FALSE)
#' 
#' # test defined in mirtCAT help file, first example
#' CATdesign <- mirtCAT(mo = mod, criteria = 'MI', design_elements = TRUE,
#'   start_item = 2)
#' 
#' # returns 2 in this case, since that was the starting item
#' findNextItem(CATdesign) 
#' 
#' # first iteration, no answered items
#' CATdesign$person$items_answered
#' 
#' # update when next item is item 2 and answered correctly
#' CATdesign <- updateDesign(CATdesign, new_item = 2, new_response = 1)
#' CATdesign$person$items_answered  # item 2 answered first
#' CATdesign$person$responses       # in item 2 element response was = 1 
#' CATdesign$person$thetas # current estimate
#' findNextItem(CATdesign) 
#' 
#' # determine next item if item 70 were also answered correctly next
#' CATdesign <- updateDesign(CATdesign, new_item = 70, new_response = 1)
#' CATdesign$person$items_answered  
#' CATdesign$person$responses       
#' findNextItem(CATdesign) 
#' 
#' # continue on, now with item 95 added next (answered incorrectly)
#' CATdesign <- updateDesign(CATdesign, new_item = 95, new_response = 0)
#' CATdesign$person$thetas
#' CATdesign$person$thetas_history
#' CATdesign$person$thetas_SE_history
#' findNextItem(CATdesign)
#' 
#' }
#' 
updateDesign <- function(x, new_item, new_response, updateTheta = TRUE){
    if(missing(x) || missing(new_item) || missing(new_response))
        stop('require inputs have not been supplied', call.=FALSE)
    if(length(new_item) > 1L || length(new_response) > 1L)
        stop("new_item and new_response must be length 1 integers")
    if(new_item > length(x$person$responses))
        stop('Items locations are larger than the length of the test.', call.=FALSE)
    if(any(x$person$items_answered %in% new_item))
        stop('The new_item selected has already been administered', call.=FALSE)
    x$person$responses[new_item] <- as.integer(new_response)
    x$person$raw_responses[new_item] <- as.character(x$person$responses[new_item] + 1L)
    pick <- min(which(is.na(x$person$items_answered)))
    x$person$items_answered[pick] <- as.integer(new_item)
    if(updateTheta)
        x$design@Update.thetas(x$design, x$person, x$test) 
    x$person$Update.info_mats(design=x$design, test=x$test)
    x$design <- Update.stop_now(x$design, person=x$person, test=x$test)
    return(x)
}