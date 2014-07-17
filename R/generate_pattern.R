#' Generate a CAT pattern
#' 
#' Generate a CAT pattern given various inputs. Returns a character vector with length equal to
#' the test size.
#' 
#' @param mirt_object something
#'
#' @param Theta something
#' 
#' @param choices something
#' 
#' @param item_answers something
#' 
#' @export generate_pattern
#' @author Phil Chalmers \email{rphilip.chalmers@@gmail.com}
#' @seealso \code{\link{mirtCAT}}
#' 
#' @examples
#' \dontrun{
#' 
#' pat <- generate_pattern(mod, Theta = 0, choices = choices, item_answers=answers)
#' 
#' 
#' 
#' }
generate_pattern <- function(mirt_object, Theta, choices, item_answers = NULL){
    nitems <- length(choices)
    pattern <- matrix(0L, 1L, nitems)
    ret <- character(nitems)
    K_1 <- do.call(c, lapply(choices, length)) - 1L
    has_item_answers <- !is.null(item_answers)
    if(has_item_answers)
        K_1[!is.na(item_answers)] <- 1L
    for(i in 1L:nitems){
        ii <- extract.item(mirt_object, i)
        P <- probtrace(ii, Theta)
        uniq <- 0L:K_1[i]
        pattern[i] <- sample(uniq, 1L, prob = P)
        if(has_item_answers){
            ret[i] <- if(pattern[i] == 1L) item_answers[i] else
                sample(choices[[i]][!choices[[i]] %in% item_answers[i]], 1L)
        } else {
            ret[i] <- choices[[i]][pattern[i]+1L]
        }
    }
    ret
}