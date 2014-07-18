#' Generate a CAT pattern
#' 
#' Generate a CAT pattern given various inputs. Returns a character vector with length equal to
#' the test size.
#' 
#' @param mirt_object single group object defined by the \code{mirt} package
#'
#' @param Theta a numeric vector indicating the latent theta values for a single person
#' 
#' @param choices a list of character vectors signifying the possible choices for each item
#' 
#' @param item_answers (optional) a character vector indicating which of the options in 
#'   \code{choices} should be considered the 'correct' answer. This is required for itemtypes that 
#'   dichotomously score items (e.g., multiple choice items scored with the 3PL model)
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
#' }
generate_pattern <- function(mirt_object, Theta, choices, item_answers = NULL){
    nitems <- length(choices)
    pattern <- matrix(0L, 1L, nitems)
    ret <- character(nitems)
    K_1 <- do.call(c, lapply(choices, length)) - 1L
    has_item_answers <- !is.null(item_answers)
    if(!is.matrix(Theta)) Theta <- matrix(Theta, 1L)
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