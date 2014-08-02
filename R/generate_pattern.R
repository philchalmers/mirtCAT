#' Generate a CAT pattern
#' 
#' Generate a CAT pattern given various inputs. Returns a character or numeric vector 
#' with length equal to the test size.
#' 
#' @param mirt_object single group object defined by the \code{mirt} package
#'
#' @param Theta a numeric vector indicating the latent theta values for a single person
#' 
#' @param choices a list of character vectors signifying the possible choices for each item.
#'   If NULL, a numeric vector is returned indicating the selected category for each item (where
#'   0 is the lowest possible category)
#' 
#' @param item_answers (optional) a character vector indicating which of the options in 
#'   \code{choices} should be considered the 'correct' answer. This is required for item types that 
#'   dichotomously score items (e.g., multiple choice items scored with the 3PL model)
#' 
#' @export generate_pattern
#' @author Phil Chalmers \email{rphilip.chalmers@@gmail.com}
#' @seealso \code{\link{mirtCAT}}
#' 
#' @examples
#' \dontrun{
#' 
#' # return real response vector given choices and (optional) answers 
#' pat <- generate_pattern(mod, Theta = 0, choices = choices, item_answers=answers)
#' # mirtCAT(questions, mirt_object=mod, local_pattern = pat)
#' 
#' # generate pattern where 0 is the lowest possible response category
#' pat2 <- generate_pattern(mod, Theta = 0)
#' # mirtCAT(mirt_object=mod, local_pattern = pat2)
#' 
#' }
generate_pattern <- function(mirt_object, Theta, choices = NULL, item_answers = NULL){
    nitems <- ncol(mirt_object@Data$data)
    pattern <- matrix(0L, 1L, nitems)
    if(!is.matrix(Theta)) Theta <- matrix(Theta, 1L)
    if(is.null(choices)){
        K <- mirt_object@Data$K
        for(i in 1L:nitems){
            ii <- extract.item(mirt_object, i)
            P <- probtrace(ii, Theta)
            uniq <- 0L:(K[i]-1)
            pattern[i] <- sample(uniq, 1L, prob = P)
        }
        return(as.numeric(pattern))
    }
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