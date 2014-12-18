#' Generate a CAT patterns
#' 
#' Generate a CAT pattern given various inputs. Returns a character vector or numeric matrix 
#' (depending on whether a \code{df} input was supplied) with columns equal to the test size and
#' rows equal to the number of rows in \code{Theta}. For simulation studies, supplying a 
#' \code{Theta} input with more than 1 row will run independent CAT session when passed to 
#' \code{mirtCAT()}.
#' 
#' @param mirt_object single group object defined by the \code{mirt} package
#'
#' @param Theta a numeric vector indicating the latent theta values for a single person
#' 
#' @param df (optional) data.frame object containing questions, options, and scoring
#'   keys. See \code{\link{mirtCAT}} for details
#' 
#' @export generate_pattern
#' @author Phil Chalmers \email{rphilip.chalmers@@gmail.com}
#' @seealso \code{\link{mirtCAT}}
#' 
#' @examples
#' \dontrun{
#' 
#' # return real response vector given choices and (optional) answers 
#' pat <- generate_pattern(mod, Theta = 0, df=df)
#' # mirtCAT(df, mirt_object=mod, local_pattern = pat)
#' 
#' # generate single pattern observed in dataset used to define mod
#' pat2 <- generate_pattern(mod, Theta = 0)
#' # mirtCAT(mirt_object=mod, local_pattern = pat2)
#'
#' # generate multiple patterns to be analyzed independently 
#' pat3 <- generate_pattern(mod, Theta = matrix(c(0, 2, -2), 3))
#' # mirtCAT(mirt_object=mod, local_pattern = pat3)
#' 
#' }
generate_pattern <- function(mirt_object, Theta, df = NULL){
    fn <- function(p, ns) sample(1L:ns, 1L, prob = p) - 1
    nitems <- ncol(mirt_object@Data$data)
    if(!is.matrix(Theta)) Theta <- matrix(Theta, 1L)
    N <- nrow(Theta)
    pattern <- matrix(0L, N, nitems)
    if(is.null(df)){
        K <- mirt_object@Data$K
        for(i in 1L:nitems){
            ii <- extract.item(mirt_object, i)
            P <- probtrace(ii, Theta)
            pattern[,i] <- apply(P, 1L, fn, ns = ncol(P))
        }
        return(t(t(pattern) + mirt_object@Data$mins))
    } else {
        choices <- df[,grepl('Option', colnames(df))]
        item_answers <- df[,grepl('Answer', colnames(df))]
        if(is.matrix(item_answers))
            stop('Only one correct answer is supported when drawing data')
    }
    ret <- character(nitems)
    has_item_answers <- length(item_answers) > 0L
    for(i in 1L:nitems){
        ii <- extract.item(mirt_object, i)
        P <- probtrace(ii, Theta)
        uniq <- 1L:ncol(P) - 1L
        pattern[i] <- sample(uniq, 1L, prob = P)
        if(has_item_answers){
            ret[i] <- if(pattern[i] == 1L) item_answers[i] else
                sample(as.character(choices[i, ][!choices[i, ] %in% item_answers[i]]), 1L)
        } else {
            ret[i] <- as.character(choices[i, ][pattern[i]+1L])
        }
    }
    ret
}