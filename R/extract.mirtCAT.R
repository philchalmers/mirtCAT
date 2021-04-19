#' Extract elements from the internal person, test, and design objects
#' 
#' This function extracts elements, as well as builds a few convenient elements, 
#' from the three internal \code{person}, \code{design}, or \code{test}
#' objects that are accessible through a \code{customNextItem} function 
#' definition (see \code{\link{mirtCAT}} for details).
#' 
#' Depending on which object is supplied, the following elements can be extracted.
#' 
#' @section The 'person' argument:
#' 
#' \describe{
#'   \item{\code{ID}}{a scalar value indicating the ID of the participant 
#'     (generally only needed in Monte Carlo simulations)}
#'  \item{\code{responses}}{an integer vector indicating how items that have been responded to. 
#'    Each element pertains to the associated item location (e.g., \code{responses[100]} is associated with the 
#'    100th item), and is \code{NA} if the item has not been responded to}
#'   \item{\code{raw_responses}}{of the same form as \code{responses}, pertaining to the observed responses
#'     in a character vector}
#'   \item{\code{items_in_bank}}{an integer vector indicating items which have not been administered yet and 
#'     are also valid candidates for administration}
#'  \item{\code{items_answered}}{an integer vector indicating the order in which items have been responded to}
#'   \item{\code{thetas}}{the current ability/latent trait estimates given the previously administered items}
#'   \item{\code{thetas_SE}}{the current ability/latent trait standard error estimates given the 
#'     previously administered items}
#'   \item{\code{thetas_history}}{history of the  ability/latent trait estimates}
#'   \item{\code{thetas_SE_history}}{history of the latent trait standard error estimates}
#'   \item{\code{item_time}}{of the same form as \code{items_answered}, pertaining to the amount of time it took the 
#'     participant to response to the item}
#'   \item{\code{demographics}}{a data.frame containing the (optional) prior survey information from the GUI interface}
#'   \item{\code{clientData}}{a list of useful information from shiny's \code{session$clientData}}
#' }
#' 
#' @section The 'design' argument:
#' 
#' \describe{
#'    \item{\code{items_not_scored}}{an integer vector indicating items which should be included but not 
#'      scored in the test (these are experimental items)}
#'    \item{\code{min_items}}{minimum number of items to administer}
#'    \item{\code{max_items}}{maximum number of items to administer}
#'    \item{\code{max_time}}{maximum amount of time alloted to the GUI}
#'    \item{\code{met_SEM}}{logical vector indicating whether the SEM criteria has been met}
#'    \item{\code{met_delta_thetas}}{logical vector indicating whether the delta_thetas criteria has been met}
#'    \item{\code{met_classify}}{logical vector indicating whether the classify criteria has been met}
#'    \item{\code{exposure}}{exposure control elements of the same form as \code{responses}}
#'    \item{\code{content}}{content constraint information}
#'    \item{\code{content_prop}}{content proportions}
#'    \item{\code{test_properties}}{user-defined \code{data.frame} of test-based properties}
#'    \item{\code{person_properties}}{user-defined \code{data.frame} of person-based properties}
#' }
#' 
#' @section The 'test' argument:
#' 
#' \describe{
#'    \item{\code{mo}}{extract the defined model from the \code{mirt} package. Afterward, users can use the 
#'      \code{\link{extract.mirt}} function to pull out a large number of internal elements for easy use}
#' }
#' 
#' @param x either the \code{person}, \code{design}, or \code{test} object defined through a 
#'   \code{customNextItem} definition
#'   
#' @param what a character vector extracting the desired element (see the Details section)
#' 
#' @export 
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
#' @seealso \code{\link{mirt}}, \code{\link{mirtCAT}}, \code{\link{extract.mirt}}, 
#'   \code{\link{findNextItem}}
#'   
#' 
#' @examples
#' 
#' \dontrun{
#'  #example test
#' set.seed(1234)
#' nitems <- 25
#' itemnames <- paste0('Item.', 1:nitems)
#' a <- matrix(rlnorm(nitems, .2, .3))
#' d <- matrix(rnorm(nitems))
#' dat <- simdata(a, d, 500, itemtype = 'dich')
#' colnames(dat) <- itemnames
#' mod <- mirt(dat, 1, verbose = FALSE, TOL = .01)
#' 
#' # simple math items
#' questions <- answers <- character(nitems)
#' choices <- matrix(NA, nitems, 5)
#' spacing <- floor(d - min(d)) + 1 #easier items have more variation in the options
#' 
#' for(i in 1:nitems){
#'   n1 <- sample(1:50, 1)
#'   n2 <- sample(51:100, 1)
#'   ans <- n1 + n2
#'   questions[i] <- paste0(n1, ' + ', n2, ' = ?')
#'   answers[i] <- as.character(ans)
#'   ch <- ans + sample(c(-5:-1, 1:5) * spacing[i,], 5)
#'   ch[sample(1:5, 1)] <- ans
#'   choices[i, ] <- as.character(ch)
#' }
#' 
#' df <- data.frame(Question=questions, Option=choices, 
#'   Type = 'radio', stringsAsFactors = FALSE)
#' df$Answer <- answers
#' 
#' pat <- generate_pattern(mod, Theta = 0, df)
#' 
#' #------------------------------------------------
#' # administer items in sequence
#' customNextItem <- function(person, design, test){
#'    # browser()
#'    items_left_2_choose_from <- extract.mirtCAT(person, 'items_in_bank')
#'    min(items_left_2_choose_from)
#' }
#' 
#' res <- mirtCAT(df, local_pattern=pat, 
#'   design = list(customNextItem=customNextItem))
#' summary(res)
#' 
#' #------------------------------------------------
#' # administer items in order, but stop after 10 items
#' customNextItem <- function(person, design, test){
#'    items_left_2_choose_from <- extract.mirtCAT(person, 'items_in_bank')
#'    items_answered <- extract.mirtCAT(person, 'items_answered')
#'    total <- sum(!is.na(items_answered))
#'    ret <- if(total < 10) min(items_left_2_choose_from)
#'      else return(NA)
#'    ret
#' }
#' 
#' res <- mirtCAT(df, local_pattern=pat, 
#'   design = list(customNextItem=customNextItem))
#' summary(res)
#' 
#' #------------------------------------------------
#' # using findNextItem() and stopping after 10 items
#' 
#' customNextItem <- function(person, design, test){
#'    items_answered <- extract.mirtCAT(person, 'items_answered')
#'    total <- sum(!is.na(items_answered))
#'    ret <- NA
#'    if(total < 10) 
#'      ret <- findNextItem(person=person, test=test, design=design, criteria = 'MI')
#'    ret
#' }
#' 
#' res <- mirtCAT(df, mod, local_pattern=pat, start_item = 'MI',
#'   design = list(customNextItem=customNextItem))
#' summary(res)
#' 
#' # equivalent to the following
#' res2 <- mirtCAT(df, mod, local_pattern=pat, start_item = 'MI', 
#'   criteria = 'MI', design = list(max_items = 10))
#' summary(res2)
#' 
#' }
extract.mirtCAT <- function(x, what){
    if(missing(x))
        stop('No person, test, or design input supplied', call.=FALSE)
    if(missing(what) || length(what) != 1L || !is.character(what))
        stop('Must supply a component to be extracted', call.=FALSE)
    cls <- class(x)
    ret <- if(cls == 'Person'){
        switch(what,
               ID = x$ID,
               raw_responses = x$raw_responses,
               responses = x$responses,
               items_answered = x$items_answered,
               items_in_bank = which(is.na(x$responses)[x$valid_item]),
               thetas = x$thetas,
               thetas_history = x$thetas_history[1L:nrow(x$thetas_history), , drop=FALSE],
               thetas_SE = x$thetas_SE_history[nrow(x$thetas_SE_history), , drop=FALSE],
               thetas_SE_history = x$thetas_SE_history[1L:nrow(x$thetas_SE_history), , drop=FALSE],
               item_time = x$item_time,
               demographics = x$demographics,
               clientData = x$clientData)
    } else if(cls == 'Test'){
        switch(what, mo = x@mo)
    } else if(cls == 'Design'){
        switch(what, 
               items_not_scored = x@items_not_scored,
               min_items = x@min_items,
               max_items = x@max_items,
               exposure = x@exposure,
               content = x@content,
               max_time = x@max_time,
               met_SEM = x@met_SEM,
               met_delta_theta = x@met_delta_theta,
               met_classify = x@met_classify,
               test_properties = x@test_properties,
               person_properties = x@person_properties)
    } else stop('supplied object type not supported by extract.mirtCAT()', call.=FALSE)
    if(is.null(ret)) 
        stop('Extracted element could not be found in extract.mirtCAT()', call.=FALSE)
    ret
}