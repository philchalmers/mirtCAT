#' Compute the values given the criteria and internal objects
#' 
#' A function that returns a named vector of evaluated criteria for each respective item in the 
#' test bank. The names are associated with the item number in the bank. 
#' Note that criteria values are returned such that the maximum value always 
#' represents the most optimal item (e.g., maximum information). In cases where the minimum value is 
#' typically selected (e.g., minimum variance) all values are multiplied by -1 to turn it into a maximization
#' problem.
#' 
#' @param x an object of class 'mirtCAT_design' returned from the \code{\link{mirtCAT}} function
#'   when passing \code{design_elements = TRUE}
#'   
#' @param criteria item selection criteria (see \code{\link{mirtCAT}}'s \code{criteria} input)
#'   
#' @param person (required when \code{x} is missing) internal person object. To be 
#'   used when \code{customNextItem} function has been defined
#' 
#' @param design (required when \code{x} is missing) internal design object. To be 
#'   used when \code{customNextItem} function has been defined
#' 
#' @param test (required when \code{x} is missing) internal test object. To be 
#'   used when \code{customNextItem} function has been defined
#'   
#' @param subset an integer vector indicating which items should be included in the optimal search;
#'   the default \code{NULL} includes all possible items. To allow only the first 10 items to be 
#'   selected from this can be modified to \code{subset = 1:10}. This is useful when administering 
#'   a multi-unidimensional CAT session where unidimensional blocks should be clustered together 
#'   for smoother presentation. Useful when using the \code{customNextItem} function in 
#'   \code{\link{mirtCAT}}
#' 
#' @param info_mats logical; if more than one trait is present in the test, should the repective information
#'   matricies be returned instead of the scalar summary statistics (e.g., D-rule). When TRUE will 
#'   return a list of matricies associated with each respective item
#'
#' @seealso \code{\link{mirtCAT}}, \code{\link{updateDesign}}, \code{\link{extract.mirtCAT}},
#'   \code{\link{findNextItem}}
#' @export 
#' @author Phil Chalmers \email{rphilip.chalmers@@gmail.com}     
#' @return a vector of criteria values for each respective item
#'   
#' @examples
#' \dontrun{
#' # test defined in mirtCAT help file, first example
#' CATdesign <- mirtCAT(df, mod, design_elements = TRUE)
#' 
#' computeCriteria(CATdesign, criteria = 'MI')
#' computeCriteria(CATdesign, criteria = 'MEI')
#' 
#' }
computeCriteria <- function(x, criteria, person = NULL, 
                            test = NULL, design = NULL, subset = NULL,
                            info_mats = FALSE){
    if(!missing(x)){
        design <- x$design
        person <- x$person
        test <- x$test
    }
    if(any(is.null(person) || is.null(test) || is.null(design)))
        stop('computeCriteria has improper inputs', call.=FALSE)
    if(missing(criteria))
        stop('Please specify a valid selection criteria', call.=FALSE)
    if(info_mats) criteria <- 'info_mats'
    design@criteria <- criteria
    return(findNextCATItem(person=person, test=test, design=design, start=FALSE,
                           subset=subset, all_index=FALSE, values=TRUE))
}
