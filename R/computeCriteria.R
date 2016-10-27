#' Compute the values given the criteria and internal objects
#' 
#' A function that returns a vector of evaluated criteria for each respective item in the 
#' test bank. Note that criteria values are returned such that the maximum value always 
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
                            test = NULL, design = NULL){
    if(!missing(x)){
        design <- x$design
        person <- x$person
        test <- x$test
    }
    if(any(is.null(person) || is.null(test) || is.null(design)))
        stop('computeCriteria has improper inputs', call.=FALSE)
    if(missing(criteria))
        stop('Please specify a valid selection criteria', call.=FALSE)
    design@criteria <- criteria
    return(findNextCATItem(person=person, test=test, design=design,
                           subset=NULL, all_index=FALSE, values=TRUE))
}
