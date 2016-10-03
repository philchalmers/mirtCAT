#' Extract elements from the internal person, test, and design objects
#' 
#' This function extracts elements, as well as builds a few convienient elements, 
#' from the three internal \code{person}, \code{design}, or \code{test}
#' objects that are accessible through a \code{customNextItem} function 
#' definition (see \code{\link{mirtCAT}} for details).
#' 
#' Depending on which object is supplied, the following elements can be extracted.
#' 
#' @section person:
#' 
#' \describe{
#'    \item{\code{something}}{}
#'    
#' }
#' 
#' @section test:
#' 
#' \describe{
#'    \item{\code{something}}{}
#'    
#' }
#' 
#' @section design:
#' 
#' \describe{
#'    \item{\code{something}}{}
#'    
#' }
#' 
#' @param x either the \code{person}, \code{design}, or \code{test} object defined through a 
#'   \code{customNextItem} definition
#'   
#' @param what a character vector extracting the desired element (see the Details section)
#' 
#' @export 
#' @author Phil Chalmers \email{rphilip.chalmers@@gmail.com}
#' @seealso \code{\link{mirt}}, \code{\link{mirtCAT}}, \code{\link{extract.mirt}}
#' 
#' @examples
#' \dontrun{
#' 
#' customNextItem <- function(person, design, test){
#'      # browser()
#'      
#'      sum(is.na(person$items_answered)) + 1L
#' }
#' 
#' }
extract.mirtCAT <- function(x, what){
    if(missing(x))
        stop('No person, test, or design input supplied', call.=FALSE)
    if(missing(what) || length(what) != 1L || !is.character(what))
        stop('Must supply a component to be extracted', call.=FALSE)
    
    ret <- 1
    ret
}