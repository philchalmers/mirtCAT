#' Function returning an object used by shiny
#' 
#' This function returns the GUI setup results by calling \code{\link{shinyApp}}. 
#' Primarily, this is only useful when hosting the application publicly, such as 
#' through \url{http://www.shinyapps.io/}. The function \code{\link{mirtCAT_preamble}} must be 
#' run \emph{before} this function is called.
#' 
#' @export createShinyGUI
#' 
#' @author Phil Chalmers \email{rphilip.chalmers@@gmail.com}
#' 
#' @seealso \code{\link{mirtCAT}}, \code{\link{mirtCAT_preamble}} 
createShinyGUI <- function(final_fun = NULL){
    if(is.null(final_fun)) final_fun <- function()
    MCE$final_fun <- final_fun
    return(shinyApp(ui=ui(), server=server))
}
