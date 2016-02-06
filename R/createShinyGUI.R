#' Function returning an object used by shiny
#' 
#' This function returns the GUI setup results by calling \code{\link{shinyApp}}. 
#' Primarily, this is only useful when hosting the application publicly, such as 
#' through \url{http://www.shinyapps.io/}. The function \code{\link{mirtCAT_preamble}} must be 
#' run \emph{before} this function is called. The object is executed by calling \code{\link{runApp}}.
#' 
#' @param ui a shiny UI function used to define the interface. If \code{NULL}, the default one will be used. 
#'   See \code{mirtCAT:::default_UI} for the internal code
#' 
#' @export createShinyGUI
#' 
#' @author Phil Chalmers \email{rphilip.chalmers@@gmail.com}
#' 
#' @seealso \code{\link{mirtCAT}}, \code{\link{mirtCAT_preamble}}, \code{\link{getPerson}}
#' 
#' @examples 
#' \dontrun{
#' 
#' mirtCAT_preamble(df = df)
#' runApp(createShinyGUI(), port = 8000)
#' 
#' person <- getPerson()
#' summary(person)
#' 
#' } 
createShinyGUI <- function(ui = NULL){
    on.exit(.MCE$preamble_defined <- NULL)
    if(is.null(.MCE$preamble_defined))
        stop('Please use a fresh mirtCAT_preamble() call prior to calling createShinyGUI().')
    if(is.null(ui)) ui <- default_UI
    return(shinyApp(ui=ui(), server=server))
}
