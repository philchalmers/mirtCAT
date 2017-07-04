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
#' @references 
#' 
#' Chalmers, R., P. (2012). mirt: A Multidimensional Item Response Theory
#' Package for the R Environment. \emph{Journal of Statistical Software, 48}(6), 1-29.
#' \doi{10.18637/jss.v048.i06}
#' 
#' Chalmers, R. P. (2016). Generating Adaptive and Non-Adaptive Test Interfaces for 
#' Multidimensional Item Response Theory Applications. \emph{Journal of Statistical Software, 71}(5), 
#' 1-39. \doi{10.18637/jss.v071.i05}
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
    on.exit(.MCE$preamble_defined <- .MCE$start_time <- NULL)
    if(is.null(.MCE$preamble_defined))
        stop('Please use a fresh mirtCAT_preamble() call prior to calling createShinyGUI().')
    if(is.null(ui)) ui <- default_UI
    return(shinyApp(ui=ui(), server=server))
}
