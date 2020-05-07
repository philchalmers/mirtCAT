#' Function returning an object used by shiny
#' 
#' This function returns the GUI setup results by calling \code{\link{shinyApp}}. 
#' Primarily, this is only useful when hosting the application publicly, such as 
#' through \url{http://www.shinyapps.io/}. The function \code{\link{mirtCAT_preamble}} must be 
#' run \emph{before} this function is called. The object is executed by calling \code{\link{runApp}}.
#' 
# @param sessionName the unique name of the session (see \code{\link{mirtCAT}} for details)
#' 
#' @param ui a shiny UI function used to define the interface. If \code{NULL}, the default one will be used. 
#'   See \code{mirtCAT:::default_UI} for the internal code
#'   
#' @param host_server logical; is \code{createShinyGUI()} being used on a remote server or executed locally?
#'   When \code{TRUE} any calls to \code{\link{stopApp}} are suppressed to allow for multiple sessions to
#'   be executed. Note that \code{FALSE} gives the same behaviour as the GUI in \code{\link{mirtCAT}}
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
#' mirtCAT_preamble(df=df)
#' runApp(createShinyGUI(host_server = FALSE), port = 8000) # run locally
#' 
#' person <- getPerson()
#' summary(person)
#' 
#' runApp(createShinyGUI(), port = 8000) # for remote server hosting
#' 
#' } 
createShinyGUI <- function(ui = NULL, host_server = TRUE){
    sessionName <- 'MASTER'
    on.exit(.MCE[[sessionName]]$preamble_defined <- .MCE[[sessionName]]$start_time <- 
                .MCE[[sessionName]]$initial_start_time <- NULL)
    if(is.null(.MCE[[sessionName]]$preamble_defined))
        stop('Please use a fresh mirtCAT_preamble() call prior to calling createShinyGUI().')
    if(is.null(ui)) ui <- default_UI
    .MCE[[sessionName]]$host_server <- host_server
    # server2 <- server
    # txt <- parse(text=sprintf("getSessionName <- function() \'%s\'", sessionName))
    # body(server2)[[2]] <- substitute(eval(txt))
    # return(shinyApp(ui=ui(sessionName), server=server2))
    return(shinyApp(ui=ui(), server=server))
}
