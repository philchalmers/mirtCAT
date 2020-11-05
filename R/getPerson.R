#' Retrieve person object after running createShinyGUI
#' 
#' This function returns a suitable person object identical to the result returned by \code{\link{mirtCAT}},
#' and is only required when the GUI is launched by the \code{\link{createShinyGUI}} method.
#' 
# @param sessionName the unique name of the session (see \code{\link{mirtCAT}} for details)
#' 
#' @export getPerson
#' 
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
#' 
#' @seealso \code{\link{mirtCAT}}, \code{\link{mirtCAT_preamble}}, \code{\link{createShinyGUI}}
#' 
#' @examples 
#' \dontrun{
#' 
#' mirtCAT_preamble(df=df)
#' runApp(createShinyGUI(), port = 8000)
#' 
#' person <- getPerson()
#' summary(person)
#' } 
getPerson <- function(){
    sessionName <- .MCE$currentSessionName
    ret <- mirtCAT_post_internal(person=.MCE[[sessionName]]$person, 
                                 design=.MCE[[sessionName]]$design,
                                 has_answers=.MCE[[sessionName]]$test@has_answers, 
                                 GUI=TRUE)
    ret
}