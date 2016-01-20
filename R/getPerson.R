#' Retrieve person object after running createShinyGUI
#' 
#' This function returns a suitable person object identical to the result returned by \code{\link{mirtCAT}},
#' and is only required when the GUI is launched by the \code{\link{createShinyGUI}} method.
#' 
#' @export getPerson
#' 
#' @author Phil Chalmers \email{rphilip.chalmers@@gmail.com}
#' 
#' @seealso \code{\link{mirtCAT}}, \code{\link{mirtCAT_preamble}}, \code{\link{createShinyGUI}}
#' 
#' @examples 
#' \dontrun{
#' 
#' mirtCAT_preamble(df = df)
#' runApp(createShinyGUI(), port = 8000)
#' 
#' person <- getPerson()
#' summary(person)
#' } 
getPerson <- function(){
    ret <- mirtCAT_post_internal(person=MCE$person, design=MCE$design)
    ret
}