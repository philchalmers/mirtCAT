#' Generate an adaptive or non-adaptive test HTML interface
#' 
#' Description
#' 
#' @param mirt_object single group object defined by the \code{mirt} package
#' @param questions a named list containing lists of \code{shiny} input types for each item. 
#'   Each element of the input should be a list of the form 
#'   \code{list(item = shinyInput(), answer = 'value')}. If no correct \code{answer} criteria exists
#'   (such as in rating and Likert-scales) then this input may either be set to NA or ommited. 
#'   
#'   Additinally, each \code{inputID} must be set to \code{'choice'}, and every item must include 
#'   the initial default as an empty character, \code{selected = ""} 
#'   (or the equivalent if the input type is different).
#' @param ... additional arguments to pass
mirtCAT <- function(mirt_object, questions, ...){
    
    itemnames <- colnames(mirt_object@Data$data)
    if(length(itemnames) != length(questions) || !all(itemnames %in% names(questions)))
        stop('Item names for mirt_object and questions do not match')
    item_answers <- as.character(
        do.call(c, lapply(questions, 
                          function(x) ifelse(is.null(x$answer), NA, x$answer))))
    item_options <- lapply(questions, function(x){
        split <- strsplit(x$item[[1L]][[2L]]$children[[1]], "\"")[[1L]]
        return(split[seq(from = 2L, to = length(split), by = 2L)])
    })
    
    #setup objects
    shinyGUI <- ShinyGUI$new(questions=questions, ...)
    test <- Test$new(mirt_object=mirt_object, item_answers=item_answers,
                     item_options=item_options, ...)
    person <- Person$new(nfact=test$nfact, nitems=length(test$itemnames), ...)
    
    #put in specific enviroment
    MCE$person <- person
    MCE$test <- test
    MCE$shinyGUI <- shinyGUI    
    
    #run interface
    runApp(list(ui = ui(), server = server))
}
