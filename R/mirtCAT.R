#' Generate an adaptive or non-adaptive test HTML interface
#' 
#' Description
#' 
#' @param mirt_object single group object defined by the \code{mirt} package
#' 
#' @param questions a named list containing lists of \code{shiny} input types for each item. 
#'   Each element of the input should be a list of the form 
#'   \code{list(item = shinyInput(), answer = 'value')}. If no correct \code{answer} criteria exists
#'   (such as in rating and Likert-scales) then this input may either be set to NA or ommited.    
#'   Additinally, each \code{inputID} must be identical to the column names used to define the data 
#'   from the \code{mirt_object} input
#'   
#' @param item_answers a character vector indicating which item should be considered 'correct'
#'   when scoring individuals. Must be the length of the test, where NA's are used if the 
#'   item is not scored
#'   
#' @param stem_locations a character vector of paths pointing to .png files to be used as item
#'   stems. Must be the length of the test, where NA's are used if the item has no corresponding
#'   .png file
#'   
#' @param method argument passed to \code{mirt::fscores()} for compting new score
#' 
#' @param criteria adpative criteria used, default is the minimum posterior variance (MPV).
#' 
#' @param adaptive logical; run the test adaptively?
#'   
#' @param ... additional arguments to pass when initializing the ReferenceClass objects
mirtCAT <- function(mirt_object, questions, item_answers=NULL, stem_locations = NULL,
                    method = 'EAP', adaptive = FALSE, criteria = 'MPV', 
                    ...){
    
    itemnames <- colnames(mirt_object@Data$data)
    if(length(itemnames) != length(questions) || !all(itemnames %in% names(questions)))
        stop('Item names for mirt_object and questions do not match')
    item_options <- lapply(questions, function(x){
        if(is(x, 'shiny.tag.list')){
            if(!is.null(x[[1L]][[2L]]$children[[1]])){ #selectInput
                split <- strsplit(x[[1L]][[2L]]$children[[1]], "\"")[[1L]]
                ret <- split[seq(from = 2L, to = length(split), by = 2L)]
            } else { #textInput
                ret <- ''
            }
        } else if(is(x, 'shiny.tag')){ #radioInput
            split <- lapply(x$children[[2L]], function(x) x$children[[1L]]$attribs$value)
            ret <- do.call(c, split)
        }
        return(ret)
    })
    
    #setup objects
    shinyGUI <- ShinyGUI$new(questions=questions, stem_locations_in=stem_locations, ...)
    test <- Test$new(mirt_object=mirt_object, item_answers_in=item_answers, adaptive=adaptive,
                     item_options=item_options, method=method, criteria=criteria, ...)
    person <- Person$new(nfact=test$nfact, nitems=length(test$itemnames), ...)
    
    #put in specific enviroment
    MCE$person <- person
    MCE$test <- test
    MCE$shinyGUI <- shinyGUI
    MCE$STOP <- FALSE
    
    #run interface
    runApp(list(ui = ui(), server = server))
    return(MCE$person)
}
