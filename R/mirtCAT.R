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
#' @param method argument passed to \code{mirt::fscores()} for compting new scores. Default is 'MAP'
#' 
#' @param criteria adpative criteria used, default is the maximum information ('MI'). 
#'   Possible inputs include: ....
#' 
#' @param adaptive logical; run the test adaptively?
#' 
#' @param local_pattern a character vector used to run the CAT application without the GUI 
#'   interface given a specific response pattern 
#'   
#' @param design_list a list of design based parameters for adaptive and non-adaptive tests. These can be
#' 
#' \describe{
#'   \item{\code{\link{mirt.model}}}{   }
#'   
#' }
#' 
#' @param test_list a list of test based parameters to be over-written. These can be
#' 
#' \describe{
#'   \item{\code{\link{mirt.model}}}{   }
#'   
#' }
#' 
#' @param shinyGUI_list a list of GUI based parameters to be over-written. These can be
#' 
#' \describe{
#'   \item{\code{\link{mirt.model}}}{   }
#'   
#' }
#' 
#' @param person_list a list of person based parameters to be over-written. These can be
#' 
#' \describe{
#'   \item{\code{\link{mirt.model}}}{   }
#'   
#' }
#'
#'   
#' @param ... additional arguments to pass when initializing the ReferenceClass objects
#' 
#' @export mirtCAT
#' @author Phil Chalmers \email{rphilip.chalmers@@gmail.com}
#' @seealso \code{\link{generate_pattern}}
#' 
#' @keywords CAT, computerized adaptive testing
#' 
#' @examples
#' \dontrun{
#' 
# scored simulated example
#' 
#' #model
#' set.seed(1234)
#' nitems <- 50
#' itemnames <- paste0('Item.', 1:nitems)
#' a <- matrix(rlnorm(nitems, .2, .3))
#' d <- matrix(rnorm(nitems))
#' dat <- simdata(a, d, 1000, itemtype = 'dich')
#' colnames(dat) <- itemnames
#' mod <- mirt(dat, 1)
#' 
#' #simple math items
#' shiny_questions <- questions <- vector('list', nitems)
#' names(shiny_questions) <- names(questions) <- itemnames
#' answers <- character(nitems)
#' choices <- vector('list', nitems)
#' spacing <- floor(d - min(d)) + 1 #easier items have more variation in the options
#' 
#' for(i in 1:nitems){
#'     n1 <- sample(1:50, 1)
#'     n2 <- sample(51:100, 1)
#'     ans <- n1 + n2
#'     questions[[i]] <- paste0(n1, ' + ', n2, ' = ?')
#'     answers[i] <- as.character(ans)
#'     ch <- ans + sample(c(-5:-1, 1:5) * spacing[i,], 5)
#'     ch[sample(1:5, 1)] <- ans
#'     choices[[i]] <- as.character(ch)
#' }
#' 
#' for(i in 1L:nitems){
#'     shiny_questions[[i]] <- radioButtons(inputId = itemnames[i],
#'                                          label = questions[[i]],
#'                                          choices = choices[[i]])
#' }
#' 
#' mirtCAT(mod, shiny_questions, item_answers=answers) #sequential
#' mirtCAT(mod, shiny_questions, item_answers=answers, adaptive=TRUE) #adaptive
#' 
#' #run locally, random response pattern given Theta
#' set.seed(1)
#' pat <- generate_pattern(mod, Theta = 0, choices = choices, item_answers=answers)
#' mirtCAT(mod, shiny_questions, item_answers=answers, local_pattern=pat)
#' mirtCAT(mod, shiny_questions, item_answers=answers, adaptive = TRUE, local_pattern=pat)
#' }
mirtCAT <- function(mirt_object, questions, item_answers=NULL, stem_locations = NULL,
                    method = 'MAP', adaptive = FALSE, criteria = 'MI', local_pattern = character(0),
                    design_list = list(), test_list = list(), shinyGUI_list = list(), 
                    person_list = list()){
    
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
    shinyGUI <- ShinyGUI$new(questions=questions, stem_locations_in=stem_locations, 
                             shinyGUI_list=shinyGUI_list)
    test <- Test$new(mirt_object=mirt_object, item_answers_in=item_answers, 
                     item_options=item_options, test_list=test_list)
    design <- Design$new(method=method, criteria=criteria, adaptive=adaptive, 
                         nfact=test$nfact, design_list=design_list)
    person <- Person$new(nfact=test$nfact, nitems=length(test$itemnames), 
                         person_list=person_list)
    
    #put in specific enviroment
    MCE$person <- person
    MCE$test <- test
    MCE$design <- design
    MCE$shinyGUI <- shinyGUI
    MCE$STOP <- FALSE
    
    if(length(local_pattern)){
        return(run_local(local_pattern))
    } else {
        #run interface
        runApp(list(ui = ui(), server = server))
        return(MCE$person)
    }
}
