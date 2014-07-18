#' Generate an adaptive or non-adaptive test HTML interface
#' 
#' Provides tools to generate an HTML interface for creating adaptive and 
#' non-adaptive educational and psychological tests using the shiny package. Suitable for 
#' applying unidimensional and multidimensional computerized adaptive tests using item 
#' response theory methodology.
#' 
#' All tests will stop once the \code{'min_SEM'} criteria has been reached. If all questions should
#' be answered, users should specify an extremely small \code{'min_SEM'} or equivalently 
#' a large \code{'min_items'} criteria.
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
#'   when scoring individuals. Must be the length of the test, where \code{NA}s are used if the 
#'   item is not scored
#'   
#' @param stem_locations a character vector of paths pointing to .png files to be used as item
#'   stems. Must be the length of the test, where \code{NA}s are used if the item has no 
#'   corresponding .png file
#'   
#' @param method argument passed to \code{mirt::fscores()} for compting new scores. Default is 'MAP'
#' 
#' @param criteria adpative criteria used, default is the maximum information (\code{'MI'}). 
#' 
#'   Possible inputs for unidimensional tests include:  
#'   
#'   Possible inputs for multidimensional tests include: \code{'Drule'} (aliased as \code{'MI'})
#'   for the determinant of the information matrix, \code{'Trule'} for the trace of the 
#'   information matrix, and \code{'Wrule'} for the weighted information critiera 
#'   (requires specified weights).
#' 
#' @param adaptive logical; run the test adaptively? Default is \code{FALSE}
#' 
#' @param local_pattern a character vector used to run the CAT application without the GUI 
#'   interface given a specific response pattern. This option requires a complete response pattern
#'   to be supplied 
#'   
#' @param design_list a list of design based parameters for adaptive and non-adaptive tests. 
#'   These can be
#' 
#' \describe{
#'   \item{\code{min_SEM}}{Default is \code{0.3}; minimum standard error for the latent traits 
#'     (thetas) before the test is stopped. If the test is multidimensional, this will be 
#'     used along wit the \code{conjunctive} criteria}
#'   
#'   \item{\code{min_items}}{Default is \code{1}; minimum number of items that must be answered 
#'     before the test is stopped}
#'   
#'   \item{\code{max_items}}{Default is \code{1e8}; maximum number of items that 
#'     can be answered}
#'   
#'   \item{\code{quadpts}}{Default is \code{49}; number of quadrature points used per dimension 
#'     for intergration (if required).}
#'   
#'   \item{\code{theta_range}}{Default is \code{c(-6,6)}; upper and lower range for the theta 
#'     integration grid. Used in conjuncting with \code{quadpts} to generate an equally spaced 
#'     quadrature grid}
#' 
#'   \item{\code{conjunctive}}{Default is \code{TRUE}; logical value indicating whether a 
#'     conjunctive or compensatory use of \code{min_SEM} should be used (applicable to 
#'     multidimensional tests only)}
#'   
#'   \item{\code{Wrule_weights}}{Default is \code{rep(1/nfact), nfact)}, where \code{nfact} 
#'     is the number of test dimensions; weights used when \code{criteria == 'Wrule'}. The default 
#'     weights the latent dimensions equally }
#'   
#' }
#' 
#' @param shinyGUI_list a list of GUI based parameters to be over-written. These can be
#' 
#' \describe{
#'   \item{\code{title}}{A character string for the test title. Default is 
#'     \code{'Title of survery'}}
#'   
#'   \item{\code{authors}}{A character string for the author names. Default is 
#'     \code{'Author of survery'}}
#' 
#'   \item{\code{firstpage}}{The first page used in the GUI for collecting demographic information
#'     generated using tools from the shiny package. The default collects only the responsend's 
#'     name and gender using the format 
#'  
#'     \preformatted{ 
#'          list(textInput(inputId = 'name', 
#'                  label = 'What is your name?',
#'                  value = ''),
#'              selectInput(inputId = 'gender',
#'                   label = 'Please select your gender.',
#'                   choices = c('', 'Male', 'Female', 'Other'),
#'                   selected = ''))
#'         }
#'      }
#'   
#'   \item{\code{lastpage}}{Last message indicating that the test has been completed 
#'     (i.e., criteria has been met). Default is 
#'   
#'     \preformatted{ list(h5("End of survey. Click \'Next\' to save results 
#'       and close application."))}
#'    }    
#'   
#' }
#' 
#' @param person_list a list of person based parameters to be over-written. These can be
#' 
#' \describe{
#'   \item{\code{thetas.start}}{a numeric vector of starting values for the theta parameters.
#'     Default is \code{rep(0, nfact)}}
#'   
#' }
#' 
#' @export mirtCAT
#' @author Phil Chalmers \email{rphilip.chalmers@@gmail.com}
#' @seealso \code{\link{generate_pattern}}
#' 
#' @return Returns a \code{\link{ReferenceClasses}} object of class \code{'Person'} containing the
#'   following fields. 
#'   
#' \describe{
#'   \item{\code{raw_responses}}{A numeric vector indicating the raws responses to the resepective
#'     items, where NA indicates the item was not answered}
#'     
#'   \item{\code{responses}}{A numeric vector of scored responses if the \code{item_answers} input
#'     was used for each respective item}
#'   
#'   \item{\code{items_answered}}{An integer vector indicating the order in which the items were 
#'     answered}
#'   
#'   \item{\code{thetas}}{A numeric vector indicating the final theta estimates}
#'   
#'   \item{\code{thetas_history}}{A matrix indicating the progression of updating the theta values
#'     during the test}
#'     
#'   \item{\code{thetas_SE_history}}{A matrix indicating the standard errors for theta after each
#'     successive item was answered}
#'   
#'   \item{\code{thetas_acov}}{The asymtotic covarance matrix for the final theta estimates}
#' 
#'   \item{\code{demographics}}{A data.frame object containing the information collected on the 
#'     first page of the shiny GUI. This is used to store the demographic information for each
#'     participant} 
#' }
#' 
#' @keywords CAT, computerized adaptive testing
#' 
#' @examples
#' \dontrun{
#' 
#' #unidimensional scored example with generated items
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
                    design_list = list(), shinyGUI_list = list(), person_list = list()){
    
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
                     item_options=item_options, quadpts_in=design_list$quadpts,
                     theta_range_in=design_list$theta_range)
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
