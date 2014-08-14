#' Generate an adaptive or non-adaptive test HTML interface
#' 
#' Provides tools to generate an HTML interface for creating adaptive and 
#' non-adaptive educational and psychological tests using the shiny package. Suitable for 
#' applying unidimensional and multidimensional computerized adaptive tests using item 
#' response theory methodology. Test scoring is performed using the \code{mirt} package.
#' However, if no scoring is required (i.e., a standard survey) then defining a \code{mirt} 
#' object may be omitted.
#' 
#' All tests will stop once the \code{'min_SEM'} criteria has been reached. If all questions should
#' be answered, users should specify an extremely small \code{'min_SEM'} or equivalently 
#' a large \code{'min_items'} criteria.
#' 
#' @param questions a named list containing lists of \code{shiny} input types for each item. 
#'   Each element of the input should be a list of the form 
#'   \code{list(item1 = shinyInput(), item2 = shinyInput(), ...)}. 
#'   Each \code{inputID} must be identical to the column names used to define the data 
#'   from the \code{mirt_object} input
#'   
#' @param mirt_object single group object defined by the \code{mirt} package. This is required
#'   if the test is to be scored adaptively
#'   
#' @param method argument passed to \code{mirt::fscores()} for computing new scores. Default is 'MAP'
#' 
#' @param criteria adaptive criteria used, default is to administer each item sequentially 
#'   (i.e., \code{criteria = 'seq'}). 
#' 
#'   Possible inputs for unidimensional adaptive tests include: \code{'MI'} for the maximum
#'   information, \code{'MEPV'} for minimum expected posterior variance, 
#'   \code{'MLWI'} for maximum likelihood weighted information, 
#'   \code{'MPWI'} for maximum posterior weighted information, \code{'MEI'} for 
#'   maximum expected information, and \code{'IKLP'} as well as \code{'IKL'} for the 
#'   Integration based Kullback-Leibler criteria with and without the prior density weight,
#'   respectively, and their root-nitem weighted counter-parts \code{'IKLn'} and 
#'   \code{'IKLPn'}.
#'   
#'   Possible inputs for multidimensional adaptive tests include: \code{'Drule'} 
#'   for the maximum determinant of the information matrix, \code{'Trule'} for the 
#'   maximum (potentially weighted) trace of the information matrix, \code{'Erule'} for the 
#'   minimum value of the information matrix, and \code{'Wrule'} for 
#'   the weighted information criteria. For each of these rules the posterior weight for 
#'   the latent trait scores can also be included with the \code{'DPrule'}, \code{'TPrule'},
#'   \code{'EPrule'}, \code{'WPrule'}, respectively. As a safety precaution, if the 
#'   selected criteria do not weight by the posterior (and therefore do not exist for 
#'   extreme response styles) the method is temporarily switched to the posterior weighting
#'   until a variable response pattern is observed and more than 5 items have been administered.
#'   
#'   Applicable to both unidimensional and multidimensional tests are the
#'   \code{'KL'} and \code{'KLn'} for point-wise Kullback-Leibler divergence and 
#'   point-wise Kullback-Leibler with a decreasing delta value (\code{delta*sqrt(n)}, 
#'   where \code{n} is the number of items previous answered), respectively. 
#'   The \code{delta} criteria is defined in the \code{design} object
#'   
#'   Non-adaptive methods which are applicable even when no \code{mirt_object} is passed 
#'   include \code{'random'} to randomly select items and \code{'seq'} for selecting 
#'   items sequentially.
#'   
#' @param item_answers a character vector indicating which item should be considered 'correct'
#'   when scoring individuals. Must be the length of the test, where \code{NA}s are used if the 
#'   item is not scored
#'   
#' @param start_item a single number indicating which item should be used as the start item.
#'   Default is 1
#'   
#' @param exposure a numeric vector specifying the amount of exposure control to apply for
#'   each successive item. The default accepts the item which demonstrates the maximum CAT 
#'   critiera, however if the item exposure is greater than 1, and \code{exposure[item] == n}, 
#'   then the \code{n} most optimal criteria will be randomly sampled from. For instance, if 
#'   \code{exposure[item] == 3}, and \code{critiera = 'MI'}, then the 3 items demonstrating 
#'   the largest information criteria will be sampled from. Naturally, the first and last 
#'   elements are ignored for the first and last items, respectively 
#' 
#' @param local_pattern a character or numeric vector used to run the CAT application without 
#'   the GUI interface given a specific response pattern. This option requires a complete response 
#'   pattern to be supplied. This input is required to be numeric if no \code{questions} list is
#'   input
#'   
#' @param design a list of design based parameters for adaptive and non-adaptive tests. 
#'   These can be
#' 
#' \describe{
#'   \item{\code{min_SEM}}{Default is \code{0.3}; minimum standard error for the latent traits 
#'     (thetas) before the test is stopped. If the test is multidimensional either a single 
#'     value or a vector may be supplied to provide an overall minimum criteria or a SEM 
#'     value for each dimension, respectively}
#'     
#'   \item{\code{thetas.start}}{a numeric vector of starting values for the theta parameters.
#'     Default is \code{rep(0, nfact)}}
#'   
#'   \item{\code{min_items}}{Default is \code{1}; minimum number of items that must be answered 
#'     before the test is stopped}
#'   
#'   \item{\code{max_items}}{Default is the length of the item bank; maximum number of items that 
#'     can be answered}
#'   
#'   \item{\code{quadpts}}{Number of quadrature points used per dimension 
#'     for integration (if required). Default is 61}
#'   
#'   \item{\code{theta_range}}{Default is \code{c(-6,6)}; upper and lower range for the theta 
#'     integration grid. Used in conjunction with \code{quadpts} to generate an equally spaced 
#'     quadrature grid}
#' 
#'   \item{\code{Wrule_weights}}{Default is \code{rep(1/nfact), nfact)}, where \code{nfact} 
#'     is the number of test dimensions; weights used when \code{criteria == 'Wrule'}. The default 
#'     weights the latent dimensions equally }
#'     
#'   \item{\code{KL_delta}}{Default is \code{0.1}; interval range used when \code{criteria = 'KL'}
#'     or \code{criteria = 'KLn'}}
#'     
#'   \item{\code{max_time}}{Default is \code{Inf}; maximum time allowed for the generated GUI, measured
#'     in seconds. For instance, if the test should stop after 10 minutes then the number 
#'     600 should be passed (10 * 60)}
#'   
#' }
#' 
#' @param shinyGUI a list of GUI based parameters to be over-written. These can be
#' 
#' \describe{
#'   \item{\code{title}}{A character string for the test title. Default is 
#'     \code{'mirtCAT'}}
#'   
#'   \item{\code{authors}}{A character string for the author names. Default is 
#'     \code{'Author of survey'}}
#'
#'   \item{\code{firstpage}}{The first page of the shiny GUI. Default prints the title
#'     and information message
#'     
#'     \preformatted{ 
#'          list(h1('Welcome to the mirtCAT interface'),
#'               The following interface was created using the mirtCAT package. 
#'               To cite the package use citation(\\'mirtCATd\\') in R.')
#'          }
#'       }
#' 
#'   \item{\code{demographics}}{The person information page used in the GUI for collecting 
#'     demographic information generated using tools from the shiny package. The default 
#'     collects only the respondent gender using the format 
#'  
#'     \preformatted{ 
#'          list(selectInput(inputId = 'gender',
#'                   label = 'Please select your gender.',
#'                   choices = c('', 'Male', 'Female', 'Other'),
#'                   selected = ''))
#'         }
#'      }
#'      
#'   \item{\code{demographics_inputIDs}}{a character vector required if a custom demographics
#'     input is used. Default is \code{demographics_inputIDs = 'gender'}, corresponding to
#'     the \code{demographics} default}
#'     
#'   \item{\code{stem_locations}}{a character vector of paths pointing to .png or .jpeg 
#'     files to be used as item stems. Must be the length of the test, where \code{NA}s are 
#'     used if the item has no corresponding file}
#'     
#'   \item{\code{lastpage}}{Last message indicating that the test has been completed 
#'     (i.e., criteria has been met). Default is 
#'   
#'     \preformatted{list(h5("End of survey. Click \'Next\' to save results 
#'       and close application."))}
#'    }    
#'   
#' }
#' 
#' @param preCAT a list object which can be used. This 
#'   specifies a pre-CAT block in which different test properties may be applied. If the
#'   list is empty no preCAT block will be used. All of the following elements are required.
#'   
#'   \describe{
#'     \item{\code{nitems}}{number of items to administer before the CAT session begins.
#'       An input greater than 0 is required}
#'     
#'     \item{\code{criteria}}{selection criteria (see above). Default is 'random'}
#'     
#'     \item{\code{method}}{selection criteria (see above). It is generally recommended to 
#'       select a method which can deal with all-or-none response patterns, such as 'EAP'
#'       or 'MAP'. Default is 'MAP'}
#'    }
#' 
#' @export mirtCAT
#' @author Phil Chalmers \email{rphilip.chalmers@@gmail.com}
#' @seealso \code{\link{generate_pattern}}
#' 
#' @return Returns a \code{\link{ReferenceClasses}} object of class \code{'Person'} containing the
#'   following fields. 
#'   
#' \describe{
#'   \item{\code{raw_responses}}{A numeric vector indicating the raws responses to the respective
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
#'   \item{\code{item_time}}{A numeric vector indicating how long the respondent took to answer
#'     each question (in seconds)}
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
#' mirtCAT(shiny_questions) #collect response only (no scoring or estimating thetas)
#' mirtCAT(shiny_questions, mod, item_answers=answers) #sequential scoring 
#' mirtCAT(shiny_questions, mod, item_answers=answers, criteria = 'random') #random
#' mirtCAT(shiny_questions, mod, item_answers=answers, criteria = 'MI') #adaptive
#' 
#' #run locally, random response pattern given Theta
#' set.seed(1)
#' pat <- generate_pattern(mod, Theta = 0, choices = choices, item_answers=answers)
#' head(pat)
#' mirtCAT(shiny_questions, mod, item_answers=answers, local_pattern=pat)
#' 
#' #same as above, but using special input vector that doesn't require shiny
#' set.seed(1)
#' pat2 <- generate_pattern(mod, Theta = 0)
#' head(pat2)
#' mirtCAT(mirt_object=mod, local_pattern=pat2)
#' 
#' #run CAT, and save results to object called person
#' person <- mirtCAT(shiny_questions, mod, item_answers=answers, criteria = 'MI', 
#'   local_pattern=pat)
#' print(person)
#' summary(person)
#' 
#' #plot the session
#' plot(person) #standard errors
#' plot(person, SE=1.96) #95 percent confidence intervals
#' }
mirtCAT <- function(questions = NULL, mirt_object = NULL, method = 'MAP', criteria = 'seq', 
                    item_answers = NULL, start_item = 1, 
                    exposure = rep(1, length(questions)), local_pattern = NULL,
                    design = list(), shinyGUI = list(), preCAT = list())
{    
    if(is.null(questions)){
        questions <- vector('list', ncol(mirt_object@Data$data))
        Names <- colnames(mirt_object@Data$data)
        names(questions) <- Names
        K <- mirt_object@Data$K
        for(i in 1L:length(K))
            questions[[i]] <- selectInput(inputId = Names[i], label = '', 
                                          choices = as.character(0L:(K[i]-1L)))
        item_answers <- NULL
    }
        
    if(is.null(names(questions)))
        stop('questions list must have names')
    if(is.null(mirt_object)){
        dat <- matrix(c(0,1), 2L, length(questions))
        colnames(dat) <- names(questions)
        mirt_object <- mirt(dat, 1L, TOL=NaN)
        score <- FALSE
        if(!(criteria %in% c('seq', 'random')))
            stop('Only random and seq criteria are available if no mirt_object was defined')
    } else score <- TRUE
    itemnames <- colnames(mirt_object@Data$data)
    if(length(itemnames) != length(questions) || !all(itemnames %in% names(questions)))
        stop('Item names for mirt_object and questions do not match')
    item_options <- lapply(questions, extract_choices)
    
    #setup objects
    shinyGUI_object <- ShinyGUI$new(questions=questions, shinyGUI=shinyGUI)
    test_object <- Test$new(mirt_object=mirt_object, item_answers_in=item_answers, 
                     item_options=item_options, quadpts_in=design$quadpts,
                     theta_range_in=design$theta_range)
    design_object <- Design$new(method=method, criteria=criteria, start_item=start_item,
                         nfact=test_object$nfact, design=design, exposure=exposure,
                         preCAT=preCAT, nitems=test_object$length)
    person_object <- Person$new(nfact=test_object$nfact, nitems=length(test_object$itemnames), 
                         thetas.start_in=design$thetas.start, score=score)
        
    #put in specific enviroment
    MCE$person <- person_object
    MCE$test <- test_object
    MCE$design <- design_object
    MCE$shinyGUI <- shinyGUI_object
    MCE$STOP <- FALSE
    MCE$outfile <- tempfile(fileext='.png')
    
    if(length(local_pattern)){
        person <- run_local(as.character(local_pattern))
        person$item_time <- numeric(0)
    } else {
        #run interface
        runApp(list(ui = ui(), server = server), launch.browser=TRUE)
        person <- MCE$person
    }
    person$items_answered <- person$items_answered[!is.na(person$items_answered)]
    ret <- list(raw_responses=person$raw_responses, 
                responses=person$responses,
                items_answered=person$items_answered,
                thetas=person$thetas,
                thetas_history=person$thetas_history,
                thetas_SE_history=person$thetas_SE_history,
                item_time=person$item_time,
                demographics=person$demographics)
    colnames(ret$thetas) <- colnames(ret$thetas_history) <-
        colnames(ret$thetas_SE_history) <- paste0('Theta_', 1L:MCE$test$nfact)
    if(!person$score)
        ret$thetas <- ret$thetas_history <- ret$thetas_SE_history <- NA
    MCE$person <- MCE$test <- MCE$design <- MCE$shinyGUI <- MCE$start_time <- 
        MCE$STOP <- MCE$outfile <- NULL
    class(ret) <- 'mirtCAT'
    ret
}