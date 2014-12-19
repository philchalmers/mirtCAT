#' Generate an adaptive or non-adaptive test HTML interface
#' 
#' Provides tools to generate an HTML interface for creating adaptive and 
#' non-adaptive educational and psychological tests using the \code{shiny} package. 
#' Suitable for applying unidimensional and multidimensional computerized adaptive tests 
#' using item response theory methodology. Test scoring is performed using the \code{mirt} package.
#' However, if no scoring is required (i.e., a standard survey) then defining a \code{mirt} 
#' object may be omitted.
#' 
#' All tests will stop once the \code{'min_SEM'} criteria has been reached or classification
#' above or below a cutoff can be made. If all questions should
#' be answered, users should specify an extremely small \code{'min_SEM'} or equivalently 
#' a large \code{'min_items'} criteria.
#' 
#' @section HTML help files, exercises, and examples:
#' 
#' To access examples, vignettes, and exercise files that have been generated with knitr please
#' visit \url{http://philchalmers.github.io/mirtCAT/mirtCAT-vignettes.html}.
#' 
#' @param df a \code{data.frame} object containing the character vector inputs required to generate 
#'   GUI questions through shiny. Each row in the object corresponds to a unique
#'   item. The object supports the follow column name combinations as inputs to specify the 
#'   type of response format, questions, options, answers, and stems:
#'   
#'   \describe{
#'   
#'   \item{\code{Type}}{Indicates the type of response input 
#'     to use from the shiny package. The supported types are: 'radio' for radio buttons,
#'     'radio_inline' for radio buttons that are organized horizontally,
#'     'select' for a pull-down box for selecting inputs, or 'text' for requiring 
#'     typed user input.} 
#'     
#'     \item{\code{Question}}{A character vector containing all the questions
#'       or stems to be generated.} 
#'       
#'     \item{\code{Option.#}}{Column names pertaining to the possible response
#'       options for each item, where the # corresponds to the specific category. For
#'       instance, a test with 4 unique response options for each item would contain
#'       the columns (\code{Option.1}, \code{Option.2}, \code{Option.3}, \code{Option.4}).
#'       If, however, some items have less categories then \code{NA}'s can be used for response
#'       options that do not apply.}
#'       
#'     \item{\code{Answer} or \code{Answer.#}}{(Optional) A character vector (or multiple character)
#'       vectors indicating a scoring key for items that have correct answer(s). If there
#'       is no correct answer for a question then a value of \code{NA} must be declared.}
#'       
#'     \item{\code{Stem}}{(Optional) a character vector of paths pointing to .png, .jpeg, or .gif
#'     files to be used as item stems. \code{NA}s are used if the item has no corresponding file.} 
#'       
#'   }
#'   
#' @param mirt_object single group object defined by the \code{mirt} package. This is required
#'   if the test is to be scored adaptively or non-adaptively, but not required for general 
#'   questionnaires. The object can be constructed by using the 
#'   \code{\link{generate.mirt_object}} function if population parameters are known or by
#'   including a calibrated model estimated from the \code{\link{mirt}} function with real data.
#'   
#' @param method argument passed to \code{mirt::fscores()} for computing new scores in the CAT 
#'   stage, with the addition of a \code{'fixed'} input to keep the latent trait estimates
#'   fixed at the previous values. Default is 'MAP'
#' 
#' @param criteria adaptive criteria used, default is to administer each item sequentially 
#'   using \code{criteria = 'seq'}. 
#' 
#'   Possible inputs for unidimensional adaptive tests include: \code{'MI'} for the maximum
#'   information, \code{'MEPV'} for minimum expected posterior variance, 
#'   \code{'MLWI'} for maximum likelihood weighted information, 
#'   \code{'MPWI'} for maximum posterior weighted information, \code{'MEI'} for 
#'   maximum expected information, and \code{'IKLP'} as well as \code{'IKL'} for the 
#'   integration based Kullback-Leibler criteria with and without the prior density weight,
#'   respectively, and their root-nitems administered weighted counter-parts, \code{'IKLn'} and 
#'   \code{'IKLPn'}.
#'   
#'   Possible inputs for multidimensional adaptive tests include: \code{'Drule'} 
#'   for the maximum determinant of the information matrix, \code{'Trule'} for the 
#'   maximum (potentially weighted) trace of the information matrix, 
#'   \code{'Arule'} for the minimum (potentially weighted) trace of the asymptotic covariance matrix,
#'   \code{'Erule'} for the  minimum value of the information matrix, and \code{'Wrule'} for 
#'   the weighted information criteria. For each of these rules, the posterior weight for 
#'   the latent trait scores can also be included with the \code{'DPrule'}, \code{'TPrule'},
#'   \code{'APrule'}, \code{'EPrule'}, and \code{'WPrule'}, respectively. 
#'   As a safety precaution, if the 
#'   selected criteria do not weight by the posterior (and therefore do not exist for 
#'   extreme response styles) and less than 5 items have been administered then 
#'   the method is temporarily switched to the posterior weighting
#'   until a variable response pattern is observed.
#'   
#'   Applicable to both unidimensional and multidimensional tests are the
#'   \code{'KL'} and \code{'KLn'} for point-wise Kullback-Leibler divergence and 
#'   point-wise Kullback-Leibler with a decreasing delta value (\code{delta*sqrt(n)}, 
#'   where \code{n} is the number of items previous answered), respectively. 
#'   The \code{delta} criteria is defined in the \code{design} object
#'   
#'   Non-adaptive methods applicable even when no \code{mirt_object} is passed 
#'   are: \code{'random'} to randomly select items, and \code{'seq'} for selecting 
#'   items sequentially.
#'   
#' @param start_item a single number indicating which item should be used as the start item.
#'   Default is 1. If \code{NA} is supplied a random starting value will be chosen
#'   
#' @param exposure a numeric vector specifying the amount of exposure control to apply for
#'   each successive item. The default vector of 1's selects items which demonstrate 
#'   the maximum CAT critiera (i.e., no exposure control), however if the item exposure 
#'   is greater than 1, and \code{exposure[item] == n}, then the \code{n} most optimal
#'   criteria will be randomly sampled from. For instance, if 
#'   \code{exposure[5] == 3}, and \code{critiera = 'MI'}, then when the fifth item is to be 
#'   selected from the remaining pool of items the top 3 candidate items demonstrating 
#'   the largest information criteria will be sampled from. Naturally, the first and last 
#'   elements of \code{exposure} are ignored since exposure control will be meaningless
#' 
#' @param local_pattern a character/numeric vector or matrix of response patterns 
#'   used to run the CAT application without generating the GUI interface. 
#'   This option requires complete response pattern(s) to be supplied. \code{local_pattern} 
#'   is required to be numeric if no \code{questions} are supplied, otherwise it must contain 
#'   character values of plausible responses
#'   
#' @param cl an object definition to be passed to the parallel package 
#'   (see \code{?parallel::parLapply} for details). If defined, and if 
#'   \code{nrow(local_pattern) > 1}, then each row will be run in parallel to help 
#'   decrease estimation times in simulation work
#'   
#' @param design_elements logical; return an object containing the test, person, and design 
#'   elements? Primarily this is to be used with the \code{\link{findNextItem}} function
#'   
#' @param design a list of design based parameters for adaptive and non-adaptive tests. 
#'   These can be
#' 
#' \describe{
#'   \item{\code{min_SEM}}{Default is \code{0.3}; minimum standard error or measurement
#'     to be reached for the latent traits (thetas) before the test is stopped. If the test is
#'     multidimensional, either a single value or a vector of values may be supplied to provide
#'     SEM criteria values for each dimension}
#'     
#'   \item{\code{thetas.start}}{a numeric vector of starting values for the theta parameters.
#'     Default is \code{rep(0, nfact)}}
#'   
#'   \item{\code{min_items}}{minimum number of items that must be answered 
#'     before the test is stopped. Default is \code{1}}
#'   
#'   \item{\code{max_items}}{maximum number of items that 
#'     can be answered. Default is the length of the item bank}
#'   
#'   \item{\code{quadpts}}{Number of quadrature points used per dimension 
#'     for integration (if required). Default is identical to scheme in \code{\link{fscores}}}
#'   
#'   \item{\code{theta_range}}{upper and lower range for the theta 
#'     integration grid. Used in conjunction with \code{quadpts} to generate an equally spaced 
#'     quadrature grid. Default is \code{c(-6,6)}}
#' 
#'   \item{\code{Wrule_weights}}{weights used when \code{criteria == 'Wrule'}. The default 
#'     weights the latent dimensions equally. Default is \code{rep(1/nfact), nfact)}, 
#'     where \code{nfact} is the number of test dimensions;  }
#'     
#'   \item{\code{KL_delta}}{interval range used when \code{criteria = 'KL'}
#'     or \code{criteria = 'KLn'}. Default is \code{0.1}}
#'     
#'   \item{\code{max_time}}{maximum time allowed for the generated GUI, measured
#'     in seconds. For instance, if the test should stop after 10 minutes then the number 
#'     600 should be passed (10 * 60). Default is \code{Inf}, therefore no time limit}
#'     
#'   \item{\code{content}}{an optional character vector indicating the type of content measured
#'     by an item. Must be supplied in conjunction with \code{content_prop}}
#'     
#'   \item{\code{content_prop}}{an optional named numeric numeric vector indicating the 
#'     distribution of item content proportions. A \code{content} vector must also be supplied
#'     to indicate the item content membership. For instance, if \code{content} contains three
#'     possible item content domains 'Addition', 'Subtraction', and 'Multiplication', and the 
#'     test should contain approximately half multiplication and a quarter of both 
#'     addition and subtraction, then a suitable input would be 
#'     
#'     \code{content_prop = c('Addition'=0.25, 'Subtraction'=0.25, 'Multiplication'=.5)}
#'     
#'     Note that \code{content_prop} must sum to 1 in order to represent valid population 
#'     proportions.
#'     }
#'     
#'   \item{\code{classify}}{a numeric vector indicating cut-off values for classification
#'     above or below some prior threshold. Default does not use the classification scheme}
#'   
#'   \item{\code{classify_CI}}{a numeric vector indicating the confident intervals used to 
#'     classify individuals being above or below values in \code{classify}. Values must 
#'     be between 0 and 1 (e.g., 0.95 gives 95\% confidence interval)}
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
#'     and information message. 
#'     
#'     \preformatted{ 
#'          list(h1('Welcome to the mirtCAT interface'),
#'               The following interface was created using the mirtCAT package. 
#'               To cite the package use citation(\\'mirtCATd\\') in R.')
#'          }
#'       }
#'       
#'    If an empty list is passed, this page will be skipped.
#' 
#'   \item{\code{demographics}}{A person information page used in the GUI for collecting 
#'     demographic information, generated using tools from the shiny package. For example,
#'     the following code asks the participants about their Gender: 
#'  
#'     \preformatted{ 
#'          list(selectInput(inputId = 'gender',
#'                   label = 'Please select your gender.',
#'                   choices = c('', 'Male', 'Female', 'Other'),
#'                   selected = ''))
#'         }
#'         
#'      By default, the demographics page is not included. 
#'         
#'      }
#'      
#'   \item{\code{demographics_inputIDs}}{a character vector required if a custom demographics
#'     input is used. Default is \code{demographics_inputIDs = 'gender'}, corresponding to
#'     the \code{demographics} default}
#'     
#'   \item{\code{temp_file}}{a character vector indicating where a temporary .rds file 
#'     containing the response information should be saved while the GUI is running. 
#'     The object will be saved after each item is successfully completed. This is used to 
#'     save response information to the hard drive in case there are power outages or 
#'     unexpected computer restarts.      
#'     
#'     If \code{NULL}, no temp file will be created. Upon completion of the test, the 
#'     temp file will be deleted}
#'     
#'   \item{\code{resume_file}}{a character vector indicating where a temporary .rds file 
#'     containing the response information was saved (see \code{temp_file}). Allows the GUI
#'     session to be continued using the previously stored demographic and response pattern 
#'     information. Note that the demographics GUI page will appear again, but this information
#'     will not be used and can be skipped.}
#'     
#'   \item{\code{lastpage}}{Last message indicating that the test has been completed 
#'     (i.e., criteria has been met). Default is 
#'   
#'     \preformatted{list(h5("End of survey. Click \'Next\' to save results 
#'       and close application."))}
#'    }    
#'    
#'    \item{\code{css}}{a character string defining CSS elements to modify the GUI presentation 
#'      elements. The input string is passed to the argument \code{tags$style(HTML(shinyGUI$css))}
#'      prior to constructing the user interface
#'    
#'    }
#'   
#' }
#' 
#' @param preCAT a list object which can be used to specify a pre-CAT block in which 
#'   different test properties may be applied prior to beginning the CAT session. If the
#'   list is empty, no preCAT block will be used. All of the following elements are required 
#'   to use the \code{preCAT} input:
#'   
#'   \describe{
#'     \item{\code{nitems}}{number of items to administer before the CAT session begins.
#'       An input greater than 0 is required}
#'     
#'     \item{\code{criteria}}{selection criteria (see above). Default is 'random'}
#'     
#'     \item{\code{method}}{estimation criteria (see above). It is generally recommended to 
#'       select a method which can deal with all-or-none response patterns, such as 'EAP'
#'       or 'MAP', or in the multidimensional case 'DPrule' or 'TPrule'. Default is 'MAP'}
#'    }
#' 
#' @export mirtCAT
#' @author Phil Chalmers \email{rphilip.chalmers@@gmail.com}
#' @seealso \code{\link{generate_pattern}}, \code{\link{generate.mirt_object}}
#' 
#' @return Returns a list object of class \code{'Person'} containing the following elements:
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
#' @keywords CAT, MCAT, computerized adaptive testing
#' 
#' @examples
#' \dontrun{
#' 
#' ### unidimensional scored example with generated items
#' 
#' # create mirt_object from estimated parameters
#' set.seed(1234)
#' nitems <- 50
#' itemnames <- paste0('Item.', 1:nitems)
#' a <- matrix(rlnorm(nitems, .2, .3))
#' d <- matrix(rnorm(nitems))
#' dat <- simdata(a, d, 1000, itemtype = 'dich')
#' mod <- mirt(dat, 1)
#' coef(mod2, simplify=TRUE)
#' 
#' # alternatively, define mirt_object from population values (not run)
#' pars <- data.frame(a1=a, d=d)
#' mod2 <- generate.mirt_object(pars, itemtype='2PL')
#' coef(mod2, simplify=TRUE)
#' 
#' # simple math items
#' questions <- answers <- character(nitems)
#' choices <- matrix(NA, nitems, 5)
#' spacing <- floor(d - min(d)) + 1 #easier items have more variation in the options
#' 
#' for(i in 1:nitems){
#'     n1 <- sample(1:50, 1)
#'     n2 <- sample(51:100, 1)
#'     ans <- n1 + n2
#'     questions[i] <- paste0(n1, ' + ', n2, ' = ?')
#'     answers[i] <- as.character(ans)
#'     ch <- ans + sample(c(-5:-1, 1:5) * spacing[i,], 5)
#'     ch[sample(1:5, 1)] <- ans
#'     choices[i, ] <- as.character(ch)
#' }
#' 
#' df <- data.frame(Question=questions, Option=choices, 
#'                               Type = 'radio', stringsAsFactors = FALSE)
#' head(df)
#' 
#' (res <- mirtCAT(df)) #collect response only (no scoring or estimating thetas)
#' summary(res)
#' 
#' # include scoring by providing Answer key
#' df$Answer <- answers
#' (res_seq <- mirtCAT(df, mod)) #sequential scoring 
#' (res_random <- mirtCAT(df, mod, criteria = 'random')) #random
#' (res_MI <- mirtCAT(df, mod, criteria = 'MI')) #adaptive
#' 
#' summary(res_seq)
#' summary(res_random)
#' summary(res_MI)
#' 
#' #-----------------------------------------
#' 
#' # run locally, random response pattern given Theta
#' set.seed(1)
#' pat <- generate_pattern(mod, Theta = 0, df=df)
#' head(pat)
#' res <- mirtCAT(df, mod, local_pattern=pat) #seq scoring with character pattern
#' summary(res)
#' 
#' # same as above, but using special input vector that doesn't require df input
#' set.seed(1)
#' pat2 <- generate_pattern(mod, Theta = 0)
#' head(pat2)
#' print(mirtCAT(mirt_object=mod, local_pattern=pat2))
#' 
#' # run CAT, and save results to object called person
#' person <- mirtCAT(df, mod, item_answers=answers, criteria = 'MI', 
#'   local_pattern=pat)
#' print(person)
#' summary(person)
#' 
#' # plot the session
#' plot(person) #standard errors
#' plot(person, SE=1.96) #95 percent confidence intervals
#' 
#' #-----------------------------------------
#'
#' ### save response object to temp directory in case session ends early
#' wdf <- paste0(getwd(), '/temp_file.rds')
#' res <- mirtCAT(df, mod, shinyGUI=list(temp_file=wdf))
#' 
#' # resume test this way if test was stopped early (and temp files were saved)
#' res <- mirtCAT(df, mod, shinyGUI=list(resume_file=wdf))
#' print(res)
#' 
#' }
mirtCAT <- function(df = NULL, mirt_object = NULL, method = 'MAP', criteria = 'seq', 
                    start_item = 1, exposure = rep(1, length(questions)), 
                    local_pattern = NULL, design_elements=FALSE, cl=NULL,
                    design = list(), shinyGUI = list(), preCAT = list(), ...)
{    
    on.exit({MCE$person <- MCE$test <- MCE$design <- MCE$shinyGUI <- MCE$start_time <- 
                MCE$STOP <- MCE$outfile <- MCE$last_demographics <- NULL})
    Names <- if(!is.null(mirt_object)) colnames(mirt_object@Data$data) else NULL
    if(is.null(df)){
        if(is.null(mirt_object)) stop('No df or mirt_object supplied')
        questions <- vector('list', ncol(mirt_object@Data$data))
        names(questions) <- Names
        K <- mirt_object@Data$K
        item_options <- vector('list', length(K))
        for(i in 1L:length(K)){
            item_options[[i]] <- as.character(0L:(K[i]-1L))
            questions[[i]] <- selectInput(inputId = Names[i], label = '', 
                                          choices = item_options[[i]])
        }
        item_answers <- NULL
    } else {
        obj <- buildShinyElements(df, itemnames = Names)
        questions <- obj$questions
        item_answers <- obj$item_answers
        item_options <- obj$item_options
        shinyGUI$stem_locations <- df$Stem
    }
    if(is.null(mirt_object)){
        dat <- matrix(c(0,1), 2L, length(questions))
        colnames(dat) <- names(questions)
        mirt_object <- mirt(dat, 1L, TOL=NaN)
        score <- FALSE
        if(!(criteria %in% c('seq', 'random')))
            stop('Only random and seq criteria are available if no mirt_object was defined')
        mirt_mins <- rep(0L, ncol(dat))
    } else {
        score <- TRUE
        mirt_mins <- mirt_object@Data$mins
    }
    if(!is.null(local_pattern)){
        if(!is.matrix(local_pattern)) local_pattern <- matrix(local_pattern, 1L)
        if(is.numeric(local_pattern))
            local_pattern <- t(t(local_pattern) - mirt_mins)
    }
    
    #setup objects
    shinyGUI_object <- ShinyGUI$new(questions=questions, shinyGUI=shinyGUI)
    test_object <- new('Test', mirt_object=mirt_object, item_answers_in=item_answers, 
                     item_options=item_options, quadpts_in=design$quadpts,
                     theta_range_in=design$theta_range, dots=list(...))
    design_object <- new('Design', method=method, criteria=criteria, 
                                start_item=if(is.na(start_item)) sample(1L:test_object@length, 1L)
                                else start_item,
                         nfact=test_object@nfact, design=design, exposure=exposure,
                         preCAT=preCAT, nitems=test_object@length)
    person_object <- Person$new(nfact=test_object@nfact, nitems=length(test_object@itemnames), 
                         thetas.start_in=design$thetas.start, score=score)
    if(!is.null(shinyGUI$resume_file)){
        person_object <- readRDS(shinyGUI$resume_file)
        MCE$last_demographics <- person_object$demographics
        shinyGUI_object$demographics <- list()
        shinyGUI_object$firstpage <- list()
        shinyGUI_object$demographic_inputIDs <- character(0)
    }
    if(design_elements){
        ret <- list(person=person_object, test=test_object, design=design_object)
        class(ret) <- "mirtCAT_design"
        return(ret)
    }
    
    #put in specific enviroment (move later TODO) 
    MCE$person <- person_object
    MCE$test <- test_object
    MCE$design <- design_object
    MCE$shinyGUI <- shinyGUI_object
    MCE$STOP <- FALSE
    MCE$outfile <- tempfile(fileext='.png')
    
    if(is.null(local_pattern)){
        runApp(list(ui = ui(), server = server), launch.browser=TRUE)
        person <- MCE$person
    } else {
        person <- run_local(local_pattern, nfact=test_object@nfact, start_item=start_item,
                            nitems=length(test_object@itemnames), cl=cl,
                            thetas.start_in=design$thetas.start, score=score, 
                            design=design_object, test=test_object, ...)
    }
    if(!is.list(person)) person <- list(person)
    ret.out <- vector('list', length(person))
    for(i in 1L:length(person)){
        person[[i]]$items_answered <- person[[i]]$items_answered[!is.na(person[[i]]$items_answered)]
        ret <- list(raw_responses=person[[i]]$raw_responses + 1L, 
                    scored_responses=if(!is.null(item_answers) || is.null(df)) 
                        as.numeric(person[[i]]$responses + mirt_mins) 
                        else as.numeric(rep(NA, length(mirt_mins))),
                    items_answered=person[[i]]$items_answered,
                    thetas=person[[i]]$thetas,
                    thetas_history=person[[i]]$thetas_history,
                    thetas_SE_history=person[[i]]$thetas_SE_history,
                    item_time=person[[i]]$item_time,
                    demographics=person[[i]]$demographics)
        if(!is.null(design$classify)){
            z <- -abs(ret$thetas - design$classify) / 
                ret$thetas_SE_history[nrow(ret$thetas_SE_history),]
            sig <- z < qnorm((1-design$classify_CI)/2)
            direction <- ifelse((ret$thetas - design$classify) > 0, 'above cutoff', 'below cutoff') 
            direction[!sig] <- 'no decision'
            ret$classification <- direction
        }
        colnames(ret$thetas) <- colnames(ret$thetas_history) <-
            colnames(ret$thetas_SE_history) <- paste0('Theta_', 1L:test_object@nfact)
        if(!person[[i]]$score)
            ret$thetas <- ret$thetas_history <- ret$thetas_SE_history <- NA
        class(ret) <- 'mirtCAT'
        ret.out[[i]] <- ret
    }
    if(length(ret.out) == 1L) return(ret.out[[1L]]) 
    return(ret.out)
}