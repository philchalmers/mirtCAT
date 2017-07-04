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
#' above or below the specified cutoffs can be made. If all questions should
#' be answered, users should specify an extremely small \code{'min_SEM'} or, equivalently, 
#' a large \code{'min_items'} criteria to the \code{design} list input.
#' 
#' @section HTML help files, exercises, and examples:
#' 
#' To access examples, vignettes, and exercise files that have been generated with \code{knitr} please
#' visit \url{https://github.com/philchalmers/mirtCAT/wiki}.
#' 
#' @param df a \code{data.frame} containing the \code{character} vector inputs required to generate 
#'   GUI questions through shiny. If \code{factor}s are supplied instead of \code{character} vectors 
#'   then the inputs will be coerced using the \code{as.character()} function (set 
#'   \code{stringsAsFactors = FALSE} when defining a \code{data.frame} to avoid this). 
#'   Each row in the object corresponds to a unique
#'   item. The object supports the follow column name combinations as inputs to specify the 
#'   type of response format, questions, options, answers, and stems:
#'   
#'   \describe{
#'   
#'     \item{\code{Type}}{Indicates the type of response input 
#'       to use from the shiny package. The supported types are: \code{'radio'} for radio buttons 
#'       (\code{\link{radioButtons}}), \code{'select'} for a pull-down box for selecting 
#'       inputs (\code{\link{selectInput}}), \code{'text'} and \code{'textArea'} for requiring 
#'       typed user input (\code{\link{textInput}} and \code{\link{textAreaInput}}), 
#'       \code{'checkbox'} for allowing multiple 
#'       responses to be checked off (\code{\link{checkboxGroupInput}}),
#'       \code{'slider'} for generating slider inputs (\code{\link{sliderInput}}), or
#'       \code{'none'} for presenting only an item stem with no selection options. Note that slider
#'       inputs require additional arguments to be passed; see \code{...} instructions below).
#'       
#'       Additionally, if the above types are not sufficient for the desired output then users 
#'       can create their own response formats and inputs via the \code{customTypes} list input 
#'       (see below). E.g., if a function with the name \code{'MyTableQuestion'} is supplied 
#'       to \code{customTypes} then supplying this type to the \code{df} will use this function for
#'       the respective item. Note that this is more advanced and requires a working knowledge of shiny's 
#'       design, inputs, and specifications. This is generally for advanced users
#'       to use on an as-per-needed basis.} 
#'     
#'     \item{\code{Question}}{A character vector containing all the questions or stems to be generated.
#'       Alternatively, when paired with the \code{StemExpression} logical value, 
#'       each element may represent a character vector of arbitrary R expressions
#'       to be evaluated as suitable item stems. These are rendered into suitable HTML code,
#'       typically through shiny. This approach is much more verbose, however it provides a large variety 
#'       of customization through the use of \code{div()} and other helpful tags. 
#'       
#'       E.g., the following would result in two bolded and italicized item stems: 
#'       \code{c("tags$b('Stem 1')", "tags$em('Stem 2')")}, along with associated \code{TRUE} values
#'       in \code{StemExpression}. See 
#'       \code{http://shiny.rstudio.com/articles/tag-glossary.html} for more examples of how
#'       to use tags and HTML generating functions.
#'       
#'       } 
#'       
#'     \item{\code{Option.#}}{Names pertaining to the possible response
#'       options for each item, where the # corresponds to the specific category. For
#'       instance, a test with 4 unique response options for each item would contain
#'       the columns (\code{Option.1}, \code{Option.2}, \code{Option.3}, \code{Option.4}).
#'       If, however, some items have fewer categories than others then \code{NA}'s can be used for response
#'       options that do not apply.}
#'       
#'     \item{\code{Answer} or \code{Answer.#}}{(Optional) A character vector (or multiple character
#'       vectors) indicating the scoring key for items that have correct answer(s). If there
#'       is no correct answer for a question then a value of \code{NA} must be declared.}
#'       
#'     \item{\code{Stem}}{(Optional) a character vector of absolute or relative paths 
#'       pointing external markdown (.md) or HTML (.html) files to be used as item stems. 
#'       \code{NA}s are used if the item has no corresponding file.} 
#'       
#'     \item{\code{StemExpression}}{(Optional) a logical vector indicating which \code{Question}
#'       elements should be evaluated first in R. }
#'       
#'     \item{\code{...}}{In cases where \code{'slider'} inputs are used instead only 
#'       the \code{Question} input is required along with (at minimum) a 
#'       \code{min}, \code{max}, and \code{step} column. In rows where the \code{Type == 'slider'} the 
#'       column names will correspond to the input arguments to \code{\link{sliderInput}}. 
#'       Other input column options such as \code{step}, \code{round}, \code{pre}, \code{post}, 
#'       \code{ticks}, \code{inline}, \code{placeholder}, \code{width}, and \code{size} 
#'       are also supported for the respective input types.} 
#'       
#'   }
#'   
#' @param mo single group object defined by the \code{mirt::mirt()} function. This is required
#'   if the test is to be scored adaptively or non-adaptively, but not required for general 
#'   questionnaires. The object can be constructed by using the 
#'   \code{\link{generate.mirt_object}} function if population parameters are known or by
#'   including a calibrated model estimated from the \code{\link{mirt}} function with real data.
#'   
#' @param method argument passed to \code{mirt::fscores()} for computing new scores in the CAT 
#'   stage, with the addition of a \code{'fixed'} input to keep the latent trait estimates
#'   fixed at the previous values. When \code{method = 'ML'}, if there is no variability 
#'   in the given response pattern during the CAT (i.e., the participant is responding completely
#'   correctly or completely incorrectly) then the method will temporarily be set to MAP until 
#'   sufficient response variability is present. Default is 'MAP'
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
#'   
#'   Applicable to both unidimensional and multidimensional tests are the
#'   \code{'KL'} and \code{'KLn'} for point-wise Kullback-Leibler divergence and 
#'   point-wise Kullback-Leibler with a decreasing delta value (\code{delta*sqrt(n)}, 
#'   where \code{n} is the number of items previous answered), respectively. 
#'   The \code{delta} criteria is defined in the \code{design} object
#'   
#'   Non-adaptive methods applicable even when no \code{mo} object is passed 
#'   are: \code{'random'} to randomly select items, and \code{'seq'} for selecting 
#'   items sequentially.
#'   
#' @param start_item two possible inputs to determine the starting item are available. 
#'   Passing a number will indicate the specific item to be used as the start item;
#'   default is 1, which selects the first item in the defined test/survey. 
#'   If a character string is passed then the item will be selected from one of 
#'   the item selections criteria available (see the \code{criteria} argument). For off-line 
#'   runs where a \code{local_pattern} input is used then a vector of numbers/characters
#'   may be supplied and will be associated with each row response vector
#'   
#' @param local_pattern a character/numeric matrix of response patterns 
#'   used to run the CAT application without generating the GUI interface. 
#'   This option requires complete response pattern(s) to be supplied. \code{local_pattern} 
#'   is required to be numeric if no \code{questions} are supplied, and the responses must be 
#'   within a valid range of the defined \code{mo} object.
#'   Otherwise, it must contain character values of plausible responses which corresponds to the
#'   answer key and/or options supplied in \code{df}. If the object contains an attribute \code{'Theta'} 
#'   then these values will be stored within the respective returned objects. 
#'   See \code{\link{generate_pattern}} to generate response patterns for Monte Carlo simulations
#'   
#' @param AnswerFuns a list with the length equal to the number of items in the item bank consisting 
#'   of user-defined functions. These functions are used to determine whether a given
#'   response obtained from the GUI is 'correct' or 'incorrect' by returning a logical scalar value, 
#'   while \code{NA}'s must be used to indicate \code{AnswerFuns} should not be used for a given item. Note 
#'   that \code{AnswerFuns} is given priority over the answers provided by \code{df}, therefore any answers
#'   provided by \code{df} will be entirely ignored.
#'   
#'   For example, the following provides a customized response function for the first item.
#'   \preformatted{
#'      AnswerFuns <- as.list(rep(NA, nrow(df)))
#'      AnswerFuns[[1]] <- function(input) input == '10' || to.lower(input) == 'ten'
#'   }
#'   
#' @param cl an object definition to be passed to the parallel package 
#'   (see \code{?parallel::parLapply} for details). If defined, and if 
#'   \code{nrow(local_pattern) > 1}, then each row will be run in parallel to help 
#'   decrease estimation times in simulation work
#'   
#' @param primeCluster logical; when a \code{cl} object is supplied, should the cluster be primed 
#'   first before running the simulations in parallel? Setting to \code{TRUE} will ensure that 
#'   using the cluster will be optimal every time a new \code{cl} is defined. Default is \code{TRUE}
#'   
#' @param customTypes an optional list input contaning functions for Designing Original Graphical Stimuli (DOGS).
#'   DOGS elements in the input list must contain a unique name, and the item with which it is associated must be
#'   declared in the a \code{df$Type} input. The functions defined must be of the form
#'   
#'   \preformatted{myDOGS <- function(inputId, df_row) ...}
#'   
#'   and must return, at the very minimum, an associated \code{shiny} input object that makes use of the
#'   \code{inputId} argument (e.g., \code{\link{radioButtons}}). Any valid shiny object can be returned,
#'   including lists of shiny objects. As well, the \code{df_row} argument contains
#'   any extra information the users wishes to obtain from the associated row in the \code{df} object. 
#'   
#'   The following is a simple example of DOGS for a true-false question and how it is passed:   
#'   \preformatted{
#'   good_dogs <- function(inputId, df_row){
#'      return(list(h2('This statement is false'),
#'                  radioButtons(inputId = inputId, label='', 
#'                               choices = c('True', 'False'), selected = '') 
#'           ))
#'      }
#'      
#'   df <- data.frame(Question = '', ..., Type = 'Doug') 
#'   results <- mirtCAT(df=df, customTypes = list(Doug = good_dogs))
#'   }
#'   
#' @param design_elements logical; return an object containing the test, person, and design 
#'   elements? Primarily this is to be used with the \code{\link{findNextItem}} function
#'   
#' @param progress logical; print a progress bar to the console 
#'   with the \code{pbapply} package for given response patterns? Useful for 
#'   guaging how long Monte Carlo simulations will take to finish
#'   
#' @param design a list of design based control parameters for adaptive and non-adaptive tests. 
#'   These can be
#' 
#' \describe{
#'   \item{\code{min_SEM}}{Default is \code{rep(0.3, nfact)}; minimum standard error or measurement
#'     to be reached for the latent traits (thetas) before the test is stopped. If the test is
#'     multidimensional, either a single value or a vector of values may be supplied to provide
#'     SEM criteria values for each dimension}
#'     
#'   \item{delta_thetas}{Default is \code{rep(0, nfact)}; stopping criteria based on the change in latent
#'     trait values (e.g., a change from \code{theta = 1.5} to \code{theta = 1.54} would 
#'     stop the CAT if \code{delta_thetas = 0.05}). The default disables this stopping criteria}
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
#'   \item{\code{max_time}}{maximum time allowed for the generated GUI, measured
#'     in seconds. For instance, if the test should stop after 10 minutes then the number 
#'     600 should be passed (10 * 60). Default is \code{Inf}, therefore no time limit}
#'   
#'   \item{\code{quadpts}}{Number of quadrature points used per dimension 
#'     for integration (if required). Default is identical to scheme in \code{\link{fscores}}}
#'   
#'   \item{\code{theta_range}}{upper and lower range for the theta 
#'     integration grid. Used in conjunction with \code{quadpts} to generate an equally spaced 
#'     quadrature grid. Default is \code{c(-6,6)}}
#' 
#'   \item{\code{weights}}{weights used when \code{criteria == 'Wrule'}, but also 
#'     will be applied for weighted trace functions in the T- and A-rules. The default 
#'     weights the latent dimensions equally. Default is \code{rep(1, nfact)}, 
#'     where \code{nfact} is the number of test dimensions}
#'     
#'   \item{\code{KL_delta}}{interval range used when \code{criteria = 'KL'}
#'     or \code{criteria = 'KLn'}. Default is \code{0.1}}
#'     
#'   \item{\code{content}}{an optional character vector indicating the type of content measured
#'     by an item. Must be supplied in conjunction with \code{content_prop}}
#'     
#'   \item{\code{content_prop}}{an optional named numeric vector indicating the 
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
#'   \item{\code{exposure}}{a numeric vector specifying the amount of exposure control to apply for
#'     each successive item (length must equal the number of items). 
#'     The default uses no exposure control. If the item exposure 
#'     is greater than 1 then the \code{n} most optimal
#'     criteria will be randomly sampled from. For instance, if 
#'     \code{exposure[5] == 3}, and \code{criteria = 'MI'}, then when the fifth item is to be 
#'     selected from the remaining pool of items the top 3 candidate items demonstrating 
#'     the largest information criteria will be sampled from. Naturally, the first and last 
#'     elements of \code{exposure} are ignored since exposure control will be meaningless.
#'     
#'     If all elements in \code{exposure} are between 0 and 1 then the Sympson-Hetter exposure 
#'     control method will be implemented. In this method, an item is administered only if it 
#'     passes a probability simulation experiment, otherwise it is removed from the item pool.
#'     Values closer to 1 are more likely to appear in the test, while value closer to 0 are more
#'     likely to be randomly discarded.}
#'     
#'   \item{\code{constraints}}{A named list declaring various item selection constraints for which
#'     particular item, where each list element is a vector of item numbers. Unless otherwise stated,
#'     multiple elements can be declared (e.g., \code{list(ordered = c(1:5), ordered = c(7:9))} is
#'     perfectly acceptable). These include:
#'     
#'     \describe{
#'          \item{\code{not_scored}}{declaring items that can be selected but will not be used in the 
#'            scoring of the CAT. This is primarily useful when including experimental items for
#'            future CATs. Only one vector of \code{not_scored} elements can be supplied}
#'          \item{\code{excluded}}{items which should not actually appear in the session 
#'            (useful when re-testing participants who have already seen some of the items). 
#'            Only one vector of \code{excluded} elements can be supplied}
#'          \item{\code{independent}}{declaring which items should never appear in the same CAT session.
#'            Use this if, for example, item 1 and item 10 have very similar questions 
#'            types and therefore should not appear within the same session}
#'          \item{\code{ordered}}{if one item is selected during the CAT, administer this 
#'            particular group of items in order according to the specified sequence}
#'          \item{\code{unordered}}{same as ordered, except the items in the group will be selected at 
#'            random until the group is complete}
#'     }
#'   }
#'   
#'   \item{\code{customUpdateThetas}}{a more advanced function of the form 
#'     \code{customUpdateThetas <- function(design, person, test)}      
#'     to update the ability/latent trait estimates throughout the CAT (or more generally, scoring) session.
#'     The \code{design}, \code{person}, and \code{test} are the same as in 
#'     \code{customNextItem}. 
#'     The latent trait terms are updated directly in the \code{person} object, which is a 
#'     \code{\link{ReferenceClasses}} type, and therefore direct assignment to the object will modify the internal
#'     elements. Hence, to avoid manual modification users can pass the latent trait estimates and their 
#'     respective standard errors to the associated \code{person$Update_thetas(theta, theta_SE)} function.
#'     Note that the \code{fscores()} function can be useful here
#'     to capitalize on the estimation algorithms implemented in \code{mirt}.
#'     
#'     For example, a minimal working function would look like the following (note the use of \code{rbind()} to
#'     append the history terms in the \code{person} object):
#'     
#'     \preformatted{
#'        myfun <- function(design, person, test){
#'            mo <- extract.mirtCAT(test, 'mo')
#'            responses <- extract.mirtCAT(person, 'responses')
#'            tmp <- fscores(mo, response.pattern = responses)
#'            person$Update_thetas(tmp[,'F1'],
#'                                 tmp[,'SE_F1', drop=FALSE])
#'            invisible()
#'         }
#'     }
#'   
#'   }
#'   
#'   \item{\code{customNextItem}}{a more advanced function of the form 
#'     \code{customNextItem <- function(design, person, test)} to use a customized item selection
#'     method. This requires more complex programming and understanding of \code{mirtCAT}s internal elements,
#'     and it's recommended to initially use a \code{\link{browser}} to understand the state 
#'     of the input arguments. When defined, all but the \code{not_scored} input 
#'     to the optional \code{constraints} list will be ignored.
#'     
#'     Use this if you wish to program your item selection techniques explicitly, though this 
#'     can be combined the internal \code{\link{findNextItem}} function with analogous inputs. 
#'     Function must return a single integer value 
#'     indicating the next item to administer or an \code{NA} value to indicate that the test
#'     should be terminated. See \code{\link{extract.mirtCAT}} for details on how to extract and manipulate
#'     various internal elements from the required functional arguments
#'   }
#'   
#'   \item{\code{constr_fun}}{a user-defined function of the form \code{function(design, person, test)} 
#'     that returns a \code{data.frame} containing the left hand side, relationship, and right hand side
#'     of the constraints for \code{\link{lp}}. 
#'     Each row corresponds to a constraint, while the number of columns should be 
#'     equal to the number of items plus 2. Note that the column names of the 
#'     returned \code{data.frame} object do not matter. 
#'   
#'     For example, say that for a given test the user wants to add 
#'     the constraint that exactly 10 items 
#'     should be administered to all participants, and that items 1 and 2 should not 
#'     be included in the same test. The input would then be defined as 
#'     \preformatted{const_fun <- function(design, person, test){
#'        nitems <- extract.mirt(test@@mo, 'nitems')
#'        lhs <- matrix(0, 2, nitems)
#'        lhs[1, ] <- 1
#'        lhs[2, c(1,2)] <- 1
#'        data.frame(item=lhs, relation=c("==", "<="), value=c(10, 1))
#'      }}
#'     The definition above corresponds to the constraints \code{1 * x1 + 1 * x2 + ... + 1 * xn = 10} 
#'     and \code{1 * x1 + 1 * x2 + 0 * x3 + ... + 0 * xn <= 1} , where 
#'     the \code{x} terms represent binary indicators for each respective item which the optimizer 
#'     is searching through. Given some objective vector supplied to \code{\link{findNextItem}},
#'     the most optimal 10 items will be selected which satisfy these two constraints, meaning that
#'     1) exactly 10 items will be administered, and 2) if either item 1 or 2 were
#'     selected these two items would never appear in the same test form (though neither is forced to
#'     appear in any given test). 
#'     See \code{\link{findNextItem}} for further details and examples
#'   }
#'   
#'   \item{\code{test_properties}}{a user-defined \code{data.frame} object to be used
#'     with a supplied \code{customNextItem} function. This should be used to define particular
#'     properties inherent to the test items (e.g., whether they are experimental, have a particular
#'     weighting scheme, should only be used for one particular group of individuals, and so on). 
#'     The number of rows must be equal to the number of items in the item bank, and each row 
#'     corresponds to the respective item. This input appears within the internal \code{design} object
#'     in a \code{test_properties} slot.
#'   }
#'   
#'   \item{\code{person_properties}}{a user-defined \code{data.frame} object to be used
#'     with a supplied \code{customNextItem} function. This should be used to define particular
#'     properties inherent to the individuals participants (e.g., known grouping variable, age, 
#'     whether they've taken the test before (and which items they took), and so on). 
#'     In off-line simulations, the number of rows must be equal to the number of participants. 
#'     This input appears within the internal \code{design} object in a \code{person_properties} slot; 
#'     for Monte Carlo simulations, rows should be manually indexed using the \code{person$ID} slot.
#'   }
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
#'     \code{'Author of survey'}. If the input is an empty string (\code{''}) then the author 
#'     information will be omitted in the GUI}
#'     
#'   \item{\code{instructions}}{A two part character vector indicating how to use the GUI. 
#'     Default is: 
#'   
#'     \preformatted{c("To progress through the interface, click on the action button below.",
#'        "Next")}
#'        
#'     The second part of the character vector provides the name for the action button.
#'   }
#'
#'   \item{\code{firstpage}}{The first page of the shiny GUI. Default prints the title
#'     and information message. 
#'     
#'     \preformatted{ 
#'          list(h1('Welcome to the mirtCAT interface'),
#'               sprintf('The following interface was created using the mirtCAT package v%s. 
#'               To cite the package use citation(\\'mirtCAT\\') in R.', 
#'                  packageVersion("mirtCAT")))
#'          }
#'          
#'      If an empty list is passed, this page will be skipped.
#'    }
#'    
#'    \item{\code{begin_message}}{Text to display on the page prior to beginning the CAT. Default is 
#'      \code{"Click the action button to begin."} for scored tests whereby a \code{mo} object has been include,
#'      while the default is \code{""} for non-scored tests (which disables the page).}
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
#'   \item{\code{stem_default_format}}{\code{shiny} function used for the stems of the items. Default uses the 
#'     \code{\link{p}} wrapper. To change this to \code{\link{h5}}, for example, 
#'     pass \code{stem_default_format = shiny::h5} to the \code{shinyGUI} list}
#'     
#'   \item{\code{temp_file}}{a character vector indicating where a temporary .rds file 
#'     containing the response information should be saved while the GUI is running. 
#'     The object will be saved after each item is successfully completed. This is used to 
#'     save response information to the hard drive in case there are power outages or 
#'     unexpected computer restarts.      
#'     
#'     If \code{NULL}, no temp file will be created. Upon completion of the test, the 
#'     temp file will be deleted. If a file already exists, however, then this will be used to 
#'     resume the GUI at the last location where the session was interrupted}
#'     
#'   \item{\code{lastpage}}{A function printing the last message, indicating that the test has been completed 
#'     (i.e., criteria has been met). The function requires exactly one argument (called \code{person}), where 
#'     the input argument is the person object that has been updated throughout the test. The default function is 
#'   
#'     \preformatted{function(person){ 
#'                     return(list(h5("You have successfully completed the interface. 
#'                                    Click the action button to terminate the application.")))
#'                      } }
#'    }    
#'    
#'    \item{\code{css}}{a character string defining CSS elements to modify the GUI presentation 
#'      elements. The input string is passed to the argument \code{tags$style(HTML(shinyGUI$css))}
#'      prior to constructing the user interface}
#'  
#'    \item{\code{theme}}{a character definition for the \code{shinytheme} package to globally change 
#'      the GUI theme}
#'      
#'    \item{\code{forced_choice}}{logical; require a response to each item? Default is \code{TRUE}.
#'      This should only be set to \code{FALSE} for surveys (not CATs)}
#'      
#'    \item{\code{choiceNames}}{a list containing the \code{choiceNames} input for each respective item when
#'      the input is 'radio' or 'checkbox' (see \code{\link{radioButtons}}). 
#'      This is used to modify the output of the controllers using 
#'      suitable HTML code. If a row in \code{df} should not have a customized names then supplying 
#'      the value \code{NA} in the associated list element will use the standard inputs instead}
#'      
#'    \item{\code{choiceValues}}{associated values to be used along with \code{choiceNames} (see above)}
#'      
#'    \item{\code{time_before_answer}}{a numeric value representing the number of seconds that must have elapsed
#'      when \code{forced_choice = FALSE} before a response can be provided or skipped. This is used 
#'      to control accidental skips over items when responses are not forced. Default is 1, indicating
#'      one full second}
#'      
#'    \item{\code{password}}{a \code{data.frame} object indicating the user name (optional) and password
#'      required prior to beginning the CAT. Possible options are
#'      \describe{
#'        \item{No User Information}{a single row \code{data.frame}. Each column supplied in this case will be associated
#'          with a suitable password for all individuals. Naturally, if only 1 column is defined then
#'          there is only 1 global password for all users}
#'        \item{User Information Pairing}{a multi-row \code{data.frame} where the first column 
#'          represents the user name and all other columns as the same as the first option. 
#'          E.g., if two users ('name1' and 'name2') 
#'          are given the same password '1234' then 
#'          \code{password = data.frame(user = c('user1', 'user2'), password = rep('1234', 2))}}      
#'      }
#'    }
#'      
#'    \item{\code{stopApp}}{logical; use a \code{stopApp()} call after the interface has been completed?
#'      Default is \code{TRUE}. However, when hosting an application on a remote server this should be set
#'      to \code{FALSE} to allow a more graceful completion (in which case the last page will be displayed
#'      until the browser tab is closed)}
#'      
#'    \item{\code{ui}}{a shiny UI function used to define the interface. If \code{NULL}, the 
#'      default one will be used. See \code{mirtCAT:::default_UI} for the internal code definition}
#'   
#' }
#' 
#' @param preCAT a list object which can be used to specify a pre-CAT block in which 
#'   different test properties may be applied prior to beginning the CAT session. If the
#'   list is empty, no preCAT block will be used. All of the following elements are required 
#'   to use the \code{preCAT} input:
#'   
#'   \describe{
#'     \item{\code{min_items}}{minimum number of items to administer before the CAT session begins.
#'       Default is 0}
#'       
#'     \item{\code{max_items}}{max number of items to administer before the CAT session begins.
#'       An input greater than 0 is required to run the preCAT stage}
#'     
#'     \item{\code{criteria}}{selection criteria (see above). Default is 'random'}
#'     
#'     \item{\code{method}}{estimation criteria (see above). It is generally recommended to 
#'       select a method which can deal with all-or-none response patterns, such as 'EAP',
#'       'MAP', or 'WLE'. Default is 'MAP'}
#'       
#'     \item{\code{response_variance}}{logical; terminate the preCAT stage when there is variability in the 
#'       response pattern (i.e., when maximum-likelihood estimation contains a potential optimum)?
#'       Default is FALSE}
#' }
#' 
#' @param ... additional arguments to be passed to \code{\link{mirt}}, \code{\link{fscores}}, 
#'   \code{\link{runApp}}, or \code{lattice}
#' 
#' @export mirtCAT
#' 
#' @author Phil Chalmers \email{rphilip.chalmers@@gmail.com}
#' 
#' @seealso \code{\link{generate_pattern}}, \code{\link{generate.mirt_object}}, 
#'   \code{\link{extract.mirtCAT}}, \code{\link{findNextItem}}, \code{\link{computeCriteria}}
#' 
#' @return Returns a list object of class \code{'Person'} containing the following elements:
#'   
#' \describe{
#'   \item{\code{raw_responses}}{A character vector indicating the raws responses to the respective
#'     items, where NA indicates the item was not answered}
#'     
#'   \item{\code{scored_responses}}{An integer vector of scored responses if the \code{item_answers} input
#'     was used for each respective item}
#'   
#'   \item{\code{items_answered}}{An integer vector indicating the order in which the items were 
#'     answered}
#'   
#'   \item{\code{thetas}}{A numeric vector indicating the final theta estimates}
#'   
#'   \item{\code{SE_thetas}}{A numeric vector indicating the standard errors of the 
#'     final theta estimates}
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
#'     
#'   \item{\code{classification}}{A character vector indicating whether the traits could be 
#'     classified as 'above' or 'below' the desired cutoffs}
#'     
#' }
#' 
#' @section Modifying the \code{design} object directly through \code{customNextItem()} (advanced):
#' 
#' In addition to providing a completely defined item-selection map via the \code{customNextItem()} function, 
#' users may also wish to control some of the more fine-grained elements of the \code{design} object to adjust 
#' the general control parameters of the CAT (e.g., modifying the maximum number of items to administer, stopping
#' the CAT if something peculiar has been detected in the response patterns, etc). Note that 
#' this feature is rarely required for most applications, though more advanced users may wish to 
#' modify these various low-level elements of the \code{design} object directly to change the flow of the CAT
#' to suit their specific needs. 
#' 
#' While the \code{person} object is defined as a \code{Reference Class} (see \code{\link{setRefClass}}) 
#' the design object is generally considered a fixed S4 class, meaning that, unlike the \code{person} object, 
#' it's elements are not mutable. Therefore, in order to make changes directly to the 
#' \code{design} object the users should follow these steps:
#' 
#' \enumerate{
#'   \item Within the defined \code{customNextItem} function, the \code{design} object slots are first modified (e.g.,
#'     \code{design@@max_items <- 20L}).
#'   \item Along with the desired next item scalar value from \code{customNextItem()}, the scalar object should also 
#'     contain an attribute with the name \code{'design'} which holds the newly defined \code{design} object
#'     (e.g., \code{attr(ret, 'design') <- design; return(ret)}).
#'  }
#'  
#' Following the above process the work-flow in \code{\link{mirtCAT}} will use the new \code{design} object in place of the
#' old one, even in Monte Carlo simulations.
#'
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
#' @keywords CAT, MCAT, computerized adaptive testing
#' 
#' @examples
#' \dontrun{
#' 
#' ### unidimensional scored example with generated items
#' 
#' # create mo from estimated parameters
#' set.seed(1234)
#' nitems <- 50
#' itemnames <- paste0('Item.', 1:nitems)
#' a <- matrix(rlnorm(nitems, .2, .3))
#' d <- matrix(rnorm(nitems))
#' dat <- simdata(a, d, 1000, itemtype = 'dich')
#' mod <- mirt(dat, 1)
#' coef(mod, simplify=TRUE)
#' 
#' # alternatively, define mo from population values (not run)
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
#' (res_MI <- mirtCAT(df, mod, criteria = 'MI', start_item = 'MI')) #adaptive, MI starting item
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
#' 
#' # seq scoring with character pattern for the entire test (adjust min_items)
#' res <- mirtCAT(df, mod, local_pattern=pat, design = list(min_items = 50)) 
#' summary(res)
#' 
#' # same as above, but using special input vector that doesn't require df input
#' set.seed(1)
#' pat2 <- generate_pattern(mod, Theta = 0)
#' head(pat2)
#' print(mirtCAT(mo=mod, local_pattern=pat2))
#' 
#' # run CAT, and save results to object called person (start at 10th item)
#' person <- mirtCAT(df, mod, item_answers = answers, criteria = 'MI', 
#'                   start_item = 10, local_pattern = pat)
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
#' res <- mirtCAT(df, mod, shinyGUI = list(temp_file = wdf))
#' 
#' # resume test this way if test was stopped early (and temp files were saved)
#' res <- mirtCAT(df, mod, shinyGUI = list(temp_file = wdf))
#' print(res)
#' 
#' }
mirtCAT <- function(df = NULL, mo = NULL, method = 'MAP', criteria = 'seq', 
                    start_item = 1, local_pattern = NULL, AnswerFuns = list(), 
                    design_elements = FALSE, cl = NULL, progress = FALSE, 
                    primeCluster = TRUE, customTypes = list(), 
                    design = list(), shinyGUI = list(), preCAT = list(), ...)
{   
    on.exit({.MCE$person <- .MCE$test <- .MCE$design <- .MCE$shinyGUI <- .MCE$start_time <- 
             .MCE$STOP <- .MCE$outfile <- .MCE$outfile2 <- .MCE$last_demographics <- 
             .MCE$preamble_defined <- NULL})
    mirtCAT_preamble(df=df, mo=mo, method=method, criteria=criteria, 
                     start_item=start_item, local_pattern=local_pattern, 
                     design_elements=design_elements, cl=cl, AnswerFuns=AnswerFuns, 
                     design=design, shinyGUI=shinyGUI, preCAT=preCAT, 
                     customTypes=customTypes, ...)
    if(design_elements){
        ret <- list(person=.MCE$person, test=.MCE$test, design=.MCE$design)
        class(ret) <- "mirtCAT_design"
        return(ret)
    }
    if(is.null(local_pattern)){
        runApp(createShinyGUI(ui=.MCE$shinyGUI$ui), launch.browser=TRUE, ...)
        person <- .MCE$person
        GUI <- TRUE
    } else {
        if(length(AnswerFuns)) 
            stop('AnswerFuns cannot be used for off-line runs', call.=FALSE)
        person <- run_local(.MCE$local_pattern, nfact=.MCE$test@nfact, start_item=start_item,
                            nitems=length(.MCE$test@itemnames), cl=cl, primeCluster=primeCluster,
                            thetas.start_in=design$thetas.start, score=.MCE$score, 
                            design=.MCE$design, test=.MCE$test, progress=progress)
        if(!is.null(attr(local_pattern, 'Theta'))){
            local_Thetas <- attr(local_pattern, 'Theta')
            if(length(person) == 1L) 
                local_Thetas <- matrix(as.numeric(local_Thetas), nrow=1L)
            for(i in seq_len(length(person)))
                person[[i]]$true_thetas <- local_Thetas[i, ]
        }
        GUI <- FALSE
    }
    ret <- mirtCAT_post_internal(person=person, design=.MCE$design, 
                                 has_answers=.MCE$test@has_answers, GUI=GUI)
    return(ret)
}