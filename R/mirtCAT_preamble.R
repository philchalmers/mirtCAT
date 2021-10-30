#' Preamble function called by mirtCAT
#' 
#' This is largely an internal function called by \code{\link{mirtCAT}}, however it is made 
#' public for better use with external web-hosting interfaces (like shinyapps.io).
#' For more information see \url{https://shiny.rstudio.com/articles/persistent-data-storage.html} for 
#' further information about saving output remotely when using \code{shiny}.
#' 
# @param sessionName the unique name of the session (see \code{\link{mirtCAT}} for details)
#' 
#' @param final_fun a function called just before the shiny GUI has been terminated, primarily for
#'   saving results externally with packages such as \code{rDrop2}, \code{RAmazonS3}, 
#'   \code{googlesheets}, \code{RMySQL}, personal servers, and 
#'   so on when applications are hosted on the web. The function
#'   must be of the form \code{final_fun <- function(person){...}}, where \code{person} is the 
#'   standard output returned from \code{\link{mirtCAT}}
#' 
#' @param ... arguments passed to \code{\link{mirtCAT}}
#' 
#' @export mirtCAT_preamble
#' 
#' @author Phil Chalmers \email{rphilip.chalmers@@gmail.com}
#' 
#' @seealso \code{\link{mirtCAT}}, \code{\link{createShinyGUI}}, \code{\link{getPerson}} 
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
#' mirtCAT_preamble(df = df)
#' 
#' }
mirtCAT_preamble <- function(..., final_fun = NULL){
    return(mirtCAT_preamble_internal(final_fun = final_fun, ...))
}

# set this up to avoid double documentation
mirtCAT_preamble_internal <- 
    function(df = NULL, mo = NULL, method = 'MAP', criteria = 'seq', AnswerFuns = list(),
             start_item = 1, local_pattern = NULL, design_elements=FALSE, cl=NULL,
             design = list(), shinyGUI = list(), preCAT = list(), customTypes = list(),
             final_fun = NULL, ...)
    {
        sessionName <- 'MASTER'
        if(!is.null(.MCE$currentSessionName)) .MCE$currentSessionName <- NULL
        is_adaptive <- !is.null(mo)
        Names <- if(!is.null(mo)) colnames(mo@Data$data) else NULL
        if(is.null(shinyGUI$stem_default_format)) 
            shinyGUI$stem_default_format <- shiny::HTML
        if(is.null(df)){
            if(is.null(mo)) stop('No df or mo supplied', call.=FALSE)
            if(design_elements) local_pattern <- rep(NA, extract.mirt(mo, 'nitems'))
            if(is.null(local_pattern)) stop('is.null df input, and no local_pattern supplied', 
                                            call.=FALSE)
            if(is.vector(local_pattern)) local_pattern <- matrix(local_pattern, 1L)
            questions <- vector('list', ncol(mo@Data$data))
            names(questions) <- Names
            K <- mo@Data$K
            item_options <- vector('list', length(K))
            for(i in seq_len(length(K)))
                item_options[[i]] <- 0L:(K[i]-1L)
            df <- list()
            Timer <- rep(NA, length(K))
            item_answers <- NULL
            if(!design_elements){
                sapply(1L:length(K), function(i, local_pattern, item_options, mins){
                    opts <- item_options[[i]] + mins[i]
                    if(!all(local_pattern[,i] %in% opts)){
                        outs <- as.character(c(i, min(opts), max(opts)))
                        stop(sprintf('For item %s, responses must be between %s and %s. Please fix.',
                                     outs[1L], outs[2L], outs[3L]), call.=FALSE)
                    }
                }, local_pattern=local_pattern, item_options=item_options, mins=mo@Data$mins)
            }
        } else {
            if(!is.data.frame(df))
                stop('df input must be a data.frame', call.=FALSE)
            if(any(colnames(df) == 'StemExpression'))
                stop('StemExpression input no longer supported. Please pass raw HTML code to Question variable in df', call.=FALSE)
            if(any(sapply(df, class) == 'factor'))
                stop('data.frame requires characters instead of factors. 
                        To avoid, use stringsAsFactors = FALSE in your data.frame',
                        call.=FALSE)
            nitems <- nrow(df)
            df_rownames <- rownames(df)
            if(is.null(shinyGUI$choiceNames))
                shinyGUI$choiceNames <- shinyGUI$choiceValues <- vector('list', nitems)
            if(!is.null(df$HTMLOptions)){
                Options <- df[grepl('Option\\.', colnames(df))]
                for(pick in which(df$HTMLOptions)){
                    tmpopts <- lapply(na.omit(as.character(Options[pick, ])), HTML)
                    names(tmpopts) <- NULL
                    shinyGUI$choiceValues[[pick]] <- shinyGUI$choiceNames[[pick]] <- tmpopts
                }
                df$HTMLOptions <- NULL
            }
            df <- lapply(df, as.character)
            df$Rendered_Question <- lapply(df$Question, function(x, fun) if(x != "") shiny::withMathJax(fun(x)),
                                  fun=shinyGUI$stem_default_format)
            if(length(customTypes)){
                pick <- df$Type %in% names(customTypes)
                df$Rendered_Question[pick] <- ''
            }
            if(length(names(shinyGUI$choiceNames)) > 0L && any(names(shinyGUI$choiceNames) %in% df_rownames)){
                tmp <- vector('list', nitems)
                names(tmp) <- df_rownames
                for(nm in names(shinyGUI$choiceNames))
                    tmp[[nm]] <- shinyGUI$choiceNames[[nm]]
                shinyGUI$choiceNames <- tmp
            }
            if(length(names(shinyGUI$choiceValues)) > 0L && any(names(shinyGUI$choiceValues) %in% df_rownames)){
                tmp <- vector('list', nitems)
                names(tmp) <- df_rownames
                for(nm in names(shinyGUI$choiceValues))
                    tmp[[nm]] <- shinyGUI$choiceValues[[nm]]
                shinyGUI$choiceValues <- tmp
            }
            if(length(shinyGUI$choiceNames) != nitems)
                stop('choiceNames input is not the correct length', call.=FALSE)
            if(length(shinyGUI$choiceValues) != nitems)
                stop('choiceValues input is not the correct length', call.=FALSE)
            obj <- buildShinyElements(df, itemnames = Names, customTypes=customTypes,
                                      choiceNames=shinyGUI$choiceNames, 
                                      choiceValues=shinyGUI$choiceValues)
            questions <- obj$questions
            item_answers <- obj$item_answers
            item_options <- obj$item_options
            shinyGUI$stem_locations <- df[["Stem"]]
            Timer <- df[["Timer"]]
            if(is.null(Timer)) Timer <- rep(NA, nitems)
        }
        if(is.null(mo)){
            dat <- matrix(c(0,1), 2L, length(questions))
            colnames(dat) <- if(!is.null(names(questions))) 
                names(questions) else paste0('Item.', 1L:ncol(dat))
            sv <- mirt(dat, 1L, pars = 'values')
            sv$est <- FALSE
            mo <- mirt(dat, 1L, TOL=NaN, pars=sv)
            score <- FALSE
            if(!(criteria %in% c('seq', 'random')))
                stop('Only random and seq criteria are available if no mo was defined', call.=FALSE)
            mirt_mins <- rep(0L, ncol(dat))
        } else {
            if(mo@Options$exploratory) 
                stop('CATs are intended for confirmatory IRT models not exploratory', call.=FALSE)
            score <- TRUE
            mirt_mins <- mo@Data$mins
        }
        .MCE[[sessionName]]$score <- score
        if(!is.null(local_pattern)){
            if(!is.matrix(local_pattern)) local_pattern <- matrix(local_pattern, 1L)
            if(is.numeric(local_pattern))
                local_pattern <- t(t(local_pattern) - mirt_mins)
        }
        
        #setup objects
        if(!is.null(df)) shinyGUI$stem_locations <- df[['Stem']]
        if(is.null(local_pattern)) 
            shinyGUI_object <- ShinyGUI$new(questions=questions, df=df, shinyGUI=shinyGUI,
                                            adaptive=is_adaptive, CustomTypes=customTypes,
                                            Timer=Timer)
        test_object <- new('Test', mo=mo, item_answers_in=item_answers, AnswerFuns=AnswerFuns,
                           item_options=item_options, quadpts_in=design$quadpts,
                           theta_range_in=design$theta_range, dots=list(...))
        design_object <- new('Design', method=method, criteria=criteria, 
                             start_item=if(is.numeric(start_item)) start_item else NaN, 
                             nfact=test_object@nfact, design=design, 
                             preCAT=preCAT, nitems=test_object@length)
        person_object <- Person$new(nfact=test_object@nfact, nitems=length(test_object@itemnames), 
                                    thetas.start_in=design$thetas.start, score=score, 
                                    theta_SEs=sqrt(diag(test_object@gp$gcov)),
                                    Info_thetas_cov = solve(test_object@gp$gcov))
        if(!is.null(local_pattern)){
            design_object@start_item <- rep(design_object@start_item, nrow(local_pattern))
            if(length(start_item) == 1L)
                start_item <- rep(start_item, nrow(local_pattern))
            stopifnot(length(start_item) == nrow(local_pattern))
        }
        if(is.character(start_item)){
            tmp <- design_object@criteria
            tmp2 <- integer(length(design_object@start_item))
            for(i in seq_len(length(design_object@start_item))){
                design_object@criteria <- start_item[i]
                if(!is.null(design$thetas.start) && is.matrix(design$thetas.start))
                    person_object$thetas <- matrix(design$thetas.start[i,], 1L)
                tmp2[i] <- findNextCATItem(person=person_object, test=test_object, 
                                          design=design_object, start=FALSE) 
            }
            design_object@start_item <- tmp2
            design_object@criteria <- tmp
        }
        .MCE[[sessionName]]$resume_file <- FALSE
        .MCE[[sessionName]]$verified <- TRUE
        if(is.null(local_pattern) && shinyGUI_object$temp_file != ''){
            if(file.exists(shinyGUI_object$temp_file)){
                person_object <- readRDS(shinyGUI_object$temp_file)
                .MCE[[sessionName]]$last_demographics <- person_object$demographics
                .MCE[[sessionName]]$resume_file <- TRUE
                shinyGUI_object$demographics <- list()
                shinyGUI_object$firstpage <- list()
                shinyGUI_object$demographic_inputIDs <- character(0)
            } 
        }
        .MCE[[sessionName]]$test <- test_object
        .MCE[[sessionName]]$design <- design_object
        .MCE[[sessionName]]$local_pattern <- local_pattern
        .MCE[[sessionName]]$mirt_mins <- mirt_mins
        .MCE[[sessionName]]$final_fun <- final_fun
        .MCE[[sessionName]]$person <- person_object
        
        if(is.null(local_pattern)){
            .MCE[[sessionName]]$STOP <- FALSE
            .MCE[[sessionName]]$outfile <- tempfile(fileext='.png')
            .MCE[[sessionName]]$outfile2 <- tempfile(fileext='.html')
            .MCE[[sessionName]]$shift_back <- 0L
            .MCE[[sessionName]]$invalid_count <- 0L
            .MCE[[sessionName]]$shinyGUI <- shinyGUI_object
        }
        
        .MCE[[sessionName]]$preamble_defined <- TRUE
    
        invisible()
    }


mirtCAT_post_internal <- function(person, design, has_answers = FALSE, GUI = FALSE){
    if(!is.list(person)) person <- list(person)
    ret.out <- vector('list', length(person))
    for(i in seq_len(length(person))){
        person[[i]]$items_answered <- person[[i]]$items_answered[!is.na(person[[i]]$items_answered)]
        ret <- list(login_name=person[[i]]$login_name,
                    raw_responses=person[[i]]$raw_responses,
                    scored_responses=if(person[[1L]]$score || any(has_answers)) 
                        as.integer(person[[i]]$responses + .MCE[['MASTER']]$mirt_mins) 
                    else rep(NA, length(person[[i]]$raw_responses)),
                    items_answered=person[[i]]$items_answered,
                    thetas=person[[i]]$thetas,
                    SE_thetas=person[[i]]$thetas_SE_history[nrow(person[[i]]$thetas_SE_history), 
                                                            ,drop=FALSE],
                    thetas_history=person[[i]]$thetas_history,
                    thetas_SE_history=person[[i]]$thetas_SE_history,
                    item_time=person[[i]]$item_time,
                    demographics=person[[i]]$demographics,
                    terminated_sucessfully=person[[i]]$terminated_sucessfully,
                    GUI=GUI,
                    clientData = if(GUI) person[[i]]$clientData)
        if(length(person[[i]]$true_thetas))
            ret$true_thetas <- person[[i]]$true_thetas
        if(design@classify_type == 'CI'){
            ret$classification <- person[[i]]$classify_decision
            ret$classify_values <- design@classify
        } else if(design@classify_type == 'SPRT'){
            ret$classification <- person[[i]]$classify_decision
            ret$classify_values <- c(design@sprt_lower, design@sprt_upper)
        }
        colnames(ret$thetas) <- colnames(ret$SE_thetas) <- colnames(ret$thetas_history) <-
            colnames(ret$thetas_SE_history) <- paste0('Theta_', 1L:.MCE[['MASTER']]$test@nfact)
        if(!person[[i]]$score)
            ret$thetas <- ret$SE_thetas <- ret$thetas_history <- ret$thetas_SE_history <- NA
        class(ret) <- 'mirtCAT'
        ret.out[[i]] <- ret
    }
    if(length(ret.out) == 1L) return(ret.out[[1L]]) 
    return(ret.out)
}

