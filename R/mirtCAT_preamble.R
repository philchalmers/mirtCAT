#' Preamble function called by mirtCAT
#' 
#' This is largely an internal function called by \code{\link{mirtCAT}}, however it is made 
#' public for better use with external web-hosting interfaces (like \url{http://www.shinyapps.io/}).
#' For more information see \url{http://shiny.rstudio.com/articles/persistent-data-storage.html} for 
#' further information about saving output remotely when using \code{shiny}.
#' 
#' @param final_fun a function called just before the shiny GUI has been terminated, primarily for
#'   saving results externally with packages such as \code{rDrop2}, \code{RAmazonS3}, 
#'   \code{googlesheets}, \code{RMySQL}, and so on when applications are hosted on the web. The function
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
#' Chalmers, R. P. (2016). Generating Adaptive and Non-Adaptive Test Interfaces for 
#' Multidimensional Item Response Theory Applications. \emph{Journal of Statistical Software, 71}(5), 
#' 1-39. doi:10.18637/jss.v071.i05
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
    function(df = NULL, mo = NULL, method = 'MAP', criteria = 'seq', 
             start_item = 1, local_pattern = NULL, design_elements=FALSE, cl=NULL,
             design = list(), shinyGUI = list(), preCAT = list(), final_fun = NULL, ...)
    {
        is_adaptive <- !is.null(mo)
        Names <- if(!is.null(mo)) colnames(mo@Data$data) else NULL
        if(is.null(shinyGUI$stem_default_format)) 
            shinyGUI$stem_default_format <- shiny::p
        if(is.null(df)){
            if(is.null(mo)) stop('No df or mo supplied', call.=FALSE)
            if(is.null(local_pattern)) stop('is.null df input, and no local_pattern supplied', 
                                            call.=FALSE)
            if(is.vector(local_pattern)) local_pattern <- matrix(local_pattern, 1L)
            questions <- vector('list', ncol(mo@Data$data))
            names(questions) <- Names
            K <- mo@Data$K
            item_options <- vector('list', length(K))
            for(i in 1L:length(K))
                item_options[[i]] <- 0L:(K[i]-1L)
            df <- list()
            item_answers <- NULL
            sapply(1L:length(K), function(i, local_pattern, item_options, mins){
                opts <- item_options[[i]] + mins[i]
                if(!all(local_pattern[,i] %in% opts)){
                    outs <- as.character(c(i, min(opts), max(opts)))
                    stop(sprintf('For item %s, responses must be between %s and %s. Please fix.',
                                 outs[1L], outs[2L], outs[3L]), call.=FALSE)
                }
            }, local_pattern=local_pattern, item_options=item_options, mins=mo@Data$mins)
        } else {
            if(!is.data.frame(df) && !is.list(df))
                stop('df input must be a data.frame or list', call.=FALSE)
            if(is.data.frame(df)){
                df <- lapply(df, as.character)
                df$Question <- lapply(df$Question, function(x, fun) shiny::withMathJax(fun(x)),
                                      fun=shinyGUI$stem_default_format)
            }
            obj <- buildShinyElements(df, itemnames = Names)
            questions <- obj$questions
            item_answers <- obj$item_answers
            item_options <- obj$item_options
            shinyGUI$stem_locations <- df$Stem
            shinyGUI$stem_expressions <- df$StemExpression
        }
        if(is.null(mo)){
            dat <- matrix(c(0,1), 2L, length(questions))
            colnames(dat) <- names(questions)
            mo <- mirt(dat, 1L, TOL=NaN)
            score <- FALSE
            if(!(criteria %in% c('seq', 'random')))
                stop('Only random and seq criteria are available if no mo was defined', call.=FALSE)
            mirt_mins <- rep(0L, ncol(dat))
        } else {
            score <- TRUE
            mirt_mins <- mo@Data$mins
        }
        .MCE$score <- score
        if(!is.null(local_pattern)){
            if(!is.matrix(local_pattern)) local_pattern <- matrix(local_pattern, 1L)
            if(is.numeric(local_pattern))
                local_pattern <- t(t(local_pattern) - mirt_mins)
        }
        
        #setup objects
        if(!is.null(df)) shinyGUI$stem_locations <- df$Stem
        if(is.null(local_pattern)) 
            shinyGUI_object <- ShinyGUI$new(questions=questions, df=df, shinyGUI=shinyGUI,
                                            adaptive=is_adaptive)
        test_object <- new('Test', mo=mo, item_answers_in=item_answers, 
                           item_options=item_options, quadpts_in=design$quadpts,
                           theta_range_in=design$theta_range, dots=list(...))
        design_object <- new('Design', method=method, criteria=criteria, 
                             start_item=if(is.numeric(start_item)) start_item else NaN,
                             max_time=shinyGUI$max_time, 
                             nfact=test_object@nfact, design=design, 
                             preCAT=preCAT, nitems=test_object@length)
        person_object <- Person$new(nfact=test_object@nfact, nitems=length(test_object@itemnames), 
                                    thetas.start_in=design$thetas.start, score=score, 
                                    theta_SEs=sqrt(diag(test_object@gp$gcov)))
        if(is.character(start_item)){
            tmp <- design_object@criteria
            design_object@criteria <- start_item
            start_item <- findNextCATItem(person=person_object, test=test_object, 
                                          design=design_object, criteria=design_object@criteria,
                                          start=FALSE)
            design_object@start_item <- start_item
            design_object@criteria <- tmp
        }
        .MCE$resume_file <- FALSE
        .MCE$verified <- TRUE
        if(is.null(local_pattern) && shinyGUI_object$temp_file != ''){
            if(file.exists(shinyGUI_object$temp_file)){
                person_object <- readRDS(shinyGUI_object$temp_file)
                .MCE$last_demographics <- person_object$demographics
                .MCE$resume_file <- TRUE
                shinyGUI_object$demographics <- list()
                shinyGUI_object$firstpage <- list()
                shinyGUI_object$demographic_inputIDs <- character(0)
            } 
        }
        .MCE$test <- test_object
        .MCE$design <- design_object
        .MCE$local_pattern <- local_pattern
        .MCE$mirt_mins <- mirt_mins
        .MCE$final_fun <- final_fun
        .MCE$person <- person_object
        
        if(is.null(local_pattern)){
            .MCE$STOP <- FALSE
            .MCE$outfile <- tempfile(fileext='.png')
            .MCE$outfile2 <- tempfile(fileext='.html')
            .MCE$shift_back <- 0L
            .MCE$invalid_count <- 0L
            .MCE$shinyGUI <- shinyGUI_object
        }
        
        .MCE$preamble_defined <- TRUE
    
        invisible()
    }


mirtCAT_post_internal <- function(person, design){
    if(!is.list(person)) person <- list(person)
    ret.out <- vector('list', length(person))
    for(i in 1L:length(person)){
        person[[i]]$items_answered <- person[[i]]$items_answered[!is.na(person[[i]]$items_answered)]
        ret <- list(login_name=person[[i]]$login_name,
                    raw_responses=person[[i]]$raw_responses,
                    scored_responses=if(person[[1L]]$score) as.integer(person[[i]]$responses + 
                                                                           .MCE$mirt_mins) 
                    else rep(NA, length(person[[i]]$raw_responses)),
                    items_answered=person[[i]]$items_answered,
                    thetas=person[[i]]$thetas,
                    SE_thetas=person[[i]]$thetas_SE_history[nrow(person[[i]]$thetas_SE_history), 
                                                            ,drop=FALSE],
                    thetas_history=person[[i]]$thetas_history,
                    thetas_SE_history=person[[i]]$thetas_SE_history,
                    item_time=person[[i]]$item_time,
                    demographics=person[[i]]$demographics)
        if(!is.nan(design@classify[1L])){
            z <- -abs(ret$thetas - design@classify) / ret$SE_thetas
            sig <- z < qnorm(design@classify_alpha)
            direction <- ifelse((ret$thetas - design@classify) > 0, 'above cutoff', 'below cutoff')
            direction[!sig] <- 'no decision'
            ret$classification <- direction
            ret$classify_values <- design@classify
        }
        colnames(ret$thetas) <- colnames(ret$SE_thetas) <- colnames(ret$thetas_history) <-
            colnames(ret$thetas_SE_history) <- paste0('Theta_', 1L:.MCE$test@nfact)
        if(!person[[i]]$score)
            ret$thetas <- ret$SE_thetas <- ret$thetas_history <- ret$thetas_SE_history <- NA
        class(ret) <- 'mirtCAT'
        ret.out[[i]] <- ret
    }
    if(length(ret.out) == 1L) return(ret.out[[1L]]) 
    return(ret.out)
}

