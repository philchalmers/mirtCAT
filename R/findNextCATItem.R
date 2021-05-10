#' Find next CAT item
#'
#' A function that returns the next item in the computerized adaptive, optimal assembly, or shadow test.
#' For direction manipulation of the internal objects this function should be used in conjunction
#' with the \code{\link{updateDesign}} and \code{customNextItem}.
#' Finally, the raw input forms can be used when a \code{customNextItem} function has been
#' defined in \code{\link{mirtCAT}}.
#'
#' When a numeric \code{objective} is supplied the next item in the computerized adaptive test is found via
#' an integer solver through searching for a maximum. The raw input forms can be used
#' when a \code{customNextItem} function has been defined in \code{\link{mirtCAT}}, and requires
#' the definition of a \code{constr_fun} (see the associated element in \code{\link{mirtCAT}} for details,
#' as well as the examples below). Can be used to for 'Optimal Test Assembly',
#' as well as 'Shadow Testing' designs (van der Linden, 2005),
#' by using the \code{\link{lp}} function. When \code{objective} is not supplied the result follows the
#' typical maximum criteria of more standard adaptive tests.
#'
#' @param x an object of class 'mirtCAT_design' returned from the \code{\link{mirtCAT}} function
#'   when passing \code{design_elements = TRUE}
#'
#' @param person (required when \code{x} is missing) internal person object. To be
#'   used when \code{customNextItem} function has been defined
#'
#' @param design (required when \code{x} is missing) internal design object. To be
#'   used when \code{customNextItem} function has been defined
#'
#' @param test (required when \code{x} is missing) internal test object. To be
#'   used when \code{customNextItem} function has been defined
#'
#' @param criteria item selection criteria (see \code{\link{mirtCAT}}'s \code{criteria} input).
#'   If not specified the value from \code{extract.mirtCAT(design, 'criteria')} will be used
#'
#' @param objective a vector of values used as the optimization criteria to be passed to
#'   \code{lp(objective.in)}. This is typically the vector of criteria values returned from
#'   \code{\link{computeCriteria}}, however supplying other
#'   criteria are possible (e.g., to minimize the number of items administered simply pass a vector
#'   of -1's)
#'
#' @param subset an integer vector indicating which items should be included in the optimal search;
#'   the default \code{NULL} includes all possible items. To allow only the first 10 items to be
#'   selected from this can be modified to \code{subset = 1:10}. This is useful when administering
#'   a multi-unidimensional CAT session where unidimensional blocks should be clustered together
#'   for smoother presentation. Useful when using the \code{customNextItem} function in
#'   \code{\link{mirtCAT}}
#'
#' @param all_index logical; return all items instead of just the most optimal?
#'   When \code{TRUE} a vector of items is returned instead of the most optimal,
#'   where the items are sorted according to how
#'   well they fit the criteria (e.g., the first element is the most optimal, followed by the second
#'   most optimal, and so on). Note that this does not work for some selection criteria (e.g.,
#'   'seq' or 'random')
#'
#' @param ... additional arguments to be passed to \code{\link{lp}}
#'
#' @seealso \code{\link{mirtCAT}}, \code{\link{updateDesign}}, \code{\link{extract.mirtCAT}}
#' @export
#' @author Phil Chalmers \email{rphilip.chalmers@@gmail.com}
#' @return typically returns an integer value indicating the index of the next item to be selected or a
#'   value of \code{NA} to indicate that the test should be terminated. However, see the arguments for
#'   further returned object descriptions
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
#' van der Linden, W. J. (2005). Linear models for optimal test design. Springer.
#'
#' @examples
#' \dontrun{
#' 
#' # test defined in mirtCAT help file, first example 
#' # equivalent to criteria = 'MI'
#' customNextItem <- function(design, person, test){
#'    item <- findNextItem(person=person, design=design, test=test,
#'                         criteria = 'MI')
#'    item
#'  }
#'  
#' set.seed(1)
#' nitems <- 100
#' itemnames <- paste0('Item.', 1:nitems)
#' a <- matrix(rlnorm(nitems, .2, .3))
#' d <- matrix(rnorm(nitems))
#' dat <- simdata(a, d, 500, itemtype = 'dich')
#' colnames(dat) <- itemnames
#' mod <- mirt(dat, 1, verbose = FALSE)
#' 
#' # simple math items
#' questions <- answers <- character(nitems)
#' choices <- matrix(NA, nitems, 5)
#' spacing <- floor(d - min(d)) + 1 #easier items have more variation in the options
#' 
#' for(i in 1:nitems){
#'  n1 <- sample(1:50, 1)
#'  n2 <- sample(51:100, 1)
#'  ans <- n1 + n2
#'  questions[i] <- paste0(n1, ' + ', n2, ' = ?')
#'  answers[i] <- as.character(ans)
#'  ch <- ans + sample(c(-5:-1, 1:5) * spacing[i,], 5)
#'  ch[sample(1:5, 1)] <- ans
#'  choices[i, ] <- as.character(ch)
#' }
#' 
#' df <- data.frame(Question=questions, Option=choices, 
#'               Type = 'radio', stringsAsFactors = FALSE)
#'    
#' response <- generate_pattern(mod, 1)
#' result <- mirtCAT(mo=mod, local_pattern = response, 
#'                   design = list(customNextItem=customNextItem))
#'                 
#' -----------------------------------------------------------
#' # direct manipulation of internal objects
#' CATdesign <- mirtCAT(df=df, mo=mod, criteria = 'MI', design_elements = TRUE)
#'
#' # returns number 1 in this case, since that's the starting item
#' findNextItem(CATdesign)
#'
#' # determine next item if item 1 and item 10 were answered correctly
#' CATdesign <- updateDesign(CATdesign, new_item = 1, new_response = 1)
#' extract.mirtCAT(CATdesign$person, 'thetas') # updated thetas
#' CATdesign <- updateDesign(CATdesign, new_item = 10, new_response = 1)
#' extract.mirtCAT(CATdesign$person, 'thetas') # updated thetas again
#' findNextItem(CATdesign)
#' findNextItem(CATdesign, all_index = TRUE) # all items rank in terms of most optimal
#'
#' #-------------------------------------------------------------
#' ## Integer programming example (e.g., shadow testing)
#'
#' # find maximum information subject to constraints
#' #  sum(xi) <= 5               ### 5 or fewer items
#' #  x1 + x2 <= 1               ### items 1 and 2 can't be together
#' #  x4 == 0                    ### item 4 not included
#' #  x5 + x6 == 1               ### item 5 or 6 must be included, but not both
#'
#' # constraint function
#' constr_fun <- function(design, person, test){
#'
#'   # left hand side constrains
#'   #    - 1 row per constraint, and ncol must equal number of items
#'   mo <- extract.mirtCAT(test, 'mo')
#'   nitems <- extract.mirt(mo, 'nitems')
#'   lhs <- matrix(0, 4, nitems)
#'   lhs[1,] <- 1
#'   lhs[2,c(1,2)] <- 1
#'   lhs[3, 4] <- 1
#'   lhs[4, c(5,6)] <- 1
#'
#'   # relationship direction
#'   dirs <- c("<=", "<=", '==', '==')
#'
#'   #right hand side
#'   rhs <- c(5, 1, 0, 1)
#'
#'   #all together
#'   constraints <- data.frame(lhs, dirs, rhs)
#'   constraints
#' }
#'
#' CATdesign <- mirtCAT(df=df, mo=mod, design_elements = TRUE,
#'                      design = list(constr_fun=constr_fun))
#'
#' # MI criteria value associated with each respective item
#' objective <- computeCriteria(CATdesign, criteria = 'MI')
#'
#' # most optimal item, given constraints
#' findNextItem(CATdesign, objective=objective)
#'
#' # all the items which solve the problem
#' findNextItem(CATdesign, objective=objective, all_index = TRUE)
#'
#' ## within a customNextItem() definition the above code would look like
#' # customNextItem <- function(design, person, test){
#' #   objective <- computeCriteria(person=person, design=design, test=test,
#' #                                criteria = 'MI')
#' #   item <- findNextItem(person=person, design=design, test=test,
#' #                        objective=objective)
#' #   item
#' # }
#'
#' }
findNextItem <- function(x, person = NULL, test = NULL, design = NULL, criteria = NULL,
                         objective = NULL, subset = NULL, all_index = FALSE, ...){
    if(!missing(x)){
        design <- x$design
        person <- x$person
        test <- x$test
    }
    if(any(is.null(person) || is.null(test) || is.null(design)))
        stop('findNextItem has improper inputs', call.=FALSE)
    if(!is.null(criteria))
        design@criteria <- criteria
    if(design@criteria == 'custom' && is.null(objective))
        stop('Please specify a valid selection criteria in findNextItem()', call.=FALSE)
    ret <- if(!is.null(objective)){
        findNextItem.lp(objective, person=person, design=design,
                        test=test, all_index=all_index, ...)
    } else {
       findNextCATItem(person=person, test=test, design=design,
                       subset=subset, all_index=all_index)
    }
    unname(ret)
}

findNextCATItem <- function(person, test, design, subset = NULL, start = TRUE,
                            all_index = FALSE, values = FALSE){

    #heavy lifting CAT stuff just to find new item
    if(all(is.na(person$responses)) && start)
        return(design@start_item)
    lastitem <- sum(!is.na(person$items_answered))
    not_answered <- is.na(person$responses)
    not_answered[!person$valid_item] <- FALSE
    not_answered[design@excluded] <- FALSE
    which_not_answered <- which(not_answered)
    if(is.null(subset)) subset <- 1L:test@length
    which_not_answered <- which_not_answered[which_not_answered %in% subset]
    criteria <- design@criteria
    if(criteria == 'seq')
        which_not_answered <- which_not_answered[which_not_answered > lastitem]
    if(!length(which_not_answered)) stop('Ran out of items to administer.', call.=FALSE)
    K <- test@mo@Data$K
    if(values){
        if(criteria %in% c('seq', 'random'))
            stop('criteria makes no sense with values=TRUE', call.=FALSE)
        which_not_answered <- 1L:test@length
        which_not_answered <- which_not_answered[which_not_answered %in% subset]
        not_answered <- rep(TRUE, length(not_answered))
    }
    if(criteria %in% c('MEI', 'MEPV', 'IKL', 'IKLP', 'IKLn', 'IKLPn')){
        possible_patterns <- matrix(person$responses, sum(K[not_answered]),
                                    length(not_answered), byrow=TRUE)
        row <- 1L
        row_loc <- numeric(nrow(possible_patterns))
        for(ii in which(not_answered)){
            resp <- 0L:(K[ii] - 1L)
            row_loc[row:(row+length(resp)-1L)] <- ii
            for(j in seq_len(length(resp))){
                possible_patterns[row, ii] <- resp[j]
                row <- row + 1L
            }
        }
    }
    method <- design@criteria_estimator
    #safety features
    if(length(unique(na.omit(person$responses))) < 2L) method <- 'MAP'
    if(sum(!is.na(person$responses)) < 5L) method <- 'MAP'
    thetas <- person$thetas

    if(criteria == 'seq'){
        return(min(which_not_answered))
    } else if(criteria == 'random'){
        if(length(which_not_answered) == 1L) item <- which_not_answered
        else item <- sample(which_not_answered, 1L)
        if(design@use_content){
            dif <- design@content_prop - design@content_prop_empirical
            tmp <- names(dif)[which.max(dif)]
            if(length(tmp) > 1L) tmp <- tmp[sample(1L:length(tmp), 1L)]
            cpick <- design@content[which_not_answered]
            if(sum(cpick == tmp) > 1L)
                item <- sample(which_not_answered[cpick == tmp], 1L)
            if(sum(cpick == tmp) == 1L)
                item <- which_not_answered[cpick == tmp]
            #otherwise 0, item does not change
        }
        return(as.integer(item))
    } else if(criteria == 'custom'){
        tmp <- try(design@customNextItem(person=person, design=design, test=test), TRUE)
        if(is(tmp, 'try-error'))
            stop(paste0('customNextItem() returned the following error message:\n\n\t', tmp[1L]))
        if(length(tmp) != 1L)
            stop('customNextItem() must return a single item index or NA to terminate the CAT',
                 call.=FALSE)
        return(tmp)
    }
    index <- which_not_answered
    crit <- if(criteria == 'KL'){
        KL(which_not_answered=which_not_answered,
           person=person, test=test, delta=design@KL_delta, thetas=thetas)
    } else if(criteria == 'KLn'){
        KL(which_not_answered=which_not_answered,
           person=person, test=test, thetas=thetas,
           delta=design@KL_delta*sqrt(sum(!is.na(person$responses))))
    } else if(criteria == 'IKL'){
        IKL(which_not_answered=which_not_answered, possible_patterns=possible_patterns,
            person=person, test=test, row_loc=row_loc, delta=design@KL_delta, thetas=thetas)
    } else if(criteria == 'IKLP'){
        IKL(which_not_answered=which_not_answered, possible_patterns=possible_patterns,
            person=person, test=test, row_loc=row_loc, delta=design@KL_delta,
            den=TRUE, thetas=thetas)
    } else if(criteria == 'IKLn'){
        IKL(which_not_answered=which_not_answered, possible_patterns=possible_patterns,
            person=person, test=test, row_loc=row_loc, thetas=thetas,
            delta=design@KL_delta*sqrt(sum(!is.na(person$responses))))
    } else if(criteria == 'IKLPn'){
        IKL(which_not_answered=which_not_answered, possible_patterns=possible_patterns,
            person=person, test=test, row_loc=row_loc, thetas=thetas,
            delta=design@KL_delta*sqrt(sum(!is.na(person$responses))))
    } else if(criteria == 'MI'){
        MI(which_not_answered=which_not_answered, person=person, test=test, thetas=thetas)
    } else if(criteria == 'MEI'){
        MEI(which_not_answered=which_not_answered, possible_patterns=possible_patterns,
            person=person, test=test, row_loc=row_loc, thetas=thetas)
    } else if(criteria == 'MEPV'){
        -MEPV(which_not_answered=which_not_answered, possible_patterns=possible_patterns,
              person=person, test=test, design=design, row_loc=row_loc, thetas=thetas)
    } else if(criteria == 'MLWI'){
        MLWI(which_not_answered=which_not_answered, person=person, test=test, thetas=thetas)
    } else if(criteria == 'MPWI'){
        MLWI(which_not_answered=which_not_answered, person=person, test=test, thetas=thetas,
             prior=TRUE)
    } else if(criteria == 'Drule' || criteria == 'DPrule'){
        Drule(which_not_answered=which_not_answered, person=person, test=test, thetas=thetas,
              prior = criteria == 'DPrule')
    } else if(criteria == 'Erule' || criteria == 'EPrule'){
        Erule(which_not_answered=which_not_answered, person=person, test=test, thetas=thetas,
              prior = criteria == 'EPrule')
    } else if(criteria == 'Trule' || criteria == 'TPrule'){
        Trule(which_not_answered=which_not_answered, person=person, test=test,
              design=design, thetas=thetas,
              prior = criteria == 'TPrule')
    } else if(criteria == 'Arule' || criteria == 'APrule'){
        -Arule(which_not_answered=which_not_answered,
               person=person, test=test, design=design, thetas=thetas,
               prior = criteria == 'APrule')
    } else if(criteria == 'Wrule' || criteria == 'WPrule'){
        Wrule(which_not_answered=which_not_answered, person=person, test=test,
              design=design, thetas=thetas,
              prior = criteria == 'WPrule')
    } else if(criteria == 'info_mats'){
        InfoMats(which_not_answered=which_not_answered, person=person, test=test, thetas=thetas)
    } else {
        stop('Selection criteria does not exist', call.=FALSE)
    }
    if(values){
        names(crit) <- which_not_answered
        return(crit)
    }
    if(all_index) return(index[order(crit, decreasing = TRUE)])

    if(design@use_content){
        if(sum(!is.na(person$responses)) > 0){
            tmp <- table(design@content[!is.na(person$responses)])
            design@content_prop_empirical <- as.numeric(tmp/sum(tmp))
        }
        dif <- design@content_prop - design@content_prop_empirical
        tmp <- names(dif)[which.max(dif)]
        if(length(tmp) > 1L) tmp <- tmp[sample(1L:length(tmp), 1L)]
        cpick <- design@content[which_not_answered]
        pick <- cpick == tmp
        if(sum(pick) > 0L){
            index <- index[pick]
            crit <- crit[pick]
        }
    }
    if(design@exposure_type != 'none'){
        if(design@exposure_type == 'sample'){
            exposure <- design@exposure[lastitem+1L]
            if(exposure == 1L){
                item <- index[which.max(crit)][1L]
            } else {
                rnk <- length(crit) - rank(crit, ties.method = 'random') + 1L
                pick <- which(rnk %in% 1L:exposure)
                item <- index[sample(pick, 1L)]
            }
        } else if(design@exposure_type == 'SH'){
            while(TRUE){
                item <- index[which.max(crit)][1L]
                comp <- runif(1, 0, 1)
                if(design@exposure[item] >= comp && person$valid_item[item]) break
                if(length(crit) == 1L) break
                person$valid_item[item] <- FALSE
                pick <- index != item
                index <- index[pick]
                crit <- crit[pick]
            }
        }
    } else item <- index[which.max(crit)][1L]
    if(length(design@constraints)){
        pick <- sapply(design@constraints, function(x, item){
            any(item == x)
        }, item=item)
        constr <- design@constraints[pick]
        if(any(names(constr) == 'independent')){
            pick2 <- sapply(constr, c)[, 1L]
            person$valid_item[pick2[pick2 != item]] <- FALSE
        } else if(any(names(constr) == 'ordered')){
            item <- constr[[1L]][1L]
        }
        prev <- last_item(person$items_answered)
        pick <- sapply(design@constraints, function(x, item){
            any(item == x)
        }, item=prev)
        tmp <- design@constraints[pick]$ordered
        constr <- design@constraints[pick]
        if(any(names(constr) == 'ordered')){
            if(any(prev == tmp)){
                tmp2 <- which(tmp == prev) + 1L
                if(tmp2 <= length(tmp)) item <- tmp[tmp2]
            }
        }
    }
    return(as.integer(item))
}

findNextItem.lp <- function(objective, person, design, test, all_index = FALSE, ...){
    stopifnot(is.numeric(objective))
    constr_fun <- design@constr_fun
    nitems <- extract.mirt(test@mo, 'nitems')
    constraints <- constr_fun(person=person, test=test, design=design)
    if(ncol(constraints) != nitems + 2)
        stop('constr_fun() does not have nitem + 2 columns')
    lhs <- as.matrix(constraints[,1L:nitems, drop=FALSE])
    dirs <- as.character(constraints[,nitems+1L])
    rhs <- constraints[,nitems+2L]
    resp <- as.numeric(!is.na(person$responses))
    items_answered <- person$items_answered[1L:sum(resp)]
    objective2 <- objective
    objective[items_answered] <- objective[items_answered] * resp[items_answered]
    lhs <- rbind(lhs, resp)
    dirs <- c(dirs, "==")
    rhs <- c(rhs, sum(resp))
    out <- lp(direction = 'max', objective, const.mat=lhs,
              const.dir=dirs, const.rhs=rhs, all.bin = TRUE)
    if(out$status != 0L)
        stop('lp() solver could not find solution', call.=FALSE)
    solution <- out$solution
    if(all_index){
        return(which(solution == 1L))
    } else {
        solution[items_answered] <- 0
        ret <- if(sum(solution) == 0) NA else which.max(solution * objective2)
        return(ret)
    }
}