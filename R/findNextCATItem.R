#' Find next CAT item
#' 
#' A function that returns the next item in the computerized adaptive test. This should be used
#' in conjunction with the \code{\link{updateDesign}} function. The raw input forms can be used
#' when a \code{customNextItem} function has been defined in \code{\link{mirtCAT}}.
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
#' @param values logical; return the raw values associated with each item 
#'   instead of the rank ordering of the items (or the default most optimal item) when an adaptive 
#'   criteria is selected? Note that criteria values are returned such that the maximum value always 
#'   represents the most optimal item (e.g., maximum information). In cases where the minimum value is 
#'   typically selected (e.g., minimum variance) all values are multiplied by -1 to turn it into a maximization
#'   problem. Note that this over-rides all values from
#'   \code{all_index} and \code{subset}
#' 
#' @seealso \code{\link{mirtCAT}}, \code{\link{updateDesign}}, \code{\link{extract.mirtCAT}}
#' @export findNextItem
#' @author Phil Chalmers \email{rphilip.chalmers@@gmail.com}     
#' @return typically returns an integer value indicating the index of the next item to be selected or a
#'   value of \code{NA} to indicate that the test should be terminated. However, see the arguments for 
#'   further returned object descriptions
#'   
#' @examples
#' \dontrun{
#' # test defined in mirtCAT help file, first example
#' CATdesign <- mirtCAT(df, mod, criteria = 'MI', design_elements = TRUE)
#' 
#' # returns number 1 in this case, since that's the starting item
#' findNextItem(CATdesign)
#' 
#' # determine next item if item 1 and item 10 were answered correctly, and Theta = 0.5
#' CATdesign <- updateDesign(CATdesign, items = c(1, 10), responses = c(1, 1), Theta = 0.5)
#' findNextItem(CATdesign)
#' findNextItem(CATdesign, all_index = TRUE) # all items rank in terms of most optimal
#' 
#' # alternatively, update the Theta using the internal ReferenceClass method
#' Person$help('Update.thetas') # internal help file for class 'Person'
#' CATdesign$person$Update.thetas(CATdesign$design, CATdesign$test) 
#' findNextItem(CATdesign)
#' 
#' # criteria value associated with each item
#' findNextItem(CATdesign, values = TRUE)
#' }
findNextItem <- function(x, person = NULL, test = NULL, design = NULL, criteria = NULL,
                         subset = NULL, all_index = FALSE, values = FALSE){
    if(!missing(x)){
        design <- x$design
        person <- x$person
        test <- x$test
    }
    if(any(is.null(person) || is.null(test) || is.null(design)))
        stop('findNextItem has improper inputs', call.=FALSE)
    if(!is.null(criteria))
        design@criteria <- criteria
    if(design@criteria == 'custom')
        stop('Please specify a valid selection criteria in findNextItem()', call.=FALSE)
    return(findNextCATItem(person=person, test=test, design=design,
                           subset=subset, all_index=all_index, values=values))
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
            for(j in 1L:length(resp)){
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
        Drule(which_not_answered=which_not_answered, person=person, test=test, thetas=thetas)
    } else if(criteria == 'Erule' || criteria == 'EPrule'){
        Erule(which_not_answered=which_not_answered, person=person, test=test, thetas=thetas)
    } else if(criteria == 'Trule' || criteria == 'TPrule'){
        Trule(which_not_answered=which_not_answered, person=person, test=test, 
              design=design, thetas=thetas)
    } else if(criteria == 'Arule' || criteria == 'APrule'){
        -Arule(which_not_answered=which_not_answered, 
               person=person, test=test, design=design, thetas=thetas)
    } else if(criteria == 'Wrule' || criteria == 'WPrule'){
        Wrule(which_not_answered=which_not_answered, person=person, test=test,
              design=design, thetas=thetas)
    } else {
        stop('Selection criteria does not exist', call.=FALSE)
    }
    if(values) return(crit)
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
                rnk <- rank(crit, ties.method = 'random')
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

#' Find next CAT item with linear (or integer) solver
#' 
#' A function that returns the next item in the computerized adaptive test which is found via 
#' an integer solver through searching for a maximum. The raw input forms can be used
#' when a \code{customNextItem} function has been defined in \code{\link{mirtCAT}}. This function
#' can be used to for 'Optimal Test Assembly', as well as 'Shadow Testing' designs (van der Linden, 2005),
#' by using the \code{\link{lp}} function.
#'
#' @param objective a vector of values used as the optimization criteria to be passed to 
#'   \code{lp(objective.in)}. This is typically the vector of criteria values returned from
#'   \code{\link{findNextItem}} (with the \code{values = TRUE} input), however supplying other
#'   criteria are possible (e.g., to minimize the number of items administered simply pass a vector
#'   of -1's) 
#'   
#' @param constr_fun a user-defined function of the form \code{function(person, test, design){...}} 
#'   that returns a \code{data.frame} containing the left hand side, relationship, and right hand side
#'   of the constraints. Each row corresponds to a constraint, while the number of columns should be 
#'   equal to the number of items plus 2. 
#'   
#'   For example, in a test with the constraint that exactly 10 items 
#'   should be administered to all participants, the input should be defined as 
#'   \preformatted{const_fun <- function(person, test, design){
#'      nitems <- extract.mirt(test@@mo, 'nitems')
#'      data.frame(item=t(rep(1, nitems)), relation="==", value=10)
#'    }}
#'    Note that the column names of the returned \code{data.frame} object do not matter.
#'   
#' @param CATdesign an object of class 'mirtCAT_design' returned from the \code{\link{mirtCAT}} function
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
#' @param all_index logical; return all the items which solve the optimization problem? Note that 
#'   this also includes items which have previously been responsed to
#'   
#' @param ... additional arguments to be passed to \code{\link{lp}}
#' 
#' @seealso \code{\link{mirtCAT}}, \code{\link{findNextItem}}, 
#'   \code{\link{updateDesign}}, \code{\link{extract.mirtCAT}}
#' @export 
#' @author Phil Chalmers \email{rphilip.chalmers@@gmail.com}     
#' @references 
#' van der Linden, W. J. (2005). Linear models for optimal test design. Springer.
#' 
#' @return typically returns an integer value indicating the index of the next item to be selected or a
#'   value of \code{NA} to indicate that the test should be terminated. However, see the arguments for 
#'   further returned object descriptions
#'   
#' @examples
#' \dontrun{
#' 
#' #-------------------------------------------------------------
#' # find maximum information subject to constraints
#' #  sum(xi) <= 5              ### 5 or fewer items
#' #  x1 + x2 <= 1              ### items 1 and 2 can't be together
#' #  x4 == 0                   ### item 4 not included
#' 
#' # MI criteria value associated with each respective item
#' objective <- findNextItem(CATdesign, criteria = 'MI', values = TRUE)
#' 
#' # constraint function
#' constr_fun <- function(person, test, design){
#'
#'   # left hand side constrains 
#'   #    - 1 row per constraint, and ncol must equal number of items
#'   nitems <- extract.mirt(test@mo, 'nitems')
#'   lhs <- matrix(0, 3, nitems)
#'   lhs[1,] <- 1
#'   lhs[2,c(1,2)] <- 1
#'   lhs[3, 4] <- 1
#'   
#'   # relationship direction
#'   dirs <- c("<=", "<=", '==')
#'   
#'   #right hand side
#'   rhs <- c(5, 1, 0)
#' 
#'   #all together
#'   constraints <- data.frame(lhs, dirs, rhs)
#'   constraints
#' }
#' 
#' 
#' # most optimal item, given constraints
#' findNextItem.lp(objective, constr_fun, CATdesign)
#' 
#' # all the items which solve the problem
#' findNextItem.lp(objective, constr_fun, CATdesign, all_index = TRUE)
#' 
#' ## within a customNextItem() definition the above code would look like
#' # customNextItem <- function(person, design, test){
#' #   objective <- findNextItem(person=person, design=design, test=test, 
#' #                             values=TRUE) 
#' #   item <- findNextItem.lp(objective, constr_fun, 
#' #                           person=person, design=design, test=test)
#' #   item
#' # }
#' 
#' }
findNextItem.lp <- function(objective, constr_fun, CATdesign, person = NULL, 
                            design = NULL, test = NULL, all_index = FALSE, ...){
    if(!missing(CATdesign)){
        design <- CATdesign$design
        person <- CATdesign$person
        test <- CATdesign$test
    }
    if(any(is.null(person) || is.null(test) || is.null(design)))
        stop('findNextItem.lp has improper inputs', call.=FALSE)
    stopifnot(is.numeric(objective))
    stopifnot(is.function(constr_fun))
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