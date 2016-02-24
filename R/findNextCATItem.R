#' Find next CAT item
#' 
#' A function that returns the next item in the computerized adaptive test. This should be used
#' in conjunction with the \code{\link{updateDesign}} function. The raw input forms can be used
#' when a \code{customNextItem} function has been defined in \code{\link{mirtCAT}}.
#' 
#' @param x an object of class 'mirtCAT_design' returned from the \code{\link{mirtCAT}} function
#'   when passing \code{design_elements = TRUE}
#'   
#' @param person internal person object. To be used when \code{customNextItem} function has been 
#'   defined 
#' 
#' @param design internal design object. To be used when \code{customNextItem} function has been 
#'   defined 
#' 
#' @param test internal test object. To be used when \code{customNextItem} function has been 
#'   defined 
#' 
#' @param criteria item selection criteria (see \code{\link{mirtCAT}}'s \code{criteria} input). 
#'   To be used when \code{customNextItem} function has been defined 
#'   
#' @param subset an integer vector indicating which items should be included in the optimal search;
#'   the default \code{NULL} includes all possible items. To allow only the first 10 items to be 
#'   selected from this can be modified to \code{subset = 1:10}. This is useful when administering 
#'   a multi-unidimensional CAT session where unidimensional blocks should be clustered together 
#'   for smoother presentation. Useful when using the \code{customNextItem} function in 
#'   \code{\link{mirtCAT}}
#' 
#' @seealso \code{\link{mirtCAT}}, \code{\link{updateDesign}}
#' @export findNextItem
#' @author Phil Chalmers \email{rphilip.chalmers@@gmail.com}     
#' @return returns an integer value indicating the index of the next item to be selected or a
#'   value of \code{NA} to indicate that the test should be terminated 
#'   
#' @examples
#' \dontrun{
#' # test defined in mirtCAT help file, first example
#' CATdesign <- mirtCAT(df, mod, criteria = 'MI', design_elements = TRUE)
#' 
#' # returns number 1 in this case, since that's the starting item
#' findNextItem(CATdesign) 
#' 
#' # detemine next item if item 1 and item 10 were answered correctly, and Theta = 0.5
#' CATdesign <- updateDesign(CATdesign, items = c(1, 10), responses = c(1, 1), Theta = 0.5)
#' findNextItem(CATdesign)
#' 
#' # alternatively, update the Theta using the internal ReferenceClass method
#' Person$help('Update.thetas') # internal help file for class 'Person'
#' CATdesign$person$Update.thetas(CATdesign$design, CATdesign$test) 
#' findNextItem(CATdesign)
#' }
findNextItem <- function(x, person = NULL, test = NULL, design = NULL, criteria = NULL,
                         subset = NULL){
    if(missing(x)){
        if(any(is.null(person) || is.null(test) || is.null(design) || is.null(criteria)))
            stop('findNextItem has improper inputs', call.=FALSE)
        return(findNextCATItem(person=person, test=test, design=design, criteria=criteria,
                               subset=subset))
    } else {
        if(class(x) != 'mirtCAT_design')
            stop('input is not the correct class', call.=FALSE)
        return(findNextCATItem(person=x$person, test=x$test, design=x$design, 
                               criteria = x$design@criteria, subset=subset))
    }
}

findNextCATItem <- function(person, test, design, criteria, subset = NULL, start = TRUE){
    
    #heavy lifty CAT stuff just to find new item
    if(all(is.na(person$responses)) && start)
        return(design@start_item)
    lastitem <- sum(!is.na(person$items_answered))
    not_answered <- is.na(person$responses)
    not_answered[!person$valid_item] <- FALSE
    not_answered[design@excluded] <- FALSE
    which_not_answered <- which(not_answered)
    if(is.null(subset)) subset <- 1L:test@length
    which_not_answered <- which_not_answered[which_not_answered %in% subset]
    if(criteria == 'seq')
        which_not_answered <- which_not_answered[which_not_answered > lastitem]
    if(!length(which_not_answered)) stop('Ran out of items to administer.', call.=FALSE)
    K <- test@mo@Data$K
    if(criteria %in% c('MEI', 'MEPV', 'MLWI', 'MPWI', 'IKL', 'IKLP', 'IKLn', 'IKLPn')){
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
    #saftey features
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
            tmp <- names(dif)[max(dif) == dif]
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
        return(as.integer(design@customNextItem(person=person, design=design, test=test)))
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
        MLWI(which_not_answered=which_not_answered, possible_patterns=possible_patterns,
             person=person, test=test, row_loc=row_loc, thetas=thetas)
    } else if(criteria == 'MPWI'){
        MPWI(which_not_answered=which_not_answered, possible_patterns=possible_patterns,
             person=person, test=test, row_loc=row_loc, thetas=thetas)
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
    
    if(design@use_content){
        if(sum(!is.na(person$responses)) > 0){
            tmp <- table(design@content[!is.na(person$responses)])
            design@content_prop_empirical <- as.numeric(tmp/sum(tmp))
        }
        dif <- design@content_prop - design@content_prop_empirical
        tmp <- names(dif)[max(dif) == dif]
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
                item <- index[which(max(crit) == crit)][1L]
            } else {
                rnk <- rank(crit, ties.method = 'random')
                pick <- which(rnk %in% 1L:exposure)
                item <- index[sample(pick, 1L)]
            }
        } else if(design@exposure_type == 'SH'){
            while(TRUE){
                item <- index[which(max(crit) == crit)][1L]
                comp <- runif(1, 0, 1)
                if(design@exposure[item] >= comp && person$valid_item[item]) break
                if(length(crit) == 1L) break 
                person$valid_item[item] <- FALSE
                pick <- index != item
                index <- index[pick]
                crit <- crit[pick]
            }
        }
    } else item <- index[which(max(crit) == crit)][1L]
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