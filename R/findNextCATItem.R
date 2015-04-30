#' Find next CAT item
#' 
#' A function that returns the next item in the computerized adaptive test. This should be used
#' in conjunction with the \code{\link{updateDesign}} function.
#' 
#' @param x an object of class 'mirtCAT_design' returned from the \code{\link{mirtCAT}} function
#'   when passing \code{design_elements = TRUE}
#'   
#' @param ... additional arguments to pass
#' 
#' @seealso \code{\link{mirtCAT}}, \code{\link{updateDesign}}
#' @export findNextItem
#' @author Phil Chalmers \email{rphilip.chalmers@@gmail.com}     
#' @return returns a numeric value indicating the index of the next item to be selected
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
findNextItem <- function(x){
    if(class(x) != 'mirtCAT_design')
        stop('input is not the correct class', call.=FALSE)
    return(findNextCATItem(person=x$person, test=x$test, design=x$design))
}

findNextCATItem <- function(person, test, design, start = TRUE){
    
    #heavy lifty CAT stuff just to find new item
    criteria <- design@criteria
    if(all(is.na(person$responses)) && start)
        return(design@start_item)
    lastitem <- sum(!is.na(person$items_answered))
    not_answered <- is.na(person$responses)
    not_answered[!person$valid_item] <- FALSE
    which_not_answered <- which(not_answered)
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
        return(as.integer(lastitem + 1L))
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
    } else if(criteria == 'KL'){
        crit <- KL(which_not_answered=which_not_answered, 
                   person=person, test=test, delta=design@KL_delta, thetas=thetas)
        index <- which_not_answered
    } else if(criteria == 'KLn'){
            crit <- KL(which_not_answered=which_not_answered, 
                       person=person, test=test, thetas=thetas,
                       delta=design@KL_delta*sqrt(sum(!is.na(person$responses))))
            index <- which_not_answered
    } else if(criteria == 'IKL'){
        crit <- IKL(which_not_answered=which_not_answered, possible_patterns=possible_patterns,
                   person=person, test=test, row_loc=row_loc, delta=design@KL_delta, thetas=thetas)
        index <- which_not_answered
    } else if(criteria == 'IKLP'){
            crit <- IKL(which_not_answered=which_not_answered, possible_patterns=possible_patterns,
                        person=person, test=test, row_loc=row_loc, delta=design@KL_delta,
                        den=TRUE, thetas=thetas)
            index <- which_not_answered
    } else if(criteria == 'IKLn'){
        crit <- IKL(which_not_answered=which_not_answered, possible_patterns=possible_patterns,
                   person=person, test=test, row_loc=row_loc, thetas=thetas,
                   delta=design@KL_delta*sqrt(sum(!is.na(person$responses))))
        index <- which_not_answered
    } else if(criteria == 'IKLPn'){
        crit <- IKL(which_not_answered=which_not_answered, possible_patterns=possible_patterns,
                    person=person, test=test, row_loc=row_loc, thetas=thetas,
                    delta=design@KL_delta*sqrt(sum(!is.na(person$responses))))
        index <- which_not_answered
    } else if(criteria == 'MI'){
        crit <- MI(which_not_answered=which_not_answered, person=person, test=test, thetas=thetas)
        index <- which_not_answered
    } else if(criteria == 'MEI'){
        crit <- MEI(which_not_answered=which_not_answered, possible_patterns=possible_patterns,
                    person=person, test=test, row_loc=row_loc, thetas=thetas)
        index <- which_not_answered
    } else if(criteria == 'MEPV'){
        crit <- -MEPV(which_not_answered=which_not_answered, possible_patterns=possible_patterns,
                    person=person, test=test, design=design, row_loc=row_loc, thetas=thetas)
        index <- which_not_answered
    } else if(criteria == 'MLWI'){
        crit <- MLWI(which_not_answered=which_not_answered, possible_patterns=possible_patterns,
                     person=person, test=test, row_loc=row_loc, thetas=thetas)
        index <- which_not_answered
    } else if(criteria == 'MPWI'){
        crit <- MPWI(which_not_answered=which_not_answered, possible_patterns=possible_patterns,
                     person=person, test=test, row_loc=row_loc, thetas=thetas)
        index <- which_not_answered
    } else if(criteria == 'Drule' || criteria == 'DPrule'){
        crit <- Drule(which_not_answered=which_not_answered, person=person, test=test, 
                      thetas=thetas)
        index <- which_not_answered
    } else if(criteria == 'Erule' || criteria == 'EPrule'){
        crit <- Erule(which_not_answered=which_not_answered, person=person, test=test, 
                      thetas=thetas)
        index <- which_not_answered
    } else if(criteria == 'Trule' || criteria == 'TPrule'){
        crit <- Trule(which_not_answered=which_not_answered, person=person, test=test, 
                      design=design, thetas=thetas)
        index <- which_not_answered
    } else if(criteria == 'Arule' || criteria == 'APrule'){
        crit <- -Arule(which_not_answered=which_not_answered, 
                      person=person, test=test, design=design, thetas=thetas)
        index <- which_not_answered
    } else if(criteria == 'Wrule' || criteria == 'WPrule'){
        crit <- Wrule(which_not_answered=which_not_answered, person=person, test=test,
                      design=design, thetas=thetas)
        index <- which_not_answered
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