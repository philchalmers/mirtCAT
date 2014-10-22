#' Find next CAT item
#' 
#' A function that returns the next item in the computerized adaptive test. 
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
#' CATdesign <- mirtCAT(shiny_questions, mod, item_answers=answers, criteria = 'MI', 
#'    design_elements = TRUE)
#' 
#' # returns number 1 in this case, since that's the starting item
#' findNextItem(CATdesign) 
#' 
#' # detemine next item if item 1 and item 10 were answered correctly, and Theta = 0.5
#' CATdesign <- updateDesign(CATdesign, items = c(1, 10), responses = c(1, 1), Theta = 0.5)
#' findNextItem(CATdesign) 
#' }
findNextItem <- function(x){
    if(class(x) != 'mirtCAT_design')
        stop('input is not the correct class')
    return(findNextCATItem(person=x$person, test=x$test, design=x$design))
}

#' @rdname findNextItem
#' @export
print.mirtCAT_design <- function(x, ...){ 
   cat('Object of class \'mirtCAT_design\'\n')
}

findNextCATItem <- function(person, test, design){
    
    #heavy lifty CAT stuff just to find new item
    criteria <- design$criteria
    if(all(is.na(person$responses)))
        return(design$start_item)
    lastitem <- sum(!is.na(person$items_answered))
    not_answered <- is.na(person$responses)
    which_not_answered <- which(not_answered)
    K <- test$mirt_object@Data$K
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
    method <- design$criteria_estimator
    #saftey features
    if(length(unique(na.omit(person$responses))) < 2L) method <- 'MAP'
    if(sum(!is.na(person$responses)) < 5L) method <- 'MAP'
    if(design$use_content){
        tmp <- table(design$content[!is.na(person$responses)])
        design$content_prop_empirical <- as.numeric(tmp/sum(tmp))
    }
    
    if(criteria == 'seq'){
        return(as.integer(lastitem + 1L))
    } else if(criteria == 'random'){
        if(length(which_not_answered) == 1L) item <- which_not_answered
        else item <- sample(which_not_answered, 1L)
        if(design$use_content){
            dif <- design$content_prop - design$content_prop_empirical
            tmp <- names(dif)[max(dif) == dif]
            if(length(tmp) > 1L) tmp <- tmp[sample(1L:length(tmp), 1L)]
            cpick <- design$content[which_not_answered]
            if(sum(cpick == tmp) > 1L)
                item <- sample(which_not_answered[cpick == tmp], 1L)
            if(sum(cpick == tmp) == 1L)
                item <- which_not_answered[cpick == tmp]
            #otherwise 0, item does not change
        }
        return(as.integer(item))
    } else if(criteria == 'KL'){
        crit <- KL(which_not_answered=which_not_answered, possible_patterns=possible_patterns,
                   person=person, test=test, row_loc=row_loc, delta=design$KL_delta)
        index <- which_not_answered
    } else if(criteria == 'KLn'){
            crit <- KL(which_not_answered=which_not_answered, possible_patterns=possible_patterns,
                       person=person, test=test, row_loc=row_loc, 
                       delta=design$KL_delta*sqrt(sum(!is.na(person$responses))))
            index <- which_not_answered
    } else if(criteria == 'IKL'){
        crit <- IKL(which_not_answered=which_not_answered, possible_patterns=possible_patterns,
                   person=person, test=test, row_loc=row_loc, delta=design$KL_delta)
        index <- which_not_answered
    } else if(criteria == 'IKLP'){
            crit <- IKL(which_not_answered=which_not_answered, possible_patterns=possible_patterns,
                        person=person, test=test, row_loc=row_loc, delta=design$KL_delta,
                        den=TRUE)
            index <- which_not_answered
    } else if(criteria == 'IKLn'){
        crit <- IKL(which_not_answered=which_not_answered, possible_patterns=possible_patterns,
                   person=person, test=test, row_loc=row_loc, 
                   delta=design$KL_delta*sqrt(sum(!is.na(person$responses))))
        index <- which_not_answered
    } else if(criteria == 'IKLPn'){
        crit <- IKL(which_not_answered=which_not_answered, possible_patterns=possible_patterns,
                    person=person, test=test, row_loc=row_loc, 
                    delta=design$KL_delta*sqrt(sum(!is.na(person$responses))))
        index <- which_not_answered
    } else if(criteria == 'MI'){
        crit <- MI(which_not_answered=which_not_answered, possible_patterns=possible_patterns,
                   person=person, test=test, row_loc=row_loc)
        index <- which_not_answered
    } else if(criteria == 'MEI'){
        crit <- MEI(which_not_answered=which_not_answered, possible_patterns=possible_patterns,
                    person=person, test=test, row_loc=row_loc)
        index <- which_not_answered
    } else if(criteria == 'MEPV'){
        crit <- -MEPV(which_not_answered=which_not_answered, possible_patterns=possible_patterns,
                    person=person, test=test, row_loc=row_loc)
        index <- which_not_answered
    } else if(criteria == 'MLWI'){
        crit <- MLWI(which_not_answered=which_not_answered, possible_patterns=possible_patterns,
                     person=person, test=test, row_loc=row_loc)
        index <- which_not_answered
    } else if(criteria == 'MPWI'){
        crit <- MPWI(which_not_answered=which_not_answered, possible_patterns=possible_patterns,
                     person=person, test=test, row_loc=row_loc)
        index <- which_not_answered
    } else if(criteria == 'Drule' || criteria == 'DPrule'){
        if(design$numerical_info){
            crit <- -Drule2(which_not_answered=which_not_answered, possible_patterns=possible_patterns,
                          person=person, test=test, row_loc=row_loc, method=method)
            index <- row_loc
        } else {
            crit <- Drule(which_not_answered=which_not_answered, possible_patterns=possible_patterns,
                   person=person, test=test, row_loc=row_loc, method=method)
            index <- which_not_answered
        }
    } else if(criteria == 'Erule' || criteria == 'EPrule'){
        if(design$numerical_info){
            crit <- -Erule2(which_not_answered=which_not_answered, possible_patterns=possible_patterns,
                          person=person, test=test, row_loc=row_loc, method=method)
            index <- row_loc
        } else {
            crit <- Erule(which_not_answered=which_not_answered, possible_patterns=possible_patterns,
                          person=person, test=test, row_loc=row_loc, method=method)
            index <- which_not_answered
        }
    } else if(criteria == 'Trule' || criteria == 'TPrule'){
        if(design$numerical_info){
            crit <- Trule2(which_not_answered=which_not_answered, possible_patterns=possible_patterns,
                          person=person, test=test, row_loc=row_loc, method=method, design=design)
            index <- row_loc
        } else {
            crit <- Trule(which_not_answered=which_not_answered, possible_patterns=possible_patterns,
                          person=person, test=test, row_loc=row_loc, method=method, design=design)
            index <- which_not_answered
        }
    } else if(criteria == 'Arule' || criteria == 'APrule'){
        crit <- -Arule(which_not_answered=which_not_answered, possible_patterns=possible_patterns,
                      person=person, test=test, row_loc=row_loc, method=method, design=design)
        index <- which_not_answered
    } else if(criteria == 'Wrule' || criteria == 'WPrule'){
        if(design$numerical_info){
            crit <- Wrule2(which_not_answered=which_not_answered, possible_patterns=possible_patterns,
                          person=person, test=test, row_loc=row_loc, method=method, design=design)
            index <- row_loc
        } else {
            crit <- Wrule(which_not_answered=which_not_answered, possible_patterns=possible_patterns,
                          person=person, test=test, row_loc=row_loc, method=method, design=design)
            index <- which_not_answered
        }
    } else {
        stop('Selection criteria does not exist')
    }
    
    exposure <- design$exposure[lastitem+1L]    
    if(design$use_content){
        dif <- design$content_prop - design$content_prop_empirical
        tmp <- names(dif)[max(dif) == dif]
        if(length(tmp) > 1L) tmp <- tmp[sample(1L:length(tmp), 1L)]
        cpick <- design$content[which_not_answered]
        pick <- cpick == tmp
        if(sum(pick) > 0L){            
            index <- index[pick]
            crit <- crit[pick]
            exposure <- min(design$exposure[lastitem+1L], sum(pick))
        }
    }
    if(exposure == 1L){
        item <- index[which(max(crit) == crit)][1L]
    } else {
        rnk <- rank(crit, ties.method = 'random')
        pick <- which(rnk %in% 1L:exposure)
        item <- index[sample(pick, 1L)]
    }
    return(as.integer(item))
}