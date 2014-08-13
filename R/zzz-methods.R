#' @rdname mirtCAT
#' @method print mirtCAT
#' @param x object of class \code{'mirtCAT'}
#' @export
print.mirtCAT <- function(x, ...){
    if(!all(is.na(x$thetas))){
        person <- c(sum(!is.na(x$responses)),
                          x$thetas[1L,],
                          x$thetas_SE_history[nrow(x$thetas_SE_history),])        
        names(person) <- c('n.items.answered', paste0('Theta_', 1:length(x$thetas)),
                           paste0('SE.Theta_', 1:length(x$thetas)))
        ret <- t(as.data.frame(person))
        rownames(ret) <- ''
        return(ret)
    } else {
        return(data.frame('n.items.answered' = sum(!is.na(x$responses))))
    }
}

#' @rdname mirtCAT
#' @method summary mirtCAT
#' @param object object of class \code{'mirtCAT'}
#' @param sort logical; sort the response patterns based on the order they 
#'   were administered? If FALSE, the raw response patterns containing NAs will be returned
#'   for items that were not administered
#' @export
summary.mirtCAT <- function(object, sort = TRUE, ...){
    pick <- if(sort){
        object$items_answered
    } else 1L:length(object$raw_responses)
    raw_responses <- object$raw_responses[pick]
    responses <- object$responses[pick]
    ret <- list(raw_responses=raw_responses,
                responses=responses,
                items_answered=object$items_answered,
                thetas_history=object$thetas_history, 
                thetas_SE_history=object$thetas_SE_history,
                demographics=object$demographics)
    if(length(object$item_time))
        ret$item_time <- object$item_time[pick]
    if(length(ret$thetas_history) == 1L || is.na(ret$thetas_history))
        ret$thetas_history <- ret$thetas_SE_history <- NULL
    if(all(ret$raw_responses == ret$responses, na.rm = TRUE))
        ret$raw_responses <- NULL
    if(!length(object$demographics))
        ret$demographics <- NULL
    ret
}

#' @rdname mirtCAT
#' @method plot mirtCAT
#' @param pick_theta a number indicating which theta to plot (only applicable for multidimensional 
#'   tests). The default is to facet each theta on one plot, but to plot only the first factor pass
#'   \code{pick_theta = 1}   
#' @param main title of the plot. Will default to \code{'CAT Standard Errors'} or 
#'   \code{'CAT ##\% Confidence Intervals'} depending on the SE input
#' @param SE size of the standard errors to plot. The default is 1, and therefore plots the
#'   standard error. To obtain the 95\% interval use \code{SE = 1.96} (from the z-distribution)
#' @param ... additional arguments to be passed to \code{lattice}
#' @export
plot.mirtCAT <- function(x, pick_theta = NULL, SE = 1, main = NULL, ...){
    if(length(x$thetas_SE_history) == 1L || is.na(x$thetas_SE_history))
        stop('plot not available for non-adaptive tests')
    p <- floor((1-(pnorm(-abs(SE))*2))*100)
    if(is.null(main)){
        if(SE == 1)
            main <- 'CAT Standard Errors'
        else main <- paste0('CAT ', p, '% Confidence Intervals')
    }
    nfact <- ncol(x$thetas)
    thetas <- data.frame(x$thetas_history)
    thetasSEhigh <- data.frame(thetas + x$thetas_SE_history*SE)
    thetasSElow <- data.frame(thetas - x$thetas_SE_history*SE)
    labels <- paste0('Theta_', 1L:nfact)
    if(!is.null(pick_theta) && nfact > 1L){
        nfact <- 1L
        labels <- labels[pick_theta]
        thetas <- thetas[,pick_theta, drop=FALSE]
        thetasSEhigh <- thetasSEhigh[,pick_theta, drop=FALSE]
        thetasSElow <- thetasSElow[,pick_theta, drop=FALSE]
    }
    thetaslong <- reshape(thetas, varying = list(colnames(thetas)), idvar = 'id', 
                          direction = 'long', timevar = 'thetas')
    thetaslong$thetas <- factor(thetaslong$thetas, labels = labels)
    thetasSElowlong <- reshape(thetasSElow, varying = list(colnames(thetasSElow)), idvar = 'id', 
                               direction = 'long', timevar = 'thetasSE')
    thetasSEhighlong <- reshape(thetasSEhigh, varying = list(colnames(thetasSEhigh)), idvar = 'id', 
                                direction = 'long', timevar = 'thetasSE')
    colnames(thetaslong) <- colnames(thetasSElowlong) <- colnames(thetasSEhighlong) <-
        c('thetas', 'F1', 'id')
    tmp <- x$items_answered
    tmp <- rep(c(0,tmp[!is.na(tmp)]), nfact)
    thetaslong$item <- factor(tmp, levels = unique(tmp))
    if(nfact > 1L){
        return(xyplot(F1 ~ item|thetas, data=thetaslong, 
                      main = main,
                      lower = thetasSElowlong$F1,
                      upper = thetasSEhighlong$F1,
                      panel = function(x, y, lower, upper, subscripts, ...){
                          lower <- lower[subscripts]; upper <- upper[subscripts]
                          panel.polygon(c(x, rev(x)), c(upper, rev(lower)), 
                                        col=grey(.9), border = FALSE, ...)
                          panel.points(x, y, ...)
                          panel.lines(x, y, ...)
                      },
                      ylim=c(min(thetasSElowlong$F1)-.1, max(thetasSEhighlong$F1)+.1),
                      ylab = expression(theta), 
                      xlab = 'Item', ...))
        
    } else {
        return(xyplot(F1 ~ item, data=thetaslong, 
                      main = main,
                      lower = thetasSElowlong$F1,
                      upper = thetasSEhighlong$F1,
                      panel = function(x, y, lower, upper, subscripts, ...){
                          panel.polygon(c(x, rev(x)), c(upper, rev(lower)), 
                                        col=grey(.9), border = FALSE, ...)
                          panel.points(x, y, ...)
                          panel.lines(x, y, ...)
                      },
                      ylim=c(min(thetasSElowlong$F1)-.1, max(thetasSEhighlong$F1)+.1),
                      ylab = expression(theta), 
                      xlab = 'Item', ...))
    }
}