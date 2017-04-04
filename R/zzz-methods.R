#' @rdname mirtCAT
#' @method print mirtCAT
#' @param x object of class \code{'mirtCAT'}
#' @export
print.mirtCAT <- function(x, ...){
    if(!all(is.na(x$thetas))){
        person <- c(sum(!is.na(x$raw_responses)),
                          x$thetas[1L,],
                          x$SE_thetas[1L,])
        names(person) <- c('n.items.answered', paste0('Theta_', 1:length(x$thetas)),
                           paste0('SE.Theta_', 1:length(x$thetas)))
        if(!is.null(x$true_thetas)){
            nms <- names(person)
            person <- c(person, x$true_thetas)
            names(person) <- c(nms, paste0('True.Theta_', 1:length(x$thetas)))
        } 
        ret <- t(as.data.frame(person))
        rownames(ret) <- ''
        print(ret)
    } else {
        print(data.frame('n.items.answered' = sum(!is.na(x$raw_responses))))
    }
    invisible()
}

#' @rdname mirtCAT
#' @method summary mirtCAT
#' @param object object of class \code{'mirtCAT'}
#' @param sort logical; sort the response patterns based on the order they 
#'   were administered? If FALSE, the raw response patterns containing NAs will be returned
#'   for items that were not administered
#' @export
summary.mirtCAT <- function(object, sort = TRUE, ...){
    if(!all(is.na(object$thetas))){
        person <- rbind(object$thetas[1L,], object$SE_thetas)
        rownames(person) <- c('Estimates', 'SEs')
    } else person <- NULL
    pick <- if(sort){
        object$items_answered
    } else 1L:length(object$raw_responses)
    raw_responses <- object$raw_responses[pick]
    scored_responses <- object$scored_responses[pick]
    ret <- list(login_name=object$login_name,
                final_estimates=person,
                raw_responses=raw_responses,
                scored_responses=scored_responses,
                items_answered=object$items_answered,
                thetas_history=object$thetas_history, 
                thetas_SE_history=object$thetas_SE_history,
                demographics=object$demographics)
    if(object$GUI) ret$terminated_sucessfully <- object$terminated_sucessfully
    if(!is.null(object$true_thetas)) ret$true_thetas <- object$true_thetas
    if(!length(object$login_name)) ret$login_name <- NULL
    if(is.null(person)) ret$final_estimates <- NULL
    if(all(is.na(scored_responses))) ret$scored_responses <- NULL
    if(sum(object$item_time) > 0)
        ret$item_time <- object$item_time[pick]
    if(length(ret$thetas_history) == 1L || is.na(ret$thetas_history))
        ret$thetas_history <- ret$thetas_SE_history <- NULL
    if(!length(object$demographics))
        ret$demographics <- NULL
    if(!is.null(object$classification)){
        ret$classification <- as.character(object$classification)
        names(ret$classification) <- colnames(ret$thetas_history)
    }
    ret
}

#' @rdname mirtCAT
#' @method plot mirtCAT
#' @param pick_theta a number indicating which theta to plot (only applicable for multidimensional 
#'   tests). The default is to facet each theta on one plot, but to plot only the first factor pass
#'   \code{pick_theta = 1}
#' @param true_thetas logical; include a horizontal line indicating where the population-level 
#'   theta values are? Only applicable to Monte Carlo simulations because this value would not 
#'   be known otherwise   
#' @param main title of the plot. Will default to \code{'CAT Standard Errors'} or 
#'   \code{'CAT ##\% Confidence Intervals'} depending on the SE input
#' @param SE size of the standard errors to plot. The default is 1, and therefore plots the
#'   standard error. To obtain the 95\% interval use \code{SE = 1.96} (from the z-distribution)
#' @param par.strip.text plotting argument passed to \code{\link{lattice}}
#' @param par.settings plotting argument passed to \code{\link{lattice}}
#' @param scales plotting argument passed to \code{\link{lattice}}
#' @export
plot.mirtCAT <- function(x, pick_theta = NULL, true_thetas = TRUE, SE = 1, main = NULL, 
                         par.strip.text = list(cex = 0.7),
                         par.settings = list(strip.background = list(col = '#9ECAE1'),
                                             strip.border = list(col = "black")),
                         scales=list(x = list(rot=90)), ...){
    if(length(x$thetas_SE_history) == 1L || is.na(x$thetas_SE_history))
        stop('plot not available for non-adaptive tests', call.=FALSE)
    p <- ceiling((1-(pnorm(-abs(SE))*2))*100)
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
    CV <- x$classify_values
    if(!is.null(CV)) CV <- rep(CV, each = nrow(thetas))
    if(nfact > 1L){
        return(xyplot(F1 ~ item|thetas, data=thetaslong, 
                      main = main,
                      lower = thetasSElowlong$F1,
                      upper = thetasSEhighlong$F1,
                      panel = function(x, y, lower, upper, subscripts, true_thetas, 
                                       true_theta_values, ...){
                          lower <- lower[subscripts]; upper <- upper[subscripts]
                          panel.polygon(c(x, rev(x)), c(upper, rev(lower)), 
                                        col="#E6E6E6", border = FALSE, ...)
                          panel.points(x, y, ...)
                          panel.lines(x, y, ...)
                          if(!is.null(CV)){
                              tmp <- CV[subscripts]
                              panel.abline(h=tmp[1], col = 'red', ...)
                          }
                          if(true_thetas && length(true_theta_values))
                              panel.abline(h = true_theta_values[subscripts][1L], col = 'red', ...)
                      },
                      ylim=c(min(thetasSElowlong$F1)-.1, max(thetasSEhighlong$F1)+.1),
                      ylab = expression(theta), 
                      xlab = 'Item', par.strip.text=par.strip.text,
                      par.settings=par.settings, scales=scales,
                      true_theta_values = rep(x$true_thetas, each=nrow(thetaslong)/nfact), 
                      true_thetas=true_thetas, ...))
        
    } else {
        return(xyplot(F1 ~ item, data=thetaslong, 
                      main = main,
                      lower = thetasSElowlong$F1,
                      upper = thetasSEhighlong$F1,
                      panel = function(x, y, lower, upper, subscripts, true_thetas, 
                                       true_theta_values, ...){
                          panel.polygon(c(x, rev(x)), c(upper, rev(lower)), 
                                        col="#E6E6E6", border = FALSE, ...)
                          panel.points(x, y, ...)
                          panel.lines(x, y, ...)
                          if(!is.null(CV))
                              panel.abline(h=CV[1], col = 'red', ...)
                          if(true_thetas && length(true_theta_values))
                              panel.abline(h = true_theta_values, col = 'red', ...)
                      },
                      ylim=c(min(thetasSElowlong$F1)-.1, max(thetasSEhighlong$F1)+.1),
                      ylab = expression(theta), 
                      xlab = 'Item', par.strip.text=par.strip.text,
                      par.settings=par.settings, scales=scales,
                      true_theta_values = x$true_thetas, true_thetas=true_thetas, ...))
    }
}