#' Generate plots following test completion
#' 
#' Generate plots following the convergence of \code{\link{mirtCAT}}.
#' 
#' @param person an object of class 'Person' returned from \code{\link{mirtCAT}}
#' 
#' @param pick_theta a number indicating which theta to plot (only applicable for multidimensional 
#'   tests). The default is to facet each theta on one plot, but to plot only the first factor pass
#'   \code{pick_theta = 1}
#'   
#' @param main title of the plot
#' 
#' @param ... additional arguments to be passed to \code{lattice}
#' 
#' @export personPlot
#' @author Phil Chalmers \email{rphilip.chalmers@@gmail.com}
#' @seealso \code{\link{mirtCAT}}
#' 
#' @examples
#' \dontrun{
#' 
#' person <- mirtCAT(mod, shiny_questions)
#' personPlot(person)
#' 
#' }

personPlot <- function(person, pick_theta = NULL, main = 'CAT Standard Errors', ...){
    nfact <- ncol(person$thetas)
    thetas <- data.frame(person$thetas_history)
    thetasSEhigh <- data.frame(thetas + person$thetas_SE_history)
    thetasSElow <- data.frame(thetas - person$thetas_SE_history)
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
    tmp <- person$items_answered
    tmp <- rep(c(0,tmp[!is.na(tmp)]), nfact)
    thetaslong$item <- factor(tmp, levels = unique(tmp))
    if(nfact > 1L){
        return(xyplot(F1 ~ item|thetas, data=thetaslong, 
                               main = main,
                               lower = thetasSElowlong$F1,
                               upper = thetasSEhighlong$F1,
                               panel = function(x, y, lower, upper, subscripts, ...){
                                   lower <- lower[subscripts]; upper <- upper[subscripts]
                                   panel.points(x, y, ...)
                                   panel.lines(x, y, ...)
                                   for(i in 1:length(lower)){
                                       panel.arrows(x0=x[i], y0=lower[i], x1 = x[i], y1= upper[i], 
                                                    col = 'grey', angle = 90, length = .05)
                                       panel.arrows(x0=x[i], y0=upper[i], x1 = x[i], y1= lower[i], 
                                                    col = 'grey', angle = 90, length = .05)
                                   }
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
                                   panel.points(x, y, ...)
                                   panel.lines(x, y, ...)
                                   for(i in 1:length(lower)){
                                       panel.arrows(x0=x[i], y0=lower[i], x1 = x[i], y1= upper[i], 
                                                    col = 'grey', angle = 90, length = .05)
                                       panel.arrows(x0=x[i], y0=upper[i], x1 = x[i], y1= lower[i], 
                                                    col = 'grey', angle = 90, length = .05)
                                   }
                               },
                               ylim=c(min(thetasSElowlong$F1)-.1, max(thetasSEhighlong$F1)+.1),
                               ylab = expression(theta), 
                               xlab = 'Item', ...))
        
    }
}