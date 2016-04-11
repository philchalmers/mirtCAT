#' Generate a mirt object from population parameters
#' 
#' This function generate a \code{mirt} object from known population parameters, which is  
#' then passed to \code{\link{mirtCAT}} function for running CAT applications.
#' 
#' @param parameters a matrix or data.frame of parameters corresponding to the model definitions
#'   listed in \code{\link{mirt}}. Each row represents a unqiue item, while the 
#'   column names correspond to the respective parameter names. If a parameter is not relavent
#'   for a particular item/row then use \code{NA}'s as placeholders
#'   
#' @param itemtype a character vector indiciating the type of item to which the parameters 
#'   refer. See the \code{itemtype} arguement in \code{\link{mirt}}. Note that this input 
#'   is only used to determine the relavent item class for the rows in \code{parameters}, 
#'   therefore many inputs are interchangable (e.g., '2PL' generates the same model as '3PL').
#'   If only a single value is provided then all items types will be assumed identical
#'   
#' @param latent_means (optional) a numeric vector used to define the population latent mean
#'   structure. By default the mean structure is centered at a 0 centroid
#' 
#' @param latent_covariance (optional) a covariance matrix used to define the population 
#'   variance-covariance structure between the latent traits. By default the relationship is 
#'   assumed to be standard normal (i.e., and identity matrix)
#'   
#' @param key scoring key required for nested-logit models. See \code{\link{mirt}} for details
#' 
#' @param min_category the value representing the lowest category index. By default this is 0,
#'   therefore the respond suitable for the first category is 0, second is 1, and so on up to 
#'   \code{K - 1}
#' 
#' @export generate.mirt_object
#' @author Phil Chalmers \email{rphilip.chalmers@@gmail.com}
#' @seealso \code{\link{mirt}}, \code{\link{mirtCAT}}, \code{\link{generate_pattern}}
#' 
#' @examples
#' \dontrun{
#' 
#' ### build a unidimensional test with all 3PL items
#' 
#' nitems <- 50
#' a1 <- rlnorm(nitems, .2,.2)
#' d <- rnorm(nitems)
#' g <- rbeta(nitems, 20, 80)
#' 
#' pars <- data.frame(a1=a1, d=d, g=g)
#' head(pars)
#' 
#' obj <- generate.mirt_object(pars, '3PL')
#' coef(obj, simplify = TRUE)
#' plot(obj, type = 'trace')
#' 
#' ### build a two-dimensional test  
#' ## all graded items with 5 response categories
#' 
#' nitems <- 30
#' as <- matrix(rlnorm(nitems*2, .2, .2), nitems)
#' diffs <- t(apply(matrix(runif(nitems*4, .3, 1), nitems), 1, cumsum)) 
#' diffs <- -(diffs - rowMeans(diffs)) 
#' ds <- diffs + rnorm(nitems)
#' pars2 <- data.frame(as, ds)
#' colnames(pars2) <- c('a1', 'a2', paste0('d', 1:4))
#' head(pars2)
#' 
#' obj <- generate.mirt_object(pars2, 'graded')
#' coef(obj, simplify = TRUE)
#' 
#' ### unidimensional mixed-item test
#' 
#' library(plyr)
#' pars3 <- rbind.fill(pars, pars2) #notice the NA's where parmeters do not exist
#' obj <- generate.mirt_object(pars3, itemtype = c(rep('2PL', 50), rep('graded', 30)))
#' coef(obj)
#' itemplot(obj, 51)
#' itemplot(obj, 1, drop.zeros=TRUE)
#' 
#' }
generate.mirt_object <- function(parameters, itemtype, latent_means = NULL, 
                                 latent_covariance = NULL, key = NULL, 
                                 min_category = rep(0L, length(itemtype))){
    if(missing(itemtype))
        stop('Must define an itemtype argument', call.=FALSE)
    if(missing(parameters))
        stop('Must define parameters argument', call.=FALSE)
    parameters <- as.matrix(parameters)
    if(length(itemtype) == 1L)
        itemtype <- rep(itemtype, nrow(parameters))
    if(nrow(parameters) != length(itemtype))
        stop('nrow(parameters) not equal to length(itemtype)', call.=FALSE)
    nitems <- length(itemtype)
    K <- integer(nitems)
    names <- colnames(parameters)    
    itemtype[itemtype %in% c('3PL', '3PLu', '4PL')] <- '2PL'
    itemtype[itemtype %in% c('3PLNRM', '3PLuNRM', '4PLNRM')] <- '2PLNRM'
    itemtype[itemtype == 'PC3PL'] <- 'PC2PL'
    for(i in 1L:nitems){
        pick <- parameters[i, !is.na(parameters[i,]), drop=FALSE]
        nms <- colnames(pick)
        if(itemtype[i] == 'Rasch'){
            itemtype[i] <- if(any(nms == 'd')) '2PL' else 'gpcm'
        }
        if(itemtype[i] %in% c('2PL', 'ideal', 'PC2PL')){
            K[i] <- 2L
        } else if(itemtype[i] %in% c('graded', 'grsm', 'gpcm', 'nominal', '2PLNRM')){
            K[i] <- max(sapply(strsplit(nms[grepl('d', nms)], 'd'), function(x) as.numeric(x[2]))) + 1
        } else {
            stop(sprintf('%s is an invalid itemtype argument. Please fix!', itemtype[i]), call.=FALSE)
        }
    }
    dat <- matrix(c(0,1), 2L, nitems)
    colnames(dat) <- dnames <- paste0('Item.', 1:nitems)
    nfact <- max(which(paste0('a', 1:250) %in% names))
    tmp <- parameters[,paste0('a', 1:nfact), drop=FALSE]
    tmp[is.na(tmp)] <- 0
    parameters[, paste0('a', 1:nfact)] <- tmp
    model <- character(nfact)
    for(i in 1L:nfact)
        model[i] <- paste0('F', i, ' = 1-', ncol(dat))
    model <- mirt.model(paste0(model, collapse = '\n'))
    sv <- mirt(dat, model, itemtype=itemtype, key=key, technical=list(customK=K), pars='values')
    for(i in 1L:nitems){
        pick <- parameters[i, !is.na(parameters[i,]), drop=FALSE]
        nms <- colnames(pick)
        wch <- which(sv$item == dnames[i])
        for(j in 1L:ncol(pick)){
            wch2 <- which(sv[wch, ]$name == nms[j])
            sv[wch[wch2], ]$value <- pick[,j]
        }
    }
    if(!is.null(latent_means))
        sv$value[sv$item == 'GROUP' & grepl('MEAN', sv$name)] <- as.numeric(latent_means)
    if(!is.null(latent_covariance)){
        if(!is.matrix(latent_covariance))
            stop('latent_covariance input must be a matrix', call.=FALSE)
        vals <- latent_covariance[lower.tri(latent_covariance, TRUE)]
        sv$value[sv$item == 'GROUP' & grepl('COV', sv$name)] <- vals
    }
    dat <- t(t(dat) + min_category)
    ret <- mirt(dat, model, itemtype=itemtype, technical=list(customK=K, warn=FALSE, message=FALSE), 
                TOL=NA, pars=sv, quadpts = 1, key=key, rotate = 'none')
    ret@Options$exploratory <- FALSE
    ret
}