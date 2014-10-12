MCE <- new.env()
MCE$complete <- TRUE

calcLL <- function(thetas){
    LL <- 0
    for(i in MCE$person$responses){
        if(!is.na(MCE$person$responses)){
            item <- extract.item(MCE$test$mirt_object, i)
            P <- probtrace(item, thetas)
            LL <- LL + log(P[ ,MCE$person$responses])
        }
    }
    LL
}

getAcovs <- function(possible_patterns, method){
    ret <- fscores(MCE$test$mirt_object, return.acov = TRUE, 
                   method = method, response.pattern = possible_patterns, mirtCAT=TRUE,
                   rotate = MCE$test$fscores_args$rotate, theta_lim = MCE$test$fscores_args$theta_lim,
                   mean = MCE$test$fscores_args$mean, cov = MCE$test$fscores_args$cov, 
                   MI = MCE$test$fscores_args$MI, quadpts = MCE$test$quadpts)
    ret <- lapply(ret, function(x, pick){
        x <- try(x[pick, pick, drop=FALSE])
        return(x)
    }, pick=!MCE$design$met_SEM)
    ret    
} 

weighted_mat <- function(mat, row_loc, which_not_answered, P = rep(1, length(row_loc))){
    if(is.list(P)) for(i in 1L:length(P)) mat[[i]] <- mat[[i]] * P[[i]]
    else for(i in 1L:length(P)) mat[[i]] <- mat[[i]] * P[i]
    mat2 <- vector('list', length(unique(row_loc)))
    for(i in 1L:length(mat2)){
        pick <- which(row_loc == which_not_answered[i])
        mat2[[i]] <- do.call(`+`, mat[pick])
    }
    mat2
}

# copied from sfsmisc1.0-26 on July 19, 2014
integrate.xy <- function(x,fx, a,b, use.spline = TRUE, xtol = 2e-8)
{
    dig <- round(-log10(xtol)) #
    f.match <- function(x,table) match(signif(x,dig), signif(table,dig))
    ## was (S+) f.match <- function(x,table) match(as.single(x), as.single(table))
    
    if(is.list(x)) {
        fx <- x$y; x <- x$x
        if(length(x) == 0)
            stop("list 'x' has no valid $x component")
    }
    if((n <- length(x)) != length(fx))
        stop("'fx' must have same length as 'x'")
    
    if(is.unsorted(x)) { i <- sort.list(x); x <- x[i]; fx <- fx[i] }
    if(any(i <- duplicated(x))) {
        n <- length(x <- x[!i])
        ## we might have to check that the same fx[] are duplicated
        ## otherwise either give an error or take the mean() of those...
        fx <- fx[!i]
    }
    if(any(diff(x) == 0))
        stop("bug in 'duplicated()' killed me: have still multiple x[]!")
    
    if(missing(a)) a <- x[1]
    else if(any(a < x[1])) stop("'a' must NOT be smaller than min(x)")
    if(missing(b)) b <- x[n]
    else if(any(b > x[n])) stop("'b' must NOT be larger  than max(x)")
    if(length(a) != 1 && length(b) != 1 && length(a) != length(b))
        stop("'a' and 'b' must have length 1 or same length !")
    else {
        k <- max(length(a),length(b))
        if(any(b < a))    stop("'b' must be elementwise >= 'a'")
    }
    
    if(use.spline) {
        xy <- spline(x,fx, n = max(1024, 3*n))
        ##-- Work around spline(.) BUG:  (ex.:  range(spline(1:20,1:20,n=95)))
        if(xy$x[length(xy$x)] < x[n]) {
            if(TRUE) cat("working around spline(.) BUG --- hmm, really?\n\n")
            xy$x <- c(xy$x,  x[n])
            xy$y <- c(xy$y, fx[n])
        }
        ## END if work around ----
        x <- xy$x; fx <- xy$y
        n <- length(x)
    }
    
    ab <- unique(c(a,b))
    xtol <- xtol * max(b - a)
    BB <- abs(outer(x,ab,"-")) < xtol
    if(any(j <- 0 == apply(BB,2,sum))) { #the j-th element(s) of ab are not in x[]
        y <- approx(x,fx, xout = ab[j])$y
        x <- c(ab[j],x)
        i <- sort.list(x)
        x <- x[i];  fx <- c(y,fx)[i];  n <- length(x)
    }
    
    ##--- now we could use 'Simpson's formula IFF the x[i] are equispaced... --
    ##--- Since this may well be wrong, just use 'trapezoid formula':
    
    ai <- rep(f.match(a,x), length = k)
    bi <- rep(f.match(b,x), length = k)
    dfx <- fx[-c(1,n)] * diff(x,lag = 2)
    r <- numeric(k)
    for (i in 1:k) {
        a <- ai[i];  b <- bi[i]
        r[i] <- (x[a+1] - x[a])*fx[a] + (x[b] - x[b-1])*fx[b] +
            sum(dfx[seq(a, length = max(0,b-a-1))])
    }
    r/2
}

extract_choices <- function(x){
    if(is(x, 'shiny.tag.list')){
        if(!is.null(x[[1L]][[2L]]$children[[1]])){ #selectInput
            split <- strsplit(x[[1L]][[2L]]$children[[1]], "\"")[[1L]]
            ret <- split[seq(from = 2L, to = length(split), by = 2L)]
        } else { #textInput
            ret <- ''
        }
    } else if(is(x, 'shiny.tag')){ #radioInput
        split <- lapply(x$children[[2L]], function(x) x$children[[1L]]$attribs$value)
        ret <- do.call(c, split)
    }
    return(ret)
}

slowTheHeckDown <- function(x = .1){
    p1 <- proc.time()
    Sys.sleep(x)
    proc.time() - p1
}

infoMatrix <- function(){
    
}

testInfoMatrix <- function(){
    
}