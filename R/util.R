.MCE <- new.env(parent=emptyenv())
.MCE$complete <- TRUE

FI <- function(mirt_item, Theta){
    .Call('ItemInfo', mirt_item, Theta)
}

getAcovs <- function(possible_patterns, method, test, design){
    ret <- fscores(test@mo, return.acov = TRUE, 
                   method = method, response.pattern = possible_patterns, mirtCAT=TRUE,
                   rotate = test@fscores_args$rotate, theta_lim = test@fscores_args$theta_lim,
                   mean = test@fscores_args$mean, cov = test@fscores_args$cov, 
                   MI = test@fscores_args$MI, quadpts = test@quadpts)
    ret <- lapply(ret, function(x, pick){
        x <- try(x[pick, pick, drop=FALSE])
        return(x)
    }, pick=!design@met_SEM)
    ret    
} 

weighted_mat <- function(mat, row_loc, which_not_answered, P = rep(1, length(row_loc))){
    if(is.list(P)) for(i in seq_len(length(P))) mat[[i]] <- mat[[i]] * P[[i]]
    else for(i in seq_len(length(P))) mat[[i]] <- mat[[i]] * P[i]
    mat2 <- vector('list', length(unique(row_loc)))
    for(i in seq_len(length(mat2))){
        pick <- which(row_loc == which_not_answered[i])
        tmp <- mat[pick]
        for(j in 2L:length(pick))
            tmp[[1L]] <- tmp[[1L]] + tmp[[j]]
        mat2[[i]] <- tmp[[1L]]
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
            stop("list 'x' has no valid $x component", call.=FALSE)
    }
    if((n <- length(x)) != length(fx))
        stop("'fx' must have same length as 'x'", call.=FALSE)
    
    if(is.unsorted(x)) { i <- sort.list(x); x <- x[i]; fx <- fx[i] }
    if(any(i <- duplicated(x))) {
        n <- length(x <- x[!i])
        ## we might have to check that the same fx[] are duplicated
        ## otherwise either give an error or take the mean() of those...
        fx <- fx[!i]
    }
    if(any(diff(x) == 0))
        stop("bug in 'duplicated()' killed me: have still multiple x[]!", call.=FALSE)
    
    if(missing(a)) a <- x[1]
    else if(any(a < x[1])) stop("'a' must NOT be smaller than min(x)", call.=FALSE)
    if(missing(b)) b <- x[n]
    else if(any(b > x[n])) stop("'b' must NOT be larger  than max(x)", call.=FALSE)
    if(length(a) != 1 && length(b) != 1 && length(a) != length(b))
        stop("'a' and 'b' must have length 1 or same length !", call.=FALSE)
    else {
        k <- max(length(a),length(b))
        if(any(b < a))    stop("'b' must be elementwise >= 'a'", call.=FALSE)
    }
    
    if(use.spline) {
        xy <- spline(x,fx, n = max(1024, 3*n))
        ##-- Work around spline(.) BUG:  (ex.:  range(spline(1:20,1:20,n=95)))
        if(xy$x[length(xy$x)] < x[n]) {
            if(TRUE) cat("working around spline(.) BUG --- hmm, really?\n\n", call.=FALSE)
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

buildShinyElements <- function(questions, itemnames, customTypes, choiceNames, choiceValues){
    J <- length(questions$Type)
    if(!all(sapply(questions[names(questions) != 'Rendered_Question'], is.character))) 
        stop('Only character classes are supported in questions input', call.=FALSE)
    if(is.null(itemnames)) itemnames <- paste0('Item.', 1L:J)
    names <- names(questions)
    Qs_char <- questions$Rendered_Question
    Type <- questions$Type
    if(is.null(Qs_char) && !any(questions$Type == 'slider')) 
        stop('Question column not specified', call.=FALSE)
    if(is.null(Type)) stop('Type column not specified', call.=FALSE)
    if(!all(Type %in% c('radio', 'select', 'text', 'textArea', 'slider', 'checkbox', 'none',
                        names(customTypes))))
        stop('Type input in shiny_questions contains invalid arguments', call.=FALSE)
    Qs <- vector('list', J)
    choices <- data.frame(questions[grepl('Option', names)], stringsAsFactors = FALSE)
    choices_list <- vector('list', J)
    names(choices) <- NULL
    for(i in seq_len(length(Qs))){
        if(Type[i] == 'radio'){
            cNs <- cVs <- cs <- NULL 
            if(length(choiceNames[[i]]) && is.list(choiceNames[[i]])){
                cNs <- choiceNames[[i]]
                cVs <- choiceValues[[i]]
                choices_list[[i]] <- as.character(cVs)
            } else {
                cs <- choices[i, !is.na(choices[i, ])]
                choices_list[[i]] <- cs
            }
            inline <- if(is.null(questions$inline[i])) FALSE else as.logical(questions$inline[i])
            width <- questions$width[i]
            Qs[[i]] <- radioButtons(inputId = itemnames[i], label='',
                                    inline = inline, width = width,
                                    choices = cs, selected = '', 
                                    choiceNames=cNs, choiceValues=cVs)
        } else if(Type[i] == 'select'){
            cs <- c('', choices[i, !is.na(choices[i, ])])
            choices_list[[i]] <- cs
            width <- questions$width[i]
            size <- questions$size[i]
            Qs[[i]] <- selectInput(inputId = itemnames[i], label='', selected = '', 
                                    choices = cs, width=width, size=size)
        } else if(Type[i] == 'text'){
            width <- questions$width[i]
            placeholder <- questions$placeholder[i]
            Qs[[i]] <- textInput(inputId = itemnames[i], label='', value = '',
                                 width=width, placeholder=placeholder)
        } else if(Type[i] == 'textArea'){
            width <- questions$width[i]
            height <- questions$height[i]
            cols <- questions$cols[i] 
            rows <- questions$rows[i]
            resize <- questions$resize[i]
            placeholder <- questions$placeholder[i]
            Qs[[i]] <- textAreaInput(inputId = itemnames[i], label='', value = '',
                                 width=width, height=height, placeholder=placeholder,
                                 cols=cols, rows=rows, resize=resize)
        } else if(Type[i] == 'slider'){
            if(is.null(questions$min) || is.null(questions$max) || is.null(questions$step))
                stop('slider Type requires a min, max, and step column element in the df object', call.=FALSE)
            VALUE <- as.numeric(ifelse(is.null(questions$value[i]), questions$min[i], questions$value[i]))
            MIN <- as.numeric(questions$min[i])
            MAX <- as.numeric(questions$max[i])
            STEP <- as.numeric(questions$step[i])
            ROUND <- ifelse(is.null(questions$round[i]), FALSE, questions$round[i])
            TICKS <- ifelse(is.null(questions$ticks[i]), TRUE, questions$ticks[i])
            WIDTH <- questions$width[i]
            PRE <- questions$pre[i]
            POST <- questions$post[i]
            Qs[[i]] <- sliderInput(inputId = itemnames[i], label='', min = MIN, max = MAX,
                                   value = VALUE, step = STEP, round = ROUND, width = WIDTH,
                                   ticks = TICKS, pre = PRE, post = POST)
            choices_list[[i]] <- as.character(seq(from=MIN, to=MAX, by=STEP))
        } else if(Type[i] == 'checkbox'){
            cNs <- cVs <- cs <- NULL 
            if(length(choiceNames[[i]]) && is.list(choiceNames[[i]])){
                cNs <- choiceNames[[i]]
                cVs <- choiceValues[[i]]
                choices_list[[i]] <- as.character(cVs)
            } else {
                cs <- choices[i, !is.na(choices[i, ])]
                choices_list[[i]] <- cs
            }
            width <- questions$width[i]
            inline <- if(is.null(questions$inline[i])) FALSE else as.logical(questions$inline[i])
            Qs[[i]] <- checkboxGroupInput(inputId = itemnames[i], label='', choices = cs,
                                          width=width, inline=inline, choiceNames=cNs, choiceValues=cVs)
        } else if(Type[i] %in% names(customTypes)){
            nm <- names(customTypes)[names(customTypes) == Type[i]]
            df_row <- as.data.frame(lapply(questions, function(x, ind) x[[ind]], ind=i), 
                                    stringsAsFactors = FALSE)
            df_row$Rendered_Question <- NULL
            Qs[[i]] <- customTypes[[nm]](inputId = itemnames[i], df_row=df_row)
        } else if(Type[i] == 'none'){
            next
        }
    }
    pick <- as.data.frame(questions[grepl('Answer', names),drop=FALSE], stringsAsFactors = FALSE)
    if(length(pick)){
        item_answers <- split(pick, 1:nrow(pick))
        item_answers <- lapply(item_answers, function(x){
            out <- na.omit(as.character(x))
            if(!length(out)) out <- NA
            out
        })
    } else item_answers <- NULL
    if(length(Qs) != J) stop('Questions have not been properly defined!', call.=FALSE)
    ret <- list(questions=Qs, item_answers=item_answers, item_options=choices_list)
    return(ret)
}

formatTime <- function(delta){
    hours <- delta %/% 3600
    mins <- delta %/% 60 - hours * 60
    if(hours > 1){
        h <- hours
        m <- ifelse((mins %/% 30) == 1, 30, 0) 
        out <- sprintf('More than %d hours and %d minutes.', h, m)
    } else if(mins >= 15) {
        m <- switch(as.character((mins %/% 15)),
                    '3' = '45', '2' = '30', '1' = '15')
        out <- sprintf('More than %s minutes.', m)
    } else {
        m <- switch(as.character((mins %/% 5) + 1),
                    '3' = '15', '2' = '10', '1' = '5')
        out <- sprintf('Less than %s minutes.', m)
    }
    out
}

last_item <- function(items_answered) items_answered[max(which(!is.na(items_answered)))]

# TODO this can be modified to accept other info 
possible_pattern_thetas <- function(possible_patterns, test, method = 'EAP'){
    suppressWarnings(tmp <- fscores(test@mo, method = method, 
                                    response.pattern = possible_patterns, theta_lim = test@fscores_args$theta_lim, 
                                    MI = test@fscores_args$MI, quadpts = test@quadpts, 
                                    mean = test@fscores_args$mean, cov = test@fscores_args$cov, 
                                    QMC = test@fscores_args$QMC, custom_den = test@fscores_args$custom_den))
    tmp
}

stemContent <- function(pick){
    if(!is.na(.MCE$shinyGUI$stem_expressions[pick])){
        return(eval(parse(text=.MCE$shinyGUI$stem_expressions[pick])))
    } else {
        file <- .MCE$shinyGUI$stem_locations[pick]
        empty <- is.na(file)
        if(!empty){
            if(grepl('\\.[mM][dD]$', file)){
                suppressWarnings(markdown::markdownToHTML(file=file, output=.MCE$outfile2, 
                                                          fragment.only = TRUE))
                contents <- readLines(.MCE$outfile2, warn = FALSE)
                return(HTML(contents))
            } else if(grepl('\\.[hH][tT][mM][lL]$', file)){
                contents <- readLines(file, warn = FALSE)
                return(HTML(contents))
            } else empty <- TRUE
        }
    }
    NULL
}

verifyPassword <- function(input, password){
    nr <- nrow(password)
    verified <- if(nr == 1L){
        input$PaSsWoRd %in% password
    } else {
        tmp <- subset(password, password[,1L] == input$UsErNaMe)
        .MCE$person$login_name <- input$UsErNaMe
        input$PaSsWoRd %in% tmp[,-1L]
    }
    verified
}