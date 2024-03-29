context('onefactor')

test_that('unidimensional', {
    
    set.seed(1)
    nitems <- 25
    itemnames <- paste0('Item.', 1:nitems)
    a <- matrix(rlnorm(nitems, .2, .3))
    d <- matrix(rnorm(nitems))
    dat <- simdata(a, d, 500, itemtype = 'dich')
    colnames(dat) <- itemnames
    mod <- mirt(dat, 1, verbose = FALSE, TOL = .01)
    
    # simple math items
    questions <- answers <- character(nitems)
    choices <- matrix(NA, nitems, 5)
    spacing <- floor(d - min(d)) + 1 #easier items have more variation in the options
    
    for(i in 1:nitems){
        n1 <- sample(1:50, 1)
        n2 <- sample(51:100, 1)
        ans <- n1 + n2
        questions[i] <- paste0(n1, ' + ', n2, ' = ?')
        answers[i] <- as.character(ans)
        ch <- ans + sample(c(-5:-1, 1:5) * spacing[i,], 5)
        ch[sample(1:5, 1)] <- ans
        choices[i, ] <- as.character(ch)
    }
    
    df <- data.frame(Question=questions, Option=choices, 
                     Type = 'radio', stringsAsFactors = FALSE)
    df2 <- df
    df2$Answer <- answers
    
    pat <- generate_pattern(mod, Theta = 0, df2)
    expect_true(all(pat == as.character(c(90,83,82,110,94,86,93,125,51,109,89,126,101,109,83,78,125,73,84,85,111,85,118,96,113))))
    pat2 <- generate_pattern(mod, Theta = -1, df2)
    expect_true(all(pat2 == as.character(c(88,73,84,116,94,92,99,115,55,105,91,116,91,103,98,70,115,64,78,87,107,83,109,96,113))))
    
    #no scoring, just collecting
    res <- mirtCAT(df, local_pattern=pat)
    expect_is(res, 'mirtCAT')
    expect_true(all(res$items_answered == 1:25))
    
    res <- mirtCAT(df, local_pattern=pat, criteria='random')
    expect_true(all(!is.na(res$raw_responses)))
    
    # custom
    customNextItem <- function(person, design, test){
        sum(is.na(person$items_answered)) + 1L
    }
    res <- mirtCAT(df, local_pattern=pat, design = list(customNextItem=customNextItem))
    expect_is(res, 'mirtCAT')
    so <- summary(res)
    expect_equal(c(1, 25:2), so$items_answered)
    
    test_properties <- data.frame(item_group = c(1, rep(c(1,2), each=12)))
    person_properties <- data.frame(group = 1)
    customNextItem <- function(person, design, test){
        pp <- with(design, person_properties)
        tp <- with(design, test_properties)
        possible_items <- pp$group == tp$item_group & is.na(person$raw_responses)
        ret <- if(sum(possible_items)) min(which(possible_items)) else NA
        ret
    }
    res <- mirtCAT(df, local_pattern=pat, design = list(customNextItem=customNextItem,
                                                        test_properties=test_properties,
                                                        person_properties=person_properties))
    so <- summary(res)
    expect_equal(1:13, so$items_answered)
    person_properties <- data.frame(group = 2)
    res <- mirtCAT(df, local_pattern=pat, design = list(customNextItem=customNextItem,
                                                        test_properties=test_properties,
                                                        person_properties=person_properties))
    so <- summary(res)
    expect_equal(c(1, 14:25), so$items_answered)
    customNextItem <- function(person, design, test){
        design@max_items <- 5L
        ret <- sum(is.na(person$items_answered)) + 1L
        attr(ret, 'design') <- design
        ret
    }
    res <- mirtCAT(df, local_pattern=pat, design = list(customNextItem=customNextItem))
    expect_equal(length(res$items_answered), 5L)
    
    #sequential
    res <- mirtCAT(df2, mod, local_pattern=pat)
    expect_equal(as.numeric(res$thetas), 0.3428296, tolerance = 1e-4)
    expect_equal(as.numeric(res$thetas_SE_history[23,]), 0.2982295, tolerance = 1e-4)
    
    oo <- plot(res)
    expect_is(oo, 'trellis')
    
    res <- mirtCAT(df2, mod, local_pattern=pat, design = list(max_items = 5))
    expect_true(sum(!is.na(res$raw_responses)) == 5L && sum(!is.na(res$scored_responses)) == 5L)
    expect_true(nrow(!is.na(res$thetas_history)) == 6L && nrow(!is.na(res$thetas_SE_history)) == 6L)
    
    #adaptive
    res <- mirtCAT(df2, mod, local_pattern=pat, criteria='MI',
                   design = list(min_SEM = .4))
    expect_equal(as.numeric(res$thetas), 0.3920612, tolerance = 1e-4)
    expect_equal(as.numeric(res$thetas_SE_history[5L,]), 0.4950726, tolerance = 1e-4)
    expect_true(sum(!is.na(res$raw_responses)) == 8L && sum(!is.na(res$scored_responses)) == 8L)
    expect_true(nrow(!is.na(res$thetas_history)) == 9L && nrow(!is.na(res$thetas_SE_history)) == 9L)
    
    res <- mirtCAT(mo = mod, local_pattern=generate_pattern(mod, matrix(c(0,1))), criteria='MI',
                   design = list(min_SEM = .4))
    so <- summary(res[[1]])
    expect_equal(so$true_thetas, 0)
    
    res <- mirtCAT(df2, mod, local_pattern=pat, criteria='MI', start_item = 'MI',
                   design = list(min_SEM = .4))
    expect_true(summary(res)$items_answered[1] == 4)
    
    res <- mirtCAT(df2, mod, local_pattern=pat, criteria='MI',
                   design = list(min_SEM = .4), method = 'EAP')
    expect_equal(as.numeric(res$thetas), 0.207163, tolerance = 1e-4)
    expect_equal(as.numeric(res$thetas_SE_history[nrow(res$thetas_SE_history),]),
                 0.3882085, tolerance = 1e-4)
    
    exposure <- rep(3L, nrow(df2))
    set.seed(1234)
    start_item=sample(c(1:nrow(df2)), 1)
    res <- mirtCAT(df2, mod, local_pattern=pat, criteria='MI',
                   design = list(min_SEM = .4, exposure=exposure), method = 'EAP', 
                   start_item=start_item)
    expect_equal(as.numeric(res$thetas), 0.4357136, tolerance = 1e-4)
    so <- summary(res)
    expect_equal(so$items_answered, c(16,4,15,22,7,11,20,2,21))
    expect_equal(as.numeric(so$thetas_SE_history[nrow(so$thetas_SE_history),]),
                 0.394788, tolerance = 1e-4)
    
    set.seed(1)
    exposure <- rep(0.75, nrow(df2))
    res <- mirtCAT(df2, mod, local_pattern=pat, criteria='MI',
                   design = list(min_SEM = .4, exposure=exposure), method = 'EAP', 
                   start_item=sample(c(1:nrow(df2)), 1))
    expect_equal(as.numeric(res$thetas), .09047353, tolerance = 1e-4)
    so <- summary(res)
    expect_equal(as.numeric(so$thetas_SE_history[nrow(so$thetas_SE_history),]),
                 0.3955568, tolerance = 1e-4)
    
    set.seed(12)
    res <- mirtCAT(df2, mod, local_pattern=pat, 
                   design = list(min_SEM = .4), method = 'EAP', criteria='random')
    expect_equal(as.numeric(res$thetas), 0.5336422, tolerance = 1e-4)
    expect_equal(as.numeric(res$thetas_SE_history[nrow(res$thetas_SE_history),]),
                 0.3939983, tolerance = 1e-4)
    
    res <- mirtCAT(df2, mod, local_pattern=pat, 
                   design = list(min_SEM = .4), method = 'EAP', criteria='MEI')
    expect_equal(as.numeric(res$thetas), 0.3310517, tolerance = 1e-4)
    expect_equal(as.numeric(res$thetas_SE_history[nrow(res$thetas_SE_history),]),
                 0.3949008, tolerance = 1e-4)
    
    res <- mirtCAT(df2, mod, local_pattern=pat, 
                   design = list(min_SEM = .4), method = 'EAP', criteria='MEPV')
    expect_equal(as.numeric(res$thetas), 0.207163, tolerance = 1e-4)
    expect_equal(as.numeric(res$thetas_SE_history[nrow(res$thetas_SE_history),]),
                 0.3882085, tolerance = 1e-4)
    
    res <- mirtCAT(df2, mod, local_pattern=pat, 
                   design = list(min_SEM = .4), method = 'EAP', criteria='MLWI')
    expect_equal(as.numeric(res$thetas), 0.207163, tolerance = 1e-4)
    expect_equal(as.numeric(res$thetas_SE_history[nrow(res$thetas_SE_history),]),
                 0.3882085, tolerance = 1e-4)
    
    res <- mirtCAT(df2, mod, local_pattern=pat, 
                   design = list(min_SEM = .4), method = 'EAP', criteria='MPWI')
    expect_equal(as.numeric(res$thetas), 0.207163, tolerance = 1e-4)
    expect_equal(as.numeric(res$thetas_SE_history[nrow(res$thetas_SE_history),]),
                 0.3882085, tolerance = 1e-4)
    
    res <- mirtCAT(df2, mod, local_pattern=pat, 
                   design = list(min_SEM = .4), method = 'EAP', criteria='KL')
    expect_equal(as.numeric(res$thetas), 0.207163, tolerance = 1e-4)
    expect_equal(as.numeric(res$thetas_SE_history[nrow(res$thetas_SE_history),]),
                 0.3882085, tolerance = 1e-4)
    
    res <- mirtCAT(df2, mod, local_pattern=pat, 
                   design = list(min_SEM = .4), method = 'ML', criteria='KLn',
                   preCAT = list(max_items = 5L, criteria = 'seq', method = 'fixed'))
    expect_equal(as.numeric(res$thetas), 0.3516709, tolerance = 1e-4)
    expect_equal(as.numeric(res$thetas_SE_history[nrow(res$thetas_SE_history),]),
                 0.389008, tolerance = 1e-4)
    
    res <- mirtCAT(df2, mod, local_pattern=pat, criteria='IKL',
                   design = list(min_SEM = .4), method = 'MAP')
    expect_equal(as.numeric(res$thetas), 0.1847392, tolerance = 1e-4)
    expect_equal(as.numeric(res$thetas_SE_history[nrow(res$thetas_SE_history),]),
                 0.3920537, tolerance = 1e-4)
    
    res <- mirtCAT(df2, mod, local_pattern=pat, criteria='IKLPn',
                   design = list(min_SEM = .4), method = 'MAP')
    expect_equal(as.numeric(res$thetas), 0.1847392, tolerance = 1e-4)
    expect_equal(as.numeric(res$thetas_SE_history[nrow(res$thetas_SE_history),]),
                 0.3920537, tolerance = 1e-4)
    
    # content balancing
    set.seed(1)
    content <- c(rep('C1', 15), rep('C2', 10))
    content_prop <- c('C1'=.7, 'C2'=.3)
    res <- mirtCAT(df2, mod, local_pattern=pat, criteria='MI',
                   design = list(min_SEM = .4, content_prop=content_prop, content=content), 
                   method = 'MAP') #should crash with 'seq'
    so <- summary(res)
    expect_equal(so$items_answered[1:5], c(1,20,4,11,22))
    expect_equal(as.numeric(table(content[so$items_answered])/8), c(.625, .375))
    
    #pass other args through ...
    res <- mirtCAT(df2, mod, local_pattern=pat, 
                   design = list(min_SEM = .4), method = 'EAP', criteria='KL', theta_lim = c(-1,1))
    expect_equal(as.numeric(res$thetas), 0.4425398, tolerance = 1e-4)
    expect_equal(as.numeric(res$thetas_SE_history[nrow(res$thetas_SE_history),]),
                 0.3605357, tolerance = 1e-4)
    
    ## classification
    res <- mirtCAT(df2, mod, local_pattern=pat, criteria='MI',
                   design = list(classify = -0.5, classify_CI=.95))
    so <- summary(res)
    expect_true(so$classification == 'above cutoff')
    expect_equal(as.numeric(res$thetas), 0.5802743, tolerance = 1e-4)
    
    ##fscores call
    responses <- res$scored_responses
    fs <- fscores(mod, response.pattern = responses)
    expect_equal(unname(fs[,'F1']), 0.6403941, tolerance = 1e-4)
    
    # excluded set
    res <- mirtCAT(df2, mod, local_pattern=pat, criteria='MI', 
                   design = list(max_items = 5, constraints = list(excluded = c(2:10))))
    expect_true(all(res$items_answered == c(1, 11,20,21,12)))
    res <- mirtCAT(df2, mod, local_pattern=pat, criteria='seq', 
                   design = list(max_items = 5, constraints = list(excluded = c(2:10))))
    expect_true(all(res$items_answered == c(1, 11:14)))
    
    ## example sim cell
    set.seed(1)
    Theta <- matrix(c(-1,0,1),3)
    pats <- generate_pattern(mod, Theta = Theta)
    expect_equal(dim(pats), c(3,25))
    res <- mirtCAT(mo = mod, local_pattern=pats, criteria='MI')
    sos <- lapply(res, summary)
    expect_equal(as.numeric(sos[[1]]$thetas_history[26,]), -0.4728543, tolerance = 1e-4)
    expect_equal(as.numeric(sos[[2]]$thetas_history[19,]), 0.02041261, tolerance = 1e-4)
    
    design <- list(thetas.start=Theta)
    res <- mirtCAT(mo = mod, local_pattern=pats, criteria='MI', start_item = 'MI', design=design)
    sos <- lapply(res, summary)
    expect_equal(as.numeric(sos[[1]]$thetas_history[1:3,]), c(-1, -0.6333807, -0.2148489), tolerance = 1e-4)
    expect_equal(as.numeric(sos[[3]]$thetas_history[1:3,]), c(1, 0.6685077, 0.8582069), tolerance = 1e-4)
})

