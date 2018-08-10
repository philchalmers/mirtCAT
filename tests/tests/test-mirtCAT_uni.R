context('onefactor')

test_that('unidimensional', {
    
    set.seed(1234)
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
    expect_true(all(pat == as.character(c(80,102,101,98,62,102,100,68,126,62,95,111,89,102,106,113,98,82,65,97,128,79,115,86,118))))
    pat2 <- generate_pattern(mod, Theta = -1, df2)
    expect_true(all(pat2 == as.character(c(74,108,101,77,64,94,92,76,134,62,101,108,82,112,98,105,102,84,69,105,124,79,115,82,110))))
    
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
    expect_equal(as.numeric(res$thetas), -0.1156397, tolerance = 1e-4)
    expect_equal(as.numeric(res$thetas_SE_history[26,]), 0.3319357, tolerance = 1e-4)
    
    oo <- plot(res)
    expect_is(oo, 'trellis')
    
    res <- mirtCAT(df2, mod, local_pattern=pat, design = list(max_items = 5))
    expect_true(sum(!is.na(res$raw_responses)) == 5L && sum(!is.na(res$scored_responses)) == 5L)
    expect_true(nrow(!is.na(res$thetas_history)) == 6L && nrow(!is.na(res$thetas_SE_history)) == 6L)
    
    #adaptive
    res <- mirtCAT(df2, mod, local_pattern=pat, criteria='MI',
                   design = list(min_SEM = .4))
    expect_equal(as.numeric(res$thetas), -.03611283, tolerance = 1e-4)
    expect_equal(as.numeric(res$thetas_SE_history[10L,]), 0.4093153, tolerance = 1e-4)
    expect_true(sum(!is.na(res$raw_responses)) == 10L && sum(!is.na(res$scored_responses)) == 10L)
    expect_true(nrow(!is.na(res$thetas_history)) == 11L && nrow(!is.na(res$thetas_SE_history)) == 11L)
    
    res <- mirtCAT(mo = mod, local_pattern=generate_pattern(mod, matrix(c(0,1))), criteria='MI',
                   design = list(min_SEM = .4))
    so <- summary(res[[1]])
    expect_equal(so$true_thetas, 0)
    
    res <- mirtCAT(df2, mod, local_pattern=pat, criteria='MI', start_item = 'MI',
                   design = list(min_SEM = .4))
    expect_true(summary(res)$items_answered[1] == 20)
    
    res <- mirtCAT(df2, mod, local_pattern=pat, criteria='MI',
                   design = list(min_SEM = .4), method = 'EAP')
    expect_equal(as.numeric(res$thetas), 0.00942354, tolerance = 1e-4)
    expect_equal(as.numeric(res$thetas_SE_history[nrow(res$thetas_SE_history),]),
                 0.3843882, tolerance = 1e-4)
    
    exposure <- rep(3L, nrow(df2))
    set.seed(1234)
    res <- mirtCAT(df2, mod, local_pattern=pat, criteria='MI',
                   design = list(min_SEM = .4, exposure=exposure), method = 'EAP', 
                   start_item=sample(c(1:nrow(df2)), 1))
    expect_equal(as.numeric(res$thetas), -0.2186294, tolerance = 1e-4)
    so <- summary(res)
    expect_equal(as.numeric(so$thetas_SE_history[nrow(so$thetas_SE_history),]),
                 0.3926076, tolerance = 1e-4)
    
    set.seed(1)
    exposure <- rep(0.75, nrow(df2))
    res <- mirtCAT(df2, mod, local_pattern=pat, criteria='MI',
                   design = list(min_SEM = .4, exposure=exposure), method = 'EAP', 
                   start_item=sample(c(1:nrow(df2)), 1))
    expect_equal(as.numeric(res$thetas), -.03342287, tolerance = 1e-4)
    so <- summary(res)
    expect_equal(as.numeric(so$thetas_SE_history[nrow(so$thetas_SE_history),]),
                 0.3986095, tolerance = 1e-4)
    
    set.seed(12)
    res <- mirtCAT(df2, mod, local_pattern=pat, 
                   design = list(min_SEM = .4), method = 'EAP', criteria='random')
    expect_equal(as.numeric(res$thetas), 0.07423068, tolerance = 1e-4)
    expect_equal(as.numeric(res$thetas_SE_history[nrow(res$thetas_SE_history),]),
                 0.3915826, tolerance = 1e-4)
    
    res <- mirtCAT(df2, mod, local_pattern=pat, 
                   design = list(min_SEM = .4), method = 'EAP', criteria='MEI')
    expect_equal(as.numeric(res$thetas), -.114286, tolerance = 1e-4)
    expect_equal(as.numeric(res$thetas_SE_history[nrow(res$thetas_SE_history),]),
                 0.3867981, tolerance = 1e-4)
    
    res <- mirtCAT(df2, mod, local_pattern=pat, 
                   design = list(min_SEM = .4), method = 'EAP', criteria='MEPV')
    expect_equal(as.numeric(res$thetas), 0.00942354, tolerance = 1e-4)
    expect_equal(as.numeric(res$thetas_SE_history[nrow(res$thetas_SE_history),]),
                 0.3843882, tolerance = 1e-4)
    
    res <- mirtCAT(df2, mod, local_pattern=pat, 
                   design = list(min_SEM = .4), method = 'EAP', criteria='MLWI')
    expect_equal(as.numeric(res$thetas), 0.00942354, tolerance = 1e-4)
    expect_equal(as.numeric(res$thetas_SE_history[nrow(res$thetas_SE_history),]),
                 0.3843882, tolerance = 1e-4)
    
    res <- mirtCAT(df2, mod, local_pattern=pat, 
                   design = list(min_SEM = .4), method = 'EAP', criteria='MPWI')
    expect_equal(as.numeric(res$thetas), 0.00942354, tolerance = 1e-4)
    expect_equal(as.numeric(res$thetas_SE_history[nrow(res$thetas_SE_history),]),
                 0.3843882, tolerance = 1e-4)
    
    res <- mirtCAT(df2, mod, local_pattern=pat, 
                   design = list(min_SEM = .4), method = 'EAP', criteria='KL')
    expect_equal(as.numeric(res$thetas), 0.00942354, tolerance = 1e-4)
    expect_equal(as.numeric(res$thetas_SE_history[nrow(res$thetas_SE_history),]),
                 0.3843882, tolerance = 1e-4)
    
    res <- mirtCAT(df2, mod, local_pattern=pat, 
                   design = list(min_SEM = .4), method = 'ML', criteria='KLn',
                   preCAT = list(max_items = 5L, criteria = 'seq', method = 'fixed'))
    expect_equal(as.numeric(res$thetas), -0.01885796, tolerance = 1e-4)
    expect_equal(as.numeric(res$thetas_SE_history[nrow(res$thetas_SE_history),]),
                 0.3995055, tolerance = 1e-4)
    
    res <- mirtCAT(df2, mod, local_pattern=pat, criteria='IKL',
                   design = list(min_SEM = .4), method = 'MAP')
    expect_equal(as.numeric(res$thetas), 0.213972, tolerance = 1e-4)
    expect_equal(as.numeric(res$thetas_SE_history[nrow(res$thetas_SE_history),]),
                 0.394090, tolerance = 1e-4)
    
    res <- mirtCAT(df2, mod, local_pattern=pat, criteria='IKLPn',
                   design = list(min_SEM = .4), method = 'MAP')
    expect_equal(as.numeric(res$thetas), 0.213972, tolerance = 1e-4)
    expect_equal(as.numeric(res$thetas_SE_history[nrow(res$thetas_SE_history),]),
                 0.3940905, tolerance = 1e-4)
    
    # content balancing
    set.seed(1)
    content <- c(rep('C1', 15), rep('C2', 10))
    content_prop <- c('C1'=.7, 'C2'=.3)
    res <- mirtCAT(df2, mod, local_pattern=pat, criteria='MI',
                   design = list(min_SEM = .4, content_prop=content_prop, content=content), 
                   method = 'MAP') #should crash with 'seq'
    so <- summary(res)
    expect_equal(so$items_answered[1:5], c(1,20,2,3,24))
    expect_equal(as.numeric(table(content[so$items_answered])/10), c(.7, .3))
    
    #pass other args through ...
    res <- mirtCAT(df2, mod, local_pattern=pat, 
                   design = list(min_SEM = .4), method = 'EAP', criteria='KL', theta_lim = c(-1,1))
    expect_equal(as.numeric(res$thetas), -0.1101559, tolerance = 1e-4)
    expect_equal(as.numeric(res$thetas_SE_history[nrow(res$thetas_SE_history),]),
                 0.3916425, tolerance = 1e-4)
    
    ## classification
    res <- mirtCAT(df2, mod, local_pattern=pat, criteria='MI',
                   design = list(classify = -0.5, classify_CI=.95))
    so <- summary(res)
    expect_true(so$classification == 'no decision')
    expect_equal(as.numeric(res$thetas), -0.11564, tolerance = 1e-4)
    
    ##fscores call
    responses <- res$scored_responses
    fs <- fscores(mod, response.pattern = responses)
    expect_equal(unname(fs[,'F1']), -0.1384693, tolerance = 1e-4)
    
    # excluded set
    res <- mirtCAT(df2, mod, local_pattern=pat, criteria='MI', 
                   design = list(max_items = 5, constraints = list(excluded = c(2:10))))
    expect_true(all(res$items_answered == c(1, 20, 15, 14, 24)))
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
    expect_equal(as.numeric(sos[[1]]$thetas_history[26,]), -0.3420789, tolerance = 1e-4)
    expect_equal(as.numeric(sos[[2]]$thetas_history[26,]), -0.8764826, tolerance = 1e-4)
    
})

