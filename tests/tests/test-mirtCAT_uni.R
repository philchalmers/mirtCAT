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
    
    #simple math items
    shiny_questions <- questions <- vector('list', nitems)
    names(shiny_questions) <- names(questions) <- itemnames
    answers <- character(nitems)
    choices <- vector('list', nitems)
    spacing <- floor(d - min(d)) + 1 #easier items have more variation in the options
    
    for(i in 1:nitems){
        n1 <- sample(1:50, 1)
        n2 <- sample(51:100, 1)
        ans <- n1 + n2
        questions[[i]] <- paste0(n1, ' + ', n2, ' = ?')
        answers[i] <- as.character(ans)
        ch <- ans + sample(c(-5:-1, 1:5) * spacing[i,], 5)
        ch[sample(1:5, 1)] <- ans
        choices[[i]] <- as.character(ch)
    }
    
    for(i in 1L:nitems){
        shiny_questions[[i]] <- radioButtons(inputId = itemnames[i],
                                             label = questions[[i]],
                                             choices = choices[[i]])
    }
    
    pat <- generate_pattern(mod, Theta = 0, choices = choices, item_answers=answers)
    expect_true(all(pat == as.character(c(67,90,109,118,111,127,118,129,112,97,93,98,77,110,98,
                             125,112,122,148,136,100,88,83,76,90))))
    pat2 <- generate_pattern(mod, Theta = -1, choices = choices, item_answers=answers)
    expect_true(all(pat2 == as.character(c(70,90,101,139,123,107,120,139,122,100,93,97,81,110,
                                           100,125,106,132,152,136,106,98,87,72,90))))
    
    #no scoring, just collecting
    res <- mirtCAT(shiny_questions, local_pattern=pat)
    expect_is(res, 'mirtCAT')
    expect_true(all(res$items_answered == 1:25))
    
    res <- mirtCAT(shiny_questions, local_pattern=pat, criteria='random')
    expect_true(all(!is.na(res$raw_responses)))
    
    #sequential
    res <- mirtCAT(shiny_questions, mod, item_answers=answers, local_pattern=pat)
    expect_equal(as.numeric(res$thetas), 0.3588322, tolerance = 1e-4)
    expect_equal(as.numeric(res$thetas_SE_history[26,]), 0.3232215, tolerance = 1e-4)
    
    oo <- plot(res)
    expect_is(oo, 'trellis')
    
    res <- mirtCAT(shiny_questions, mod, item_answers=answers, local_pattern=pat,
                   design = list(max_items = 5))
    expect_true(sum(!is.na(res$raw_responses)) == 5L && sum(!is.na(res$responses)) == 5L)
    expect_true(nrow(!is.na(res$thetas_history)) == 6L && nrow(!is.na(res$thetas_SE_history)) == 6L)
    
    #adaptive
    res <- mirtCAT(shiny_questions, mod, item_answers=answers, local_pattern=pat, criteria='MI',
                   design = list(min_SEM = .4))
    expect_equal(as.numeric(res$thetas), 0.3708466, tolerance = 1e-4)
    expect_equal(as.numeric(res$thetas_SE_history[11L,]), 0.3930376, tolerance = 1e-4)
    expect_true(sum(!is.na(res$raw_responses)) == 10L && sum(!is.na(res$responses)) == 10L)
    expect_true(nrow(!is.na(res$thetas_history)) == 11L && nrow(!is.na(res$thetas_SE_history)) == 11L)
    
    res <- mirtCAT(shiny_questions, mod, item_answers=answers, local_pattern=pat, criteria='MI',
                   design = list(min_SEM = .4), method = 'EAP')
    expect_equal(as.numeric(res$thetas), 0.2896889, tolerance = 1e-4)
    expect_equal(as.numeric(res$thetas_SE_history[nrow(res$thetas_SE_history),]),
                 0.3975697, tolerance = 1e-4)
    
    exposure <- rep(3L, length(shiny_questions))
    set.seed(1234)
    res <- mirtCAT(shiny_questions, mod, item_answers=answers, local_pattern=pat, criteria='MI',
                   design = list(min_SEM = .4), method = 'EAP', exposure=exposure,
                   start_item=sample(c(1:length(shiny_questions)), 1))
    expect_equal(as.numeric(res$thetas), 0.6289133, tolerance = 1e-4)
    so <- summary(res)
    expect_equal(as.numeric(so$thetas_SE_history[nrow(so$thetas_SE_history),]),
                 0.3921753, tolerance = 1e-4)
    
    set.seed(12)
    res <- mirtCAT(shiny_questions, mod, item_answers=answers, local_pattern=pat, 
                   design = list(min_SEM = .4), method = 'EAP', criteria='random')
    expect_equal(as.numeric(res$thetas), 0.02426384, tolerance = 1e-4)
    expect_equal(as.numeric(res$thetas_SE_history[nrow(res$thetas_SE_history),]),
                 0.398455, tolerance = 1e-4)
    
    res <- mirtCAT(shiny_questions, mod, item_answers=answers, local_pattern=pat, 
                   design = list(min_SEM = .4), method = 'EAP', criteria='MEI')
    expect_equal(as.numeric(res$thetas), 0.1963373, tolerance = 1e-4)
    expect_equal(as.numeric(res$thetas_SE_history[nrow(res$thetas_SE_history),]),
                 0.3991926, tolerance = 1e-4)
    
    res <- mirtCAT(shiny_questions, mod, item_answers=answers, local_pattern=pat, 
                   design = list(min_SEM = .4), method = 'EAP', criteria='MEPV')
    expect_equal(as.numeric(res$thetas), 0.1795497, tolerance = 1e-4)
    expect_equal(as.numeric(res$thetas_SE_history[nrow(res$thetas_SE_history),]),
                 0.3994549, tolerance = 1e-4)
    
    res <- mirtCAT(shiny_questions, mod, item_answers=answers, local_pattern=pat, 
                   design = list(min_SEM = .4), method = 'EAP', criteria='MLWI')
    expect_equal(as.numeric(res$thetas), 0.2896889, tolerance = 1e-4)
    expect_equal(as.numeric(res$thetas_SE_history[nrow(res$thetas_SE_history),]),
                 0.3975697, tolerance = 1e-4)
    
    res <- mirtCAT(shiny_questions, mod, item_answers=answers, local_pattern=pat, 
                   design = list(min_SEM = .4), method = 'EAP', criteria='MPWI')
    expect_equal(as.numeric(res$thetas), 0.2896889, tolerance = 1e-4)
    expect_equal(as.numeric(res$thetas_SE_history[nrow(res$thetas_SE_history),]),
                 0.3975697, tolerance = 1e-4)
    
    res <- mirtCAT(shiny_questions, mod, item_answers=answers, local_pattern=pat, 
                   design = list(min_SEM = .4), method = 'EAP', criteria='KL')
    expect_equal(as.numeric(res$thetas), 0.2896889, tolerance = 1e-4)
    expect_equal(as.numeric(res$thetas_SE_history[nrow(res$thetas_SE_history),]),
                 0.3975697, tolerance = 1e-4)
    
    res <- mirtCAT(shiny_questions, mod, item_answers=answers, local_pattern=pat, 
                   design = list(min_SEM = .4), method = 'ML', criteria='KLn',
                   preCAT = list(nitems = 5L, criteria = 'seq'))
    expect_equal(as.numeric(res$thetas), 0.188614, tolerance = 1e-4)
    expect_equal(as.numeric(res$thetas_SE_history[nrow(res$thetas_SE_history),]),
                 0.3966434, tolerance = 1e-4)
    
    res <- mirtCAT(shiny_questions, mod, item_answers=answers, local_pattern=pat, criteria='IKL',
                   design = list(min_SEM = .4), method = 'MAP')
    expect_equal(as.numeric(res$thetas), 0.1017891, tolerance = 1e-4)
    expect_equal(as.numeric(res$thetas_SE_history[nrow(res$thetas_SE_history),]),
                 0.3974447, tolerance = 1e-4)
    
    res <- mirtCAT(shiny_questions, mod, item_answers=answers, local_pattern=pat, criteria='IKLPn',
                   design = list(min_SEM = .4), method = 'MAP')
    expect_equal(as.numeric(res$thetas), 0.1017891, tolerance = 1e-4)
    expect_equal(as.numeric(res$thetas_SE_history[nrow(res$thetas_SE_history),]),
                 0.3974447, tolerance = 1e-4)
    
    # content balancing
    set.seed(1)
    content <- c(rep('C1', 10), rep('C2', 10), rep('C3', 5))
    content_prop <- c('C1'=.45, 'C2'=.35, 'C3'=.2)
    res <- mirtCAT(shiny_questions, mod, item_answers=answers, local_pattern=pat, criteria='random',
                   design = list(min_SEM = .4, content_prop=content_prop, content=content), 
                   method = 'MAP') #should crash with 'seq'
    so <- summary(res)
    expect_equal(so$items_answered[1:5], c(1,14,25,10,17))
    
    content_prop <- c('C1'=.8, 'C2'=.1, 'C3'=.1)
    res <- mirtCAT(shiny_questions, mod, item_answers=answers, local_pattern=pat, criteria='MI',
                   design = list(min_SEM = .4, content_prop=content_prop, content=content), 
                   method = 'MAP') 
    so <- summary(res)
    expect_equal(so$items_answered[1:5], c(1,20,2,3,24))
    
    #pass other args through ...
    res <- mirtCAT(shiny_questions, mod, item_answers=answers, local_pattern=pat, 
                   design = list(min_SEM = .4), method = 'EAP', criteria='KL', theta_lim = c(-1,1))
    expect_equal(as.numeric(res$thetas), 0.4432854, tolerance = 1e-4)
    expect_equal(as.numeric(res$thetas_SE_history[nrow(res$thetas_SE_history),]),
                 0.3612738, tolerance = 1e-4)
    
    ## classification
    res <- mirtCAT(shiny_questions, mod, item_answers=answers, local_pattern=pat, criteria='MI',
                   design = list(classify = -0.5, classify_CI=.95))
    so <- summary(res)
    expect_true(so$classification == 'above cutoff')
    expect_equal(as.numeric(res$thetas), 0.5866167, tolerance = 1e-4)
    
    
})

