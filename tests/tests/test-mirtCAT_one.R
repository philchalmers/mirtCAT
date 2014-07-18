context('mirtCAT')

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
    
    #sequential
    res <- mirtCAT(mod, shiny_questions, item_answers=answers, local_pattern=pat)
    expect_equal(as.numeric(res$thetas), 0.3588322, tolerance = 1e-4)
    expect_equal(as.numeric(res$thetas_SE_history[26,]), 0.3232215, tolerance = 1e-4)
    
    res <- mirtCAT(mod, shiny_questions, item_answers=answers, local_pattern=pat,
                   design_list = list(max_items = 5))
    expect_true(sum(!is.na(res$raw_responses)) == 5L && sum(!is.na(res$responses)) == 5L)
    expect_true(nrow(!is.na(res$thetas_history)) == 6L && nrow(!is.na(res$thetas_SE_history)) == 6L)
    
    #adaptive
    res <- mirtCAT(mod, shiny_questions, item_answers=answers, local_pattern=pat, adaptive=TRUE,
                   design_list = list(min_SEM = .4))
    expect_equal(as.numeric(res$thetas), 0.3708466, tolerance = 1e-4)
    expect_equal(as.numeric(res$thetas_SE_history[11L,]), 0.3930376, tolerance = 1e-4)
    expect_true(sum(!is.na(res$raw_responses)) == 10L && sum(!is.na(res$responses)) == 10L)
    expect_true(nrow(!is.na(res$thetas_history)) == 11L && nrow(!is.na(res$thetas_SE_history)) == 11L)
    
    res <- mirtCAT(mod, shiny_questions, item_answers=answers, local_pattern=pat, adaptive=TRUE,
                   design_list = list(min_SEM = .4), method = 'EAP')
    expect_equal(as.numeric(res$thetas), 0.2896889, tolerance = 1e-4)
    expect_equal(as.numeric(res$thetas_SE_history[nrow(res$thetas_SE_history),]),
                 0.3975697, tolerance = 1e-4)
    
    res <- mirtCAT(mod, shiny_questions, item_answers=answers, local_pattern=pat, adaptive=TRUE,
                   design_list = list(min_SEM = .4), method = 'EAP', criteria='random')
    expect_equal(as.numeric(res$thetas), 0.4229113, tolerance = 1e-4)
    expect_equal(as.numeric(res$thetas_SE_history[nrow(res$thetas_SE_history),]),
                 0.3916775, tolerance = 1e-4)
    
})

