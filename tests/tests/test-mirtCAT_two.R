context('twofactor')

test_that('multidimensional', {
    
    set.seed(1234)
    nitems <- 40
    itemnames <- paste0('Item.', 1:nitems)
    a <- matrix(c(rlnorm(nitems/2, .2, .3), rnorm(nitems/4, 0, .3), numeric(nitems/2), 
                  rnorm(nitems/4, 0, .3), rlnorm(nitems/2, .2, .3)), nitems)
    d <- matrix(rnorm(nitems))
    dat <- simdata(a, d, 100, itemtype = 'dich', sigma = matrix(c(1,.5,.5,1),2)) #dummy data
    colnames(dat) <- itemnames
    
    #use population values for model
    sv <- mirt(dat, 2, pars = 'values')
    sv$value[sv$name == 'a1'] <- a[,1]
    sv$value[sv$name == 'a2'] <- a[,2]
    sv$value[sv$name == 'd'] <- d[,1]
    sv$value[sv$name == 'COV'] <- 0.5
    mod2 <- mirt(dat, 2, pars = sv, TOL = NaN)
    
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
    
    pat <- generate_pattern(mod2, Theta = c(0, 1), choices = choices, item_answers=answers)
    expect_true(all(pat == as.character(c(96,60,86,81,120,82,89,97,124,81,80,126,101,131,108,89,
                                          125,113,121,106,95,71,97,82,109,58,62,141,92,85,105,104,
                                          120,107,115,111,107,108,119,105))))
    
    set.seed(1234)
    pat2 <- generate_pattern(mod2, Theta = c(0, 1))
    expect_true(all(pat2 == c(1,1,1,0,1,1,0,1,1,1,0,0,0,0,1,1,0,0,1,0,1,1,0,1,1,1,1,0,
                                 0,0,1,1,1,1,1,1,1,1,0,1)))
    
    ## test numeric input
    res <- mirtCAT(mirt_object=mod2, local_pattern=pat2)
    expect_equal(as.numeric(res$thetas), c(0.09103849, 0.718113), tolerance = 1e-4)
    
    #sequential
    res <- mirtCAT(shiny_questions, mod2, item_answers=answers, local_pattern=pat)
    expect_equal(as.numeric(res$thetas), c(0.5529561, 0.7744459), tolerance = 1e-4)
    expect_equal(as.numeric(res$thetas_SE_history[41,]), c(0.3945902, 0.4083016), tolerance = 1e-4)
    
    oo <- plot(res)
    expect_is(oo, 'trellis')
    oo <- plot(res, pick_theta=1)
    
    #adaptive
    res <- mirtCAT(shiny_questions, mod2, item_answers=answers, local_pattern=pat, criteria='DPrule',
                   design = list(min_SEM = .5))
    expect_equal(as.numeric(res$thetas), c(0.3748623, 0.8975062), tolerance = 1e-4)
    expect_equal(as.numeric(res$thetas_SE_history[nrow(res$thetas_SE_history),]), 
                 c(0.49504,  0.4959223), tolerance = 1e-4)
    
    res <- mirtCAT(shiny_questions, mod2, item_answers=answers, local_pattern=pat, criteria='Drule',
                   design = list(min_SEM = .5))
    expect_equal(as.numeric(res$thetas), c(0.2470195, 0.9285525), tolerance = 1e-4)
    expect_equal(as.numeric(res$thetas_SE_history[nrow(res$thetas_SE_history),]), 
                 c(0.4969313, 0.4976671), tolerance = 1e-4)
    
    res <- mirtCAT(shiny_questions, mod2, item_answers=answers, local_pattern=pat, 
                   design = list(min_SEM = .5), criteria = 'TPrule')
    expect_equal(as.numeric(res$thetas), c(0.3748623, 0.8975062), tolerance = 1e-4)
    expect_equal(as.numeric(res$thetas_SE_history[nrow(res$thetas_SE_history),]), 
                 c(0.49504,  0.4959223), tolerance = 1e-4)
    
    res <- mirtCAT(shiny_questions, mod2, item_answers=answers, local_pattern=pat, 
                   design = list(min_SEM = .5), criteria = 'WPrule')
    expect_equal(as.numeric(res$thetas), c(0.3565503, 0.9837686), tolerance = 1e-4)
    expect_equal(as.numeric(res$thetas_SE_history[nrow(res$thetas_SE_history),]), 
                 c(0.4920396, 0.4857099), tolerance = 1e-4)
    
    res <- mirtCAT(shiny_questions, mod2, item_answers=answers, local_pattern=pat, 
                   design = list(min_SEM = .5), criteria = 'EPrule')
    expect_equal(as.numeric(res$thetas), c(0.3449841, 0.9562076), tolerance = 1e-4)
    expect_equal(as.numeric(res$thetas_SE_history[nrow(res$thetas_SE_history),]), 
                 c(0.4867828, 0.4995883), tolerance = 1e-4)
    
    res <- mirtCAT(shiny_questions, mod2, item_answers=answers, local_pattern=pat, criteria = 'KL')
    expect_equal(as.numeric(res$thetas), c(0.5529561, 0.7744459), tolerance = 1e-4)
    expect_equal(as.numeric(res$thetas_SE_history[nrow(res$thetas_SE_history),]), 
                 c(0.3945902, 0.4083016), tolerance = 1e-4)
    
    res <- mirtCAT(shiny_questions, mod2, item_answers=answers, local_pattern=pat, criteria = 'KLn')
    expect_equal(as.numeric(res$thetas), c(0.5529561, 0.7744459), tolerance = 1e-4)
    expect_equal(as.numeric(res$thetas_SE_history[nrow(res$thetas_SE_history),]), 
                 c(0.3945902, 0.4083016), tolerance = 1e-4)
    
})

