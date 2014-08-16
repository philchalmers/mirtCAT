context('ordered')

test_that('ordered', {
    mod <- mirt(Science, 1, TOL = 1) 
    itemnames <- colnames(Science)
    nitems <- ncol(Science)
    
    set.seed(1234)
    pat <- generate_pattern(mod, Theta = 0)
    expect_equal(c(3,2,3,2), pat)
    res <- mirtCAT(mirt_object = mod, local_pattern = pat)
    expect_equal(as.numeric(so$responses), c(3,2,3,2))
    expect_equal(print(res)[2], -0.6676493, tolerance=1e-4)
    
    shiny_questions <- questions <- vector('list', nitems)
    names(shiny_questions) <- names(questions) <- itemnames
    choices <- c('SD', 'D', 'A', 'SA')
    choices <- list(choices, choices, choices, choices)
    
    for(i in 1L:nitems){
        shiny_questions[[i]] <- radioButtons(inputId = itemnames[i],
                                             label = 'Dummy',
                                             choices = choices[[i]])
    }
    
    set.seed(1234)
    pat <- generate_pattern(mod, Theta = 0, choices = choices)
    expect_equal(c('A', 'D', 'A', 'D'), pat)
    
    res <- mirtCAT(shiny_questions, mod, local_pattern = pat)
    so <- summary(res)
    expect_equal(as.numeric(so$responses), c(3,2,3,2))
    expect_equal(print(res)[2], -0.6676493, tolerance=1e-4)
    
})