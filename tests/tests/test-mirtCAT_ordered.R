context('ordered')

test_that('ordered', {
    mod <- mirt(Science, 1, TOL = NaN, verbose=FALSE) 
    itemnames <- colnames(Science)
    nitems <- ncol(Science)
    
    set.seed(1234)
    pat <- generate_pattern(mod, Theta = 0)
    expect_equal(c(3,2,3,2), pat)
    res <- mirtCAT(mirt_object = mod, local_pattern = pat)
    so <- summary(res)
    expect_equal(as.numeric(so$responses), c(3,2,3,2))
    expect_equal(print(res)[2], -0.694133, tolerance=1e-4)
    
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
    expect_equal(print(res)[2], -0.694133, tolerance=1e-4)
    
    res <- mirtCAT(shiny_questions, local_pattern = pat)
    so <- summary(res)
    expect_equal(as.numeric(so$responses), c(3,2,3,2))
    
    res <- mirtCAT(shiny_questions, mod, local_pattern = pat, criteria = 'MI', method = 'ML')
    so <- summary(res)
    expect_equal(as.numeric(so$responses), c(3,2,2,3))
    
    CATdesign <- mirtCAT(shiny_questions, design_elements = TRUE)
    expect_equal(findNextItem(CATdesign), 1L) 
    CATdesign$person$responses[c(1,2)] <- c(4L, 4L)
    CATdesign$person$items_answered[c(1,2)] <- c(1L, 2L)
    CATdesign$person$thetas <- matrix(1.5)
    expect_equal(findNextItem(CATdesign), 3L) 
})