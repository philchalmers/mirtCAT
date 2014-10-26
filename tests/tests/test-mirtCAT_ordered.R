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
    
    mod2 <- mirt(Science, 2, TOL=NaN)
    res <- mirtCAT(shiny_questions, mod2, local_pattern = pat, criteria = 'Drule')
    so <- summary(res)
    expect_equal(as.numeric(so$responses), c(3,2,2,3))
    
    # MD
    set.seed(1)
    a <- matrix(c(rlnorm(50, .2, .3), numeric(100), rlnorm(50, .2, .3)), 100)
    d <- matrix(seq(1.5, -1.5, length.out = 4), 100, 4, byrow=TRUE) + rnorm(100)
    dat <- simdata(a, d, 100, itemtype = 'graded')
    model <- mirt.model('F1 = 1-50
                        F2 = 51-100')
    sv <- mirt(dat, model, pars='values')
    sv$value[sv$name == 'a1'] <- a[,1]
    sv$value[sv$name == 'a2'] <- a[,2]
    sv$value[sv$name %in% c('d1', 'd2', 'd3', 'd4')] <- as.numeric(t(d))
    mod <- mirt(dat, model, pars = sv, TOL=NaN)
    
    pat <- generate_pattern(mod, Theta = c(-0.5, 0.5))
    res <- mirtCAT(mirt_object = mod, local_pattern = pat, criteria = 'Drule')
    so <- summary(res)
    expect_equal(nrow(so$thetas_history), 22)
    expect_equal((so$items_answered), c(1,61,4,56,11,70,31,15,95,19,68,39,55,18,92,21,93,48,83,40,8))
    
    res <- mirtCAT(mirt_object = mod, local_pattern = pat, criteria = 'Drule', start_item = 10,
                   preCAT = list(method = 'fixed', nitems = 5, criteria = 'KL'), 
                   design = list(thetas.start = c(-0.5, 0.5)))
    so <- summary(res)
    expect_equal((so$items_answered), c(10,61,70,56,1,4,31,11,95,15,68,19,39,55,18,92,21,93,48,83,40))
    expect_equal(head(so$thetas_history[,1]), c(-0.5,-0.5,-0.5,-0.5,-0.5,-0.2594008),
                 tolerance = 1e-4)

    sv <- mirt(dat, model, itemtype = 'gpcm', pars='values')
    sv$value[sv$name == 'a1'] <- a[,1]
    sv$value[sv$name == 'a2'] <- a[,2]
    sv$value[sv$name %in% c('d1', 'd2', 'd3', 'd4')] <- as.numeric(t(d))
    mod <- mirt(dat, model, itemtype = 'gpcm', pars = sv, TOL=NaN)
    
    set.seed(1234)
    pat <- generate_pattern(mod, Theta = c(0,0))
    res <- mirtCAT(mirt_object = mod, local_pattern = pat, criteria = 'Drule', 
                   design = list(min_SEM=0.2))
    so <- summary(res)
    expect_equal((so$items_answered), c(1,61,11,70,4,31,56,39,83,15,92,95,50,68,21,55,18,93,
                                        43,40,48,19,96,44,59,25,87,20,8,89,73))
    expect_equal(as.numeric(so$thetas_history[nrow(so$thetas_history), ]), 
                 c(-0.08138413, -0.29611701), tolerance = 1e-4)
})