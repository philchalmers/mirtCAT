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
    sv$value[sv$name == 'COV_21'] <- 0.5
    mod2 <- mirt(dat, 2, pars = sv, TOL = NaN)
    
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
    
    df <- data.frame(Question=questions, Option=choices, Answer=answers,
                     Type = 'radio', stringsAsFactors = FALSE)
    
    pat <- generate_pattern(mod2, Theta = c(0, 1), df)
    expect_true(all(pat == as.character(c(74,96,107,118,102,88,114,96,101,93,80,100,92,52,105,98,120,120,81,114,87,102,68,96,111,100,106,107,108,139,90,124,58,122,114,98,56,114,98,66))))
    
    set.seed(1234)
    pat2 <- generate_pattern(mod2, Theta = c(0, 1))
    expect_true(all(pat2 == c(0,1,1,0,1,1,0,1,1,1,1,0,0,1,1,1,0,0,0,0,1,0,0,0,1,1,1,1,1,0,1,1,1,1,0,1,0,1,1,1)))
    
    ## test numeric input
    res <- mirtCAT(mo=mod2, local_pattern=pat2)
    expect_equal(as.numeric(res$thetas), c(0.1445553, 0.5474594), tolerance = 1e-4)
    
    #sequential
    res <- mirtCAT(df, mod2, local_pattern=pat)
    expect_equal(as.numeric(res$thetas), c(-0.2624228, 0.4408263), tolerance = 1e-4)
    expect_equal(as.numeric(res$thetas_SE_history[41,]), c(0.3690100, 0.3812945), tolerance = 1e-4)
    
    oo <- plot(res)
    expect_is(oo, 'trellis')
    oo <- plot(res, pick_theta=1)
    
    DE <- mirtCAT(df, mod2, design_elements = TRUE)
    res <- computeCriteria(DE, criteria = 'Trule')
    expect_equal(unname(res[1:4]), c(0.16260541, 0.11847584, 0.71467191, 0.08177519 ), tolerance=1e-4)
    res <- computeCriteria(DE, criteria = 'Trule', info_mats = TRUE)
    expect_is(res[[1]], 'matrix')
    expect_equal(diag(res[[1]]), c(0.1626054, 0.0000000), tolerance = 1e-4)
    
    res <- mirtCAT(df, mod2, local_pattern=pat, criteria='DPrule',
                   design = list(min_SEM = .4))
    expect_equal(res$items_answered, c(1,20,21,39,3,37,5,36,14,15,35,24,11,16,29,30,32,6,13,19,22,10,28,23,7,27,12,9,8,26,38,18,2,25,31))
    expect_equal(as.numeric(res$thetas), c(-0.2061207, 0.5225479), tolerance = 1e-4)
    expect_equal(as.numeric(res$thetas_SE_history[nrow(res$thetas_SE_history),]), 
                 c(0.3723255, 0.3933855), tolerance = 1e-4)
    
    res <- mirtCAT(df, mod2, local_pattern=pat, criteria='Drule',
                   design = list(min_SEM = .5))
    expect_equal(as.numeric(res$thetas), c(-0.06375424, 0.55081663), tolerance = 1e-4)
    expect_equal(as.numeric(res$thetas_SE_history[nrow(res$thetas_SE_history),]), 
                 c(0.4796803, 0.4957047), tolerance = 1e-4)
    
    res <- mirtCAT(df, mod2, local_pattern=pat, 
                   design = list(min_SEM = .5), criteria = 'TPrule')
    expect_equal(as.numeric(res$thetas), c(-0.08072858,0.51927881), tolerance = 1e-4)
    expect_equal(as.numeric(res$thetas_SE_history[nrow(res$thetas_SE_history),]), 
                 c(0.4483342,0.4926736), tolerance = 1e-4)
    
    res <- mirtCAT(df, mod2, local_pattern=pat, 
                   design = list(min_SEM = .5), criteria = 'WPrule')
    expect_equal(as.numeric(res$thetas), c(0.008799144,0.593384091), tolerance = 1e-4)
    expect_equal(as.numeric(res$thetas_SE_history[nrow(res$thetas_SE_history),]), 
                 c(0.4758097,0.4978876), tolerance = 1e-4)
    
    res <- mirtCAT(df, mod2, local_pattern=pat, 
                   design = list(min_SEM = .5), criteria = 'EPrule')
    expect_equal(as.numeric(res$thetas), c(0.008799144,0.593384091), tolerance = 1e-4)
    expect_equal(as.numeric(res$thetas_SE_history[nrow(res$thetas_SE_history),]), 
                 c(0.4758097,0.4978876), tolerance = 1e-4)
    
    res <- mirtCAT(df, mod2, local_pattern=pat, 
                   design = list(min_SEM = .5), criteria = 'APrule')
    expect_equal(as.numeric(res$thetas), c(0.0004752878, 0.6161572073), tolerance = 1e-4)
    expect_equal(as.numeric(res$thetas_SE_history[nrow(res$thetas_SE_history),]), 
                 c(0.477021,0.498364), tolerance = 1e-4)
    
})

