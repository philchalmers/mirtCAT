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
    mod2@Options$exploratory <- FALSE
    
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
    expect_true(all(pat == as.character(c(120,102,155,122,99,70,79,95,69,119,71,87,86,86,152,78,119,98,59,111,115,89,119,110,83,133,102,106,81,108,77,114,77,99,80,95,99,104,108,113))))
    
    set.seed(1234)
    pat2 <- generate_pattern(mod2, Theta = c(0, 1))
    expect_true(all(pat2 == c(0,1,1,0,1,1,0,1,1,1,1,0,0,1,1,1,0,0,0,0,1,0,0,0,1,1,1,1,1,0,1,1,1,1,0,1,0,1,1,1)))
    
    ## test numeric input
    res <- mirtCAT(mo=mod2, local_pattern=pat2)
    expect_equal(as.numeric(res$thetas), c(0.1445555, 0.5474600), tolerance = 1e-4)
    
    #sequential
    res <- mirtCAT(df, mod2, local_pattern=pat)
    expect_equal(as.numeric(res$thetas), c(-0.8075782,  0.7005797), tolerance = 1e-4)
    expect_equal(as.numeric(res$thetas_SE_history[41,]), c(0.3948417, 0.3981225), tolerance = 1e-4)
    
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
    expect_equal(res$items_answered, c(1,20,21,37,3,39,5,36,14,15,6,35,24,11,16,29,30,9,2,8,32,10,19,22,23,27,28,13,38,12,31,26,34,7,25,18,17,4))
    expect_equal(as.numeric(res$thetas), c(-0.8116076,0.6794745), tolerance = 1e-4)
    expect_equal(as.numeric(res$thetas_SE_history[nrow(res$thetas_SE_history),]), 
                 c(0.3950384,0.3999207), tolerance = 1e-4)
    
    res <- mirtCAT(df, mod2, local_pattern=pat, criteria='Drule',
                   design = list(min_SEM = .5))
    expect_equal(as.numeric(res$thetas), c(-0.3512403,0.581766), tolerance = 1e-4)
    expect_equal(as.numeric(res$thetas_SE_history[nrow(res$thetas_SE_history),]), 
                 c(0.4805048,0.498071), tolerance = 1e-4)
    
    res <- mirtCAT(df, mod2, local_pattern=pat, 
                   design = list(min_SEM = .5), criteria = 'TPrule')
    expect_equal(as.numeric(res$thetas), c(-0.4911178,0.5707988), tolerance = 1e-4)
    expect_equal(as.numeric(res$thetas_SE_history[nrow(res$thetas_SE_history),]), 
                 c(0.4710924,0.4971046), tolerance = 1e-4)
    
    res <- mirtCAT(df, mod2, local_pattern=pat, 
                   design = list(min_SEM = .5), criteria = 'WPrule')
    expect_equal(as.numeric(res$thetas), c(-0.2761875,0.3295394), tolerance = 1e-4)
    expect_equal(as.numeric(res$thetas_SE_history[nrow(res$thetas_SE_history),]), 
                 c(0.4974878,0.4971879), tolerance = 1e-4)
    
    res <- mirtCAT(df, mod2, local_pattern=pat, 
                   design = list(min_SEM = .5), criteria = 'EPrule')
    expect_equal(as.numeric(res$thetas), c(-0.2761875,0.3295394), tolerance = 1e-4)
    expect_equal(as.numeric(res$thetas_SE_history[nrow(res$thetas_SE_history),]), 
                 c(0.4974878,0.4971879), tolerance = 1e-4)
    
    res <- mirtCAT(df, mod2, local_pattern=pat, 
                   design = list(min_SEM = .5), criteria = 'APrule')
    expect_equal(as.numeric(res$thetas), c(-0.2761875,0.3295394), tolerance = 1e-4)
    expect_equal(as.numeric(res$thetas_SE_history[nrow(res$thetas_SE_history),]), 
                 c(0.4974878,0.4971879), tolerance = 1e-4)
    
})

