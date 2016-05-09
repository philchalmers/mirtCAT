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
    expect_true(all(pat == as.character(c(96,60,86,81,120,82,89,97,124,81,80,126,101,131,108,89,125,113,121,106,95,71,97,82,109,58,62,141,92,85,105,104,120,107,115,111,107,108,119,105))))
    
    set.seed(1234)
    pat2 <- generate_pattern(mod2, Theta = c(0, 1))
    expect_true(all(pat2 == c(0,1,1,0,1,1,0,1,1,1,1,0,0,1,1,1,0,0,0,0,1,0,0,0,1,1,1,1,1,0,1,1,1,1,0,1,0,1,1,1)))
    
    ## test numeric input
    res <- mirtCAT(mo=mod2, local_pattern=pat2)
    expect_equal(as.numeric(res$thetas), c(0.1445553, 0.5474594), tolerance = 1e-4)
    
    #sequential
    res <- mirtCAT(df, mod2, local_pattern=pat)
    expect_equal(as.numeric(res$thetas), c(0.6080447, 0.8036752), tolerance = 1e-4)
    expect_equal(as.numeric(res$thetas_SE_history[41,]), c(0.3960947, 0.4065036), tolerance = 1e-4)
    
    oo <- plot(res)
    expect_is(oo, 'trellis')
    oo <- plot(res, pick_theta=1)
    
    res <- mirtCAT(df, mod2, local_pattern=pat, criteria='DPrule',
                   design = list(min_SEM = .4))
    expect_equal(res$items_answered, c(1,20,21,37,3,5,35,30,24,36,16,11,39,14,29,7,13,23,32,18,17,22,12,15,28,19,10,26,27,38,31,6,25,9,8,34,4,33,2,40))
    expect_equal(as.numeric(res$thetas), c(0.6080447, 0.8036752), tolerance = 1e-4)
    expect_equal(as.numeric(res$thetas_SE_history[nrow(res$thetas_SE_history),]), 
                 c(0.3960947, 0.4065036), tolerance = 1e-4)
    
    res <- mirtCAT(df, mod2, local_pattern=pat, criteria='Drule',
                   design = list(min_SEM = .5))
    expect_equal(as.numeric(res$thetas), c(0.4718224, 0.8060655), tolerance = 1e-4)
    expect_equal(as.numeric(res$thetas_SE_history[nrow(res$thetas_SE_history),]), 
                 c(0.4937443, 0.4917037), tolerance = 1e-4)
    
    res <- mirtCAT(df, mod2, local_pattern=pat, 
                   design = list(min_SEM = .5), criteria = 'TPrule')
    expect_equal(as.numeric(res$thetas), c(0.4446583, 0.8397193), tolerance = 1e-4)
    expect_equal(as.numeric(res$thetas_SE_history[nrow(res$thetas_SE_history),]), 
                 c(0.4404435, 0.4914105), tolerance = 1e-4)
    
    res <- mirtCAT(df, mod2, local_pattern=pat, 
                   design = list(min_SEM = .5), criteria = 'WPrule')
    expect_equal(as.numeric(res$thetas), c(0.6661044, 1.0048121), tolerance = 1e-4)
    expect_equal(as.numeric(res$thetas_SE_history[nrow(res$thetas_SE_history),]), 
                 c(0.4564088, 0.4913090), tolerance = 1e-4)
    
    res <- mirtCAT(df, mod2, local_pattern=pat, 
                   design = list(min_SEM = .5), criteria = 'EPrule')
    expect_equal(as.numeric(res$thetas), c(0.4468275, 0.9738567), tolerance = 1e-4)
    expect_equal(as.numeric(res$thetas_SE_history[nrow(res$thetas_SE_history),]), 
                 c(0.4933418, 0.4957023), tolerance = 1e-4)
    
    res <- mirtCAT(df, mod2, local_pattern=pat, 
                   design = list(min_SEM = .5), criteria = 'APrule')
    expect_equal(as.numeric(res$thetas), c(0.5543021, 0.8456234), tolerance = 1e-4)
    expect_equal(as.numeric(res$thetas_SE_history[nrow(res$thetas_SE_history),]), 
                 c(0.488113, 0.493311), tolerance = 1e-4)
    
})

