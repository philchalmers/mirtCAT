context('ordered')

test_that('ordered', {
    mod <- mirt(Science, 1, TOL = NaN, verbose=FALSE) 
    itemnames <- colnames(Science)
    nitems <- ncol(Science)
    
    set.seed(1)
    pat <- generate_pattern(mod, Theta = 0)
    expect_equal(c(3,3,3,4), pat[1,])
    res <- mirtCAT(mo = mod, local_pattern = pat)
    so <- summary(res)
    expect_equal(as.numeric(so$raw_responses), c(3,3,3,4))
    expect_equal(so$final_estimates[1], .4418983, tolerance=1e-4)
    
    #fscores call
    responses <- res$scored_responses
    fs <- fscores(mod, response.pattern = responses)
    expect_equal(unname(fs[,'F1']), 0.4293075, tolerance = 1e-4)
    
    choices <- c('SD', 'D', 'A', 'SA')
    df <- data.frame(Type = 'radio', Question = as.character(1:nitems), stringsAsFactors = FALSE)
    df$Option.1 <- 'SD'
    df$Option.2 <- 'D'
    df$Option.3 <- 'A'
    df$Option.4 <- 'SA'
    
    set.seed(1234)
    pat <- generate_pattern(mod, Theta = 0, df=df)
    expect_equal(c('A', 'D', 'A', 'D'), pat)
    
    res <- mirtCAT(df, mod, local_pattern = pat)
    so <- summary(res)
    expect_equal(as.numeric(so$raw_responses), c(3,2,3,2))
    expect_equal(so$final_estimates[1], -0.694133, tolerance=1e-4)
    
    res <- mirtCAT(df, local_pattern = pat)
    so <- summary(res)
    expect_equal(as.numeric(so$raw_responses), c(3,2,3,2))
    
    res <- mirtCAT(df, mod, local_pattern = pat, criteria = 'MI', method = 'ML')
    so <- summary(res)
    expect_equal(as.numeric(so$raw_responses), c(3,2,2,3))
    
    CATdesign <- mirtCAT(df, design_elements = TRUE)
    expect_equal(findNextItem(CATdesign), 1L) 
    CATdesign$person$responses[c(1,2)] <- c(4L, 4L)
    CATdesign$person$items_answered[c(1,2)] <- c(1L, 2L)
    CATdesign$person$thetas <- matrix(1.5)
    expect_equal(findNextItem(CATdesign), 3L) 
    
    mod2 <- mirt(Science, 2, TOL=NaN)
    res <- mirtCAT(df, mod2, local_pattern = pat, criteria = 'Drule')
    so <- summary(res)
    expect_equal(as.numeric(so$raw_responses), c(3,2,2,3))
    
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
    res <- mirtCAT(mo = mod, local_pattern = pat, criteria = 'Drule')
    so <- summary(res)
    expect_equal(nrow(so$thetas_history), 22)
    expect_equal((so$items_answered), c(1,61,4,70,11,56,31,95,15,19,68,39,55,18,92,83,21,48,93,40,8))
    
    res <- mirtCAT(mo = mod, local_pattern = pat, criteria = 'Drule', start_item = 10,
                   preCAT = list(method = 'fixed', max_items = 5, criteria = 'KL'), 
                   design = list(thetas.start = c(-0.5, 0.5)))
    so <- summary(res)
    expect_equal((so$items_answered), c(10,61,70,56,1,4,31,11,95,15,68,19,39,55,18,92,83,21,48,93,40))
    expect_equal(head(so$thetas_history[,1]), c(-0.5,-0.5,-0.5,-0.5,-0.5,-0.2594009),
                 tolerance = 1e-4)

    sv <- mirt(dat, model, itemtype = 'gpcm', pars='values')
    sv$value[sv$name == 'a1'] <- a[,1]
    sv$value[sv$name == 'a2'] <- a[,2]
    sv$value[sv$name %in% c('d1', 'd2', 'd3', 'd4')] <- as.numeric(t(d))
    mod <- mirt(dat, model, itemtype = 'gpcm', pars = sv, TOL=NaN)
        
    set.seed(1234)
    pat <- generate_pattern(mod, Theta = c(0,0))
    res <- mirtCAT(mo = mod, local_pattern = pat, criteria = 'Drule', 
                   design = list(min_SEM=0.2))
    so <- summary(res)
    expect_equal((so$items_answered), c(1,61,4,56,11,70,31,95,39,68,15,55,50,21,93,18,83,43,92,40))
    expect_equal(as.numeric(so$thetas_history[nrow(so$thetas_history), ]), 
                 c(0.02524291, -0.09594028), tolerance = 1e-4)
    
    # generate.mirt_object tests
    set.seed(1)
    nitems <- 50
    a1 <- rlnorm(nitems, .2,.2)
    d <- rnorm(nitems)
    g <- rbeta(nitems, 20, 80)
    pars <- data.frame(a1=a1, d=d, g=g)
    obj <- generate.mirt_object(pars, '3PL')
    expect_is(obj, 'SingleGroupClass')
    cfs <- coef(obj, simplify=TRUE, digits = 50)
    expect_equal(as.numeric(cfs[[1]][1:3, 1:2]), as.numeric(as.matrix(pars[1:3, 1:2])), 
                 tolerance = 1e-10)
    
    #parallel test
    require(parallel, quietly=TRUE, warn.conflicts=FALSE)
    cl <- makeCluster(4)
    pats <- generate_pattern(obj, Theta = matrix(c(-2,-1,1,2),4))
    ret <- mirtCAT(mo=obj, local_pattern = pats, criteria = 'MI')
    ret2 <- mirtCAT(mo=obj, local_pattern = pats, criteria = 'MI', cl=cl)
    for(i in 1:4)
        expect_true(as.numeric(ret[[i]]$thetas_SE_history[43,]) == 
                         as.numeric(ret2[[i]]$thetas_SE_history[43,]))
    stopCluster(cl)
})