context('classify')

test_that('classify', {
    
    set.seed(1234)
    nitems <- 25
    itemnames <- paste0('Item.', 1:nitems)
    a <- matrix(rlnorm(nitems, .2, .3))
    d <- matrix(rnorm(nitems))
    dat <- simdata(a, d, 500, itemtype = 'dich')
    colnames(dat) <- itemnames
    mod <- mirt(dat, 1, verbose = FALSE)
    
    set.seed(1)
    Theta <- matrix(c(-1.5, 0, 1.5))
    pats <- generate_pattern(mod, Theta = Theta)
    res <- mirtCAT(mo=mod, criteria = 'KL', start_item = 'MI', local_pattern = pats,
                   design = list(classify=0, classify_CI=.95))
    out <- sapply(res, function(x) x$classification)
    expect_true(all(out == c("below cutoff", "no decision", "above cutoff")))
    out <- sapply(res, function(x) x$thetas)
    expect_equal(out, c(-0.9336809, -0.5006111, 1.2879748), tolerance = 1e-4)
    
    set.seed(1234)
    nitems <- 50
    itemnames <- paste0('Item.', 1:nitems)
    a <- matrix(c(rlnorm(nitems/2, .2, .3), numeric(nitems), rlnorm(nitems/2, .2, .3)), 
                nitems, 2)
    d <- matrix(rnorm(nitems))
    dat <- simdata(a, d, 500, itemtype = 'dich')
    colnames(dat) <- itemnames
    mod2 <- mirt(dat, mirt.model('F1 = 1-25
                                 F2 = 26-50
                                 COV = F1*F2'), verbose = FALSE)
    Theta <- matrix(c(-1.5, 0, 1.5,-1.5,0,2), 3, 2)
    pats <- generate_pattern(mod2, Theta = Theta)
    res <- mirtCAT(mo=mod2, criteria = 'Drule', start_item = 'Drule', local_pattern = pats,
                   design = list(classify=c(0,0), classify_CI=.95))
    out <- sapply(res, function(x) x$classification)
    expect_true(all(as.character(out) == c("below cutoff", "below cutoff",
                                           "no decision", "no decision", 
                                           "above cutoff", "above cutoff")))
    out <- sapply(res, function(x) x$thetas)
    expect_equal(as.numeric(out), c(-0.9285472, -1.6153565, -0.3266127, -0.0433436,  1.0499338,  1.0864347), tolerance = 1e-2)
    
    preCAT <- list(response_variance = TRUE, min_items = 1, max_items = 20, method = 'fixed')    
    res <- mirtCAT(mo=mod, criteria = 'KL', start_item = 'MI', local_pattern = pats,
                   design = list(classify=0, classify_CI=.95), preCAT=preCAT)
    expect_equal(summary(res[[1]])$thetas_history[7:9,1], c(-0.7648334, -0.8532602, -0.7561935), tolerance = 1e-4)
    expect_equal(summary(res[[2]])$thetas_history[7:9,1], c(-0.3134263, -0.5170504, -0.3037696), tolerance = 1e-4)
    scored <- summary(res[[3]], sort=FALSE)$scored_responses
    out <- fscores(mod, response.pattern = scored)
    expect_equal(as.numeric(out[,'F1']), 1.04979, tolerance = 1e-4)
    
})