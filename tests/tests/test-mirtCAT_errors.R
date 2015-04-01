context('errors')

test_that('errors', {
    
    mod <- mirt(Science, 1, TOL = NaN, verbose=FALSE) 
    response <- c(0, 1, 2, 3)
    
    expect_error(mirtCAT(mo=mod, local_pattern = response), 
                 "For item 1, responses must be between 1 and 4. Please fix.")
                 
    
})