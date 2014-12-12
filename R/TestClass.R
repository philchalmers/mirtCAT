Test <- setRefClass("Test", 
                    
                      fields = list(mirt_object = 'SingleGroupClass',
                                    ThetaGrid = 'matrix',
                                    density = 'numeric',
                                    quadpts = 'numeric',
                                    theta_range = 'numeric',
                                    item_answers = 'list',
                                    item_options = 'list',
                                    item_class = 'character',
                                    itemnames = 'character',
                                    nfact = 'integer',
                                    length = 'integer',
                                    itemloc2 = 'integer',
                                    gp = 'list',
                                    fscores_args = 'list'),
                    
                      methods = list(
                          initialize = function(mirt_object, item_answers_in, item_options,
                                                quadpts_in, theta_range_in, dots){
                              tmpobj <- mirt_object
                              tmpobj@exploratory <- FALSE
                              itemnames <<- colnames(tmpobj@Data$data)
                              tmpobj@Data$mins <- rep(0L, length(tmpobj@Data$min))
                              mirt_object <<- tmpobj
                              item_class <<- sapply(mirt_object@pars, class)
                              if(is.null(item_answers_in))
                                  item_answers_in <- as.character(rep(NA, length(itemnames)))
                              item_answers <<- as.list(item_answers_in)
                              item_options <<- item_options
                              length <<- length(item_answers)
                              nfact <<- tmpobj@nfact
                              if(is.null(quadpts_in)) 
                                  quadpts <<- switch(as.character(tmpobj@nfact), 
                                                    '1'=61, '2'=31, '3'=15, '4'=9, '5'=7, 3)
                              else quadpts <<- quadpts_in
                              if(is.null(theta_range_in)) theta_range <<- c(-6, 6)
                              else theta_range <<- theta_range_in
                              gp <<- mirt:::ExtractGroupPars(mirt_object@pars[[length + 1L]])
                              if(tmpobj@nfact == 1L){
                                  ThetaGrid <<- mirt:::thetaComb(seq(theta_range[1L],theta_range[2L], 
                                                                     length.out=quadpts),
                                                                 tmpobj@nfact)
                                  density <<- mirt:::mirt_dmvnorm(ThetaGrid, mean=gp$gmeans, 
                                                                  sigma=gp$gcov)
                              }
                              tmp <- mirt_object@itemloc
                              itemloc2 <<- tmp[-length(tmp)]
                              tmp <- list(rotate = 'none', theta_lim = c(-6,6), mean = gp$gmean,
                                                    cov=gp$gcov, MI = 0)
                              if(length(dots)){
                                  if(!is.null(dots$rotate))
                                      warning('rotation not supported in mirtCAT. Using fixed
                                           slope coefficients')
                                  if(!is.null(dots$theta_lim))
                                      tmp$theta_lim <- dots$theta_lim
                                  if(!is.null(dots$mean))
                                      tmp$mean <- dots$mean
                                  if(!is.null(dots$cov))
                                      tmp$cov <- dots$cov
                                  if(!is.null(dots$MI))
                                      tmp$MI <- dots$MI
                              } 
                              fscores_args <<- tmp
                          })
                    
)