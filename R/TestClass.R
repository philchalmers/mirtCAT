Test <- setRefClass("Test", 
                    
                      fields = list(mirt_object = 'ConfirmatoryClass',
                                    ThetaGrid = 'matrix',
                                    density = 'numeric',
                                    quadpts = 'numeric',
                                    theta_range = 'numeric',
                                    item_answers = 'character',
                                    item_options = 'list',
                                    item_class = 'character',
                                    itemnames = 'character',
                                    nfact = 'integer',
                                    length = 'integer',
                                    itemloc2 = 'integer',
                                    gp = 'list'),
                    
                      methods = list(
                          initialize = function(mirt_object, item_answers_in, item_options,
                                                quadpts_in, theta_range_in){
                              tmpobj <- mirt_object
                              if(is(tmpobj, 'ExploratoryClass'))
                                  class(tmpobj) <- 'ConfirmatoryClass'
                              itemnames <<- colnames(tmpobj@Data$data)
                              tmpobj@Data$mins <- rep(0L, length(tmpobj@Data$min))
                              mirt_object <<- tmpobj
                              item_class <<- sapply(mirt_object@pars, class)
                              if(is.null(item_answers_in))
                                  item_answers_in <- as.character(rep(NA, length(itemnames)))
                              item_answers <<- item_answers_in
                              item_options <<- item_options
                              length <<- length(item_answers)
                              nfact <<- tmpobj@nfact
                              if(is.null(quadpts_in)) quadpts <<- 61
                              else quadpts <<- quadpts_in
                              if(is.null(theta_range_in)) theta_range <<- c(-6, 6)
                              else theta_range <<- theta_range_in
                              if(tmpobj@nfact == 1L){
                                  ThetaGrid <<- mirt:::thetaComb(seq(theta_range[1L],theta_range[2L], 
                                                                     length.out=quadpts),
                                                                 tmpobj@nfact)
                                  density <<- mirt:::mirt_dmvnorm(ThetaGrid)
                              }
                              tmp <- mirt_object@itemloc
                              itemloc2 <<- tmp[-length(tmp)]
                              gp <<- mirt:::ExtractGroupPars(mirt_object@pars[[length + 1L]])
                          })
                    
)