Test <- setRefClass("Test", 
                    
                      fields = list(mirt_object = 'ConfirmatoryClass',
                                    ThetaGrid = 'matrix',
                                    quadpts = 'numeric',
                                    theta_range = 'numeric',
                                    item_answers = 'character',
                                    item_options = 'list',
                                    itemnames = 'character',
                                    nfact = 'integer',
                                    length = 'integer'),
                    
                      methods = list(
                          initialize = function(mirt_object, item_answers_in, item_options,
                                                test_list){
                              tmpobj <- mirt_object
                              if(is(tmpobj, 'ExploratoryClass'))
                                  class(tmpobj) <- 'ConfirmatoryClass'
                              itemnames <<- colnames(tmpobj@Data$data)
                              tmpobj@Data$mins <- rep(0L, length(tmpobj@Data$min))
                              mirt_object <<- tmpobj
                              if(is.null(item_answers_in))
                                  item_answers_in <- as.character(rep(NA, length(itemnames)))
                              item_answers <<- item_answers_in
                              item_options <<- item_options
                              length <<- length(item_answers)
                              nfact <<- tmpobj@nfact
                              quadpts <<- 49
                              theta_range <<- c(-6, 6)
                              if(length(test_list)){
                                  if(!is.null(test_list$quadpts))
                                      quadpts <<- test_list$quadpts
                                  if(!is.null(test_list$theta_range))
                                      theta_range <<- test_list$theta_range
                              }
                              ThetaGrid <<- mirt:::thetaComb(seq(theta_range[1L],theta_range[2L], 
                                                                 length.out=quadpts),
                                                             tmpobj@nfact)
                          })
                    
)