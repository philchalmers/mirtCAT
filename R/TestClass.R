Test <- setRefClass("Test", 
                    
                      fields = list(mirt_object = 'ConfirmatoryClass',
                                    ThetaGrid = 'matrix',
                                    item_answers = 'character',
                                    item_options = 'list',
                                    itemnames = 'character',
                                    nfact = 'integer',
                                    length = 'integer'),
                    
                      methods = list(
                          initialize = function(mirt_object, item_answers_in, item_options){
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
                              ThetaGrid <<- mirt:::thetaComb(seq(-6,6, length.out=49),
                                                             tmpobj@nfact)
                          })
                    
)