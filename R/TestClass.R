Test <- setRefClass("Test", 
                    
                      fields = list(mirt_object = 'ConfirmatoryClass',
                                    item_answers = 'character',
                                    item_options = 'list',
                                    itemnames = 'character',
                                    nfact = 'integer',
                                    adaptive = 'logical',
                                    length = 'integer'),
                    
                      methods = list(
                          initialize = function(mirt_object, item_answers, item_options, adaptive=FALSE){
                              tmpobj <- mirt_object
                              if(is(tmpobj, 'ExploratoryClass'))
                                  class(tmpobj) <- 'ConfirmatoryClass'
                              itemnames <<- colnames(tmpobj@Data$data)
                              mirt_object <<- tmpobj
                              item_answers <<- item_answers
                              item_options <<- item_options
                              length <<- length(item_answers)
                              nfact <<- tmpobj@nfact
                              adaptive <<- adaptive
                          })
                    
)