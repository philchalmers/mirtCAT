Test <- setRefClass("Test", 
                    
                      fields = list(mirt_object = 'ConfirmatoryClass',
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
                              tmpobj@Data$min <- rep(0L, length(tmpobj@Data$min))
                              mirt_object <<- tmpobj
                              item_answers <<- item_answers_in
                              item_options <<- item_options
                              length <<- length(item_answers)
                              nfact <<- tmpobj@nfact
                          })
                    
)