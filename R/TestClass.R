Test <- setRefClass("Test", 
                    
                      fields = list(mirt_object = 'ConfirmatoryClass',
                                    item_answers = 'character',
                                    item_options = 'list',
                                    itemnames = 'character',
                                    method = 'character',
                                    criteria = 'character',
                                    nfact = 'integer',
                                    adaptive = 'logical',
                                    length = 'integer'),
                    
                      methods = list(
                          initialize = function(mirt_object, item_answers_in, item_options, 
                                                method, criteria, adaptive){
                              tmpobj <- mirt_object
                              if(is(tmpobj, 'ExploratoryClass'))
                                  class(tmpobj) <- 'ConfirmatoryClass'
                              itemnames <<- colnames(tmpobj@Data$data)
                              tmpobj@Data$min <- rep(0L, length(tmpobj@Data$min))
                              mirt_object <<- tmpobj
                              if(is.null(item_answers_in))
                                  item_answers_in <- as.character(rep(NA, length(itemnames)))
                              item_answers <<- item_answers_in
                              item_options <<- item_options
                              length <<- length(item_answers)
                              nfact <<- tmpobj@nfact
                              method <<- method
                              criteria <<- criteria
                              adaptive <<- adaptive
                          })
                    
)