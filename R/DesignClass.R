Design <- setRefClass("Design", 
                    
                    fields = list(method = 'character',
                                  criteria = 'character',
                                  adaptive = 'logical',
                                  Trule_weights = 'numeric'),
                    
                    methods = list(
                        initialize = function(method, criteria, adaptive, Trule_weights){
                            method <<- method
                            criteria <<- criteria
                            adaptive <<- adaptive
                            #TODO Trule_weights <<- rep(1, nfact)
                        })
                    
)