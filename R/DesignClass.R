Design <- setRefClass("Design", 
                    
                    fields = list(method = 'character',
                                  criteria = 'character',
                                  adaptive = 'logical',
                                  conjunctive = 'logical',
                                  Wrule_weights = 'numeric'),
                    
                    methods = list(
                        initialize = function(method, criteria, adaptive, nfact, design_list){
                            method <<- method
                            criteria <<- criteria
                            adaptive <<- adaptive
                            conjunctive <<- TRUE
                            Wrule_weights <<- rep(1/nfact, nfact)
                            if(length(design_list)){
                                browser()
                                
                                
                            }
                        })
                    
)