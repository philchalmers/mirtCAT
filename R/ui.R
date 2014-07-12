ui <- function(){
    
    return(bootstrapPage(
        
        headerPanel("mirtCAT"),
        
        mainPanel(            
            uiOutput("Main"),
            actionButton("Next", "Next")    
        )
        
    )) #end bootstrapPage
    
}