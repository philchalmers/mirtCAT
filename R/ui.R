ui <- function(){
    
    return(fluidPage(
        
        tags$head(
            tags$style(HTML(MCE$shinyGUI$css))
        ),
        
        #  Application title
        headerPanel(MCE$shinyGUI$title),
        
        if(is.finite(MCE$design@max_time)){
            h6(paste0('Time remaining: ', 
                      formatTime(MCE$design@max_time - sum(MCE$person$item_time))))
        } else NULL,
        
        sidebarPanel(
            if(MCE$shinyGUI$author != '') h4("Authors:") else NULL,
            if(MCE$shinyGUI$author != '') h5(MCE$shinyGUI$author) else NULL,
            if(MCE$shinyGUI$author != '') hr() else NULL,
            h4("\nInstructions:"),
            h5("To progress through the interface, click on the button below."),            
            actionButton("Next", "Next")
        ),
        
        mainPanel(
            imageOutput("item_stem", width = 'auto', height = 'auto'),
            uiOutput("Main")    
        )
        
    )) #end bootstrapPage
    
}