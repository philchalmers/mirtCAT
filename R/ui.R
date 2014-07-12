ui <- function(){
    
    return(pageWithSidebar(
        
        #  Application title
        headerPanel("mirtCAT v.01"),
        
        sidebarPanel(
            h4("Title:"),
            h5(MCE$shinyGUI$title),
            h4("Authors:"),
            h5(MCE$shinyGUI$author),
            h4("\nInstructions:"),
            h6("To progress through the survey, click the \'Next\' button.")
        ),
        
        mainPanel(            
            uiOutput("Main"),
            actionButton("Next", "Next")    
        )
        
    )) #end bootstrapPage
    
}