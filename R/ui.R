ui <- function(){
    
    return(pageWithSidebar(
        
        #  Application title
        headerPanel(MCE$shinyGUI$header),
        
        sidebarPanel(
            h4("Title:"),
            h5(MCE$shinyGUI$title),
            h4("Authors:"),
            h5(MCE$shinyGUI$author),
            hr(),
            h4("\nInstructions:"),
            h6("To progress through the survey/test, click the \'Next\' button.")
        ),
        
        mainPanel(            
            imageOutput("item_stem", width = 'auto', height = 'auto'),
            uiOutput("Main"),
            actionButton("Next", "Next")    
        )
        
    )) #end bootstrapPage
    
}