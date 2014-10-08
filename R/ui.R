ui <- function(){
    
    return(fluidPage(
        
        tags$head(
            tags$style(HTML(MCE$shinyGUI$css))
        ),
        
        #  Application title
        headerPanel(MCE$shinyGUI$title),
        
        sidebarPanel(
            h4("Authors:"),
            h5(MCE$shinyGUI$author),
            hr(),
            h4("\nInstructions:"),
            h5("To progress through the survey/test, click on the button below."),            
            actionButton("Next", "Next")
        ),
        
        mainPanel(
            imageOutput("item_stem", width = 'auto', height = 'auto'),
            uiOutput("Main")    
        )
        
    )) #end bootstrapPage
    
}