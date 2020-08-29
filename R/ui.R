default_UI <- function(){
    
    sessionName <- 'MASTER'
    
    fluidPage(theme = if(.MCE[[sessionName]]$shinyGUI$theme != '') 
        if(requireNamespace("shinythemes", quietly = TRUE)){
            shinythemes::shinytheme(.MCE[[sessionName]]$shinyGUI$theme)
        } else NULL, 
        
        shiny::withMathJax(),
        
        tags$head(
            tags$style(HTML(.MCE[[sessionName]]$shinyGUI$css))
        ),
        
        #  Application title
        headerPanel(.MCE[[sessionName]]$shinyGUI$title),
        
        sidebarPanel(
            h5(textOutput("currentTime")),
            h5(em(textOutput("itemTime"))),
            if(.MCE[[sessionName]]$shinyGUI$author != '') h4("Authors:") else NULL,
            if(.MCE[[sessionName]]$shinyGUI$author != '') h5(.MCE[[sessionName]]$shinyGUI$author) else NULL,
            div(
                if(.MCE[[sessionName]]$shinyGUI$author != '') hr() else NULL,
                helpText("\n", .MCE[[sessionName]]$shinyGUI$instructions[1L]),            
                actionButton("Next", .MCE[[sessionName]]$shinyGUI$instructions[2L])
            )
        ),
        
        mainPanel(
            htmlOutput("item_stem_html"),
            uiOutput("Main")    
        )
        
    ) #end bootstrapPage
    
}