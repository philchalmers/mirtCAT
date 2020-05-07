library("mirtCAT")
setwd('tests/GUI-tests/')

options(stringsAsFactors = FALSE)

# simple interface, no stems
options <- matrix(c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree"),
                  nrow = 3, ncol = 5, byrow = TRUE)
questions <- c("Building CATs with mirtCAT is difficult.",
               "mirtCAT requires a substantial amount of coding.",
               "I would use mirtCAT in my research.")
df <- data.frame(Question = questions, Option = options, Type = "radio")

results <- mirtCAT(df = df)
summary(results)

# max_time
results <- mirtCAT(df = df, design = list(max_time = 600))
summary(results)

# manual HTML tags, not forced
df2 <- df
questions <- c("Building <strong>CATs</strong> with mirtCAT is <br> <br> difficult.",
               "mirtCAT requires a substantial amount of coding.",
               "I would use mirtCAT in my research.")
results <- mirtCAT(df = df2, shinyGUI=list(forced_choice = FALSE))
summary(results)

# correct answer scoring
df2 <- df
df2$Answer <- c('Agree', 'Agree', 'Agree')
df2$Mastery <- c(TRUE, FALSE, FALSE)
results <- mirtCAT(df = df2)
summary(results)

# character coercion of R HTML constructors
questions <- c(as.character(div(strong('Something'), br(), strong('Something'))),
               "mirtCAT requires a substantial amount of coding.",
               "I would use mirtCAT in my research.")
df3 <- data.frame(Question = questions, Option = options, Type = "radio")

results <- mirtCAT(df = df3)
summary(results)

#theme
results <- mirtCAT(df = df, shinyGUI = list(theme = 'journal'))

# password
results <- mirtCAT(df = df, shinyGUI = list(password = data.frame('1234')))
summary(results)
results <- mirtCAT(df = df, shinyGUI = 
                list(password = data.frame(c('user1', 'user2'), c('1234', '1234'))))
summary(results)

# css mod ('Readable' file downloaded from http://bootswatch.com/)
css <- readLines('bootstrap.css')
results <- mirtCAT(df = df, shinyGUI = list(css = css))

# select input
df2 <- df
df2$Type[1] <- 'select'
results <- mirtCAT(df = df2)

df2 <- df
df2$Type[1] <- 'rankselect'
results <- mirtCAT(df = df2)

# width/inline change
df$inline <- TRUE
df$width <- "50%"
results2 <- mirtCAT(df = df, shinyGUI = list(forced_choice = FALSE))
summary(results2)

# choiceNames
df2 <- df
choiceNames <- list(
    list(
        icon("calendar"),
        HTML("<p style='color:red;'>Red Text</p>"),
        "Normal text"),
    NULL, NULL
)
choiceValues = list(
    list("icon", "html", "text"), NULL, NULL)
results2 <- mirtCAT(df = df, shinyGUI = list(forced_choice = TRUE, 
                                             choiceNames=choiceNames,
                                             choiceValues=choiceValues))
summary(results2)

# mathJax test
df3 <- df
df3$Question[1] <- 'Something something \\(\\sqrt{2}\\)? Why yes, $$e = mc^2$$'
df3$Option.1[1] <- '\\(\\alpha\\)'
df3$Option.1[2] <- '\\(\\beta\\)'
results <- mirtCAT(df = df3, shinyGUI = list(forced_choice = FALSE))
summary(results)

# change final message
lastpagefun <- function(person){
    #browser() ## browser can be helpful here to see the contents of 'person'
    est <- as.vector(person$thetas)
    return(list(h5("You have successfully completed the interface."), 
                   h6(sprintf("Your final theta estimate to two decimal places is %.2f.", est))))
} 
results2 <- mirtCAT(df = df, shinyGUI = list(forced_choice = FALSE, lastpage=lastpagefun))

# save and resume temp file
results <- mirtCAT(df = df, shinyGUI = list(temp_file = 'thisfile.rds')) #stop early
summary(results)
results <- mirtCAT(df = df, shinyGUI = list(temp_file = 'thisfile.rds')) #this resumes and deletes
summary(results)

## two step hosting
sessionName <- 'My session'
my_fun <- function(person) cat('Hello world\n')
mirtCAT_preamble(sessionName=sessionName, df=df, final_fun = my_fun)
runApp(createShinyGUI(sessionName=sessionName), port = 8000)
person <- getPerson(sessionName=sessionName)
summary(person)

# custom UI
myUI <- function(sessionName){
    fluidPage(
        
        shiny::withMathJax(), 
        
        mainPanel(
            uiOutput("Main")    
        ),
        
        sidebarPanel(
            actionButton("Next", 'Next')
        )
        
    ) #end bootstrapPage
}

mirtCAT_preamble(sessionName=sessionName, df=df)
runApp(createShinyGUI(sessionName=sessionName, ui=myUI), port = 8000)
person2 <- mirtCAT(df=df, shinyGUI=list(ui=myUI))
summary(person2)

# slider input
df$Option.5 <- NULL
df$min <- c(1,NA,NA)
df$max <- c(5,NA,NA)
df$value <- c(3,NA,NA)
df$step <- c(1, NA, NA)
df$Type[1] <- 'slider'
results <- mirtCAT(df = df, shinyGUI = list(forced_choice = FALSE))
summary(results)

df <- data.frame(Question = questions, min=rep(1,3), max=rep(5,3), step=rep(1,3), Type = 'slider')
results2 <- mirtCAT(df = df, shinyGUI = list(forced_choice = FALSE))
summary(results2)

# text input
df <- data.frame(Question = questions, Type = 'text')
results <- mirtCAT(df = df, shinyGUI = list(forced_choice = FALSE))
summary(results)

# textArea input
df <- data.frame(Question = questions, Type = 'textArea')
df$height = '50%'
results <- mirtCAT(df = df, shinyGUI = list(forced_choice = FALSE))
summary(results)

# HTML/markdown stems
df <- data.frame(Question = c("", "", "Just a standard stem."), Option = options, Type = "radio",
                 Stem = c('Math-stem.html', 'Question.md', ''))
results <- mirtCAT(df = df, shinyGUI = list(forced_choice = FALSE))

# expressions
df <- data.frame(Question = c("", as.character(h1("My header")), 
                              as.character(tags$b("This text is bold."))), 
                 Option = options, Type = "radio",
                 Stem = c('Math-stem.html', '', ''))
results <- mirtCAT(df = df, shinyGUI = list(forced_choice = FALSE))

# divs
df <- data.frame(Question = sapply(list(div(HTML("Here is <strong>one</strong> way to insert <em>arbitrary</em> HTML.")), 
                                        div(tags$style("#text { font-size: 35px; height: 200px; overflow: auto; }"), 
                                            div(id = "text", paste(names(tags), collapse = ", "))), 
                                        div(tags$style("#text { font-size: 20px; height: 65px; overflow: auto; }"), 
                                            div(id = "text", paste(names(tags), collapse = ", ")))), as.character)
                     , 
                 Option = options, Type = "radio")
results <- mirtCAT(df = df, shinyGUI = list(forced_choice = FALSE))

# table panels
df <- data.frame(Question = c(as.character(tabsetPanel(tabPanel("Panel 1", "some text"), 
                                                       tabPanel("Panel 2", "some more text"))),
                              'Something', 
                              as.character(tags$b("This text is bold."))),
                 Option = options, Type = "radio")
results <- mirtCAT(df = df, shinyGUI = list(forced_choice = FALSE))

# custom choices
myfun <- function(inputId, df_row){
    tabsetPanel(tabPanel("Panel 1", "some text"), 
                tabPanel("Panel 2", "some more text"), 
                tabPanel("Response", "Final Text", 
                         radioButtons(inputId = inputId, label='', 
                                      choices = c('True', 'False'), selected = '')))
}

options <- matrix(c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree"),
                  nrow = 3, ncol = 5, byrow = TRUE)
questions <- c("",
               "mirtCAT requires a substantial amount of coding.",
               "I would use mirtCAT in my research.")
df <- data.frame(Question = questions, Option = options, Type = "radio")
df$Type[1] <- 'myQ'
results <- mirtCAT(df = df, shinyGUI = list(forced_choice = FALSE),
                   customTypes=list(myQ=myfun))

# audio/video
dirname <- paste0(getwd(), '/www')
shiny::addResourcePath('www', dirname)
df <- data.frame(Question = c("", 
                              as.character(tags$audio(src = "www/clip.mp3", type = "audio/mp3",
                                                      autoplay = TRUE, controls = TRUE)),
                              as.character(tags$video(src = "www/vid.mp4", type = "video/mp4",
                                         controls = TRUE, height=260, width=260))),
                              Option = options, Type = "radio",
                 Stem = c('Math-stem.html', '', ''))
results <- mirtCAT(df = df, shinyGUI = list(forced_choice = FALSE))

# checkbox input
df <- data.frame(Question = questions, Option=options, Type = 'checkbox')
results <- mirtCAT(df = df, shinyGUI = list(forced_choice = FALSE))
summary(results)

# none type
df <- data.frame(Question = c('Empty Q', questions), 
                 Options=rbind(NA, options), Type = c('none', rep('radio', 3)))
results <- mirtCAT(df = df)
summary(results)
results <- mirtCAT(df = df, shinyGUI = list(forced_choice = FALSE))
summary(results)

#------------------------------------------------------------
### more lower level tests
set.seed(1234)
nitems <- 120
itemnames <- paste0("Item.", 1:nitems)
a <- matrix(c(rlnorm(nitems/2, 0.2, 0.3), rnorm(nitems/4, 0, 0.3), numeric(nitems/2),
              rnorm(nitems/4, 0, 0.3), rlnorm(nitems/2, 0.2, 0.3)), nitems)
d <- matrix(rnorm(nitems))
pars <- data.frame(a, d)
colnames(pars) <- c("a1", "a2", "d")
trait_cov <- matrix(c(1, 0.5, 0.5, 1), 2, 2)

# create mirt_object
mod <- generate.mirt_object(pars, itemtype = "2PL", latent_covariance = trait_cov)

# math items definitions addition for one factor and multiplication for the other
questions <- answers <- character(nitems)
options <- matrix("", nitems, 5)
spacing <- floor(d - min(d)) + 1  #easier items have more variation

for (i in 1:nitems) {
    if (i < 31) {
        # addition
        n1 <- sample(1:100, 1)
        n2 <- sample(101:200, 1)
        ans <- n1 + n2
        questions[i] <- paste0(n1, " + ", n2, " = ?")
    } else if (i < 61) {
        # addition and multiplication
        n1 <- sample(1:50, 1)
        n2 <- sample(51:100, 1)
        m1 <- sample(1:10, 1)
        m2 <- sample(1:10, 1)
        ans <- n1 + n2 + m1 * m2
        questions[i] <- paste0(n1, " + ", n2, " + ", m1, " * ", m2, " = ?")
    } else if (i < 91) {
        # multiplication and addition
        n1 <- sample(1:10, 1)
        n2 <- sample(1:10, 1)
        m1 <- sample(1:25, 1)
        m2 <- sample(1:25, 1)
        ans <- n1 + n2 + m1 * m2
        questions[i] <- paste0(m1, " * ", m2, " + ", n1, " + ", n2, " = ?")
    } else {
        # multiplication
        m1 <- sample(1:50, 1)
        m2 <- sample(1:50, 1)
        ans <- n1 + n2 + m1 * m2
        questions[i] <- paste0(m1, " * ", m2, " = ?")
    }
    answers[i] <- as.character(ans)
    ch <- ans + sample(c(-5:-1, 1:5) * spacing[i, ], 5)
    ch[sample(1:5, 1)] <- ans
    options[i, ] <- as.character(ch)
}

# load list of items and their answers
df <- data.frame(Question = questions, Option = options, Answer = answers, Type = "radio")

design_list <- list(max_items = 3, min_items = 1, min_SEM = 0.4, exposure = rep(3, 120))

# in pre-CAT stage select 5 items using DPrule and use EAP estimates
preCAT_list <- list(max_items = 5, criteria = "DPrule", method = "EAP")

# change aesthetics of GUI, including title, authors, header, and initial message
title <- "Example Test"
authors <- "I. M. D. Author"
firstpage <- list(h2("Example Test"), h5("Please answer each item to the best of your ability.\n
                                         The results of this test will remain completely anonymous\n
                                         and are only used for research purposes."))
lastpage <- function(person) 
              return(list(h3("Thank you for completing the test. Please click 'Next' to\n
                    save your results.")))
demographics <- list(textInput(inputId = "occupation", label = "What is your occupation?",
                               value = ""), selectInput(inputId = "gender", label = "Please select your gender.",
                                                        choices = c("", "Male", "Female", "Other"), selected = ""))
shinyGUI_list <- list(title = title, authors = authors, demographics = demographics,
                      demographics_inputIDs = c("occupation", "gender"), firstpage = firstpage, lastpage = lastpage)

# run the customized GUI interface
results <- mirtCAT(df = df, mo = mod, criteria = "Drule", start_item = "DPrule",
                   shinyGUI = shinyGUI_list, design = design_list, preCAT = preCAT_list)
summary(results)

# customized answer function
preCAT_list$max_items <- 1
design_list$max_items <- 2
AnswerFuns <- as.list(rep(NA, nrow(df)))
AnswerFuns[[1]] <- function(text) text == '147'
df$Type[1] <- 'text'
results2 <- mirtCAT(df = df, mo = mod, criteria = "Drule", start_item = 1, AnswerFuns=AnswerFuns,
                   shinyGUI = shinyGUI_list, design = design_list, preCAT = preCAT_list)
summary(results2)

#------------------------------------------------------------
# reused custom choices
myfun <- function(inputId, df_row){
    with(df_row, 
         tabsetPanel(tabPanel("Panel 1", tab.1), 
                     tabPanel("Panel 2", tab.2), 
                     tabPanel("Response", Question, 
                              radioButtons(inputId = inputId, label='', 
                                           choices = c(Options.1, Options.2), selected = ''))))
}

questions <- c('1) Text for response','2) Text for response','3) Text for response')
tabs <- rbind(c('Text for tab1', 'Text for tab1'), 
             NA, c('Different text for tab1', 'Different for tab1'))
options <- matrix(c('True', 'False'), 3, 2, byrow=TRUE)

df <- data.frame(Question = questions, Options=options, Type = c("myQ", 'radio', 'myQ'), 
                 tab=tabs, stringsAsFactors = FALSE)
results <- mirtCAT(df = df, customTypes=list(myQ=myfun))
summary(results)
