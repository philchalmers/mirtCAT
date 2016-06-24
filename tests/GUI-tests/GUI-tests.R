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

# forced and unforced
results <- mirtCAT(df = df)
results <- mirtCAT(df = df, shinyGUI = list(stopApp = FALSE))

df$inline <- TRUE
df$width <- "50%"
results2 <- mirtCAT(df = df, shinyGUI = list(forced_choice = FALSE))

# change final message
lastpagefun <- function(person){
    #browser() ## browser can be helpful here to see the contents of 'person'
    est <- as.vector(person$thetas)
    return(list(h5("You have successfully completed the interface."), 
                   h6(sprintf("Your final theta estimate to two decimal places is %.2f.", est))))
} 
results2 <- mirtCAT(df = df, shinyGUI = list(forced_choice = FALSE, lastpage=lastpagefun))

# save and resume temp file
mirtCAT(df = df, shinyGUI = list(temp_file = 'thisfile.rds')) #stop early
mirtCAT(df = df, shinyGUI = list(temp_file = 'thisfile.rds')) #this resumes and deletes

## two step hosting
my_fun <- function(person) cat('Hello world\n')
mirtCAT_preamble(df, final_fun = my_fun)
runApp(createShinyGUI(), port = 8000)
person <- getPerson()
person$raw_responses

# custom UI
myUI <- function(){
    return(fluidPage(
        
        mainPanel(
            htmlOutput("item_stem_html"),
            uiOutput("Main")    
        ),
        
        sidebarPanel(
            actionButton("Next", 'Next')
        )
        
    )) #end bootstrapPage
}

mirtCAT_preamble(df)
runApp(createShinyGUI(ui=myUI), port = 8000)
mirtCAT(df=df, shinyGUI=list(ui=myUI))

# slider input
df$Option.5 <- NULL
df$min <- c(1,NA,NA)
df$max <- c(5,NA,NA)
df$value <- c(3,NA,NA)
df$step <- c(1, NA, NA)
df$Type[1] <- 'slider'
results <- mirtCAT(df = df, shinyGUI = list(forced_choice = FALSE))

df <- data.frame(Question = questions, min=rep(1,3), max=rep(5,3), step=rep(1,3), Type = 'slider')
results2 <- mirtCAT(df = df, shinyGUI = list(forced_choice = FALSE))

# text input
df <- data.frame(Question = questions, Type = 'text')
results <- mirtCAT(df = df, shinyGUI = list(forced_choice = FALSE))

# HTML/markdown stems
df <- data.frame(Question = c("", "", "Just a standard stem."), Option = options, Type = "radio",
                 Stem = c('Math-stem.html', 'Question.md', ''))
results <- mirtCAT(df = df, shinyGUI = list(forced_choice = FALSE))

# checkbox input
df <- data.frame(Question = questions, Option=options, Type = 'checkbox')
results <- mirtCAT(df = df, shinyGUI = list(forced_choice = FALSE))

# shiny input questions
questions <- list(h4("Building CATs with mirtCAT is difficult."),
                  h4("mirtCAT requires a substantial amount of coding."),
                  list(h4("I would use mirtCAT in my"), h2("research.")))

df <- list(Question = questions, 
           Option.1 = options[,1], 
           Option.2 = options[,2], 
           Option.3 = options[,3], 
           Option.4 = options[,4], 
           Option.5 = options[,5], 
           Type = rep("radio", 3))

## Run the mirtCAT web interface and store results
results <- mirtCAT(df = df)

questions <- c("Building CATs with mirtCAT is difficult.",
               "mirtCAT requires a substantial amount of coding.",
               "I would use mirtCAT in my research.")

# none type
df <- data.frame(Question = c('Empty Q', questions), 
                 Options=rbind(NA, options), Type = c('none', rep('radio', 3)))
results <- mirtCAT(df = df)
results <- mirtCAT(df = df, shinyGUI = list(forced_choice = FALSE))

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

