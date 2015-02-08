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
results2 <- mirtCAT(df = df, shinyGUI = list(forced_choice = FALSE))

# HTML/markdown stems
df <- data.frame(Question = "", Option = options, Type = "radio",
                 Stem = c('Math-stem.html', 'Question.md', ''))
results <- mirtCAT(df = df, shinyGUI = list(forced_choice = FALSE))
