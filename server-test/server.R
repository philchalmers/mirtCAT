library("mirtCAT")
options(stringsAsFactors = FALSE)

### Create simple non-adaptive interface ###

## Potential options for each item
options <- matrix(c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree"),
				  nrow = 3, ncol = 5, byrow = TRUE)

questions <- c("Building CATs with mirtCAT is difficult.",
			   "Building tests with mirtCAT requires a lot of coding.",
			   "I would use mirtCAT in my research.")

df <- data.frame(Question = questions, Option = options, Type = "radio")

final_fun <- function(person){
	# save file to a directory which can be accessed by any user on the server
	time <- paste0('/public/', gsub(' ', '_', as.character(Sys.time())), '.rds')
	saveRDS(person, time)
}

mirtCAT_preamble(df = df, final_fun = final_fun)
createShinyGUI()
