# Codebook describing process for analysing TfL travel history data



analyse_tube_train_data <- function()
{
    source("travel-functions.R")
    
    # Extract the tube and train data from all available data files
    tubetraindata <- get_tube_train_data(import_all_journeys())
}