RSPath <- "C:/Users/mzamzo/Documents/themen/RS"
available_data <- dir(file.path(RSPath, "output"))
folder <- available_data[3]

included_folders <- available_data
lakeRS::create_database(
  RSPath = RSPath, 
  included_folders = available_data[1:2],
  dbName = "BB")

lakeRS::add_to_database(RSPath = RSPath, dbName = "BB", folder = folder)

df_in <- lakeRS::load_database(
  RSPath = RSPath, 
  dbName = "BB")


