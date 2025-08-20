RSPath <- "C:/Users/mzamzo/Documents/themen/RS"
df_in <- lakeRS::load_database(
  RSPath = RSPath, 
  dbName = "BB")

yearly_spread <- lakeRS::ndtri_spread(
  df = df_in, 
  aggregationType = "modus"
)

outputN <- lakeRS::EO_assessment_numeric(
  yearly_spread = yearly_spread, 
  statusYears = 3, 
  shortTermYears = 3, 
  longTermYears = 10
)
outputC <- lakeRS::EO_assessment_class(
  yearly_spread = yearly_spread, 
  nClass = 10
)

lakeRS::plot_numeric_assessment(
  numeric_assessment = outputN, 
  lakeName = "Körbaer Teich"
)

lakeRS::plot_class_assessment(
  class_assessment = class_assessment, 
  rowNumbers = 1:3)
