RSPath <- "C:/Users/mzamzo/Documents/themen/RS"
df_in <- lakeRS::load_database(
  RSPath = RSPath, 
  dbName = "BB")

yearly_spread <- lakeRS::ndtri_spread(
  df = df_in, 
  aggregationType = "modus"
)


outputN <- lakeRS::EO_assessment_numeric(yearly_spread = yearly_spread)
outputC <- lakeRS::EO_assessment_class(yearly_spread = yearly_spread)


lakeRS::plot_numeric_assessment(numeric_assessment = outputN, lakeName = "Körbaer Teich")
outputN$periods
