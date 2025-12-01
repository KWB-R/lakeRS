# 1. write data and save as .rda file in the package/data folder
# --> can be done by usethis::use_data()
# (the filename should be the same as the variable name)

# 2. add the variable to the global variables of the package in the
# package/R/zzz.R file to avoid the note about missing global bindings by the 
# devtool check

# 3. Document the dataset in the file data_properties_names.R in the package/R
# folder
# NULL can be assigned to the documentation to avoid the devtools warning
# message "Variables with usage in documentation ... but not in code"

# Trophic colors
oligo <- rgb(0, 51, 153, maxColorValue = 255)
meso1 <- rgb(0, 176, 240, maxColorValue = 255)
meso2 <- rgb(102, 153, 0, maxColorValue = 255)
eu1 <- rgb(240, 220, 0, maxColorValue = 255)
eu2 <- rgb(237, 179, 36, maxColorValue = 255)
poly1 <- rgb(204, 102, 0, maxColorValue = 255)
poly2 <- rgb(200, 0, 0, maxColorValue = 255)
hyper <- rgb(128, 0, 128, maxColorValue = 255)
no_water <- rgb(180, 180, 180, maxColorValue = 255)

value_color <- data.frame(
  "estimated_average" = round(c(-1, -0.6, -0.385, -0.268, -0.153, -0.035, 0.082, 0.194, 0.37, 0.5, 1), 3),
  "color" = c(oligo, oligo, meso1, meso2, eu1, eu2, poly1, poly2, hyper, no_water, no_water))

NDTrIColors <- merge(
  x = data.frame("ndtri" = round(seq(-1, 1, by = 0.001), 3)), 
  y = value_color, 
  by.x = "ndtri", 
  by.y = "estimated_average", 
  all.x = TRUE
)



col_defined <- which(!is.na(NDTrIColors$color))
for(i in 2:length(col_defined)){
  col_from <- col2rgb(NDTrIColors$color[col_defined[i-1]])
  col_to <- col2rgb(NDTrIColors$color[col_defined[i]])
  n_inter <- col_defined[i] - col_defined[i-1] + 1
  
  col_step <- (col_to - col_from) /n_inter
  
  NDTrIColors$color[col_defined[i-1]:col_defined[i]] <- 
    sapply(1:n_inter, function(s) {
      new_color <- col_from + col_step * s
      rgb(new_color[1], new_color[2], new_color[3], maxColorValue = 255)
    })
}

usethis::use_data(NDTrIColors, overwrite = TRUE)

# Class Colors -----------------------------------------------------------------
tenClassColors <- data.frame(
  "class" = 1:10,
  "color" = c(
    rgb(94,79,162, maxColorValue = 255),
    rgb(50,136,189, maxColorValue = 255),
    rgb(102,194,164, maxColorValue = 255),
    rgb(171,221,164, maxColorValue = 255),
    rgb(230,245,152, maxColorValue = 255),
    rgb(254,224,139, maxColorValue = 255),
    rgb(253,174,94, maxColorValue = 255),
    rgb(244,109,67, maxColorValue = 255),
    rgb(213,62,79, maxColorValue = 255),
    rgb(158,1,66, maxColorValue = 255)
  ))

usethis::use_data(tenClassColors, overwrite = TRUE)


# Cluster Colors -----------------------------------------------------------------
tenClusterColors <- data.frame(
  "cluster" = 1:10,
  "color" = c(
    rgb(31, 119, 180, maxColorValue = 255),
    rgb(255, 127, 14, maxColorValue = 255),
    rgb(44, 160, 44, maxColorValue = 255),
    rgb(214, 39, 40, maxColorValue = 255),
    rgb(148, 103, 189, maxColorValue = 255),
    rgb(140, 86, 75, maxColorValue = 255),
    rgb(255, 187, 120, maxColorValue = 255),
    rgb(23, 190, 207, maxColorValue = 255),
    rgb(227, 119, 194, maxColorValue = 255),
    rgb(127, 127, 127, maxColorValue = 255)
  ))


usethis::use_data(tenClusterColors, overwrite = TRUE)
