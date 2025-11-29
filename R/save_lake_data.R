#' Save or update data of the lake
#' 
#' Maps are saved as .html file, plots as .png, R objects as .rds and a 
#' summary is saved as csv table. The folder where data is saved is a subfolder
#' of the defined outputPath and is named by a combination of lake name and lake 
#' ID.
#' 
#' @param outputPath The path of the Remote Sensing data. One level above input
#' and output folder
#' @param nc The netCDF data list created by [load_netcdf()]
#' @param ncImage the data list created by [ndi_per_image()]
#' @param sceneProportion A list of the proportions of different scene, created
#' by [waterscene_proportion()]
#' @param ndtriPixels A list of NDTrI per pixel of one or more years, created by
#' [get_pixel_index()]
#' @param ndtriLake A list of NDTrI data per lake of one or more years, created by
#' [aggregate_NDTrI()]
#' @param save_plots If TRUE all maps and plots are saved.
#' 
#' @importFrom grDevices dev.off png
#' @importFrom utils read.csv write.table
#' 
#' @export
#' 
save_lake_data <- function(
    outputPath, nc, ncImage, sceneProportion, ndtriPixels, ndtriLake, save_plots = TRUE
){
  op <- file.path(outputPath, paste0(ndtriPixels$lakeInfo, collapse = "_"))
  
  if(dir.exists(op)){
    existing_files <- dir(op)
    
  } else {
    dir.create(op)
    existing_files <- NULL
  }
  
  listNames <- grep(pattern = "^y", names(ndtriPixels), value = TRUE)
  years <- gsub(
    pattern = "y", 
    replacement = "", 
    x = listNames
  )
  year_range <- paste0(unique(c(years[1], years[length(years)])), collapse = "-")
  
  if(save_plots){
    
    # Water scene
    lakeRS::save_leaflet(
      x = lakeRS::plot_layer(
        ncLayer = sceneProportion$water, 
        nc = nc, 
        aboveValues = c(0,0.3, 0.5, 0.7, 0.8, 0.9), 
        highestValue = 1,
        aboveColors = c("white","gray", "lightgreen","forestgreen",
                        "steelblue", "darkblue"), 
        legendTitle = "Water Scene Proportion"
      ),
      file = file.path(op, paste0("proportion_water_", year_range, ".html"))
    )
    
    # NDTrI Pixel maps
    for(year in years){
      if(!(paste0("NDTrI_", year, ".html") %in% existing_files)){
        if(ndtriLake[[paste0("y", year)]]$nValidPixel > 0L){
          lakeRS::save_leaflet(
            x = lakeRS::plot_layer(
              ncLayer = ndtriPixels[[paste0("y", year)]]$RSindex, 
              nc = nc, 
              aboveValues = lakeRS::NDTrIColors$ndtri, 
              highestValue = 1.0000000001,
              aboveColors = lakeRS::NDTrIColors$color, 
              plotLegend = TRUE, 
              legendTitle = "NDTrI per Pixel", 
              zoom = 15
            ),
            file = file.path(op, paste0("NDTrI_", year, ".html"))
          )
        }
      }
    }
    
    # Histrograms
    for(year in years){
      if(!(paste0("NDTrI_histogram_", year, ".png") %in% existing_files)){
        if(ndtriLake[[paste0("y", year)]]$nValidPixel > 0L){
          png(filename = file.path(op, paste0("NDTrI_histogram_", year, ".png")), 
              width = 8, height = 6, units = "in", res = 300)
          plot_lake_ndtri_histogram(ndtriLake = ndtriLake, year = year)
          dev.off()
        }
      }
    }
  }
  
  # save or update rds file of ndtriLake
  if("ndtriLake.rds" %in% existing_files){
    previous <- readRDS(file = file.path(op, "ndtriLake.rds"))
    # check if both lake infos are similar
    if(!identical(x = previous$lakeInfo, y = ndtriLake$lakeInfo)){
      stop("lakeInfo of previous and new ndtriLake file do not match")
    } 
    
    previous_listNames <- grep(pattern = "^y", names(previous), value = TRUE)
    add_years <- listNames[!(listNames %in% previous_listNames)]
    if(length(add_years) > 0L){
      for(listName in add_years){
        previous[[listName]] <- ndtriLake[[listName]]
      }
      saveRDS(object = previous, file = file.path(op, "ndtriLake.rds"))
    } else {
      print("No new NDTrI for this lake")
    }
  } else {
    saveRDS(object = ndtriLake, file = file.path(op, "ndtriLake.rds"))
  }
  
  # save or update rds file of ndtriPixel
  if("ndtriPixels.rds" %in% existing_files){
    previous <- readRDS(file = file.path(op, "ndtriPixels.rds"))
    # check if both lake infos are similar
    if(!identical(x = previous$lakeInfo, y = ndtriPixels$lakeInfo)){
      stop("lakeInfo of previous and new ndtriPixels file do not match")
    } 
    
    previous_listNames <- grep(pattern = "^y", names(previous), value = TRUE)
    add_years <- listNames[!(listNames %in% previous_listNames)]
    if(length(add_years) > 0L){
      for(listName in add_years){
        previous[[listName]] <- ndtriPixels[[listName]]
      }
      saveRDS(object = previous, file = file.path(op, "ndtriPixels.rds"))
    } else {
      print("No new Pixel NDTrIs for this lake")
    }
  } else {
    saveRDS(object = ndtriPixels, file = file.path(op, "ndtriPixels.rds"))
  }
  
  # save or update csv file 
  df <- ndtriLake_to_df(ndtriLake = ndtriLake)
  if("_summary.csv" %in% existing_files){
    previous <- read.csv(
      file = file.path(op, "_summary.csv"),
      header = TRUE, 
      sep = ";",
      dec = "."
    )
    
    # check if both lake infos are similar
    if(unique(previous$lakeID) !=  unique(df$lakeID)){
      stop("lakeID of previous and new _summary tables do not match")
    } 
    
    new_data <- which(!(df$year %in% previous$year))
    if(length(new_data) > 0){
      output_df <- rbind(previous, df)
      output_df <- output_df[order(output_df$year),]
      write.table(
        x = output_df, 
        file = file.path(op, "_summary.csv"), 
        sep = ";", dec = ".", 
        row.names = FALSE)
    }
  } else {
    write.table(
      x = df, 
      file = file.path(op, "_summary.csv"), 
      sep = ";", dec = ".", 
      row.names = FALSE)
  }
}
