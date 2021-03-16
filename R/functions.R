
addRotatedCoordinates <- function(data,
                                  lon_field = 'longitude',
                                  lat_field = 'latitude',
                                  rotations = c(10, 15, 30, 45),
                                  ...){

  for (rt in 1:length(rotations)){

    rot_coords <- rotateCoords(x = data[[lon_field]],
                               y = data[[lat_field]],
                               angle = rotations[rt],
                               ...)
    data[[paste0('x_', rotations[rt])]] <- rot_coords$V1
    data[[paste0('y_', rotations[rt])]] <- rot_coords$V2
  }
  data
}

rotateCoords <- function(x, y, angle){

  coords <- cbind(x, y)
  spdep::Rotation(coords, angle * pi / 180) %>% as.data.frame()
}

assignBorder <- function(validate_df,
                         district_field,
                         k = 5,
                         fname = 'border_5'){

  validate_df[['district']] <- validate_df[[district_field]]

  # Identify "Border" properties
  coords <- cbind(validate_df$longitude, validate_df$latitude)
  sp_df = sp::SpatialPointsDataFrame(sp::SpatialPoints(coords), validate_df)
  nbl <- sp_df %>%
    spdep::knearneigh(., k = k, longlat = NULL, RANN=TRUE) %>%
    spdep::knn2nb(.)

  validate_df[[fname]] <- FALSE
  for (i in 1:nrow(validate_df)){
    nbls <- validate_df$district[nbl[[i]]]
    if (length(which(nbls != validate_df$district[i])) > 0){
      validate_df[[fname]][i] <- TRUE
    }
  }
  validate_df
}

theme_vtc <- function(
  base_size = 11,
  base_family = "",
  base_line_size = base_size/22,
  base_rect_size = base_size/22) {
  theme_bw(base_size = base_size, base_family = base_family,
           base_line_size = base_line_size, base_rect_size = base_rect_size) +
    theme(axis.ticks = element_blank(),
          legend.key = element_blank(),
          panel.border = element_blank(), strip.background = element_blank(),
          legend.position = 'bottom',
          panel.background = element_rect(fill = "transparent"),
          plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
          legend.background = element_rect(fill = "transparent"), # get rid of legend bg
          legend.box.background = element_rect(fill = "transparent"),
          plot.caption = element_text(color = "gray10", face = "italic", hjust = 0, size = 8))
}
