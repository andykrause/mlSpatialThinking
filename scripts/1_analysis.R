#***************************************************************************************************
#*
#*  Analysis and plot generation for Valtech Conference
#*
#***************************************************************************************************

 ## Load Libraries
 library(tidyverse)
 library(hpiR)
 library(sf)
 library(ranger)
 library(pdp)
 library(RColorBrewer)
 library(patchwork)
 library(tmap)

 ## Load data and functions
 data_ <- readRDS(file.path(getwd(), 'data', 'studydata.rds'))
 source(file.path(getwd(), 'R', 'functions.R'))

 ## Set plot parameters

 # Baseline text size
 bs <- 26

 # Primary and secondary colors
 prim_color <- 'goldenrod1'
 sec_color <- 'khaki'

### Data Prep --------------------------------------------------------------------------------------

  ## Log price and remove those with missing ZIP code
  sales_df <- data_$sales %>%
    dplyr::mutate(log_price = log(sale_price)) %>%
    tidyr::drop_na('zip_code')

 # Split training and test data
  train_df <- sales_df %>%
    dplyr::filter(sale_date < '2019-11-01')

  test_df <- sales_df %>%
    dplyr::filter(sale_date >= '2019-11-01') %>%
    dplyr::mutate(sale_month = '2019-10')

  # Extract simulation data (remove all NA zipcodes)
  simxy_sf <- data_$sim %>%
     tidyr::drop_na("zip_code")

####################################################################################################
### Interpretability -------------------------------------------------------------------------------

 ## Create Model

  mod_srf <- ranger::ranger(log_price ~ sqft + sqft_lot + year_built + grade +
                               condition + stories + beds + baths +
                               present_use + wfnt + view_score + sale_month + latitude +
                               longitude,
                            data = train_df,  importance = 'impurity')

### ...Non Spatial Inter ---------------------------------------------------------------------------

  # Create dataframe of variable importance
  varimp_df <- data.frame(feat = names(mod_srf$variable.importance),
                          imp = mod_srf$variable.importance) %>%
     dplyr::arrange(desc(feat)) %>%
     dplyr::mutate(feat = c('Year Built', 'Waterfront', 'View Score', 'Stories', 'Sqft of Lot',
                     'Sqft of Home', 'Sale Month', 'Use Type', 'Longitude', 'Latitude',
                     'Grade/Quality', 'Condition', 'Beds', 'Baths'))

  # Variable importance plot
  ggplot() +
     geom_bar(data = varimp_df, aes(feat, imp),
              fill = prim_color, stat = 'identity') +
     xlab('\n\nProperty Features\n') +
     ylab('\nRelative Measure of Importance') +
     labs(title = 'Perturbation Feature Importance',
          subtitle = 'An Example with King County, WA Data') +
     scale_y_continuous(breaks = c(400, 1300), labels = c('Low', 'High')) +
     coord_flip() +
     theme_vtcdark(base_size =  bs) ->
     varimp_plot
  varimp_plot

  ggsave(file.path(getwd(), 'plots', 'varimp_plot_dark.png'), varimp_plot, bg = "transparent")

  ## SF and YearBuilt PDPs

  # Set up simulation data.  Here we take just 100 random sales from data
  sim_df <- hpiR::rfSimDf(rf_df = test_df,
                          seed = 1,
                          sim_count = 500)

  # Calculate Individual Condition Expectations for  year built
  yb_ice <- pdp::partial(object = mod_srf,
                                train = sim_df,
                                pred.var = "year_built",
                                pred.grid = data.frame(year_built = seq(1900, 2020, 5)),
                                ice = TRUE) %>%
     dplyr::group_by(yhat.id) %>%
     dplyr::mutate(index = ((yhat - yhat[1]) + 1) * 100)

  # Convert to PDP
  yb_pdp <- yb_ice %>%
     dplyr::group_by(year_built) %>%
     dplyr::summarize(index = median(index))

  # Plot
  ggplot() +
     geom_line(data = yb_ice,
               aes(x = year_built, y = index, group = yhat.id), color = sec_color, alpha = .9,
               size = .1) +
     geom_line(data = yb_pdp,
               aes(x = year_built, y = index), color = prim_color, size = 1.2) +
     ylab('Impact on Value\nChange from Built Year of 1900\n') +
     xlab('\n Year Built') +
     scale_y_continuous(breaks = seq(60, 110, 10),
                        labels = c('-40%', '-30%', '-20%', '-10%', 'Even', '+10%')) +
     labs(title = 'Partial Dependence Plot',
          subtitle = 'Effect of Vintage on Value') +
    theme_vtcdark(base_size = bs) ->
     yb_pdp_plot
     yb_pdp_plot

  # Save Plot
  ggsave(file.path(getwd(), 'plots', 'yb_pdp__plot_dark.png'), yb_pdp_plot, bg = "transparent")

  # Calculate individual conditional expectations
  sf_ice <- pdp::partial(object = mod_srf,
                         train = sim_df,
                         pred.var = "sqft",
                         pred.grid = data.frame(sqft = seq(800, 4800, 50)),
                         ice = TRUE) %>%
     dplyr::group_by(yhat.id) %>%
     dplyr::mutate(index = ((yhat - yhat[1]) + 1) * 100)

  # Convert to PDF
  sf_pdp <- sf_ice %>%
     dplyr::group_by(sqft) %>%
     dplyr::summarize(index = median(index))

  # Plot
  ggplot() +
    geom_line(data = sf_ice,
              aes(x = sqft, y = index, group = yhat.id), color = sec_color, alpha = .5,
              size = .1) +
    geom_line(data = sf_pdp,
              aes(x = sqft, y = index), color = prim_color, size = 1.2) +
    ylab('Impact on Value\nChange in Square Footage\n') +
    xlab('\n Home Size in SqFt') +
    scale_y_continuous(breaks = seq(100, 160, 10),
                       labels = c('Even', '+10%', '+20%', '+30%', '+40%', '+50%', '+60%')) +
    labs(title = 'Partial Dependence Plot',
         subtitle = 'Effect of Home Size on Value') +
    coord_cartesian(ylim = c(95, 155), xlim = c(500, 4750)) +
    theme_vtcdark(base_size = bs+10)  ->
    sf_pdp_dplot
    sf_pdp_dplot

    # Save
    ggsave(file.path(getwd(), 'plots', 'sf_pdp_plot_dark.png'), sf_pdp_dplot, bg = "transparent")

### ...Spatial Interpretability -------------------------------------------------------------------

  ### Add Rotations

  train_df <- addRotatedCoordinates(train_df, rotations = c(15, 30, 45, 60, 75))
  test_df <- addRotatedCoordinates(test_df, rotations = c(15, 30, 45, 60, 75))
  simxy_sf <- addRotatedCoordinates(simxy_sf, rotations = c(15, 30, 45, 60, 75))

  ## Create simulated data sets for predictions
  idx <- 105
  sim1_df <- simxy_sf %>%
     dplyr::mutate(sqft = test_df$sqft[idx],
                   sqft_lot = test_df$sqft_lot[idx],
                   year_built = test_df$year_built[idx],
                   grade = test_df$grade[idx],
                   condition = test_df$condition[idx],
                   stories = test_df$stories[idx],
                   beds = test_df$beds[idx],
                   baths = test_df$baths[idx],
                   present_use = test_df$present_use[idx],
                   wfnt = test_df$wfnt[idx],
                   view_score = test_df$view_score[idx],
                   sale_month = test_df$sale_month[idx])

  # With New SF
  sim2_df <- sim1_df %>%
     dplyr::mutate(sqft = sqft + 200)

  # With New Year Built
  sim3_df <- sim1_df %>%
     dplyr::mutate(year_built = 1990)

  # SF differences
  sf1_pred <- predict(mod_srf, as.data.frame(sim1_df))$predictions
  sf2_pred <- predict(mod_srf, as.data.frame(sim2_df))$predictions

  # YearBuilt differences
  yb1_pred <- predict(mod_srf, as.data.frame(sim1_df))$predictions
  yb2_pred <- predict(mod_srf, as.data.frame(sim3_df))$predictions

  # Add differences to sim SF
  simxy_sf$sf_diff = sf2_pred - sf1_pred
  simxy_sf$yb_diff = yb2_pred - yb1_pred

  ## Create Plot
  simxy_sf <-
    simxy_sf %>%
      dplyr::mutate(sfdiff = cut(sf_diff, quantile(sf_diff, seq(0, 1, by = .1))),
                    sffct = as.factor(sfdiff))

  ## Make Legend
  tmap_mode('plot')
  tm_shape(simxy_sf %>% tidyr::drop_na(sffct), alpha = .95) +
    tm_layout (frame = FALSE, bg.color = "transparent",
               legend.outside = TRUE, legend.text.color = 'gray90') +
    tm_symbols(col='sffct', border.alpha = 0, shape = 16, title.col = '',
               alpha = .95, size = .2, palette = c('goldenrod', 'royalblue'),
               labels = c('  1st Decile (Lowest)', '  2nd Decile', '  3rd Decile',
                          '  4th Decile', '  5th Decile', '  6th Decile', '  7th Decile',
                          '  8th Decile', '  9th Decile', '  10th Decile (Highest)')) ->
    pdp_legend
    pdp_legend

  # Save
  tmap_save(pdp_legend,
            filename = file.path(getwd(), 'plots', 'pdp_legend.png'),
            bg="transparent")

  ## Sf Plot
  tmap_mode('plot')
  tm_shape(simxy_sf %>% tidyr::drop_na(sffct), alpha = .95) +
    tm_layout (frame = FALSE, bg.color = "transparent", legend.show=FALSE) +
    tm_symbols(col='sffct', border.alpha = 0, shape = 16, title.col = 'Price Change for +100sf',
               alpha = .95, size = .2, palette = c('goldenrod', 'royalblue')) ->
    sf_pdp_map
    sf_pdp_map

  tmap_save(sf_pdp_map,
            filename = file.path(getwd(), 'plots', 'sfmap1.png'),
            bg="transparent")

  ## Year Built Plot
  simxy_sf <-
    simxy_sf %>%
    dplyr::mutate(ybdiff = cut(yb_diff, quantile(yb_diff, seq(0, 1, by = .1))),
                  ybfct = as.factor(ybdiff))

  tmap_mode('plot')
  tm_shape(simxy_sf %>% tidyr::drop_na(ybfct), alpha = .95) +
    tm_layout (frame = FALSE, bg.color = "transparent", legend.show=FALSE) +
    tm_symbols(col='ybfct', border.alpha = 0, shape = 16, title.col = 'Price Change for +30Yr',
               alpha = .95, size = .2, palette = c('goldenrod', 'royalblue')) ->
    yb_pdp_map
    yb_pdp_map

    tmap_save(yb_pdp_map,
              filename = file.path(getwd(), 'plots', 'ybmap1.png'),
              bg="transparent")

 ## Add rotations

  # Redo model
  mod_ssrf <- ranger::ranger(log_price ~ sqft + sqft_lot + year_built + grade +
                                condition + stories + beds + baths +
                                present_use + wfnt + view_score + sale_month + latitude +
                                longitude + x_15 + y_15 + x_30 + y_30 + x_45 + y_45 +
                                x_60 + y_60 + x_75 + y_75,
                            data = train_df,  importance = 'impurity')

  # SF differences
  rsf1_pred <- predict(mod_ssrf, as.data.frame(sim1_df))$predictions
  rsf2_pred <- predict(mod_ssrf, as.data.frame(sim2_df))$predictions

  # YearBuilt differences
  ryb1_pred <- predict(mod_ssrf, as.data.frame(sim1_df))$predictions
  ryb2_pred <- predict(mod_ssrf, as.data.frame(sim3_df))$predictions

  # Add differences to sim SF
  simxy_sf$rsf_diff = rsf2_pred - rsf1_pred
  simxy_sf$ryb_diff = ryb2_pred - ryb1_pred

  ## Create Plot
  simxy_sf <-
    simxy_sf %>%
    dplyr::mutate(rsfdiff = cut(rsf_diff, quantile(rsf_diff, seq(0, 1, by = .1))),
                  rsffct = as.factor(rsfdiff))

  # Plot SF
  tmap_mode('plot')
  tm_shape(simxy_sf %>% tidyr::drop_na(rsffct), alpha = .95) +
    tm_layout (frame = FALSE, bg.color = "transparent", legend.show=FALSE) +
    tm_symbols(col='rsffct', border.alpha = 0, shape = 16, title.col = 'Price Change for +100sf',
               alpha = .95, size = .2, palette = c('goldenrod', 'royalblue')) ->
    sf_pdp_map2
  sf_pdp_map2

  tmap_save(sf_pdp_map2,
            filename = file.path(getwd(), 'plots', 'sfmap2.png'),
            bg="transparent")


  ## Plot YB
  simxy_sf <-
    simxy_sf %>%
    dplyr::mutate(rybdiff = cut(ryb_diff, quantile(ryb_diff, seq(0, 1, by = .1))),
                  rybfct = as.factor(rybdiff))

  tmap_mode('plot')
  tm_shape(simxy_sf %>% tidyr::drop_na(rybfct), alpha = .95) +
    tm_layout (frame = FALSE, bg.color = "transparent", legend.show=FALSE) +
    tm_symbols(col='rybfct', border.alpha = 0, shape = 16, title.col = 'Price Change for +100sf',
               alpha = .95, size = .2, palette = c('goldenrod', 'royalblue')) ->
    yb_pdp_map2
  yb_pdp_map2

  tmap_save(yb_pdp_map2,
            filename = file.path(getwd(), 'plots', 'ybmap2.png'),
            bg="transparent")

### Uncertainty ------------------------------------------------------------------------------------

  mod_ssrf <- ranger::ranger(log_price ~ sqft + sqft_lot + year_built + grade +
                               condition + stories + beds + baths +
                               present_use + wfnt + view_score + sale_month + latitude +
                               longitude + x_15 + y_15 + x_30 + y_30 + x_45 + y_45 +
                               x_60 + y_60 + x_75 + y_75,
                             data = train_df,  quantreg = TRUE)

  # Make Predictions
  unc_pred <- predict(mod_ssrf,
                      as.data.frame(sim1_df),
                      type = 'quantiles',
                      quantiles = c(.25, .5, .75))$predictions %>%
    as.data.frame() %>%
    dplyr::mutate(riqr = `quantile= 0.75` - `quantile= 0.25`)

  # Add to simulation data
  simxy_sf <-
    simxy_sf %>%
    dplyr::mutate(rf_riqr = unc_pred$riqr) %>%
    dplyr::mutate(rfriqr = cut(rf_riqr, quantile(rf_riqr, seq(0, 1, by = .1))))

  # Make Plot
  tmap_mode('plot')
  tm_shape(simxy_sf %>% tidyr::drop_na(rfriqr), alpha = .95) +
    tm_layout (frame = FALSE, bg.color = "transparent",
               legend.outside = TRUE, legend.text.color = 'gray90') +
    tm_symbols(col='rfriqr', border.alpha = 0, shape = 16, title.col = '',
               alpha = .95, size = .2, palette = c('royalblue', 'white'),
               labels = c('  1st Decile (Lowest)', '  2nd Decile', '  3rd Decile',
                          '  4th Decile', '  5th Decile', '  6th Decile', '  7th Decile',
                          '  8th Decile', '  9th Decile', '  10th Decile (Highest)')) ->
    unc_legend
    unc_legend

    # Save
  tmap_save(unc_legend,
            filename = file.path(getwd(), 'plots', 'unc_legend.png'),
            bg="transparent")

  ## Make the non-legend plot
  tmap_mode('plot')
  tm_shape(simxy_sf %>% tidyr::drop_na(rfriqr), alpha = .95) +
    tm_layout (frame = FALSE, bg.color = "transparent", legend.show=FALSE) +
    tm_symbols(col='rfriqr', border.alpha = 0, shape = 16, title.col = '',
               alpha = .95, size = .2, palette = c('royalblue', 'white')) ->
    unc_map1
    unc_map1

  tmap_save(unc_map1,
            filename = file.path(getwd(), 'plots', 'uncmap1.png'),
            bg="transparent")

### Fairness ---------------------------------------------------------------------------------------

  ## Boundary Analysis
  test_df <- test_df %>%
    assignBorder(., 'zip_code', k= 5)

  # Create Linear model
  mod_lm <- stats::lm(log(sale_price) ~ log(sqft) + sqft_lot + year_built + grade +
                        condition + stories + beds + baths + sale_month + as.factor(present_use) +
                        wfnt + view_score + as.factor(zip_code),
                      data = train_df)

  # Make predictions
  pred_lm <- predict(mod_lm, test_df %>% dplyr::mutate(sale_month = '2019-10'))
  pred_rf <- predict(mod_ssrf, test_df %>% dplyr::mutate(sale_month = '2019-10'))$predictions

  # Calculate errors
  test_df$lm_error = pred_lm - log(test_df$sale_price)
  test_df$rf_error = pred_rf - log(test_df$sale_price)

  # Create table of results
  test_df %>% dplyr::group_by(border_5) %>%
    dplyr::summarize(lm = median(abs(lm_error)),
                     rf = median(abs(rf_error))) %>%
    t() %>%
    as.data.frame() -> fair_df

  names(fair_df) <- c('no', 'yes')
  fair_df <- fair_df %>%
    dplyr::mutate(diff = ((yes / no) - 1) * 100)

  # Show table
  fair_df

  ## Plot

  # Create spatial data
  test_sf <- test_df %>%
    sf::st_as_sf(., coords = c('longitude', 'latitude'),
                 remove = FALSE)

  # Plot
  tmap_mode('plot')
  tm_shape(test_sf, alpha = .95) +
    tm_layout (frame = FALSE, bg.color = "transparent", legend.show=TRUE,
               legend.outside = TRUE, legend.text.color = 'gray90',
               legend.title.color = 'gray90') +
    tm_symbols(col='border_5', border.alpha = 0, title.col = 'Boundary Property',
               shape = 19,
               alpha = .95, size = .2, palette = c('gray50', 'royalblue')) ->
    fair_map1
  fair_map1

  # Save
  tmap_save(fair_map1,
            filename = file.path(getwd(), 'plots', 'fairmap1.png'),
            bg="transparent")

#***************************************************************************************************
#***************************************************************************************************





