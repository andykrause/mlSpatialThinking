#***************************************************************************************************
#*
#*  Baseline analysis for Valtech Conference
#*
#***************************************************************************************************

 library(tidyverse)
 library(hpiR)
 library(sf)
 library(ranger)
 library(pdp)
 library(RColorBrewer)
 library(patchwork)
 library(tmap)

 # useful on MacOS to speed up rendering of geom_sf() objects
 # if(!identical(getOption("bitmapType"), "cairo") && isTRUE(capabilities()[["cairo"]])){
 #    options(bitmapType = "cairo")
 # }

 data_ <- readRDS(file.path(getwd(), 'data', 'studydata.rds'))
 source(file.path(getwd(), 'R', 'functions.R'))

 bs = 16
### Data Prep --------------------------------------------------------------------------------------

  sales_df <- data_$sales %>%
    dplyr::mutate(log_price = log(sale_price)) %>%
    tidyr::drop_na('zip_code')

  train_df <- sales_df %>%
    dplyr::filter(sale_date < '2019-11-01')

  test_df <- sales_df %>%
    dplyr::filter(sale_date >= '2019-11-01') %>%
    dplyr::mutate(sale_month = '2019-10')

  simxy_sf <- data_$sim %>%
     tidyr::drop_na("zip_code")

### Interpretability -------------------------------------------------------------------------------

 ## Create Model

  mod_srf <- ranger::ranger(log_price ~ sqft + sqft_lot + year_built + grade +
                               condition + stories + beds + baths +
                               present_use + wfnt + view_score + sale_month + latitude +
                               longitude,
                            data = train_df,  importance = 'impurity')


### ...Non Spatial Inter ---------------------------------------------------------------------------

  varimp_df <- data.frame(feat = names(mod_srf$variable.importance),
                          imp = mod_srf$variable.importance) %>%
     dplyr::arrange(desc(feat)) %>%
     dplyr::mutate(feat = c('Year Built', 'Waterfront', 'View Score', 'Stories', 'Sqft of Lot',
                     'Sqft of Home', 'Sale Month', 'Use Type', 'Longitude', 'Latitude',
                     'Grade/Quality', 'Condition', 'Beds', 'Baths'))

  ggplot() +
     geom_bar(data = varimp_df, aes(feat, imp), fill = 'royalblue', stat = 'identity') +
     xlab('\n\nProperty Features\n') +
     ylab('\nRelative Measure of Importance') +
     labs(title = 'Perturbation Feature Importance',
          subtitle = 'An Example with King County, WA Data') +
     scale_y_continuous(breaks = c(400, 1300), labels = c('Low', 'High')) +
     coord_flip() +
     theme_vtc(base_size =  bs) ->
     varimp_plot

  png(filename = file.path(getwd(), 'plots', 'varimp_plot.png'),
      width = 800, height = 500)
   varimp_plot
  dev.off()



  # Set up simulation data.  Here we take just 100 random sales from data
  sim_df <- hpiR::rfSimDf(rf_df = test_df,
                          seed = 1,
                          sim_count = 500)


  # Calculate Partial dependency (or how prediction changes if only that variable is changed)
  # For Year Built.  You can set range in the last parameter, here i'm using 1800 to 2019.
  yb_ice <- pdp::partial(object = mod_srf,
                                train = sim_df,
                                pred.var = "year_built",
                                pred.grid = data.frame(year_built = seq(1900, 2020, 5)),
                                ice = TRUE) %>%
     dplyr::group_by(yhat.id) %>%
     dplyr::mutate(index = ((yhat - yhat[1]) + 1) * 100)

  yb_pdp <- yb_ice %>%
     dplyr::group_by(year_built) %>%
     dplyr::summarize(index = median(index))

  ggplot() +
     geom_line(data = yb_ice,
               aes(x = year_built, y = index, group = yhat.id), color = 'royalblue', alpha = .9,
               size = .1) +
     geom_line(data = yb_pdp,
               aes(x = year_built, y = index), color = 'navy', size = 1.2) +
     ylab('Impact on Value\nChange from Built Year of 1900\n') +
     xlab('\n Year Built') +
     scale_y_continuous(breaks = seq(60, 110, 10),
                        labels = c('-40%', '-30%', '-20%', '-10%', 'Even', '+10%')) +
     labs(title = 'Partial Dependence Plot',
          subtitle = 'Effect of Vintage on Value') +
    theme_vtc(base_size = bs) ->
     yb_pdp_plot

  png(filename = file.path(getwd(), 'plots', 'yb_pdp.png'),
      width = 800, height = 500)
    yb_pdp_plot
  dev.off()


  sf_ice <- pdp::partial(object = mod_srf,
                         train = sim_df,
                         pred.var = "sqft",
                         pred.grid = data.frame(sqft = seq(800, 4800, 50)),
                         ice = TRUE) %>%
     dplyr::group_by(yhat.id) %>%
     dplyr::mutate(index = ((yhat - yhat[1]) + 1) * 100)

  sf_pdp <- sf_ice %>%
     dplyr::group_by(sqft) %>%
     dplyr::summarize(index = median(index))

  ggplot() +
     geom_line(data = sf_ice,
               aes(x = sqft, y = index, group = yhat.id), color = 'royalblue', alpha = .9,
               size = .1) +
     geom_line(data = sf_pdp,
               aes(x = sqft, y = index), color = 'navy', size = 1.2) +
     ylab('Impact on Value\nChange in Square Footab\n') +
     xlab('\n Home Size in SqFt') +
     scale_y_continuous(breaks = seq(100, 150, 10),
                        labels = c('Even', '+10%', '+20%', '+30%', '+40%', '+50%')) +
     labs(title = 'Partial Dependence Plot',
          subtitle = 'Effect of Home Size on Value') +
     coord_cartesian(ylim = c(95, 155) ) +
    theme_vtc(base_size = bs) ->
     sf_pdp_plot

  png(filename = file.path(getwd(), 'plots', 'sf_pdp.png'),
      width = 800, height = 500)
  sf_pdp_plot
  dev.off()

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


  tmap_mode("view")
  tm_shape(simxy_sf %>% tidyr::drop_na(sffct), alpha = .8) +
  tm_symbols(col='sffct', border.alpha = 0, shape = 16, title.col = 'Price Change for +100sf',
             alpha = .5, size = .1, palette = c('goldenrod', 'blue'),
             labels = c('1st Decile (Lowest)', '2nd Decile', '3rd Decile',
                        '4th Decile', '5th Decile', '6th Decile', '7th Decile',
                        '8th Decile', '9th Decile', '10th Decile (Highest)'))

  ## Create Plot
  simxy_sf <-
    simxy_sf %>%
    dplyr::mutate(ybdiff = cut(yb_diff, quantile(yb_diff, seq(0, 1, by = .1))),
                  ybfct = as.factor(ybdiff))

  tm_shape(simxy_sf %>% tidyr::drop_na(ybfct), alpha = .8) +
    tm_symbols(col='ybfct', border.alpha = 0, shape = 16, title.col = 'Price Change for -30 Home Age',
               alpha = .5, size = .1, palette = c('goldenrod', 'blue'),
               labels = c('1st Decile (Lowest)', '2nd Decile', '3rd Decile',
                          '4th Decile', '5th Decile', '6th Decile', '7th Decile',
                          '8th Decile', '9th Decile', '10th Decile (Highest)'))

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


  tmap_mode("view")
  tm_shape(simxy_sf %>% tidyr::drop_na(rsffct), alpha = .8) +
    tm_symbols(col='rsffct', border.alpha = 0, shape = 16, title.col = 'Price Change for +100sf',
               alpha = .5, size = .1, palette = c('goldenrod', 'blue'),
               labels = c('1st Decile (Lowest)', '2nd Decile', '3rd Decile',
                          '4th Decile', '5th Decile', '6th Decile', '7th Decile',
                          '8th Decile', '9th Decile', '10th Decile (Highest)'))

  ## Create Plot
  simxy_sf <-
    simxy_sf %>%
    dplyr::mutate(rybdiff = cut(ryb_diff, quantile(ryb_diff, seq(0, 1, by = .1))),
                  rybfct = as.factor(rybdiff))

  tm_shape(simxy_sf %>% tidyr::drop_na(rybfct), alpha = .8) +
    tm_symbols(col='rybfct', border.alpha = 0, shape = 16, title.col = 'Price Change for -30 Home Age',
               alpha = .5, size = .1, palette = c('goldenrod', 'blue'),
               labels = c('1st Decile (Lowest)', '2nd Decile', '3rd Decile',
                          '4th Decile', '5th Decile', '6th Decile', '7th Decile',
                          '8th Decile', '9th Decile', '10th Decile (Highest)'))

### Uncertainty ------------------------------------------------------------------------------------

  mod_ssrf <- ranger::ranger(log_price ~ sqft + sqft_lot + year_built + grade +
                               condition + stories + beds + baths +
                               present_use + wfnt + view_score + sale_month + latitude +
                               longitude + x_15 + y_15 + x_30 + y_30 + x_45 + y_45 +
                               x_60 + y_60 + x_75 + y_75,
                             data = train_df,  quantreg = TRUE)

  unc_pred <- predict(mod_ssrf,
                      as.data.frame(sim1_df),
                      type = 'quantiles',
                      quantiles = c(.25, .5, .75))$predictions %>%
    as.data.frame() %>%
    dplyr::mutate(riqr = `quantile= 0.75` - `quantile= 0.25`)


  simxy_sf <-
    simxy_sf %>%
    dplyr::mutate(rf_riqr = unc_pred$riqr) %>%
    dplyr::mutate(rfriqr = cut(rf_riqr, quantile(rf_riqr, seq(0, 1, by = .1))))

  tm_shape(simxy_sf %>% tidyr::drop_na(rfriqr), alpha = .8) +
    tm_symbols(col='rfriqr', border.alpha = 0, shape = 16,
               title.col = 'Valuation Uncertainty',
               alpha = .6, size = .1, palette = c('gray20', 'royalblue'),
               labels = c('1st Decile (Lowest)', '2nd Decile', '3rd Decile',
                          '4th Decile', '5th Decile', '6th Decile', '7th Decile',
                          '8th Decile', '9th Decile', '10th Decile (Highest)'))

### Fairness ---------------------------------------------------------------------------------------

  ## Boundary Analysis

  test_df <- test_df %>%
    assignBorder(., 'zip_code', k= 5)

  mod_lm <- stats::lm(log(sale_price) ~ log(sqft) + sqft_lot + year_built + grade +
                        condition + stories + beds + baths + sale_month + as.factor(present_use) +
                        wfnt + view_score + as.factor(zip_code),
                      data = train_df)

  pred_lm <- predict(mod_lm, test_df %>% dplyr::mutate(sale_month = '2019-10'))
  pred_rf <- predict(mod_ssrf, test_df %>% dplyr::mutate(sale_month = '2019-10'))$predictions

  test_df$lm_error = pred_lm - log(test_df$sale_price)
  test_df$rf_error = pred_rf - log(test_df$sale_price)

  test_df %>% dplyr::group_by(border_5) %>%
    dplyr::summarize(lm = median(abs(lm_error)),
                     rf = median(abs(rf_error))) %>% t() %>% as.data.frame() -> kk
  names(kk) <- c('no', 'yes')
  kk <- kk %>%
    dplyr::mutate(diff = ((yes / no) - 1) * 100)
  kk

test_sf <- test_df %>%
  sf::st_as_sf(., coords = c('longitude', 'latitude'),
               remove = FALSE)

  tm_shape(test_sf, alpha = .8) +
    tm_symbols(col='border_5', border.alpha = 0, shape = 16,
               title.col = 'Boundary Property',
               alpha = .6, size = .1, palette = c('gray20', 'red'))














  # Aspatial
   mod_lm <- stats::lm(log(sale_price) ~ log(sqft) + sqft_lot + year_built + grade +
                      condition + stories + beds + baths + sale_month + as.factor(present_use) +
                        wfnt + view_score,
                    data = train_df)

  # Spatial
   mod_slm <- stats::lm(update(mod_lm, . ~ . + as.factor(zip_code)),
                     data = train_df)


  # Add the predictions to test data set
   test_df <- test_df %>%
      dplyr::mutate(pred_lm = predict(mod_lm, .),
                    error_lm = log_price - pred_lm)

### RF model ---------------------------------------------------------------------------------------

  mod_rf <- ranger::ranger(log_price ~ sqft + sqft_lot + year_built + grade +
                              condition + stories + beds + baths + sale_month +
                              present_use + wfnt + view_score,
                            data = train_df,  importance = 'impurity')
   mod_srf <- ranger::ranger(log_price ~ sqft + sqft_lot + year_built + grade +
                              condition + stories + beds + baths +
                                present_use + wfnt + view_score + sale_month + latitude +
                               longitude,
                            data = train_df,  importance = 'impurity')


   sim_df$pred = exp(b$predictions) - exp(a$predictions)



   ####

   trainr_df <- addRotatedCoordinates(train_df)
   mod_srrf <- ranger::ranger(log_price ~ sqft + sqft_lot + year_built + grade +
                                condition + stories + beds + baths +
                                present_use + wfnt + view_score + sale_month + latitude +
                                longitude + x_15 + x_30 + x_45 +
                                 y_15 + y_30 + y_45,
                             data = trainr_df,  importance = 'impurity')
   idx <- 1
   sim_df <- simxy_sf %>%
      addRotatedCoordinates() %>%
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

   sim2_df <- sim_df %>%
      dplyr::mutate(sqft = sqft + 100)

   a = predict(mod_srrf, as.data.frame(sim_df))
   b = predict(mod_srrf, as.data.frame(sim2_df))
   sim_df$pred = exp(b$predictions) - exp(a$predictions)
   plot(sim_df[, c(1,27)])











 pred_lm <- predict(mod_lm, score_df %>% dplyr::mutate(sale_month = '2019-10'))
 pred_slm <- predict(mod_slm, score_df %>% dplyr::mutate(sale_month = '2019-10'))
 pred_rf <- predict(mod_rf, score_df %>% dplyr::mutate(sale_month = '2019-10'))
 pred_srf <- predict(mod_srf, score_df %>% dplyr::mutate(sale_month = '2019-10'))

 score_df$pred_lm = exp(pred_lm)
 score_df$pred_slm = exp(pred_slm)
 score_df$pred_rf = exp(pred_rf$predictions)
 score_df$pred_srf = exp(pred_srf$predictions)

 score_df$lm_error = log(score_df$pred_lm) - log(score_df$sale_price)
 score_df$slm_error = log(score_df$pred_slm) - log(score_df$sale_price)
 score_df$rf_error = log(score_df$pred_rf) - log(score_df$sale_price)
 score_df$srf_error = log(score_df$pred_srf) - log(score_df$sale_price)

 error_df <- score_df %>%
   dplyr::select(sale_id, latitude, longitude, sale_price, lm_error, slm_error, rf_error, srf_error) %>%
   tidyr::pivot_longer(., -c('sale_id', 'latitude', 'longitude', 'sale_price'),
                       names_to = 'model', values_to = 'error')

 error_df %>%
   dplyr::group_by(model) %>%
   dplyr::summarize(mdape = median(abs(error), na.rm = TRUE),
                    mdpe = median(error, na.rm=TRUE))



 error_sf <- error_df %>%
   sf::st_as_sf(., coords = c('longitude', 'latitude'),
                remove = FALSE)







 # Extraction Predictions
 rf_pred <- mod_rf$predictions

 ## Gather Partial Dependency Plots

 # Set up simulation data.  Here we take just 100 random sales from data
 sim_df <- hpiR::rfSimDf(rf_df = score_df,
                         seed = 1,
                         sim_count = 100)

 # Calculate Partial dependency (or how prediction changes if only that variable is changed)
 # For Year Built.  You can set range in the last parameter, here i'm using 1800 to 2019.
 yearbuilt_pdp <- pdp::partial(object = mod_rf,
                               train = sim_df,
                               pred.var = "year_built",
                               pred.grid = data.frame(year_built = 1880:2019))


 ggplot(yearbuilt_pdp, aes(x = year_built, y = exp(yhat))) +
   geom_line() +
   ylab('Median Sale Price') +
   xlab('\n Year Built') +
   ggtitle('Partial Dependence Plot')

 yearbuilt_pdp <- pdp::partial(object = mod_rf,
                               train = sim_df,
                               pred.var = "sqft",
                               pred.grid = data.frame(sqft = 800:3800))


 ggplot(yearbuilt_pdp, aes(x = sqft, y = exp(yhat))) +
   geom_line() +
   ylab('Median Sale Price') +
   xlab('\n Year Built') +
   ggtitle('Partial Dependence Plot')




 score_df <- assignBorder(score_df, 'zip_code', k = 4)
 score_df <- assignBorder(score_df, 'zip_code', k = 8, 'border_10')
 score_df$border_10 <- ifelse(score_df$border_5, FALSE, score_df$border_10)

 score_df %>% dplyr::group_by(border_5, border_10) %>%
   dplyr::summarize(count = dplyr::n(),
                    mape = median(abs(slm_error), na.rm=TRUE),
                    m = median(abs(srf_error), na.rm=TRUE))


 sp_df <- sp::SpatialPointsDataFrame(
   sp::SpatialPoints(cbind(score_df$longitude, score_df$latitude)),
   score_df)
 nbl <- sp_df %>%
   spdep::knearneigh(., k = 5, longlat = NULL, RANN=TRUE) %>%
   spdep::knn2nb(.) %>%
   spdep::nb2listw(.)

 #
 err <- grep('error', names(score_df))
 spac_ <- list()

 for(i in 1:length(err)){
   x <- score_df[, err[i]]
   spac_[[i]] <- spdep::moran.test(unlist(x), nbl)
 }
 names(spac_) <- names(score_df)[err]

 spac_
