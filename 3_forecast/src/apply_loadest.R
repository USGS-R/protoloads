apply_loadest <- function(output_rds, eList) {

  # Define a lookup table for translating INFO$param.units to conc.units
  units_lookup <- c('mg/l as N'='mg/L')

  sys_time <- system.time({
    # Fit the 7-parameter model
    fit_data <- eList$Sample %>%
      select(Date, ConcLow, ConcHigh, Q) %>%
      dplyr::filter(Date < eList$INFO$ref_date)
    fit <- loadReg(
      survival::Surv(ConcLow, ConcHigh, type="interval2") ~ model(9),
      data=fit_data,
      flow="Q",
      dates="Date",
      conc.units=units_lookup[eList$INFO$param.units],
      flow.units='cms',
      station=eList$INFO$site.no)

    # Generate load forecasts
    est_data <- eList$Daily %>%
      select(Date, Q) %>%
      dplyr::filter(Date >= eList$INFO$ref_date)
    preds_load <- predLoad(
      fit, newdata=est_data, by='day'
    )
    preds_conc <- predConc(
      fit, newdata=est_data, by='day'
    )
    preds <- full_join(preds_load, preds_conc, by=c('Date','Flow'), suffix=c('.Flux', '.Conc')) %>%
      mutate(Site = eList$INFO$site_no)
  })
  message(sprintf("Fitted and forecast from LOADEST model in %0.2f seconds", sys_time[['elapsed']]))

  # Prepare the full model output as an augmented eList
  aList <- c(eList, list(fit_data=fit_data, est_data=est_data, fit=fit, preds=preds))

  # Write the forecasts to file
  saveRDS(aList, output_rds)
}
