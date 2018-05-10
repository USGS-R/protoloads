aggregate_nwis <- function(ind_file, raw_ind_file, remake_file, sites_yml, gd_config) {

  # aggregating NWIS data to a daily scale
  sites = yaml::yaml.load_file(sc_retrieve(sites_yml,remake_file = remake_file))

  input_raw = readRDS(sc_retrieve(raw_ind_file, remake_file = remake_file))

  agg_flow <- input_raw$flow %>%
    dplyr::filter(site_no %in% sites) %>% # stats package overrides dplyr filter()
    mutate(date = as.Date(dateTime)) %>%
    group_by(site_no, date, parm_cd) %>%
    summarise(
      daily_mean = mean(result_va),
      daily_cd = ifelse('<' %in% remark_cd, '<', '')) %>%
    ungroup() %>%
    mutate(tz_cd = 'UTC') %>%
    as_data_frame()

  agg_sensor = input_raw$nitrate_sensor %>%
    dplyr::filter(
      site_no %in% sites,
      remark_cd != 'P Eqp') %>%
    mutate(date = as.Date(dateTime)) %>%
    group_by(site_no, date, parm_cd) %>%
    summarise(
      daily_mean = mean(result_va),
      daily_cd = ifelse('<' %in% remark_cd, '<', '')) %>%
    ungroup() %>%
    mutate(tz_cd = 'UTC') %>%
    as_data_frame()

  agg_grab = input_raw$nitrate_grab %>%
    dplyr::filter(site_no %in% sites, parm_cd != '99133') %>% # stats package overrides dplyr filter()
    mutate(date = as.Date(startDateTime)) %>%
    group_by(site_no, date, parm_cd) %>%
    summarise(
      daily_mean = mean(result_va),
      daily_cd = ifelse('<' %in% remark_cd, '<', '')) %>%
    ungroup() %>%
    mutate(tz_cd = 'UTC') %>%
    as_data_frame()

  data_file <- as_data_file(ind_file)
  saveRDS(list(flow = agg_flow, nitrate_sensor = agg_sensor, nitrate_grab = agg_grab), data_file)
  gd_put(remote_ind = ind_file, local_source = data_file, config_file = gd_config)
}

aggregate_nwm <- function(ind_file, raw_ind_file, remake_file, sites_yml, comids_file, gd_config) {
  # this is just a place holder for now until nwm nc files are in correct format

####### commenting out for now until nwm nc issues are sorted out; creating fake hydrographs below #####
  # aggregating NWM data to a daily scale
  # sites = yaml::yaml.load_file(sc_retrieve(sites_yml,remake_file = remake_file))
  #
  # comids_lookup = readr::read_delim(sc_retrieve(comids_file, remake_file = remake_file), delim='\t')
  #
  # input_raw = nc_open(sc_retrieve(raw_ind_file, remake_file = remake_file))
  #
  # keep <- input_raw$dim$feature_id$vals %in% comids_lookup$COMID # comids
  #
  # dimids <- input_raw$var$streamflow$dimids
  #
  # site_inds <- which(keep)
  #
  # if(length(dimids) == 2) {
  #   streamflow <- matrix(nrow=input_raw$dim$time$len, ncol=length(site_inds))
  # } else if (length(dimids) == 3) {
  #   if(!all(input_raw$var$streamflow$dimids == c(0,2,1))) stop("dimids now as expected")
  #   streamflow <- array(dim = c(length(site_inds), input_raw$dim$time$len, input_raw$dim$reference_time$len))
  # }
  #
  # for(s in 1:length(site_inds)) {
  #   if(length(dimids) == 2) {
  #     # Note axis order is assumed here!!!
  #     streamflow[,s] <- ncvar_get(input_raw, input_raw$var$streamflow,
  #                                 start = c(site_inds[s], 1),
  #                                 count = c(1, -1))
  #     # time <- input_raw$var$streamflow$dim[[2]]$vals # time value; minutes since 1970-01-01 00:00:00 UTC
  #
  #
  #
  #   } else if(length(dimids) == 3) {
  #     for(r in 1:input_raw$dim$reference_time$len) {
  #       streamflow[s,,r] <- ncvar_get(input_raw, input_raw$var$streamflow,
  #                                     start = c(site_inds[s], 1, r),
  #                                     count = c(1, -1, 1))
  #     }
  #   }
  # }
  # # stream flow is [site, valid time, reference time]
  #
  # ncvar_put(new_nc, new_nc$var$streamflow, streamflow)
  #
  # if("reference_time" %in% names(nc$dim)) {
  #   ncvar_put(new_nc, "reference_time", nc$dim$reference_time$vals)
  # }
  #
  #
######

  # agg_nwm = lapply(sites, function(site){
  #   agg_out = input_raw$nitrate_sensor %>%
  #     dplyr::filter(site_no == site) %>% # stats package overrides dplyr filter()
  #     mutate(date = as.Date(dateTime)) %>%
  #     group_by(date) %>%
  #     summarise(X_99133_000000_mean = mean(X_99133_00000)) %>%
  #     mutate(site_no = site, tz_cd = 'UTC')
  # }) %>% bind_rows() %>% as_data_frame()

  #fake hydrograph for now
  date_retro = seq(as.Date('1993-01-01'), as.Date('2018-03-01'), by = 'days') # retro dates
  date_forecast = seq(as.Date('2017-01-01'), as.Date('2018-05-01'), by = 'days') # reference dates
  model_range = ifelse(length(grep('retro', ind_file))>0,NA, ifelse(length(grep('med', ind_file))>0, 10, 30)) # model range in days

  sites = yaml::yaml.load_file(sc_retrieve(sites_yml,remake_file = remake_file))
  comids_lookup = readr::read_delim(sc_retrieve(comids_file, remake_file = remake_file), delim='\t')

  agg_out = data.frame()
  for(site in 1:length(sites)){
    if(length(grep('retro',ind_file))>0){

      hydrograph = rgamma(length(date_retro),shape = 1, scale = 2)

      agg_cur = data.frame(date = date_retro,
                           flow = hydrograph,
                           site_no = rep(sites[site], length(date_retro)),
                           comid = rep(comids_lookup$COMID[comids_lookup$site_id==sites[site]], length(date_retro)))

    }else{
      agg_cur = lapply(date_forecast, function(date){

        valid = seq(date,
                    date + as.difftime(model_range-1, units = 'days'),
                    by = 'days')

        data.frame(ref_date = rep(date, model_range),
                   valid_date = valid,
                   flow = rgamma(model_range,shape = 1, scale = 2),
                   site_no = rep(sites[site], model_range),
                   comid = rep(comids_lookup$COMID[comids_lookup$site_id==sites[site]], model_range))
      }) %>% bind_rows
    }
    agg_out = rbind(agg_out, agg_cur)
  }

  data_file <- as_data_file(ind_file)
  saveRDS(agg_out, data_file)
  gd_put(remote_ind = ind_file, local_source = data_file, config_file = gd_config)
}


