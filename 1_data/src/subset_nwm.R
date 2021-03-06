subset_nwm <- function(ind_file, model_configuration, comids, remake_file, gd_config, gd_post = T) {

  out_file <- as_data_file(ind_file)

  nc <- nc_open(model_configuration)

  comid_list <- readr::read_delim(sc_retrieve(comids, remake_file = remake_file), delim = "\t") %>%
    dplyr::pull(COMID)

  site_inds <- which(nc$dim$feature_id$vals %in% comid_list) # indices into original nc file

  if(length(site_inds) > length(comid_list)) {
    warning("found duplicate comids that match in the input file.")
  }

  new_feature_id <- nc$dim$feature_id$vals[site_inds] # comid list

  new_feature_id_dim <- ncdim_def(nc$dim$feature_id$name,
                                  units = "",
                                  vals = c(1:length(new_feature_id)),
                                  create_dimvar = F)

  if(!"reference_time" %in% names(nc$dim)) {

    time_dim <- ncdim_def(nc$dim$time$name,
                          units = "",
                          vals = nc$dim$time$vals,
                          unlim = FALSE, create_dimvar = T)

    vars <- list(ncvar_def(nc$var$streamflow$name,
                           units = nc$var$streamflow$units,
                           prec = "integer",
                           dim = list(new_feature_id_dim,
                                      time_dim)))
  } else {

    time_dim <- ncdim_def(nc$dim$time$name,
                          units = "",
                          vals = 1:nc$dim$time$len,
                          unlim = FALSE, create_dimvar = FALSE)

    reference_time_dim <- ncdim_def(nc$dim$reference_time$name,
                                    units = "",
                                    vals = 1:length(nc$dim$reference_time$vals),
                                    create_dimvar = FALSE)

    vars <- list(ncvar_def(nc$var$streamflow$name,
                           units = nc$var$streamflow$units,
                           prec = "integer",
                           dim = list(new_feature_id_dim,
                                      time_dim,
                                      reference_time_dim)),
                 ncvar_def(nc$dim$time$name,
                           units = nc$dim$time$units,
                           prec = "integer",
                           dim = list(time_dim,
                                      reference_time_dim)),
                 ncvar_def(nc$dim$reference_time$name,
                           units = nc$dim$reference_time$units,
                           dim = reference_time_dim))
  }

  vars <- c(vars,
            list(ncvar_def(nc$dim$feature_id$name,
                           units = "",
                           dim = new_feature_id_dim,
                           prec = "integer"),
                 ncvar_def(nc$var$latitude$name,
                           units = nc$var$latitude$units,
                           dim = new_feature_id_dim),
                 ncvar_def(nc$var$longitude$name,
                           units = nc$var$longitude$units,
                           dim = new_feature_id_dim)))

  new_nc <- nc_create(out_file, vars)

  for(var in c(nc$var, nc$dim, list(list(name = 0)))) {
    atts <- ncatt_get(nc, var$name)
    for(att in names(atts)) ncatt_put(new_nc, var$name, attname = att, attval = atts[[att]])
  }

  nc_close(new_nc)
  new_nc <- nc_open(out_file, write = TRUE)

  dimids <- nc$var$streamflow$dimids

  for(s in 1:length(site_inds)) {
    if(length(dimids) == 2) {
      # Note axis order is assumed here!!!
      ncvar_put(new_nc, new_nc$var$streamflow,
                ncvar_get(nc, nc$var$streamflow,
                          start = c(site_inds[s], 1),
                          count = c(1, -1), raw_datavals = TRUE),
                start = c(s, 1), count = c(1, -1))
    } else if(length(dimids) == 3) {
      for(r in 1:nc$dim$reference_time$len) {
        ncvar_put(new_nc, new_nc$var$streamflow,
                  ncvar_get(nc, nc$var$streamflow,
                            start = c(site_inds[s], 1, r),
                            count = c(1, -1, 1), raw_datavals = TRUE),
                  start = c(s, 1, r),
                  count = c(1, -1, 1))
      }
    }
  }

  if("reference_time" %in% names(nc$dim)) {
    ncvar_put(new_nc, "reference_time", nc$dim$reference_time$vals)
    ncvar_put(new_nc, "time", nc$dim$time$vals)
  }

  ncvar_put(new_nc, new_nc$var$latitude, ncvar_get(nc, nc$var$latitude)[site_inds])
  ncvar_put(new_nc, new_nc$var$longitude, ncvar_get(nc, nc$var$longitude)[site_inds])

  ncvar_put(new_nc, new_nc$dim$feature_id$name, ncvar_get(nc, nc$dim$feature_id$name)[site_inds])

  nc_close(new_nc)
  nc_close(nc)

  if(gd_post){
    gd_put(remote_ind=ind_file, local_source=out_file, config_file=gd_config)
  }
}
