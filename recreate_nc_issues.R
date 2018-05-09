
# we made a script to show some of the issues we were running into; There's a list of issues below; you should be able to march through the script

#### 1. feature_id dimension is strange ####
nc = ncdf4::nc_open(sc_retrieve( "1_data/out/nwm_retro.nc.ind", remake_file = '2_munge.yml')) # nc file for sticking in streamflow data

nc$dim$feature_id$vals # there are 12 comids in the feature id dim when we would expect 8
duplicated(nc$dim$feature_id$vals) # 4 of these comids are repeats

#### 2. streamflow sequences don't look realistic ####
streamflow <- matrix(nrow=nc$dim$time$len, ncol=nc$dim$feature_id$len)

for(s in 1:ncol(streamflow)) {
  streamflow[,s] <- ncvar_get(input_raw, input_raw$var$streamflow,
                                start = c(s, 1),
                                count = c(1, -1))
}

ylim = range(streamflow)
col= grDevices::rainbow(n = ncol(streamflow))
plot(streamflow[,1],cex=0,ylim=ylim)
for(i in 1:ncol(streamflow)){
  lines(streamflow[,i],col=col[i],lwd=3)
}

# notice that all the sites have the same data but not quite the same data
length(which(streamflow[,1]==streamflow[,2]))/nrow(streamflow) # fraction same data


#### 3. looks to us like streamflow matrix is transposed ####

out_file <- 'throw_out.nc'

comid_list <- readr::read_delim(sc_retrieve('1_data/out/comids.tsv.ind', remake_file = '1_data.yml'), delim = "\t") %>%
  dplyr::pull(COMID)

keep <- nc$dim$feature_id$vals %in% comid_list

new_feature_id <- nc$dim$feature_id$vals[keep] # comid list

new_feature_id_dim <- ncdim_def(nc$dim$feature_id$name,
                                units = "",
                                vals = c(1:length(new_feature_id)),
                                create_dimvar = F)

time_dim <- ncdim_def(nc$dim$time$name,
                        units = "",
                        vals = nc$dim$time$vals,
                        unlim = FALSE, create_dimvar = T)

vars <- list(ncvar_def(nc$var$streamflow$name,
                         units = nc$var$streamflow$units,
                         prec = "integer",
                         dim = list(new_feature_id_dim,
                                    time_dim)))

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

site_inds <- which(keep) # indices into original nc file


# we created fake streamflow for first three columns of matrix and zeros for rest of columns to try and understand this a bit better

n = new_nc$dim$time$len

streamflow = matrix(c(seq(1,10, length.out = n),
                      seq(3,4, length.out = n),
                      seq(15,20, length.out = n),
                    rep(rep(0,n),new_nc$dim$feature_id$len - 3)),
                    ncol= new_nc$dim$feature_id$len)

ylim = range(streamflow)
col= grDevices::rainbow(n = ncol(streamflow))
plot(streamflow[,1],cex=0,ylim=ylim)
for(i in 1:ncol(streamflow)){
  lines(streamflow[,i],col=col[i],lwd=3)
}

# we put this into streamflow nc
ncvar_put(new_nc, new_nc$var$streamflow, streamflow*(1/new_nc$var$streamflow$scaleFact))

# when we pull out from nc we should get same 'hydrographs'

streamflow_nc <- matrix(nrow=new_nc$dim$time$len, ncol=new_nc$dim$feature_id$len)

for(s in 1:ncol(streamflow_nc)) {
  streamflow_nc[,s] <- ncvar_get(new_nc, new_nc$var$streamflow,
                              start = c(s, 1),
                              count = c(1, -1))
}

ylim = range(streamflow_nc)
col= grDevices::rainbow(n = ncol(streamflow_nc))
plot(streamflow_nc[,1],cex=0,ylim=ylim)
for(i in 1:ncol(streamflow_nc)){
  lines(streamflow_nc[,i],col=col[i],lwd=3)
} #  they're all the same 'hydrograph'

# when we loop through the sites and use ncvar_put(), it seems to work

for(s in 1:length(site_inds)) {
    ncvar_put(new_nc,
              new_nc$var$streamflow,
              streamflow[,s]*(1/new_nc$var$streamflow$scaleFact),  # need to multiply streamflow data by scale factor in nc var
              start = c(s, 1),
              count = c(1,-1))
}

streamflow_nc <- matrix(nrow=new_nc$dim$time$len, ncol=new_nc$dim$feature_id$len)

for(s in 1:ncol(streamflow_nc)) {
  streamflow_nc[,s] <- ncvar_get(new_nc, new_nc$var$streamflow,
                                 start = c(s, 1),
                                 count = c(1, -1))
}

ylim = range(streamflow_nc)
col= grDevices::rainbow(n = ncol(streamflow_nc))
plot(streamflow_nc[,1],cex=0,ylim=ylim)
for(i in 1:ncol(streamflow_nc)){
  lines(streamflow_nc[,i],col=col[i],lwd=3)
}

nc_close(new_nc)


