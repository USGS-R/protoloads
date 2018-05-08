# list tasks
# (2 model types per reference date)*(3 sites)*(30 valid dates)*(10sec/model)/(60sec*60min) = 40 minutes
list_tasks <- function(sites_yml, remake_file) {
  sites <- yaml::yaml.load_file(sc_retrieve(sites_yml, remake_file=remake_file))
  valid_dates <- as.Date('2017-05-04') %>%
    {seq(
      .,
      .+as.difftime(11,units='days'),
      by=as.difftime(1,units='days'))}
  tasks <-
    tidyr::crossing(
      model_range=c('med', 'long1'), # 'long2', 'long3', 'long4'
      valid_date=valid_dates
    ) %>%
    group_by(model_range, valid_date) %>%
    dplyr::do(with(., {
      forecast_range <- ifelse(model_range=='med', 10, 30)
      first_ref <- valid_date - as.difftime(forecast_range-1, units='days')
      ref_seq <- seq(first_ref, valid_date, by=as.difftime(1, units='days'))
      data_frame(
        model_range,
        valid_date,
        ref_date=ref_seq
        #last_valid_date=ref_date+as.difftime(forecast_range-1, units='days'))
      )
    })) %>%
    ungroup() %>%
    select(-valid_date) %>%
    distinct() %>%
    group_by(model_range, ref_date) %>%
    do(with(., {data_frame(model_range, ref_date, site=sites)})) %>%
    mutate(ref_datestr=format(ref_date, '%Y%m%d')) %>%
    tidyr::unite(
      task_name, site, model_range, ref_datestr,
      sep='_', remove=FALSE) %>%
    select(-ref_datestr)

  return(tasks)
}


# prepare a plan for downloading (from WQP) and posting (to GD) one data file
# per state
plan_forecasts <- function(
  tasks_df, folders,
  site_info_ind, nwis_data_ind,
  nwm_retro_ind, nwm_med_ind, nwm_long1_ind, nwm_long2_ind, nwm_long3_ind, nwm_long4_ind,
  remake_file
) {

  # function to sprintf a bunch of key-value (string-variableVector) pairs, then
  # paste them together with a good separator for constructing remake recipes
  psprintf <- function(..., sep='\n      ') {
    args <- list(...)
    strs <- mapply(function(string, variables) {
      spargs <- if(string == '') list(variables) else c(list(string), as.list(variables))
      do.call(sprintf, spargs)
    }, string=names(args), variables=args)
    paste(strs, collapse=sep)
  }

  # steps: subset, forecast, post, retrieve
  subset <- scipiper::create_task_step(
    step_name = 'subset',
    target_name = function(task_name, step_name, ...) {
      sprintf('inputs_%s', task_name)
    },
    command = function(task_name, ...) {
      task_info <- filter(rename(tasks_df, tn=task_name), tn==task_name)
      psprintf(
        "prep_inputs(",
        "nwis_site=I('%s'),"=task_info$site,
        "nwm_model=I('%s'),"=task_info$model_range,
        "ref_date=I('%s'),"=task_info$ref_date,
        "site_info_ind='%s',"=site_info_ind,
        "nwis_data_ind='%s',"=nwis_data_ind,
        "nwm_retro_ind='%s',"=nwm_retro_ind,
        "nwm_forecast_ind='%s',"=if(task_info$model_range=='med') nwm_med_ind else nwm_long1_ind,
        "remake_file='%s')"=remake_file,
        sep="\n      ")
    }
  )

  forecast_rloadest <- scipiper::create_task_step(
    step_name = 'forecast_rloadest',
    target_name = function(task_name, step_name, ...) {
      file.path(folders$tmp, sprintf('preds_rloadest_%s.rds', task_name))
    },
    command = function(task_name, ...) {
      paste(
        "apply_loadest(",
        "ind_file=target_name,",
        sprintf("inputs=inputs_%s)", task_name),
        sep="\n      ")
    }
  )

  task_plan <- scipiper::create_task_plan(
    task_names=tasks_df$task_name,
    task_steps=list(subset, forecast_rloadest),
    final_steps='forecast_rloadest',
    add_complete=FALSE,
    ind_dir=folders$log)

}

create_forecast_makefile <- function(makefile, task_plan, remake_file) {
  create_task_makefile(
    makefile=makefile, task_plan=task_plan,
    include=remake_file,
    packages=c('dplyr', 'scipiper'),
    file_extensions=c('ind'))
}