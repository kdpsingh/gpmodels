#' New internal helper function
gpm_define_steps = function(groups, temporal_id, step, step_units, max_length, baseline, max_step_times_per_id,
                            lookback_converted, window_converted, output_folder,
                            log_file) {

  if (!is.null(baseline) && baseline) {
    output_frame = data.frame(time = 0)
    return(output_frame)
  }

  max_step_time =
    max_step_times_per_id %>%
    dplyr::filter(!!rlang::parse_expr(temporal_id) == groups[[temporal_id]]) %>%
    dplyr::pull(gpm_step_time)

  if (length(max_step_time) == 0 || max_step_time < 0) { # This should only be the case if someone has no observations in temporal_data after time 0
    stop(paste0('No temporal data was found during the relevant period for ',
                temporal_id, ' ', groups[[temporal_id]], '.'))
  }

  if  (!is.null(max_length)) {
    if (!is.null(step_units)) {
      max_step_time = pmin(lubridate::time_length(max_length, unit = step_units), max_step_time)
    } else {
      max_step_time = pmin(max_length, max_step_time)
    }
  }

  time = seq(0, max_step_time, by = step)

  # window_num = 1:(lookback_converted/window_converted)

  # window_time = window_num*window_converted

  # return_frame =
  #   tidyr::expand_grid(time, window_time)

  return_frame = data.frame(time = time)

  return(return_frame)
}

#' New internal helper function
gpm_calc = function(groups, temporal_id, temporal_variable, temporal_value, temporal_time,
                    lookback_converted, dots, window_converted, temporal_data_of_interest,
                    stats, impute, pb, all_temporal_vars, missing_value_frame, strategy) {

  if (lookback_converted < 0) { # E.g. if it is a lookahead
    output_item =
      temporal_data_of_interest %>%
      dplyr::filter(!!rlang::parse_expr(temporal_id) == groups[[temporal_id]][1] &
                      !!rlang::parse_expr(temporal_time) > groups$time[1] & # outcome cannot include right now
                      !!rlang::parse_expr(temporal_time) <= groups$time[1] - lookback_converted)

    output_item = output_item %>%
      dplyr::mutate(window_time = abs(ceiling((groups$time[1] - !!rlang::parse_expr(temporal_time)) /
                                                window_converted) * window_converted))
  } else { # if it is a lookback
    if (!is.null(dots[['growing']]) && dots[['growing']]) {
      output_item =
        temporal_data_of_interest %>%
        dplyr::filter(!!rlang::parse_expr(temporal_id) == groups[[temporal_id]][1] &
                        !!rlang::parse_expr(temporal_time) <= groups$time[1] & # includes now in predictors
                        !!rlang::parse_expr(temporal_time) >= 0) # Includes time 0
      output_item = output_item %>%
        dplyr::mutate(window_time = 48) # arbitrary, will ignore
    } else {
      output_item =
        temporal_data_of_interest %>%
        dplyr::filter(!!rlang::parse_expr(temporal_id) == groups[[temporal_id]][1] &
                        !!rlang::parse_expr(temporal_time) <= groups$time[1] & # includes now in predictors
                        !!rlang::parse_expr(temporal_time) > groups$time[1] - lookback_converted) # up to X hours ago but not including X
      output_item = output_item %>%
        dplyr::mutate(window_time = floor((groups$time[1] - !!rlang::parse_expr(temporal_time)) /
                                            window_converted) * window_converted + window_converted)
    }
  }

  # If it's *not* a growing predictor, then convert the window to a factor variable
  if (is.null(dots[['growing']]) || !dots[['growing']]) {
    output_item = output_item %>%
      dplyr::mutate(window_time = factor(window_time,
                                         levels = abs(1:(lookback_converted/window_converted)*window_converted)))
  }

  # convert temporal_variable to a factor variable, which is useful
  # to fill in missing values
  output_item =
    output_item %>%
    dplyr::mutate(!!rlang::parse_expr(temporal_variable) :=
                    factor(!!rlang::parse_expr(temporal_variable), levels = all_temporal_vars))

  # If there are *no* values returned
  if (nrow(output_item) == 0) {
    output_item =
      tidyr::crossing(
        dplyr::tibble(!!rlang::parse_expr(temporal_variable) := all_temporal_vars),
        dplyr::tibble(gpm_stat = names(stats))
      ) %>%
      tidyr::crossing(
        dplyr::tibble(window_time = 1:(lookback_converted/window_converted)*window_converted)
      ) %>%
      dplyr::mutate(!!rlang::parse_expr(temporal_id) := groups[[temporal_id]][1],
                    time = groups$time[1]) %>%
      dplyr::select(!!rlang::parse_expr(temporal_id), time, window_time,
                    !!rlang::parse_expr(temporal_variable), dplyr::everything()) %>%
      dplyr::mutate(gpm_value = NA)
  } else {
    output_item =
      output_item %>%
      dplyr::arrange(!!rlang::parse_expr(temporal_variable), !!rlang::parse_expr(temporal_time)) %>%
      dplyr::group_by(!!rlang::parse_expr(temporal_variable), window_time) %>%
      dplyr::summarize_at(temporal_value,
                          .funs = stats) %>%
      tidyr::complete(window_time) %>%
      dplyr::ungroup() %>%
      tidyr::gather(gpm_stat, gpm_value, -!!rlang::parse_expr(temporal_variable), -window_time) %>%
      tidyr::complete(!!rlang::parse_expr(temporal_variable), window_time, gpm_stat) %>%
      dplyr::mutate(window_time = window_time %>% as.character() %>% as.numeric()) %>%
      dplyr::mutate(!!rlang::parse_expr(temporal_variable) :=
                      !!rlang::parse_expr(temporal_variable) %>% as.character()) %>%
      dplyr::mutate(!!rlang::parse_expr(temporal_id) := groups[[temporal_id]][1],
                    time = groups$time[1]) %>%
      dplyr::select(!!rlang::parse_expr(temporal_id), time, window_time,
                    !!rlang::parse_expr(temporal_variable), dplyr::everything())
  }

  # Fill in precalculated missing values (separately for each statistic)
  suppressMessages({
    output_item =
      dplyr::left_join(
        output_item,
        missing_value_frame
      ) %>%
      dplyr::mutate(gpm_value = ifelse(is.na(gpm_value), gpm_missing_value, gpm_value)) %>%
      dplyr::select(-gpm_missing_value) %>%
      dplyr::mutate(gpm_value = dplyr::na_if(gpm_value, -Inf)) %>%
      dplyr::mutate(gpm_value = dplyr::na_if(gpm_value, Inf))
  })

  # Imputation
  if (impute) {
    output_item =
      output_item %>%
      dplyr::arrange(!!rlang::parse_expr(temporal_variable),
                     gpm_stat,
                     dplyr::desc(window_time * sign(window_converted))) %>%
      dplyr::group_by(!!rlang::parse_expr(temporal_variable),
                      gpm_stat) %>%
      tidyr::fill(-!!rlang::parse_expr(temporal_variable),
                  -gpm_stat) %>%
      dplyr::ungroup()
  }

  # Name the variables
  output_item =
    output_item %>%
    dplyr::mutate(gpm_variable =
                    paste0(!!rlang::parse_expr(temporal_variable),
                           '_', gpm_stat),
                  gpm_value = gpm_value) %>%
    dplyr::select(-!!rlang::parse_expr(temporal_variable), -gpm_stat)

  if (lookback_converted < 0) { # e.g. if it is a lookahead
    output_item =
      output_item %>%
      dplyr::mutate(gpm_variable = paste0('outcome_', gpm_variable, '_',
                                          stringr::str_pad(abs(window_time),
                                                           nchar(abs(lookback_converted)), pad = '0'))) %>%
      dplyr::select(-window_time)
  } else { # if it is a lookback

    if (!is.null(dots[['baseline']]) && dots[['baseline']]) {
      output_item =
        output_item %>%
        dplyr::mutate(gpm_variable = paste0('baseline_', gpm_variable, '_',
                                            stringr::str_pad(abs(window_time),
                                                             nchar(abs(lookback_converted)), pad = '0'))) %>%
        dplyr::select(-window_time)

    } else if (!is.null(dots[['growing']]) && dots[['growing']]){
      output_item =
        output_item %>%
        dplyr::mutate(gpm_variable = paste0('growing_', gpm_variable)) %>%
        dplyr::select(-window_time)
    } else {
      output_item =
        output_item %>%
        dplyr::mutate(gpm_variable = paste0(gpm_variable, '_',
                                            stringr::str_pad(abs(window_time),
                                                             nchar(abs(lookback_converted)), pad = '0'))) %>%
        dplyr::select(-window_time)
    }
  }

  output_item =
    output_item %>%
    tidyr::spread(gpm_variable, gpm_value) %>%
    as.data.frame()

  if (!is.null(dots[['baseline']]) && dots[['baseline']]) {
    output_item =
      output_item %>% dplyr::select(-time)
  }

  if (strategy == 'sequential') {
    pb$tick()
  }
  return(output_item)
}


#' New furrr-enabled add_predictors function
#' Internal only
gpm_add_predictors_internal = function(time_frame = NULL,
                              variables = NULL,
                              category = NULL,
                              lookback = lubridate::hours(48),
                              window = lookback,
                              stats = c(mean = mean,
                                        min = min,
                                        max = max),
                              impute = TRUE,
                              output_file = TRUE,
                              log_file = TRUE,
                              check_size_only = FALSE,
                              filename_prefix = '',
                              ...) {
  dots = list(...)

  if (!is.null(time_frame$chunk_size) && !output_file) {
    stop('If you set a chunk_size, then output_file must be set to TRUE.')
  }

  if (is.null(variables) && is.null(category)) {
    stop('You must specify either a variable or a category.')
  }

  if (!is.null(variables) && !is.null(category)) {
    stop('You must specify only a variable or a category, not both.')
  }

  if (!is.null(variables)) {
    for (variable in variables) {
      if (!variable %in% time_frame$temporal_data_dict$variable) {
        stop(paste0('The variable ', variable, ' could not be found in the temporal data.'))
      }
    }
  }

  if (!is.null(category) && !any(grepl(category, time_frame$temporal_data[[time_frame$temporal_category]]))) {
    stop(paste0('The category ', category, ' could not be found in the temporal data.'))
  }

  gpm_variables = variables
  gpm_category = category

  if (!is.null(time_frame$step_units)) {
    lookback_converted = lubridate::time_length(lookback, unit = time_frame$step_units)
    window_converted = lubridate::time_length(window, unit = time_frame$step_units)
  } else {
    lookback_converted = lookback
    window_converted = window
  }

  if (lookback_converted == 0) {
    stop('lookback/lookahead cannot be 0.')
  }
  if (window_converted == 0) {
    stop('window cannot be 0.')
  }

  if ((lookback_converted > 0 && window_converted < 0) ||
      (lookback_converted < 0 && window_converted > 0)) {
    stop(paste0('The lookback/lookahead and window must either *both* be positive ',
                '(for lookback) or *both* negative (for lookahead).'))
  }

  if (lookback_converted %% window_converted != 0) {
    stop ('The lookback must be divisible by the window (with no remainder).')
  }

  temporal_data_of_interest = time_frame$temporal_data

  # temporal_data_of_interest =
  #   time_frame$temporal_data %>%
  #   dplyr::group_by(!!rlang::parse_expr(time_frame$temporal_id)) %>%
  #   dplyr::mutate(gpm_step_time =
  #                   !!rlang::parse_expr(time_frame$temporal_time) %/%
  #                   time_frame$step *
  #                   time_frame$step +
  #                   time_frame$step) %>%
  #   dplyr::mutate(gpm_lookback_time =
  #                   gpm_step_time - lookback_converted) %>%
  #   dplyr::ungroup()


  # fixed_end and fixed_start should *always* be available now because they are now created if not provided
  max_step_times_per_id =
    temporal_data_of_interest %>%
    dplyr::left_join(., time_frame$fixed_data %>%
                       dplyr::select_at(c(time_frame$fixed_id, time_frame$fixed_start, time_frame$fixed_end)) %>%
                       dplyr::rename(!!rlang::parse_expr(time_frame$temporal_id) := !!rlang::parse_expr(time_frame$fixed_id)) %>%
                       dplyr::mutate(gpm_fixed_start_time = !!rlang::parse_expr(time_frame$fixed_start)) %>%
                       dplyr::mutate(gpm_fixed_end_time = !!rlang::parse_expr(time_frame$fixed_end))
    ) %>%
    dplyr::distinct(!!rlang::parse_expr(time_frame$temporal_id), gpm_fixed_start_time, gpm_fixed_end_time)

  if (!is.null(time_frame$step_units)) {
    max_step_times_per_id =
      max_step_times_per_id %>%
      dplyr::mutate(gpm_step_time =
                      lubridate::time_length(gpm_fixed_end_time - gpm_fixed_start_time, unit = time_frame$step_units) %/% time_frame$step * time_frame$step) %>% # will select furthest time with complete step data
      dplyr::select(!!rlang::parse_expr(time_frame$temporal_id), gpm_step_time)
  } else {
    max_step_times_per_id =
      max_step_times_per_id %>%
      dplyr::mutate(gpm_step_time = (gpm_fixed_end_time - gpm_fixed_start_time) %/% time_frame$step * time_frame$step) %>% # will select furthest time with complete step data
      dplyr::select(!!rlang::parse_expr(time_frame$temporal_id), gpm_step_time)
  }


  # Check to see if there is any data in the max_step_times_per_id
  if (nrow(max_step_times_per_id) == 0) {
    message('No values found for the selected variable(s) during the time period.')
    if (log_file) {
      write(paste0(Sys.time(), ': No values found for the selected variable(s) during the time period.'),
            file.path(time_frame$output_folder, 'gpm_log.txt'), append = TRUE)
    }

    if (output_file == TRUE) {
      return(invisible(time_frame))
    }

    return(NULL)
  }

  # Filter temporal_data_of_interest to only the variables of interest
  if (!is.null(variables)) {
    temporal_data_of_interest =
      temporal_data_of_interest %>%
      dplyr::filter(!!rlang::parse_expr(time_frame$temporal_variable) %in% gpm_variables)
  } else if (!is.null(category)) {
    temporal_data_of_interest =
      temporal_data_of_interest %>%
      dplyr::filter(stringr::str_detect(!!rlang::parse_expr(time_frame$temporal_category), gpm_category))
  } else {
    stop('This option should not be possible.')
  }

  if ('character' %in% (time_frame$temporal_data_dict %>%
                        dplyr::filter(variable %in% temporal_data_of_interest[[time_frame$temporal_variable]]) %>%
                        dplyr::pull(class)) &&
      'numeric' %in% (time_frame$temporal_data_dict %>%
                      dplyr::filter(variable %in% temporal_data_of_interest[[time_frame$temporal_variable]]) %>%
                      dplyr::pull(class))) {
    stop(paste0('Please select variables that are either all numeric or all categorical. ',
                'They cannot be mixed. If both are to be selected, then you must dummy ',
                'code the categorical variables using pre_dummy_code().'))
  }

  # If all variables are numeric, convert value column to numeric prior to calculating stats
  if (all(time_frame$temporal_data_dict %>%
          dplyr::filter(variable %in% temporal_data_of_interest[[time_frame$temporal_variable]]) %>%
          dplyr::pull(class) == 'numeric')) {
    temporal_data_of_interest[[time_frame$temporal_value]] =
      as.numeric(temporal_data_of_interest[[time_frame$temporal_value]])
  } else {
    temporal_data_of_interest[[time_frame$temporal_value]] =
      as.character(temporal_data_of_interest[[time_frame$temporal_value]])
  }

  # Check to see if there is any data in the period of interest
  if (nrow(temporal_data_of_interest) == 0) {
    message('No values found for the selected variable(s) during the time period.')
    if (log_file) {
      write(paste0(Sys.time(), ': No values found for the selected variable(s) during the time period.'),
            file.path(time_frame$output_folder, 'gpm_log.txt'), append = TRUE)
    }

    if (output_file == TRUE) {
      return(invisible(time_frame))
    }

    return(NULL)
  }


  # Test to make sure all stats are calculable
  for (stat in stats) {
    tryCatch({
      do.call(stat, list(temporal_data_of_interest[[time_frame$temporal_value]]))},
      error = function (e) {
        stop(paste0('At least one of the statistics could not be calculated for the ',
                    'selected variables in the temporal data. Did you perhaps forget to ',
                    'run pre_dummy_code() on one of the variables of interest?'))
      })
  }

  if (!is.null(variables)) {
    message(paste0('Processing variables: ', paste0(variables, collapse = ', '), '...'))
    if (log_file) {
      write(paste0(Sys.time(), ': Processing variables: ', paste0(variables, collapse = ', '), '...'),
            file.path(time_frame$output_folder, 'gpm_log.txt'), append = TRUE)
    }
  } else if (!is.null(category)) {
    message(paste0('Processing category: ', category, '...'))
    if (log_file) {
      write(paste0(Sys.time(), ': Processing category: ', category, '...'),
            file.path(time_frame$output_folder, 'gpm_log.txt'), append = TRUE)
    }
  }

  # final_output_rows = max_step_times_per_id %>%
  #   dplyr::filter(gpm_step_time >= 0) %>%
  #   dplyr::pull(gpm_step_time) %>%
  #   {. / time_frame$step + 1} %>% # e.g., if max step for an id is 18 and step is 6, there will rows for 0, 6, 12, 18 (or 18/6 + 1 rows)
  #   {sum(.)}
  #
  # message(paste0('Anticipated number of rows in final output: ', final_output_rows))
  # if (log_file) {
  #   write(paste0(Sys.time(), ': Anticipated number of rows in final output: ', final_output_rows),
  #         file.path(time_frame$output_folder, 'gpm_log.txt'), append = TRUE)
  # }


  message('Allocating memory...')
  if (log_file) {
    write(paste0(Sys.time(), ': Allocating memory...'),
          file.path(time_frame$output_folder, 'gpm_log.txt'), append = TRUE)
  }

  output_frame =
    dplyr::tibble(!!rlang::parse_expr(time_frame$temporal_id) :=
                    unique(time_frame$temporal_data[[time_frame$temporal_id]])) %>%
    tidyr::crossing(
      dplyr::tibble(
        !!rlang::parse_expr(time_frame$temporal_variable) :=
          unique(temporal_data_of_interest[[time_frame$temporal_variable]]))) %>%
    dplyr::group_by(!!rlang::parse_expr(time_frame$temporal_id)) %>%
    dplyr::group_modify(~gpm_define_steps(groups = .y,
                                          temporal_id = time_frame$temporal_id,
                                          step = time_frame$step,
                                          step_units = time_frame$step_units,
                                          max_length = time_frame$max_length,
                                          baseline = dots[['baseline']],
                                          max_step_times_per_id = max_step_times_per_id,
                                          lookback_converted = lookback_converted,
                                          window_converted = window_converted,
                                          output_folder = time_frame$output_folder,
                                          log_file = log_file))

  message(paste0('Number of rows in final output: ', nrow(output_frame)))
  if (log_file) {
    write(paste0(Sys.time(), ': Number of rows in final output: ', nrow(output_frame)),
          file.path(time_frame$output_folder, 'gpm_log.txt'), append = TRUE)
  }

  if (check_size_only) {
    return(nrow(output_frame))
  }


  if (!is.null(dots[['baseline']]) && dots[['baseline']]) {
    if (!is.null(time_frame$step_units)) {
      temporal_data_of_interest[[time_frame$temporal_time]] =
        temporal_data_of_interest[[time_frame$temporal_time]] +
        lubridate::time_length(dots[['offset']], unit = time_frame$step_units)
    } else {
      temporal_data_of_interest[[time_frame$temporal_time]] =
        temporal_data_of_interest[[time_frame$temporal_time]] + offset
    }
  }

  total_num_groups = nrow(output_frame)

  if ('sequential' %in% class(future::plan())) {
    strategy = 'sequential'
    message('Parallel processing is DISABLED. Calculations are happening sequentially.')
    if (log_file) {
      write(paste0(Sys.time(), ': Parallel processing is DISABLED. Calculations are happening sequentially.'),
            file.path(time_frame$output_folder, 'gpm_log.txt'), append = TRUE)
    }
    pb = progress::progress_bar$new(format = "[:bar] :current/:total (:percent) Time remaining: :eta",
                                    total = total_num_groups) # intermediate_output_rows)

    pb$tick(0)
  } else {
    strategy = 'parallel'
    message('Parallel processing is ENABLED.')
    if (log_file) {
      write(paste0(Sys.time(), ': Parallel processing is ENABLED.'),
            file.path(time_frame$output_folder, 'gpm_log.txt'), append = TRUE)
    }
    pb = NULL
  }

  message('Determining missing values for each statistic...')
  if (log_file) {
    write(paste0(Sys.time(), ': Determining missing values for each statistic...'),
          file.path(time_frame$output_folder, 'gpm_log.txt'), append = TRUE)
  }
  # Use a bit of R magic. Looking for is.null() because median(NULL) returns NULL
  # Note: mean(NULL) returns NA, sum(NULL) returns 0, length(NULL) returns 0
  suppressWarnings({
    missing_value_frame = dplyr::tibble(gpm_stat = names(stats),
                                        gpm_missing_value =
                                          sapply(stats, function (x) {
                                            ifelse(is.null(do.call(x, list(NULL))),
                                                   NA,
                                                   do.call(x, list(NULL)))}))
  })

  message('Beginning calculation...')
  if (log_file) {
    write(paste0(Sys.time(), ': Beginning calculation...'),
          file.path(time_frame$output_folder, 'gpm_log.txt'), append = TRUE)
  }

  output_list =
    output_frame %>%
    dplyr::group_by(!!rlang::parse_expr(time_frame$temporal_id),
                    time) %>%
    dplyr::group_split()

  all_temporal_vars = unique(temporal_data_of_interest[[time_frame$temporal_variable]]) %>% as.factor()

  suppressWarnings({
    output_frame =
      output_list %>%
      furrr::future_map_dfr(.f = gpm_calc,
                            # groups = .x,
                            temporal_id = time_frame$temporal_id,
                            temporal_variable = time_frame$temporal_variable,
                            temporal_value = time_frame$temporal_value,
                            temporal_time = time_frame$temporal_time,
                            lookback_converted = lookback_converted,
                            dots = dots,
                            window_converted = window_converted,
                            temporal_data_of_interest = temporal_data_of_interest,
                            stats = stats,
                            impute = impute,
                            pb = pb,
                            all_temporal_vars = all_temporal_vars,
                            missing_value_frame = missing_value_frame,
                            strategy = strategy,
                            .progress = TRUE)
  })

  message('Completed calculation.')
  if (log_file) {
    write(paste0(Sys.time(), ': Completed calculation.'),
          file.path(time_frame$output_folder, 'gpm_log.txt'), append = TRUE)
  }

  # Check to see if there is any data in the output_frame
  if (nrow(output_frame) == 0) {
    message('No values found for the selected variable(s) during the time period.')
    if (log_file) {
      write(paste0(Sys.time(), ': No values found for the selected variable(s) during the time period.'),
            file.path(time_frame$output_folder, 'gpm_log.txt'), append = TRUE)
    }

    if (output_file == TRUE) {
      return(invisible(time_frame))
    }

    return(NULL)
  }

  if (lookback_converted < 0) {
    file_type = '_outcomes_'
  } else {
    file_type = '_predictors_'
  }

  if (!is.null(dots[['baseline']]) && dots[['baseline']]) {
    file_type = paste0('baseline', file_type)
  } else if (!is.null(dots[['growing']]) && dots[['growing']]) {
    file_type = paste0('growing', file_type)
  } else {
    file_type = paste0('rolling', file_type)
  }

  output_file_name = if (!is.null(category)) {
    file.path(time_frame$output_folder,
              paste0(filename_prefix, file_type, '_category_', category, '_', lubridate::now()) %>%
                janitor::make_clean_names() %>%
                paste0('.csv'))
  } else {
    file.path(time_frame$output_folder,
              paste0(filename_prefix, file_type, '_variables_', paste0(variables, collapse = '_'),
                     '_', lubridate::now()) %>%
                janitor::make_clean_names() %>%
                paste0('.csv'))
  }


  if (output_file == TRUE) {
    data.table::fwrite(output_frame, output_file_name)
    message(paste0('The output file was written to: ', output_file_name))
    if (log_file) {
      write(paste0(Sys.time(), ': The output file was written to: ', output_file_name),
            file.path(time_frame$output_folder, 'gpm_log.txt'), append = TRUE)
    }
    return(invisible(time_frame))
  }

  return(output_frame)
}
