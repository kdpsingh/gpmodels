#' Function to add predictors
#' before fixed_start.
#' @export
gpm_add_rolling_predictors = function(time_frame = NULL,
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
                              last_chunk_completed = NULL) {

  if (is.null(time_frame$chunk_size)) {
    gpm_add_predictors_internal(time_frame = time_frame,
                                variables = variables,
                                category = category,
                                lookback = lookback,
                                window = window,
                                stats = stats,
                                impute = impute,
                                output_file = output_file,
                                log_file = log_file,
                                check_size_only = check_size_only)
  } else {
    assertthat::assert_that(time_frame$chunk_size > 0)

    # Make chunks based on temporal data, not fixed data
    unique_temporal_ids = sort(unique(time_frame$temporal_data[[time_frame$temporal_id]]))
    chunk_ids = ceiling(seq_len(length(unique_temporal_ids)) / time_frame$chunk_size)
    unique_chunks = unique(chunk_ids)
    n_chunks = max(unique_chunks)

    for (chunk_num in unique_chunks) {
      if (!is.null(last_chunk_completed) && chunk_num <= last_chunk_completed) {
        message(paste0('Skipping chunk # ', chunk_num, ' out of ', n_chunks, '...'))
        if (log_file) {
          write(paste0(Sys.time(), ': Skipping chunk # ', chunk_num, ' out of ', n_chunks, '...'),
                file.path(time_frame$output_folder, 'gpm_log.txt'), append = TRUE)
        }
        next
      }

      message(paste0('Processing chunk # ', chunk_num, ' out of ', n_chunks, '...'))
      if (log_file) {
        write(paste0(Sys.time(), ': Processing chunk # ', chunk_num, ' out of ', n_chunks, '...'),
              file.path(time_frame$output_folder, 'gpm_log.txt'), append = TRUE)
      }

      time_frame_chunk = time_frame

      time_frame_chunk$temporal_data =
        time_frame_chunk$temporal_data %>%
        dplyr::filter(!!rlang::parse_expr(time_frame_chunk$temporal_id) %in%
                        unique_temporal_ids[chunk_ids == chunk_num])

      time_frame_chunk$fixed_data =
        time_frame_chunk$fixed_data %>%
        dplyr::filter(!!rlang::parse_expr(time_frame_chunk$fixed_id) %in%
                        time_frame_chunk$temporal_data[[time_frame_chunk$temporal_id]])


      gpm_add_predictors_internal(time_frame = time_frame_chunk,
                                  variables = variables,
                                  category = category,
                                  lookback = lookback,
                                  window = window,
                                  stats = stats,
                                  impute = impute,
                                  output_file = output_file,
                                  log_file = log_file,
                                  check_size_only = check_size_only,
                                  filename_prefix = paste0('chunk_',
                                                           stringr::str_pad(chunk_num,
                                                                            nchar(n_chunks),
                                                                            pad = '0'),
                                                           '_'))
    }
  }
}



#' Function to add baseline predictors
#' Offset of hours(1) would mean that everything would be anchored to 1 hour
#' before fixed_start.
#' @export
gpm_add_baseline_predictors = function(time_frame = NULL,
                                       variables = NULL,
                                       category = NULL,
                                       lookback = lubridate::hours(48),
                                       window = lookback,
                                       offset = lubridate::hours(0),
                                       stats = c(mean = mean,
                                                 min = min,
                                                 max = max),
                                       impute = TRUE,
                                       output_file = TRUE,
                                       log_file = TRUE,
                                       check_size_only = FALSE,
                                       last_chunk_completed = NULL) {

  if (is.null(time_frame$chunk_size)) {
    gpm_add_predictors_internal(time_frame = time_frame,
                                variables = variables,
                                category = category,
                                lookback = lookback,
                                window = window,
                                stats = stats,
                                impute = impute,
                                output_file = output_file,
                                log_file = log_file,
                                check_size_only = check_size_only,
                                baseline = TRUE,
                                offset = offset)
  } else {
    assertthat::assert_that(time_frame$chunk_size > 0)

    # Make chunks based on temporal data, not fixed data
    unique_temporal_ids = sort(unique(time_frame$temporal_data[[time_frame$temporal_id]]))
    chunk_ids = ceiling(seq_len(length(unique_temporal_ids)) / time_frame$chunk_size)
    unique_chunks = unique(chunk_ids)
    n_chunks = max(unique_chunks)

    for (chunk_num in unique_chunks) {
      if (!is.null(last_chunk_completed) && chunk_num <= last_chunk_completed) {
        message(paste0('Skipping chunk # ', chunk_num, ' out of ', n_chunks, '...'))
        if (log_file) {
          write(paste0(Sys.time(), ': Skipping chunk # ', chunk_num, ' out of ', n_chunks, '...'),
                file.path(time_frame$output_folder, 'gpm_log.txt'), append = TRUE)
        }
        next
      }

      message(paste0('Processing chunk # ', chunk_num, ' out of ', n_chunks, '...'))
      if (log_file) {
        write(paste0(Sys.time(), ': Processing chunk # ', chunk_num, ' out of ', n_chunks, '...'),
              file.path(time_frame$output_folder, 'gpm_log.txt'), append = TRUE)
      }

      time_frame_chunk = time_frame

      time_frame_chunk$temporal_data =
        time_frame_chunk$temporal_data %>%
        dplyr::filter(!!rlang::parse_expr(time_frame_chunk$temporal_id) %in%
                        unique_temporal_ids[chunk_ids == chunk_num])

      time_frame_chunk$fixed_data =
        time_frame_chunk$fixed_data %>%
        dplyr::filter(!!rlang::parse_expr(time_frame_chunk$fixed_id) %in%
                        time_frame_chunk$temporal_data[[time_frame_chunk$temporal_id]])

      gpm_add_predictors_internal(time_frame = time_frame_chunk,
                                  variables = variables,
                                  category = category,
                                  lookback = lookback,
                                  window = window,
                                  stats = stats,
                                  impute = impute,
                                  output_file = output_file,
                                  log_file = log_file,
                                  check_size_only = check_size_only,
                                  baseline = TRUE,
                                  offset = offset,
                                  filename_prefix = paste0('chunk_',
                                                           stringr::str_pad(chunk_num,
                                                                            nchar(n_chunks),
                                                                            pad = '0'),
                                                           '_'))

    }
  }
}



#' Function to add baseline predictors
#' Offset of hours(1) would mean that everything would be anchored to 1 hour
#' before fixed_start.
#' @export
add_growing_predictors = function(time_frame = NULL,
                                       variables = NULL,
                                       category = NULL,
                                       stats = c(mean = mean,
                                                 min = min,
                                                 max = max),
                                       output_file = TRUE,
                                       log_file = TRUE,
                                       check_size_only = FALSE,
                                       last_chunk_completed = NULL) {

  if (is.null(time_frame$chunk_size)) {
    add_predictors_internal(time_frame = time_frame,
                                variables = variables,
                                category = category,
                                lookback = lubridate::hours(48), # will ignore this in _internal
                                window = lubridate::hours(48), # will ignore this in _internal
                                stats = stats,
                                impute = FALSE,
                                output_file = output_file,
                                log_file = log_file,
                                check_size_only = check_size_only,
                                growing = TRUE)
  } else {
    assertthat::assert_that(time_frame$chunk_size > 0)

    # Make chunks based on temporal data, not fixed data
    unique_temporal_ids = sort(unique(time_frame$temporal_data[[time_frame$temporal_id]]))
    chunk_ids = ceiling(seq_len(length(unique_temporal_ids)) / time_frame$chunk_size)
    unique_chunks = unique(chunk_ids)
    n_chunks = max(unique_chunks)

    for (chunk_num in unique_chunks) {
      if (!is.null(last_chunk_completed) && chunk_num <= last_chunk_completed) {
        message(paste0('Skipping chunk # ', chunk_num, ' out of ', n_chunks, '...'))
        if (log_file) {
          write(paste0(Sys.time(), ': Skipping chunk # ', chunk_num, ' out of ', n_chunks, '...'),
                file.path(time_frame$output_folder, 'gpm_log.txt'), append = TRUE)
        }
        next
      }

      message(paste0('Processing chunk # ', chunk_num, ' out of ', n_chunks, '...'))
      if (log_file) {
        write(paste0(Sys.time(), ': Processing chunk # ', chunk_num, ' out of ', n_chunks, '...'),
              file.path(time_frame$output_folder, 'gpm_log.txt'), append = TRUE)
      }

      time_frame_chunk = time_frame

      time_frame_chunk$temporal_data =
        time_frame_chunk$temporal_data %>%
        dplyr::filter(!!rlang::parse_expr(time_frame_chunk$temporal_id) %in%
                        unique_temporal_ids[chunk_ids == chunk_num])

      time_frame_chunk$fixed_data =
        time_frame_chunk$fixed_data %>%
        dplyr::filter(!!rlang::parse_expr(time_frame_chunk$fixed_id) %in%
                        time_frame_chunk$temporal_data[[time_frame_chunk$temporal_id]])

      add_predictors_internal(time_frame = time_frame_chunk,
                                  variables = variables,
                                  category = category,
                                  lookback = lubridate::hours(48), # will ignore this in _internal
                                  window = lubridate::hours(48), # will ignore this in _internal
                                  stats = stats,
                                  impute = FALSE,
                                  output_file = output_file,
                                  log_file = log_file,
                                  check_size_only = check_size_only,
                                  growing = TRUE,
                                  filename_prefix = paste0('chunk_',
                                                           stringr::str_pad(chunk_num,
                                                                            nchar(n_chunks),
                                                                            pad = '0'),
                                                           '_'))

    }
  }
}


# Other functions to add:
# gpm_add_final_outcomes() # similar to gpm_add_baseline_predictors() but occurs after the final outcome
# gpm_add_shrinking_outcomes() # cumulatively shrinking window
# for outcomes, will need to respect max_length

