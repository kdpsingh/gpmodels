#'
#' @param time_frame A time frame object
#' @param variables A vector or list of variable names
#' @param category A vector or list of category names
#' @param lookahead A lubridate instant, how far into the future to predict outcome
#' @param window A lubridate instant, break/interval to divide lookahead
#' @param stats A list of summary stats
#' @param impute Carry forward last value
#' @param output_file Logical, whether or not to output to file or console
#' @param log_file Logical, whether or not to write to log
#' @param check_size_only Logical, returns expected number of rows for predictor or predictor chunk if applicable
#' @param last_chunk_completed Integer, the previous chunk completed if chunking is used
#'
#' @export
add_rolling_outcomes = function(time_frame = NULL,
                            variables = NULL,
                            category = NULL,
                            lookahead = lubridate::hours(48),
                            window = lookahead,
                            stats = c(mean = mean,
                                      min = min,
                                      max = max),
                            impute = FALSE,
                            output_file = TRUE,
                            log_file = TRUE,
                            check_size_only = FALSE,
                            last_chunk_completed = NULL) {

  if (is.null(time_frame$chunk_size)) {
    gpm_add_predictors_internal(time_frame = time_frame,
                                variables = variables,
                                category = category,
                                lookback = -lookahead,
                                window = -window,
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
        filter(!!rlang::parse_expr(time_frame_chunk$temporal_id) %in%
                        unique_temporal_ids[chunk_ids == chunk_num])

      time_frame_chunk$fixed_data =
        time_frame_chunk$fixed_data %>%
        filter(!!rlang::parse_expr(time_frame_chunk$fixed_id) %in%
                        time_frame_chunk$temporal_data[[time_frame_chunk$temporal_id]])

      gpm_add_predictors_internal(time_frame = time_frame_chunk,
                                  variables = variables,
                                  category = category,
                                  lookback = -lookahead,
                                  window = -window,
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
