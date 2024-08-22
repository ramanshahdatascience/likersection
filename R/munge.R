process_univariate_segmentation <- function(data, spec, seg, ind, subtitle=NULL, good_first=TRUE) {
  seg_spec <- spec[["segments"]][[seg]]
  ind_spec <- spec[["indicators"]][[ind]]

  list(summary=tabulate_univariate_segmentation(data, spec, seg, ind),
       title=paste0("[", ind, "] ", ind_spec$description),
       subtitle=subtitle,
       x_tick_labels=seg_spec$labels,
       x_axis_label=paste0("[", seg, "] ", seg_spec$description),
       colorbar_labels=ind_spec$labels[-(ind_spec$na_level)],
       good_first=good_first,
       aic=aic_of_bubble_plot(univariate_counts_by_bubble(data, spec, seg, ind)))
}

univariate_counts_by_bubble <- function(data, spec, seg, ind) {
  seg_spec <- spec[["segments"]][[seg]]
  ind_spec <- spec[["indicators"]][[ind]]

  ind_prep <- paste0(ind, "_prep")
  seg_prep <- paste0(seg, "_prep")

  bubble_response <- data[, .N, by=.(seg=get(seg_prep), ind=get(ind_prep))]
  wide_bubble_response <- dcast(bubble_response, seg ~ ind, value.var="N")

  result <- matrix(0, nrow=nrow(wide_bubble_response), ncol=length(ind_spec$labels))

  for (i in 1:length(ind_spec$labels)) {
    if (i == ind_spec$na_level) {
      result[, i] = wide_bubble_response[["NA"]]
    } else {
      result[, i] = wide_bubble_response[[as.character(i)]]
    }
  }

  result[is.na(result)] <- 0
  result
}

tabulate_univariate_segmentation <- function(data, spec, seg, ind) {
  seg_spec <- spec[["segments"]][[seg]]
  ind_spec <- spec[["indicators"]][[ind]]

  ind_prep <- paste0(ind, "_prep")
  seg_prep <- paste0(seg, "_prep")

  summary_nonblank <- data[!is.na(get(ind_prep)),
                           .(avg_nonblank=mean(get(ind_prep)), num_nonblank=.N),
                           by=.(x=get(seg_prep))]
  summary_blank <- data[is.na(get(ind_prep)), .(num_blank=.N), by=.(x=get(seg_prep))]

  result <- data.table(x=unique(c(seg_spec$output_levels,
                                  seg_spec$na_level)))
  result[summary_nonblank, avg_nonblank := avg_nonblank, on=.(x)]
  result[summary_nonblank, num_nonblank := num_nonblank, on=.(x)]
  result[summary_blank, num_blank := num_blank, on=.(x)]
  result[is.na(num_nonblank), num_nonblank := 0]
  result[is.na(num_blank), num_blank := 0]

  result
}

process_all_univariate_segmentations <- function(data, spec, ind) {
  pus <- function(seg) process_univariate_segmentation(data, spec, seg, ind)
  metadata <- lapply(names(spec$segments), pus)
  aics <- lapply(metadata, function(x) x$aic)
  metadata[order(unlist(aics))]
}

process_bivariate_segmentation <- function(data, spec, seg_x, seg_y, ind,
                                           subtitle=NULL, good_first=TRUE) {
  seg_x_spec <- spec[["segments"]][[seg_x]]
  seg_y_spec <- spec[["segments"]][[seg_y]]
  ind_spec <- spec[["indicators"]][[ind]]

  list(summary=tabulate_bivariate_segmentation(data, spec, seg_x, seg_y, ind),
       title=paste0("[", ind, "] ", ind_spec$description),
       subtitle=subtitle,
       x_tick_labels=seg_x_spec$labels,
       x_axis_label=paste0("[", seg_x, "] ", seg_x_spec$description),
       y_tick_labels=seg_y_spec$labels,
       y_axis_label=paste0("[", seg_y, "] ", seg_y_spec$description),
       colorbar_labels=ind_spec$labels[-(ind_spec$na_level)],
       good_first=good_first,
       aic=aic_of_bubble_plot(bivariate_counts_by_bubble(data, spec, seg_x, seg_y, ind)))
}

bivariate_counts_by_bubble <- function(data, spec, seg_x, seg_y, ind) {
  seg_x_spec <- spec[["segments"]][[seg_x]]
  seg_y_spec <- spec[["segments"]][[seg_y]]
  ind_spec <- spec[["indicators"]][[ind]]

  ind_prep <- paste0(ind, "_prep")
  seg_x_prep <- paste0(seg_x, "_prep")
  seg_y_prep <- paste0(seg_y, "_prep")

  bubble_response <- data[, .N, by=.(seg_x=get(seg_x_prep),
                                     seg_y=get(seg_y_prep), ind=get(ind_prep))]
  wide_bubble_response <- dcast(bubble_response, seg_x + seg_y ~ ind, value.var="N")

  result <- matrix(0, nrow=nrow(wide_bubble_response), ncol=length(ind_spec$labels))

  for (i in 1:length(ind_spec$labels)) {
    if (i == ind_spec$na_level) {
      result[, i] = wide_bubble_response[["NA"]]
    } else {
      result[, i] = wide_bubble_response[[as.character(i)]]
    }
  }

  result[is.na(result)] <- 0
  result
}

tabulate_bivariate_segmentation <- function(data, spec, seg_x, seg_y, ind) {
  seg_x_spec <- spec[["segments"]][[seg_x]]
  seg_y_spec <- spec[["segments"]][[seg_y]]
  ind_spec <- spec[["indicators"]][[ind]]

  ind_prep <- paste0(ind, "_prep")
  seg_x_prep <- paste0(seg_x, "_prep")
  seg_y_prep <- paste0(seg_y, "_prep")

  summary_nonblank <- data[!is.na(get(ind_prep)),
                           .(avg_nonblank=mean(get(ind_prep)), num_nonblank=.N),
                           by=.(x=get(seg_x_prep), y=get(seg_y_prep))]
  summary_blank <- data[is.na(get(ind_prep)),
                        .(num_blank=.N),
                        by=.(x=get(seg_x_prep), y=get(seg_y_prep))]

  result <- data.table(
    expand.grid(x=unique(c(seg_x_spec$output_levels, seg_x_spec$na_level)),
                y=unique(c(seg_y_spec$output_levels, seg_y_spec$na_level))))[
      order(x, y)]

  result[summary_nonblank, avg_nonblank := avg_nonblank, on=.(x, y)]
  result[summary_nonblank, num_nonblank := num_nonblank, on=.(x, y)]
  result[summary_blank, num_blank := num_blank, on=.(x, y)]
  result[is.na(num_nonblank), num_nonblank := 0]
  result[is.na(num_blank), num_blank := 0]

  result
}

process_all_bivariate_segmentations <- function(data, spec, ind) {
  pus <- function(seg_pair) {
    process_bivariate_segmentation(data, spec, seg_pair$x, seg_pair$y, ind)
  }

  # 2 x N-choose-2 array of all distinct pairs of segments
  pair_array <- combn(names(spec$segments), 2)
  pairs <- lapply(seq_len(ncol(pair_array)), function(col)
    list(x=pair_array[1, as.integer(col)], y=pair_array[2, as.integer(col)]))

  metadata <- lapply(pairs, pus)
  aics <- lapply(metadata, function(x) x$aic)
  metadata[order(unlist(aics))]
}

process_all_univariate_and_bivariate_segmentations <- function(data, spec, ind) {
  uni <- process_all_univariate_segmentations(data, spec, ind)
  bi <- process_all_bivariate_segmentations(data, spec, ind)
  both <- c(uni, bi)
  aics <- lapply(both, function(x) x$aic)
  both[order(unlist(aics))]
}
