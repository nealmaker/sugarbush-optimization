  sim$trees |> dplyr::filter(year %% (2 * sim$params$steplength) == 0) |> 
    dplyr::group_by(year, plot) |> 
    dplyr::summarize(ba = sum(ba_ac * cumsurv)) |> 
    ggplot2::ggplot(ggplot2::aes(ba)) +
    ggplot2::geom_histogram() +
    ggplot2::facet_wrap(~ year)
  
opt$trees |> dplyr::group_by(plot) |> 
  dplyr::summarize(regen_yr = max(cutyr)) |> 
  dplyr::group_by(regen_yr) |> 
  dplyr::summarise(pct = 100 * dplyr::n() / nrow(opt$plots))

