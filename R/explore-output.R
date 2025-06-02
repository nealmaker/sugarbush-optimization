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

View(sim$trees |> 
       dplyr::group_by(plot, year) |> 
       dplyr::summarize(taps = sum(taps * tpa * cumsurv), 
                        ba = sum(ba_ac * cumsurv)))

sim$trees |> 
  dplyr::group_by(plot, year) |> 
  dplyr::summarize(taps = sum(taps * tpa * cumsurv), 
                   ba = sum(ba_ac * cumsurv)) |>
  dplyr::group_by(year) |> 
  dplyr::summarize(taps = mean(taps)) |> 
  ggplot2::ggplot(ggplot2::aes(year, taps)) +
  ggplot2::geom_line()
