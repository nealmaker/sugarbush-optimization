load("results/psc-marteloscope-maple-sim-2025.rda")
library("dplyr")
library("ggplot2")

map_inx <- sim$trees$spp %in% c("hard maple", "red maple")
plots <- nrow(sim$plots)
ptemp <- sim$trees |> group_by(plot) |> summarize(regen_yr = max(cutyr))
pyr <- sim$trees |> group_by(plot, year) |> summarize(entry = any(cutyr == year))
sim$trees <- left_join(sim$trees, ptemp)
sim$trees <- left_join(sim$trees, pyr, by = c("plot", "year"))

# Patterns to which trees cut at what sizes? ###################################
sim$trees[!map_inx,] |> 
  group_by(cutyr) |> 
  summarize(n = sum(tpa) / nrow(sim$plot))
# non-maples cut first or second (to reduce logging costs) entry

sim$trees[sim$trees$spp == "red maple",] |>
  filter(year == cutyr) |> ggplot(aes(dbh)) + geom_histogram()
# overall, red maples cut at different sizes

sim$trees[sim$trees$spp == "hard maple",] |>
  filter(year == cutyr) |> ggplot(aes(dbh)) + geom_histogram()
# sugar maples tend to be cut later than red maples

# Patterns to trees cut to regenerate plots? ###################################
sim$trees[sim$trees$spp == "red maple",] |>
  filter(year == regen_yr) |> ggplot(aes(dbh)) + geom_histogram()
# Red maples cut to regenerate plot at ~ 46-48" dbh 
# from 3 plots where a single red maple was all that was left yrs 150 - 180
#(two crappy plots w/ little red maples also regenerated yr 0 b/c 
# dominated by non-maples)

sim$trees[sim$trees$spp == "hard maple",] |>
  filter(year == regen_yr) |> ggplot(aes(dbh)) + geom_histogram()
# Sugar maples cut to regenerate plot at 41-44" dbh 
# from 26 plots where a single sugar maple was all that was left yrs 165 - 210
# & one plot where 2 sugar maples were left at year 
#(one crappy plot w/ little sugar maple also regenerated yr 0 b/c 
# dominated by non-maples)

sim$trees[sim$trees$spp == "red maple",] |>
  filter(year == regen_yr) |> ggplot(aes(cr)) + geom_histogram()
# 100 % live crowns on all red maple grown to maturity

sim$trees[sim$trees$spp == "hard maple",] |>
  filter(year == regen_yr) |> ggplot(aes(cr)) + geom_histogram()
# 100 % live crowns on all sugar maple grown to maturity


# Patterns to trees cut to thin plots? #########################################
View(sim$trees[map_inx,] |> filter(entry, year != regen_yr) |> 
  group_by(plot, year, spp) |> 
  summarise(cut = sum(cutyr == year),
            kept = sum(cutyr > year),
            cr_cut = mean(cr[cutyr == year]),
            cr_kept = mean(cr[cutyr > year]),
            dbh_cut = mean(dbh[cutyr == year]),
            dbh_kept = mean(dbh[cutyr > year])) |> 
    arrange(plot, year))
# keep larger trees with larger crown ratios
# when crown ratios are all high (above ~ 60%) prioritize keeping larger dbh trees
# when there are crown ratios < 60%, prioritize keeping trees with larger cr
# may thin out larger, more vigorous trees later in period if they have higher timber values
# remove red maples over sugar maples, unless they're a lot bigger




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
