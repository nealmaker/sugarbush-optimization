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


# Patterns to stocking? ########################################################
sim$trees |> group_by(plot, year) |> 
  summarize(ba = sum(ba_ac * cumsurv), tpa = sum(tpa * cumsurv)) |> 
  full_join(data.frame(plot = sim$plots$plot)) |> 
  mutate(ba = if_else(is.na(ba), 0, ba),
         tpa = if_else(is.na(tpa), 0, tpa)) |> 
  # group_by(year) |> 
  # summarize(ba = mean(ba)) |> 
  ggplot(aes(year, ba)) + geom_line() + facet_wrap(~plot)
# at each entry, thin any neigborhood that got over 200 sq ft / ac
# & remove ~ 25-35 % of the stocking


# Patterns to regeneration? ####################################################
ptemp |> group_by(regen_yr) |> 
  summarize(pct_regenerated = n() / nrow(ptemp))
# regenerating 15% of stand right away
# then another 10% after about 160 years
# the remaining 75% regenerated between 180 and 210 years

View(sim$trees |> filter(year == 0) |> group_by(plot) |> 
  summarize(tpa_hm = sum(tpa[spp == "hard maple"]),
            tpa_rm = sum(tpa[spp == "red maple"]),
            tpa_map = sum(tpa[spp %in% c("hard maple", "red maple")]),
            regen_year = regen_yr[1]) |> 
  arrange(regen_year))
# regenerate any neighborhood w/ insuficient maple production
# (< 40 maple stems / ac when they're poles or small sawtimber; 
# fewer stems is okay if they're bigger)
# cut red maple crop trees around 46-48" dbh
# cut hard maple crop trees around 41-44" dbh (but longer growing than red)

View(sim$trees |> filter(year == regen_yr) |> group_by(plot) |> 
       summarize(tpa_hm = sum(tpa[spp == "hard maple"] * cumsurv[spp == "hard maple"]),
                 tpa_rm = sum(tpa[spp == "red maple"] * cumsurv[spp == "red maple"]),
                 tpa_map = sum(tpa[spp %in% c("hard maple", "red maple")] * 
                                 cumsurv[spp %in% c("hard maple", "red maple")]),
                 regen_year = regen_yr[1]) |> 
       arrange(regen_year))

sim$trees |> filter(year == regen_yr, regen_yr > 100) |> group_by(plot) |> 
  summarize(tpa_hm = sum(tpa[spp == "hard maple"] * cumsurv[spp == "hard maple"]),
            tpa_rm = sum(tpa[spp == "red maple"] * cumsurv[spp == "red maple"]),
            tpa_map = sum(tpa[spp %in% c("hard maple", "red maple")] * 
                            cumsurv[spp %in% c("hard maple", "red maple")]),
            regen_year = regen_yr[1]) |> 
  arrange(regen_year) |> 
  ggplot(aes(regen_year, tpa_map)) + geom_point()

sim$trees |> group_by(plot, year) |> 
  summarize(ba = sum(ba_ac * cumsurv), tpa = sum(tpa * cumsurv), regen_yr = regen_yr[1]) |> 
  full_join(data.frame(plot = sim$plots$plot)) |> 
  mutate(ba = if_else(is.na(ba), 0, ba),
         tpa = if_else(is.na(tpa), 0, tpa)) |> 
  # group_by(year) |> 
  # summarize(ba = mean(ba)) |> 
  ggplot(aes(year, tpa)) + geom_line() + 
  facet_wrap(~plot) +
  scale_y_continuous(limits = c(0, 50))


# evolution of tap density #####################################################
sim$trees |> 
  dplyr::group_by(plot, year) |> 
  dplyr::summarize(taps = sum(taps * tpa * cumsurv), 
                   ba = sum(ba_ac * cumsurv)) |>
  dplyr::group_by(year) |> 
  dplyr::summarize(taps = mean(taps)) |> 
  ggplot2::ggplot(ggplot2::aes(year, taps)) +
  ggplot2::geom_line()


# what does first cut look like ################################################
firstcut <- sim$trees |> filter(year == 0) |> mutate(cut0 = cutyr == 0) |> 
  group_by(plot) |> 
  summarize(ba_kept = sum(ba_ac[!cut0] * cumsurv[!cut0]),
            ba_cut = sum(ba_ac[cut0] * cumsurv[cut0]),
            tpa_kept = sum(tpa[!cut0] * cumsurv[!cut0]),
            tpa_cut = sum(tpa[cut0] * cumsurv[cut0])) |> 
  mutate(pct_cut = 100 * ba_cut / (ba_cut + ba_kept))
summary(firstcut)
# 40% ba removed overall
# 15% of area regenerated; 61% thinned, 24% left alone

spp_first <- sim$trees |> filter(year == 0) |> mutate(cut0 = cutyr == 0) |> 
  group_by(spp) |> 
  summarise(ba = sum(ba_ac) / 34,
            ba_cut = sum(ba_ac[cut0]) / 34) |>
  mutate(pct_ba_cut = 100 * ba_cut / ba) |> 
  arrange(desc(ba))
spp_first$pct_total <- 100 * spp_first$ba / sum(spp_first$ba)
# basically everything driven by removing non-maples