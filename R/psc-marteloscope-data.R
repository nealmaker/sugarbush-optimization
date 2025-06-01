# data collected at Paul Smiths College in 2025 for capstone seminar
# 1.7 acre marteloscope divided into 17, 1/10 ac plots
# seems to be all trees >= 5" DBH (except one was measured smaller)

# Neal further divided plots into east and west halves so they would be a 
# better size for forestmaker, so now 34, 1/20 ac plots
# Neal also added site class, elev, lat, lon using average PSC values
# as of May31, 2025, logs is wrong (same for all trees), but data is available
# in original dataset to construct that

trees <- read.csv("data/marteloscopenew.csv")
spp_levels <- 
  c("apple", "ash", "aspen", "basswood", "beech", "black ash", "black cherry", 
    "black willow", "butternut", "cedar", "cottonwood", "elm", "fir", 
    "gray birch", "hard maple", "hemlock", "hickory", "hophornbeam", 
    "norway spruce", "other hardwood", "other softwood", "paper birch", 
    "pin cherry", "red maple", "red oak", "red pine", "red spruce", 
    "scots pine", "shrubs", "silver maple", "striped maple", "tamarack", 
    "white oak", "white pine", "white spruce", "yellow birch")

trees <- trees |> 
  dplyr::mutate(spp = factor(spp, levels = spp_levels), 
                site_class = as.numeric(site_class),
                elev = as.numeric(elev),
                ba_ac = ba_tree * tpa,
                live = TRUE) |> 
  dplyr::group_by(plot) |> 
  dplyr::mutate(ba = sum(ba_ac),
                bal = forestgrower::bal(dbh, ba_ac)) |>
  dplyr::ungroup() |> 
  # getting empty rows for some reason, so we'll remove them
  dplyr::filter(dbh > 0)
trees$tree <- 1:nrow(trees)
trees$logs <- as.character(trees$logs)

plots <- data.frame(plot = unique(trees$plot))

dat <- list(trees = trees, plots = plots)
class(dat) <- "simcruise"

rm(trees, plots, spp_levels)
