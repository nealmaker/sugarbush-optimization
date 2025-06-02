source("R/psc-marteloscope-data.R")

params <- forestgrower::params_default
params$drate <- .05
params$endyr <- 210 # must be multiple of 15 b/c 15 year cutting cycle
params$lev <- 4400 # from maple lev analysis in 'bare-land-values' project

models <- treemodeler::initialize_models("5.3", c("slim"), c("taper", "growth"))

# try optimizing one plot to test ----------------------------------------------
p1 <- "6e"
d1 <- dat
d1$trees <- d1$trees[which(d1$trees$plot == p1), ]
d1$plots <- d1$plots |> dplyr::filter(plot %in% p1)

opt <- forestmaker::opt_mngmt(d1, params, "maple", models)
View(opt$trees |> dplyr::select(spp, dbh, cr, height, bal, cutyr))
# ------------------------------------------------------------------------------

# scale up to 8 plots ----------------------------------------------------------
p1 <- dat$plots$plot[8:15]
d1 <- dat
d1$trees <- d1$trees[which(d1$trees$plot %in% p1), ]
d1$plots <- d1$plots |> dplyr::filter(plot %in% p1)

opt <- forestmaker::opt_mngmt(d1, params, "maple", models)
View(opt$trees |> dplyr::select(plot, spp, dbh, cr, bal, cutyr))
# ------------------------------------------------------------------------------

# the whole enchilada ----------------------------------------------------------
opt <- forestmaker::opt_mngmt(dat, params, "maple", models)
View(opt$trees |> dplyr::select(spp, dbh, cr, height, bal, cutyr))
# ------------------------------------------------------------------------------

sim <- forestgrower::simulation(opt$trees$cutyr, opt$trees, opt$plots, 
                                opt$params, models, maple = T)
