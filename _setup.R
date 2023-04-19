# MODIFY IF NEEDED: ---------------------------------------------------------------------------
esti_dir <- file.path("E:", "estimands_in_ml_files")
#esti_dir <- "//gaia/fme/home/ralpers/public/estimands_in_ml_files"


# DO NOT MODIFY: ------------------------------------------------------------------------------
dir.create(esti_dir, showWarnings = FALSE)

lit_dir <- file.path(esti_dir, "literature_review")
dir.create(lit_dir, showWarnings = FALSE)

sim_dir <- file.path(esti_dir, "simulation_study")
dir.create(sim_dir, showWarnings = FALSE)

rwd_dir <- file.path(esti_dir, "real_world_data")
dir.create(rwd_dir, showWarnings = FALSE)
dir.create(file.path(rwd_dir, "data_raw"), showWarnings = FALSE)
dir.create(file.path(rwd_dir, "data"), showWarnings = FALSE)
# dir.create(file.path(rwd_dir, "splits"), showWarnings = FALSE) 
# TODO: remove

#dir.create(file.path(rwd_dir, "sim"), showWarnings = FALSE)
#dir.create(file.path(rwd_dir, "sim_results"), showWarnings = FALSE)