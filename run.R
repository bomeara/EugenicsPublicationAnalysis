source("_targets.R")
#tar_invalidate(everything())
tar_make()

#tar_load_everything()

#save(list=ls(), file="outputs/loaded.RData")
