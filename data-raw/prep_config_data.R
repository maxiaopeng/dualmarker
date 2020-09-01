#color.quadrant.1 <- c(R1="#F9BEC0",R2="#CFECF2", R3="#F1F1F1", R4="#CFD7E4")
# npg
t1 <- ggsci::pal_npg(alpha=0.1)(4)
color.quadrant.1 <- t1[c(1,2,4,3)] %>% setNames(c("R1","R2","R3","R4"))
t2 <- ggsci::pal_npg()(4)
color.quadrant.2 <- t2[c(1,2,4,3)] %>% setNames(c("R1","R2","R3","R4"))

library(devtools)
use_data(color.quadrant.1, color.quadrant.2, internal = T, overwrite = T)
