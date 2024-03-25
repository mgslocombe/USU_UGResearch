### Load packages ----
library(randomizr)

### Create dataframe ----
bin <- rep(1:16, 4)
block <- c(rep(1,16), rep(2,16), rep(3,16), rep(4,16))
trt <- c("FBSat", "FB5", "FB15", "OBSat", "OB5", "OB15", "PSSat", "PS5", "PS15", "SCSat", "SC5", "SC15", "PRDSat", "PRD5", "PRD15", "Blank")

### Randomly assign treatments to tanks in blocks ----
set.seed(201)
Z <- block_ra(blocks = block, conditions = trt) 
#Run 10 and 11 at the same time to ensure that random assignment stays the same
table(Z, block)

### Create dataframe with newly assigned treatments ----
grhouseassign <- data.frame(block, bin, Z)

##I am changing the code