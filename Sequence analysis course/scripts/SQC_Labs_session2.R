###############################################################################
# LESSON 2 GUIDED LAB
# MEASURING SIMILARITY BETWEEN LIFE-COURSE TRAJECTORIES
#
# Main question:
# "What do we mean when we say that two trajectories are similar?"
#
#
###############################################################################


# ============================================================================
# 0. PACKAGES AND DATA
# ============================================================================

# install.packages(c("TraMineR", "dplyr", "tidyr", "ggplot2"))

library(TraMineR)
library(dplyr)
library(tidyr)
library(ggplot2)

stopifnot(file.exists("data/sequence_course_wide.csv"))

wide <- read.csv(
  "data/sequence_course_wide.csv",
  stringsAsFactors = FALSE
)

emp_cols <- paste0("emp_", 18:30)

employment_alphabet <- c(
  "education",
  "full_time",
  "part_time",
  "unemployed",
  "inactive_care"
)

employment_codes <- c("EDU", "FT", "PT", "UN", "IC")

employment_labels <- c(
  "Education",
  "Full-time employment",
  "Part-time employment",
  "Unemployment",
  "Inactive/care"
)

emp_seq <- seqdef(
  wide[, emp_cols],
  alphabet = employment_alphabet,
  states = employment_codes,
  labels = employment_labels,
  id = wide$id,
  cnames = 18:30,
  xtstep = 2,
  tick.last = TRUE
)




# ============================================================================
# 1. START WITH TWO VERY SIMPLE TOY TRAJECTORIES
# ============================================================================

toy_data <- data.frame(
  t1 = c("E", "E"),
  t2 = c("E", "E"),
  t3 = c("E", "U"),
  t4 = c("U", "U"),
  t5 = c("U", "E"),
  t6 = c("E", "E")
)

rownames(toy_data) <- c("Sequence_A", "Sequence_B")

toy_seq <- seqdef(
  toy_data,
  alphabet = c("E", "U"),
  states = c("E", "U"),
  labels = c("Employment", "Unemployment"),
  cnames = 1:6
)

toy_seq




# ============================================================================
# 2. HAMMING DISTANCE: EXACT POSITION-BY-POSITION COMPARISON
# ============================================================================

toy_sm_ham <- seqsubm(
  toy_seq,
  method = "CONSTANT",
  cval = 1
)

toy_sm_ham



toy_ham <- seqdist(
  toy_seq,
  method = "HAM",
  sm = toy_sm_ham,
  norm = "none"
)

toy_ham



# ============================================================================
# 3. OPTIMAL MATCHING: ALLOW EDIT OPERATIONS
# ============================================================================

toy_sm_om <- seqsubm(
  toy_seq,
  method = "CONSTANT",
  cval = 2
)


toy_om_low <- seqdist(
  toy_seq,
  method = "OM",
  sm = toy_sm_om,
  indel = 0.5,
  norm = "none"
)

toy_om_balanced <- seqdist(
  toy_seq,
  method = "OM",
  sm = toy_sm_om,
  indel = 1,
  norm = "none"
)

toy_om_high <- seqdist(
  toy_seq,
  method = "OM",
  sm = toy_sm_om,
  indel = 3,
  norm = "none"
)

toy_om_low
toy_om_balanced
toy_om_high



# ============================================================================
# 4. FULL DATA: CONSTANT-COST OPTIMAL MATCHING
# ============================================================================

sm_constant <- seqsubm(
  emp_seq,
  method = "CONSTANT",
  cval = 2
)

sm_constant



dist_om <- seqdist(
  emp_seq,
  method = "OM",
  sm = sm_constant,
  indel = 1,
  norm = "none",
  full.matrix = TRUE
)



dim(dist_om)
dist_om[1:6, 1:6]


# ============================================================================
# 5. FULL DATA: HAMMING DISTANCE
# ============================================================================

sm_hamming <- seqsubm(
  emp_seq,
  method = "CONSTANT",
  cval = 1
)

dist_ham <- seqdist(
  emp_seq,
  method = "HAM",
  sm = sm_hamming,
  norm = "none",
  full.matrix = TRUE
)

dist_ham[1:6, 1:6]



# ============================================================================
# 6. COMPARE THE STRUCTURE OF TWO DISTANCE MATRICES
# ============================================================================

om_vector <- as.vector(as.dist(dist_om))
ham_vector <- as.vector(as.dist(dist_ham))


cor(om_vector, ham_vector)



# ============================================================================
# 7. DATA-DRIVEN SUBSTITUTION COSTS: TRANSITION RATES
# ============================================================================

trate_costs <- seqcost(
  emp_seq,
  method = "TRATE"
)

trate_costs$sm
trate_costs$indel



dist_om_trate <- seqdist(
  emp_seq,
  method = "OM",
  sm = trate_costs$sm,
  indel = trate_costs$indel,
  norm = "none",
  full.matrix = TRUE
)

cor(
  as.vector(as.dist(dist_om)),
  as.vector(as.dist(dist_om_trate))
)



# ============================================================================
# 8. EXAMINE PARTICULAR PAIRS
# ============================================================================

pair_table <- data.frame(
  pair = c("1 vs 2", "1 vs 3", "1 vs 5", "3 vs 5"),
  Hamming = c(
    dist_ham[1,2],
    dist_ham[1,3],
    dist_ham[1,5],
    dist_ham[3,5]
  ),
  OM_constant = c(
    dist_om[1,2],
    dist_om[1,3],
    dist_om[1,5],
    dist_om[3,5]
  ),
  OM_transition_rate = c(
    dist_om_trate[1,2],
    dist_om_trate[1,3],
    dist_om_trate[1,5],
    dist_om_trate[3,5]
  )
)

pair_table

emp_seq[c(1,2,3,5), ]



# ============================================================================
# 9. OPTIONAL: NORMALISATION
# ============================================================================

dist_om_normalised <- seqdist(
  emp_seq,
  method = "OM",
  sm = sm_constant,
  indel = 1,
  norm = "maxlength",
  full.matrix = TRUE
)

summary(as.vector(as.dist(dist_om)))
summary(as.vector(as.dist(dist_om_normalised)))



# ============================================================================
# 10. SAVE DISTANCES FOR LESSON 3
# ============================================================================

dir.create("outputs", showWarnings = FALSE)

saveRDS(dist_om, "outputs/dist_om_constant.rds")
saveRDS(dist_ham, "outputs/dist_hamming.rds")
saveRDS(dist_om_trate, "outputs/dist_om_trate.rds")




