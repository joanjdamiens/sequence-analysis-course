###############################################################################
# LESSON 3 GUIDED LAB
# FROM DISTANCES TO TRAJECTORY TYPES
#
# Main question:
# "Can we summarise many individual trajectories into a small number of
# useful and interpretable groups?"
#
# IMPORTANT:
# Clusters are analytical summaries, not discovered natural species of people.
#
###############################################################################


# ============================================================================
# 0. PACKAGES AND SEQUENCE OBJECT
# ============================================================================

# install.packages(c("TraMineR", "WeightedCluster", "dplyr", "tidyr", "ggplot2"))

library(TraMineR)
library(WeightedCluster)
library(dplyr)
library(tidyr)
library(ggplot2)

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

emp_seq <- seqdef(
  wide[, emp_cols],
  alphabet = employment_alphabet,
  states = c("EDU", "FT", "PT", "UN", "IC"),
  labels = c(
    "Education",
    "Full-time employment",
    "Part-time employment",
    "Unemployment",
    "Inactive/care"
  ),
  id = wide$id,
  cnames = 18:30,
  xtstep = 2,
  tick.last = TRUE
)


# ============================================================================
# 1. LOAD OR RECOMPUTE THE DISTANCE MATRIX
# ============================================================================

if (file.exists("outputs/dist_om_constant.rds")) {
  
  dist_om <- readRDS("outputs/dist_om_constant.rds")
  
} else {
  
  sm_constant <- seqsubm(
    emp_seq,
    method = "CONSTANT",
    cval = 2
  )
  
  dist_om <- seqdist(
    emp_seq,
    method = "OM",
    sm = sm_constant,
    indel = 1,
    norm = "none",
    full.matrix = TRUE
  )
}


# ============================================================================
# 2. A CLUSTERING METHOD NEEDS A "dist" OBJECT
# ============================================================================

dist_for_clustering <- as.dist(dist_om)

class(dist_for_clustering)



# ============================================================================
# 3. COMPARE LINKAGE METHODS
# ============================================================================

tree_single <- hclust(
  dist_for_clustering,
  method = "single"
)

tree_complete <- hclust(
  dist_for_clustering,
  method = "complete"
)

tree_average <- hclust(
  dist_for_clustering,
  method = "average"
)

tree_ward <- hclust(
  dist_for_clustering,
  method = "ward.D2"
)



# ============================================================================
# 4. LOOK AT THE DENDROGRAMS
# ============================================================================

par(mfrow = c(2, 2))

plot(
  tree_single,
  labels = FALSE,
  hang = -1,
  main = "Single linkage"
)

plot(
  tree_complete,
  labels = FALSE,
  hang = -1,
  main = "Complete linkage"
)

plot(
  tree_average,
  labels = FALSE,
  hang = -1,
  main = "Average linkage"
)

plot(
  tree_ward,
  labels = FALSE,
  hang = -1,
  main = "Ward.D2"
)

par(mfrow = c(1, 1))



# ============================================================================
# 5. SAME k, DIFFERENT LINKAGE
# ============================================================================

cl4_single <- cutree(tree_single, k = 4)
cl4_complete <- cutree(tree_complete, k = 4)
cl4_average <- cutree(tree_average, k = 4)
cl4_ward <- cutree(tree_ward, k = 4)


table(cl4_single)
table(cl4_complete)
table(cl4_average)
table(cl4_ward)



# ============================================================================
# 6. WARD AS THE COMMON TEACHING SOLUTION
# ============================================================================

plot(
  tree_ward,
  labels = FALSE,
  hang = -1,
  main = "Ward hierarchical clustering"
)

rect.hclust(
  tree_ward,
  k = 4
)



# ============================================================================
# 7. COMPARE DIFFERENT NUMBERS OF CLUSTERS
# ============================================================================

clusters_2 <- cutree(tree_ward, k = 2)
clusters_3 <- cutree(tree_ward, k = 3)
clusters_4 <- cutree(tree_ward, k = 4)
clusters_5 <- cutree(tree_ward, k = 5)
clusters_6 <- cutree(tree_ward, k = 6)

table(clusters_3)
table(clusters_4)
table(clusters_5)
table(clusters_6)



# ============================================================================
# 8. CLUSTER QUALITY: ASW, CH, HUBERT'S GAMMA
# ============================================================================

quality_for_k <- function(k) {
  
  membership <- factor(
    cutree(tree_ward, k = k)
  )
  
  q <- wcClusterQuality(
    dist_for_clustering,
    membership
  )
  
  data.frame(
    k = k,
    ASW = unname(q$stats["ASW"]),
    CH = unname(q$stats["CH"]),
    HG = unname(q$stats["HG"])
  )
}



quality_table <- bind_rows(
  lapply(2:6, quality_for_k)
)

quality_table




# ============================================================================
# 9. WHAT DO THE QUALITY INDICATORS MEAN?
# ============================================================================



# ============================================================================
# 10. VISUALISE THE INDICATORS
# ============================================================================

quality_long <- quality_table %>%
  pivot_longer(
    cols = c(ASW, CH, HG),
    names_to = "indicator",
    values_to = "value"
  )

ggplot(
  quality_long,
  aes(x = k, y = value)
) +
  geom_line() +
  geom_point() +
  facet_wrap(
    ~ indicator,
    scales = "free_y"
  ) +
  scale_x_continuous(
    breaks = 2:6
  ) +
  labs(
    title = "Cluster quality for alternative Ward solutions",
    x = "Number of clusters",
    y = NULL
  ) +
  theme_minimal()


# ============================================================================
# 11. WHICH k "WINS" EACH STATISTIC?
# ============================================================================

quality_table[
  which.max(quality_table$ASW),
]

quality_table[
  which.max(quality_table$CH),
]

quality_table[
  which.max(quality_table$HG),
]


# ============================================================================
# 12. VISUALISE A FOUR-CLUSTER TEACHING SOLUTION
# ============================================================================

cluster4 <- factor(clusters_4)

seqdplot(
  emp_seq,
  group = cluster4,
  main = "Four-cluster solution: state distributions"
)


seqIplot(
  emp_seq,
  group = cluster4,
  sortv = seqtransn(emp_seq),
  main = "Four-cluster solution: individual trajectories"
)



# ============================================================================
# 13. NUMERICAL CLUSTER SUMMARIES
# ============================================================================

duration <- as.data.frame(
  seqistatd(emp_seq)
)

# Give transparent names independent of TraMineR display coding.
names(duration) <- c(
  "education_years",
  "full_time_years",
  "part_time_years",
  "unemployment_years",
  "inactive_care_years"
)

cluster_summary_data <- duration %>%
  mutate(
    cluster = cluster4,
    n_transitions = seqtransn(emp_seq)
  )

cluster_summary <- cluster_summary_data %>%
  group_by(cluster) %>%
  summarise(
    n = n(),
    across(
      ends_with("_years"),
      mean
    ),
    mean_transitions = mean(n_transitions),
    .groups = "drop"
  )

cluster_summary


# ============================================================================
# 14. REPRESENTATIVE SEQUENCES
# ============================================================================

seqrplot(
  emp_seq,
  group = cluster4,
  diss = dist_om,
  coverage = 0.25,
  main = "Representative sequences by cluster"
)




# ============================================================================
# 15. COMPARE k=3, 4 AND 5 SUBSTANTIVELY
# ============================================================================

seqdplot(
  emp_seq,
  group = factor(clusters_3),
  main = "Three-cluster solution"
)

seqdplot(
  emp_seq,
  group = factor(clusters_4),
  main = "Four-cluster solution"
)

seqdplot(
  emp_seq,
  group = factor(clusters_5),
  main = "Five-cluster solution"
)




# ============================================================================
# 16. SAVE THE COMMON TEACHING TYPOLOGY
# ============================================================================

dir.create("outputs", showWarnings = FALSE)

saveRDS(cluster4, "outputs/cluster4_teaching.rds")
saveRDS(tree_ward, "outputs/tree_ward.rds")

write.csv(
  data.frame(
    id = wide$id,
    cluster4 = cluster4
  ),
  "outputs/cluster4_teaching.csv",
  row.names = FALSE
)


