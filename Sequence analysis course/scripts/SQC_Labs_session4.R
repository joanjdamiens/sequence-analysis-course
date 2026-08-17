###############################################################################
# LESSON 4 GUIDED LAB
# LINKING TRAJECTORIES TO DETERMINANTS AND LATER OUTCOMES
#
# Main question:
# "Once we have constructed trajectory types, what can we responsibly do
# with them?"
#
# Guided example:
# employment trajectory clusters, ages 18-30
# -> mental-health symptom score at age 32
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

wide <- read.csv(
  "data/sequence_course_wide.csv",
  stringsAsFactors = FALSE
)

person <- read.csv(
  "data/sequence_course_person.csv",
  stringsAsFactors = FALSE
)

emp_cols <- paste0("emp_", 18:30)

emp_seq <- seqdef(
  wide[, emp_cols],
  alphabet = c(
    "education",
    "full_time",
    "part_time",
    "unemployed",
    "inactive_care"
  ),
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
# 1. LOAD THE COMMON FOUR-CLUSTER TEACHING SOLUTION
# ============================================================================

if (file.exists("outputs/cluster4_teaching.rds")) {
  
  cluster4 <- readRDS(
    "outputs/cluster4_teaching.rds"
  )
  
} else {
  
  # Fallback: recreate the Lesson 2 distance and Lesson 3 clustering.
  
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
  
  tree_ward <- hclust(
    as.dist(dist_om),
    method = "ward.D2"
  )
  
  cluster4 <- factor(
    cutree(tree_ward, k = 4)
  )
}



# ============================================================================
# 2. BUILD ONE ANALYTICAL DATA SET
# ============================================================================

person <- person %>%
  arrange(id)

wide <- wide %>%
  arrange(id)

stopifnot(all(person$id == wide$id))

analysis_data <- person %>%
  mutate(
    cluster = factor(cluster4)
  )



# ============================================================================
# 3. START DESCRIPTIVELY: HOW LARGE ARE THE CLUSTERS?
# ============================================================================

table(analysis_data$cluster)

prop.table(
  table(analysis_data$cluster)
)



# ============================================================================
# 4. WHO FOLLOWS EACH TRAJECTORY?
# ============================================================================

tab_parental_ed <- table(
  analysis_data$parental_education,
  analysis_data$cluster
)

tab_parental_ed

prop.table(
  tab_parental_ed,
  margin = 1
)



# ============================================================================
# 5. OPTIONAL DESCRIPTIVE ASSOCIATION TEST
# ============================================================================

chisq.test(tab_parental_ed)



# ============================================================================
# 6. LATER OUTCOME: MENTAL HEALTH AT AGE 32
# ============================================================================

analysis_data %>%
  group_by(cluster) %>%
  summarise(
    n = n(),
    mean_mental_health = mean(
      mental_health_score_32
    ),
    sd_mental_health = sd(
      mental_health_score_32
    ),
    .groups = "drop"
  )




# ============================================================================
# 7. MAKE THE DESCRIPTIVE RESULT VISIBLE
# ============================================================================

mh_by_cluster <- analysis_data %>%
  group_by(cluster) %>%
  summarise(
    n = n(),
    mean = mean(mental_health_score_32),
    sd = sd(mental_health_score_32),
    se = sd / sqrt(n),
    lower = mean - 1.96 * se,
    upper = mean + 1.96 * se,
    .groups = "drop"
  )



ggplot(
  mh_by_cluster,
  aes(x = cluster, y = mean)
) +
  geom_point() +
  geom_errorbar(
    aes(
      ymin = lower,
      ymax = upper
    ),
    width = 0.1
  ) +
  labs(
    title = "Mental-health symptoms at age 32 by employment trajectory",
    x = "Trajectory cluster",
    y = "Mean symptom score (higher = worse)"
  ) +
  theme_minimal()




# ============================================================================
# 8. CHOOSE A REFERENCE CLUSTER
# ============================================================================

cluster_sizes <- table(
  analysis_data$cluster
)

largest_cluster <- names(
  which.max(cluster_sizes)
)

largest_cluster

analysis_data$cluster <- relevel(
  analysis_data$cluster,
  ref = largest_cluster
)



# ============================================================================
# 9. SIMPLE REGRESSION: CLUSTER AS EXPLANATORY VARIABLE
# ============================================================================

model1 <- lm(
  mental_health_score_32 ~ cluster,
  data = analysis_data
)

summary(model1)



# ============================================================================
# 10. ADJUST FOR EARLIER CHARACTERISTICS
# ============================================================================

model2 <- lm(
  mental_health_score_32 ~
    cluster +
    sex +
    parental_education +
    childhood_sep +
    baseline_health_18,
  data = analysis_data
)

summary(model2)



# ============================================================================
# 11. COMPARE UNADJUSTED AND ADJUSTED CLUSTER COEFFICIENTS
# ============================================================================

coef(model1)
coef(model2)


# ============================================================================
# 12. A TEACHING EXAMPLE OF QUESTIONABLE "MORE ADJUSTMENT"
# ============================================================================

model3 <- lm(
  mental_health_score_32 ~
    cluster +
    sex +
    parental_education +
    childhood_sep +
    baseline_health_18 +
    income_32_eur,
  data = analysis_data
)

summary(model3)



# ============================================================================
# 13. CLUSTER AS AN OUTCOME: DESCRIPTIVE VERSION
# ============================================================================

# Earlier question:
# "Which earlier characteristics are associated with following each
# employment trajectory?"

tab_sep <- table(
  analysis_data$childhood_sep,
  analysis_data$cluster
)

round(
  100 * prop.table(tab_sep, margin = 1),
  1
)



# ============================================================================
# 14. CLUSTERS VERSUS NUMERICAL SEQUENCE INDICATORS
# ============================================================================

analysis_data$unemployment_years <- rowSums(
  wide[, emp_cols] == "unemployed"
)

analysis_data$n_transitions <- seqtransn(
  emp_seq
)

head(
  analysis_data[
    ,
    c(
      "cluster",
      "unemployment_years",
      "n_transitions",
      "mental_health_score_32"
    )
  ]
)



# ============================================================================
# 15. MODEL USING SPECIFIC SEQUENCE INDICATORS
# ============================================================================

model_indicators <- lm(
  mental_health_score_32 ~
    unemployment_years +
    n_transitions +
    sex +
    parental_education +
    childhood_sep +
    baseline_health_18,
  data = analysis_data
)

summary(model_indicators)



# ============================================================================
# 16. DO NOT ASK "WHICH REPRESENTATION IS TRUE?"
# ============================================================================



# ============================================================================
# 17. TIME ORDER CHECK
# ============================================================================



# ============================================================================
# 18. SIMPLE SENSITIVITY IDEA
# ============================================================================



# ============================================================================
# 19. SAVE A CLEAN ANALYSIS DATASET
# ============================================================================

dir.create("outputs", showWarnings = FALSE)

write.csv(
  analysis_data,
  "outputs/lesson4_analysis_data.csv",
  row.names = FALSE
)

saveRDS(
  model2,
  "outputs/lesson4_main_model.rds"
)



# ============================================================================
# 20. RECORD THE SOFTWARE ENVIRONMENT
# ============================================================================

sessionInfo()


