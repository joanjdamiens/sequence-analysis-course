###############################################################################
# LESSON 1 GUIDED LAB
# BUILDING AND DESCRIBING LIFE-COURSE SEQUENCES
#
# Course: Introduction to Sequence Analysis for Life-Course Research (with R)
#
# Use this script with the lab sheet document. 
# Lab sheet contains many notes and explanations; add your notes and answers to the question in that script.  
#
# The guided example uses EMPLOYMENT trajectories from age 18 to 30.
#
###############################################################################


# ============================================================================
# 0. PACKAGES
# ============================================================================

# Run these lines only if the packages are not yet installed:
# install.packages(c("TraMineR", "dplyr", "tidyr", "ggplot2"))

library(TraMineR)
library(dplyr)
library(tidyr)
library(ggplot2)


# ============================================================================
# 1. FIND THE DATA
# ============================================================================

getwd()


stopifnot(file.exists("data/sequence_course_long.csv"))
stopifnot(file.exists("data/sequence_course_person.csv"))
stopifnot(file.exists("data/sequence_course_wide.csv"))



# ============================================================================
# 2. IMPORT THE THREE TEACHING FILES
# ============================================================================

long <- read.csv(
  "data/sequence_course_long.csv",
  stringsAsFactors = FALSE
)

person <- read.csv(
  "data/sequence_course_person.csv",
  stringsAsFactors = FALSE
)

wide_backup <- read.csv(
  "data/sequence_course_wide.csv",
  stringsAsFactors = FALSE
)


# ============================================================================
# 3. FIRST LOOK AT THE DATA
# ============================================================================

dim(long)
dim(person)


head(long)
head(person)


str(long)
str(person)



# ============================================================================
# 4. CHECK THE LONGITUDINAL STRUCTURE
# ============================================================================

n_distinct(long$id)
range(long$age)

obs_per_person <- long %>%
  count(id, name = "n_observations")

table(obs_per_person$n_observations)


sum(is.na(long$employment_state))
sum(is.na(long$family_state))
sum(is.na(long$housing_state))


# ============================================================================
# 5. INSPECT THE EMPLOYMENT ALPHABET
# ============================================================================

table(long$employment_state)

sort(unique(long$employment_state))


# ============================================================================
# 6. MOVE FROM LONG TO WIDE FORMAT
# ============================================================================

employment_wide <- long %>%
  select(id, age, employment_state) %>%
  mutate(age = paste0("emp_", age)) %>%
  pivot_wider(
    names_from = age,
    values_from = employment_state
  ) %>%
  arrange(id)


head(employment_wide)
dim(employment_wide)


# ============================================================================
# 7. VERIFY THE RESHAPE
# ============================================================================

all(employment_wide$id == wide_backup$id)


all(employment_wide$emp_18 == wide_backup$emp_18)
all(employment_wide$emp_30 == wide_backup$emp_30)


# ============================================================================
# 8. DEFINE THE STATE SEQUENCE OBJECT
# ============================================================================

emp_cols <- paste0("emp_", 18:30)


employment_alphabet <- c(
  "education",
  "full_time",
  "part_time",
  "unemployed",
  "inactive_care"
)

employment_codes <- c(
  "EDU",
  "FT",
  "PT",
  "UN",
  "IC"
)

employment_labels <- c(
  "Education",
  "Full-time employment",
  "Part-time employment",
  "Unemployment",
  "Inactive/care"
)

emp_seq <- seqdef(
  data = employment_wide[, emp_cols],
  alphabet = employment_alphabet,
  states = employment_codes,
  labels = employment_labels,
  id = employment_wide$id,
  cnames = 18:30,
  xtstep = 2,
  tick.last = TRUE
)


class(emp_seq)
alphabet(emp_seq)
summary(emp_seq)


# ============================================================================
# 9. LOOK AT INDIVIDUAL SEQUENCES
# ============================================================================

emp_seq[1:8, ]



# ============================================================================
# 10. SEQUENCE INDEX PLOT
# ============================================================================

n_transitions <- seqtransn(emp_seq)

seqIplot(
  emp_seq,
  sortv = n_transitions,
  main = "Employment trajectories, ages 18-30"
)


# ============================================================================
# 11. STATE DISTRIBUTION PLOT
# ============================================================================

seqdplot(
  emp_seq,
  main = "Employment-state distribution by age"
)


# ============================================================================
# 12. COMPARE GROUPS DESCRIPTIVELY
# ============================================================================

person_for_seq <- person %>%
  arrange(id)

stopifnot(all(person_for_seq$id == employment_wide$id))

seqdplot(
  emp_seq,
  group = person_for_seq$parental_education,
  main = "Employment trajectories by parental education"
)



# ============================================================================
# 13. TIME SPENT IN EACH STATE
# ============================================================================

state_duration <- seqistatd(emp_seq)

head(state_duration)


colMeans(state_duration)



# ============================================================================
# 14. NUMBER OF TRANSITIONS
# ============================================================================

n_transitions <- seqtransn(emp_seq)

summary(n_transitions)


# ============================================================================
# 15. ENTROPY AND COMPLEXITY
# ============================================================================

entropy <- seqient(emp_seq)
complexity <- seqici(emp_seq)

summary(entropy)
summary(complexity)

cor(entropy, complexity)



# ============================================================================
# 16. BUILD A SMALL ANALYTICAL DATA FRAME
# ============================================================================

lesson1_summary <- person_for_seq %>%
  mutate(
    n_transitions = n_transitions,
    entropy = entropy,
    complexity = complexity,
    unemployment_years = rowSums(
      employment_wide[, emp_cols] == "unemployed"
    )
  )


head(lesson1_summary)

lesson1_summary %>%
  group_by(parental_education) %>%
  summarise(
    mean_unemployment = mean(unemployment_years),
    mean_transitions = mean(n_transitions),
    mean_complexity = mean(complexity),
    .groups = "drop"
  )


# ============================================================================
# 17. SAVE OBJECTS FOR LATER LESSONS
# ============================================================================

dir.create("outputs", showWarnings = FALSE)

saveRDS(emp_seq, "outputs/emp_seq.rds")
saveRDS(employment_wide, "outputs/employment_wide.rds")
saveRDS(lesson1_summary, "outputs/lesson1_summary.rds")


