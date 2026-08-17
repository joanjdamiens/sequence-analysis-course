# Lesson 4 Lab Sheet — Linking Trajectories to a Substantive Question

**Use side by side with:** `guided_labs/lesson4_guided.R`


---

## 0. PACKAGES AND DATA

**Objective:** Load packages, person-level data, and sequences.

```r
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
```


---

## 1. LOAD THE COMMON FOUR-CLUSTER TEACHING SOLUTION

**Objective:** Load the common four-cluster teaching solution.

```r
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
```

We deliberately use one common k=4 solution today.
This lets everyone practise interpretation and second-stage modelling.

It does NOT imply that k=4 is the only defensible solution.


---

## 2. BUILD ONE ANALYTICAL DATA SET

**Objective:** Add cluster membership to the person-level data.

```r
person <- person %>%
  arrange(id)

wide <- wide %>%
  arrange(id)

stopifnot(all(person$id == wide$id))

analysis_data <- person %>%
  mutate(
    cluster = factor(cluster4)
  )
```

`mutate(cluster = ...)` adds the generated cluster classification to the
person-level data.


**Question:** Was `cluster` directly observed in the original data?
cluster was NOT directly observed in the original dataset.
It was constructed through:
states -> sequences -> distances -> clustering -> choice of k.


---

## 3. START DESCRIPTIVELY: HOW LARGE ARE THE CLUSTERS?

**Objective:** Count people and proportions in each cluster.

```r
table(analysis_data$cluster)

prop.table(
  table(analysis_data$cluster)
)
```

`table()` gives counts.
`prop.table()` turns those counts into proportions.

**Question:** Why check cluster sizes before modelling?
Always know how many people are represented by a trajectory type before
entering it into a regression model.


---

## 4. WHO FOLLOWS EACH TRAJECTORY?

**Objective:** Compare parental education across clusters.

```r
tab_parental_ed <- table(
  analysis_data$parental_education,
  analysis_data$cluster
)

tab_parental_ed

prop.table(
  tab_parental_ed,
  margin = 1
)
```

`margin=1` calculates row percentages.

Each row now sums to 1.

We can interpret as: "Among people in this parental-education category, what proportion follows
each trajectory?"

Try margin=2 yourself:
`prop.table(tab_parental_ed, margin=2)``

That answers a different question: "Within this cluster, what is its parental-education composition?"

**Question:** Why are those two percentages not interchangeable? In other terms, What is the difference between row and column percentages?


---

## 5. OPTIONAL DESCRIPTIVE ASSOCIATION TEST

**Objective:** Run the chi-square test only if time allows.

```r
chisq.test(tab_parental_ed)
```

chi-square test evaluates whether two categorical variables are statistically
independent in the sample.

BUT, it does NOT:
- establish causality;
- tell us which trajectory contrast matters most;
- measure substantive importance by itself.

For this course, the cross-tabulation and percentages are often more
interpretable than simply reporting a p-value. A p-value does not replace the descriptive pattern.


---

## 6. LATER OUTCOME: MENTAL HEALTH AT AGE 32

**Objective:** Compare mean mental-health scores across clusters.

```r
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
```

**IMPORTANT**:
`mental_health_score_32` is coded so that HIGHER values indicate MORE symptoms.

Before regression, what is the unadjusted substantive pattern?


---

## 7. MAKE THE DESCRIPTIVE RESULT VISIBLE

**Objective:** Plot mean mental health with confidence intervals.
```r
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
```

se = standard error of the group mean.
mean +/- 1.96*SE gives an approximate 95% confidence interval.

```r
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
```


A clear descriptive graph should usually come BEFORE a regression table.


---

## 8. CHOOSE A REFERENCE CLUSTER

**Objective:** Set a reference cluster.

```r
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
```

**Question:** What will the other cluster coefficients be compared with?

Regression with a categorical predictor needs a reference category.

`relevel()` changes which category is the baseline.

Here, we use the largest cluster simply to make the script robust.

In a real paper, after interpreting the trajectory groups, you might deliberately select
"predominantly stable employment" as the reference instead.


---

## 9. SIMPLE REGRESSION: CLUSTER AS EXPLANATORY VARIABLE

**Objective:** Run the unadjusted regression.

```r
model1 <- lm(
  mental_health_score_32 ~ cluster,
  data = analysis_data
)

summary(model1)
```

`lm()` fits an ordinary linear regression.

Formula syntax:

outcome ~ predictor

The intercept =
mean outcome in the reference cluster.

Each cluster coefficient =
estimated difference from the reference cluster.

Be careful:
A coefficient is an ASSOCIATION here, not automatically a causal effect.


---

## 10. ADJUST FOR EARLIER CHARACTERISTICS

**Objective:** Adjust for clearly earlier characteristics.

```r
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
```

**Question:** Why these covariates? Why does the timing of covariates matter?

They are defined BEFORE or at the start of the age-18-to-30 trajectory.
They can plausibly be related both to the trajectory and later health.

The goal is not "include as many variables as possible".
Adjustment should follow the substantive research question and time order.


---

## 11. COMPARE UNADJUSTED AND ADJUSTED CLUSTER COEFFICIENTS

**Objective:** Compare unadjusted and adjusted coefficients.

```
coef(model1)
coef(model2)
```

`coef()` extracts the estimated regression coefficients.

**Question:** Which trajectory contrasts become smaller after adjustment?
What could that mean?


It may indicate that some of the raw difference is associated with earlier
characteristics shared by trajectory membership and later health.
It does NOT prove a particular causal pathway.


---

## 12. A TEACHING EXAMPLE OF QUESTIONABLE "MORE ADJUSTMENT"

**Objective:** Add income at age 32 as a deliberate teaching example.

```r
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
```

This illustrates possible **post-treatment adjustment**.

Do NOT present model3 as automatically "better" than previous models.

`income_32_eur` is measured AFTER the trajectory window and may itself be
influenced by the employment trajectory.

If employment trajectory -> later income -> later health,
adjusting for later income blocks part of the pathway we may wish to study.

This is why "more adjusted" does not automatically mean "better" or "less biased".

**Question:** What research question does model3 answer compared with model2?


---

## 13. CLUSTER AS AN OUTCOME: DESCRIPTIVE VERSION

**Objective:** Treat cluster membership descriptively as an outcome.

**Earlier question**: "Which earlier characteristics are associated with following each
employment trajectory?"

```r
tab_sep <- table(
  analysis_data$childhood_sep,
  analysis_data$cluster
)

round(
  100 * prop.table(tab_sep, margin = 1),
  1
)
```

Each row is a childhood-SEP category.
Percentages show the distribution of later trajectory membership.

Trajectory types can be outcomes as well as predictors. 
Cluster membership can be something we try to EXPLAIN rather than something
we use to explain a later outcome.

A multinomial regression could extend this analysis, but the conceptual
point is the temporal direction:

earlier characteristics -> trajectory


---

## 14. CLUSTERS VERSUS NUMERICAL SEQUENCE INDICATORS

**Objective:** Create unemployment duration and number of transitions.

```r
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
```

We now represent trajectories in TWO different ways:

1. cluster = broad whole-pathway typology
2. unemployment_years + n_transitions = specific numerical dimensions

These answer different substantive questions.


**Question:** What question does each indicator answer?

---

## 15. MODEL USING SPECIFIC SEQUENCE INDICATORS

**Objective:** Use the numerical indicators in a model.

```r
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
```

Indicators test more specific mechanisms than broad clusters.

**INTERPRETATION:**

`unemployment_years` asks about ACCUMULATION/DURATION.

`n_transitions` asks about the frequency of STATE CHANGES,
conditional here on unemployment duration and other covariates.

This may offer a more direct test of a specific life-course mechanism than
broad trajectory clusters.

But it also removes information about ORDER, timing and which transitions
occurred.


---

## 16. DO NOT ASK "WHICH REPRESENTATION IS TRUE?"

**Objective:** Compare the purpose of clusters, duration, and transitions.

Better questions:

- Does my theory concern the whole pathway or a specific dimension?
- Do timing and ordering matter jointly?
- Is cumulative exposure the mechanism of interest?
- Does the typology reveal combinations that a single indicator misses?

A useful research design can use:

clusters for description / pathway discovery
+
indicators for targeted hypothesis testing


**Understand:** Choose the representation that matches the question.

---

## 17. TIME ORDER CHECK

**Objective:** Check the temporal order of the sequence window and outcome.

Our guided analysis is temporally coherent:

ages 18 ----------------------------------------- 30     age 32
|----------- employment trajectory ---------------|------ outcome

The complete exposure window precedes the later outcome.


BAD DESIGN EXAMPLE:

ages 18 ---------------- age 25 ------------------ 30
|------ trajectory -------X------------------------|
                        outcome


If we use the FULL age-18-to-30 cluster to "predict" an outcome at 25,
the predictor contains states observed AFTER the outcome.

That is conditioning on future information.


**Question:** What is wrong with ages 18–30 explaining an outcome at age 25?

---

## 18. SIMPLE SENSITIVITY IDEA

**Objective:** Think of one reasonable alternative specification.

If Lesson 3 produced another defensible cluster solution, repeat the
descriptive outcome comparison with it.

For example:

cluster5 <- factor(cutree(tree_ward, k = 5))

Then ask yourself: "Does the broad substantive conclusion persist?""

Sensitivity analysis asks whether the overall story or the substantive conclusion is robust,
not whether every individual retains the exact same cluster number.


---

## 19. SAVE A CLEAN ANALYSIS DATASET

**Objective:** Save the final analysis data and main model.

```r
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
```

Saving final analysis objects can facilitate checking and reproducibility.
But the script remains the primary record of how the results were produced


---

## 20. RECORD THE SOFTWARE ENVIRONMENT

```r
sessionInfo()
```

`sessionInfo()` reports R version, operating system and loaded package versions.

Package behaviour can change over time.
Recording the computational environment supports reproducibility.

---

## 21. END-OF-LAB QUESTIONS

Make sure you can answer:

- What are the main ways trajectory types can be used in further analysis? Three possible main roles. 
- Why should a later outcome occur after the complete sequence window?
- Why is more adjustment not automatically better?
- Why could adjusting for income at age 32 be problematic?
- When might a sequence indicator be preferable to a cluster?
- What question does unemployment duration answer? What question does number of transitions answer?
- What does a cluster coefficient in model1 mean? Why is model2 not automatically causal?

---

# MINI-PROJECT — TODAY'S CHECKPOINT

**Report section(s):** 4. Results; 5. Interpretation, sensitivity and limitations

1. Complete the comparison that answers your research question.
2. Choose your main figure.
3. Complete at least one sensitivity check.
4. Write your main finding in one sentence.
5. Write one important limitation.
6. Prepare your four-slide presentation.

**Save before leaving:** your final script, figure, sensitivity result, and presentation.
