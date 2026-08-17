# Lesson 3 Lab Sheet — From Distances to Trajectory Types

**Use side by side with:** `guided_labs/lesson3_guided.R`


---

## 0. PACKAGES AND SEQUENCE OBJECT

**Objective:** Load packages and recreate the sequence object.

```r
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
```

---

## 1. LOAD OR RECOMPUTE THE DISTANCE MATRIX

**Objective:** Load the OM distance matrix from Lesson 2.

```r
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
```


`if (...) { ... } else { ... }` makes the script robust.

If, in Lesson 2, you already created the distance object, read it.
Otherwise, recreate it.

OBJECT:
dist_om is the pairwise dissimilarity matrix we now want to summarise.


**Note:** Clustering starts from pairwise sequence dissimilarities.

---

## 2. A CLUSTERING METHOD NEEDS A "dist" OBJECT

**Objective:** Convert the matrix to a `dist` object.

```r
dist_for_clustering <- as.dist(dist_om)

class(dist_for_clustering)
```


`as.dist()` stores only the unique pairwise dissimilarities.

`stats::hclust()` expects an object of class "dist".

We are NOT recalculating distances here.
We are changing how the same distances are stored.


---

## 3. COMPARE LINKAGE METHODS

**Objective:** Run single, complete, average, and Ward clustering.

```r
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
```


`hclust()` performs hierarchical agglomerative clustering.

Every person begins as a separate group.
Groups are merged step by step until one group remains.

**Question:** What does a linkage method decide?

`method=` decides how the algorithm evaluates a possible merge once groups
contain more than one person.

`single`:
distance between groups = closest pair.
Can produce "chains".

`complete`:
distance between groups = most distant pair.
Tends toward compact groups and can react strongly to atypical cases.

`average`:
distance between groups = average pairwise distance.

`ward.D2`:
chooses mergers according to Ward's minimum-variance criterion as
implemented by R. Operationally, it tends to favour compact groups.


---

## 4. LOOK AT THE DENDROGRAMS

**Objective:** Compare the four dendrograms.

```r
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
```

`par(mfrow=c(2,2))` divides the graphics device into four panels.

`plot(hclust_object)` draws a dendrogram.

`labels=FALSE` suppresses 1000 unreadable individual labels.

`hang=-1` aligns the terminal leaves at the bottom.

A dendrogram contains MANY possible partitions.
Choosing k=4 does not require rerunning the hierarchical algorithm.

**Question:** Why can the same distance matrix produce different trees?

---

## 5. SAME k, DIFFERENT LINKAGE

**Objective:** Cut each tree into four clusters and compare cluster sizes.

```r
cl4_single <- cutree(tree_single, k = 4)
cl4_complete <- cutree(tree_complete, k = 4)
cl4_average <- cutree(tree_average, k = 4)
cl4_ward <- cutree(tree_ward, k = 4)
```

`cutree()` "cuts" the hierarchical tree into k groups.

OBJECT:
each cl4_* vector has one cluster number per individual.

```r
table(cl4_single)
table(cl4_complete)
table(cl4_average)
table(cl4_ward)
```

Check that:
Do some methods produce a very small cluster?
Does single linkage create a highly unbalanced solution?

**Key question:** Does changing linkage change the groups?
The distance matrix is identical in all four analyses.
Only the linkage rule changed.


---

## 6. WARD AS THE COMMON TEACHING SOLUTION

**Objective:** Focus on the Ward dendrogram.

```r
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
```

`rect.hclust()` draws boxes around the branches corresponding to a chosen
number of clusters.

**Notes:** k=4 is being used here as a TEACHING EXAMPLE.
We have NOT yet established that four is the preferred final solution.

**Question:** Why is Ward useful but not automatically correct?
Ward tends to favour relatively compact groups.

---

## 7. COMPARE DIFFERENT NUMBERS OF CLUSTERS

**Objective:** Create Ward solutions for k = 2 to 6.

```r
clusters_2 <- cutree(tree_ward, k = 2)
clusters_3 <- cutree(tree_ward, k = 3)
clusters_4 <- cutree(tree_ward, k = 4)
clusters_5 <- cutree(tree_ward, k = 5)
clusters_6 <- cutree(tree_ward, k = 6)

table(clusters_3)
table(clusters_4)
table(clusters_5)
table(clusters_6)
```

**Question:** What exactly is being split when we move from k=3 to k=4?
What is split from k=4 to k=5? What new distinction appears when one cluster is added?

This substantive question is as important as "which statistic is largest?"


---

## 8. CLUSTER QUALITY: ASW, CH, HUBERT'S GAMMA

**Objective:** Calculate ASW, CH, and Hubert's Gamma.

```r
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
```


`function(k)` creates our own small reusable function.

For a requested k:
1. cut the tree;
2. calculate cluster-quality indicators;
3. return the three indicators used in the course.

`factor()`:
tells R that cluster numbers are categories, not numerical quantities.

`unname()`:
removes duplicated internal names so the output is a clean data frame.

```r
quality_table <- bind_rows(
  lapply(2:6, quality_for_k)
)

quality_table
```

`lapply(2:6, quality_for_k)`
runs quality_for_k() once for k=2, 3, 4, 5 and 6.

`bind_rows()` stacks the five results.


---

## 9. WHAT DO THE QUALITY INDICATORS MEAN?

**Objective:** Review what each indicator means.

**ASW = Average Silhouette Width**

Intuition:
Is a person closer to their OWN cluster than to their nearest alternative?

Higher is better.
Values can range from -1 to 1.
Negative individual silhouette values suggest poor assignment.


**CH = Calinski-Harabasz pseudo-F statistic**

Intuition:
How large is between-cluster separation relative to within-cluster
heterogeneity, taking the number of groups into account?

Higher is better within the set of solutions being compared.

CAUTION:
Its classical variance interpretation is most straightforward for
Euclidean data. Treat it as one diagnostic, not an automatic oracle.


**HG = Hubert's Gamma**

Intuition:
Do pairs in DIFFERENT clusters tend to have larger original distances
than pairs in the SAME cluster?

Higher is better.


---

## 10. VISUALISE THE INDICATORS

**Objective:** Plot the indicators across k.

```r
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
```

WHY facet with `scales="free_y"`?
ASW, CH and HG are on very different numerical scales.
Free y-scales let us inspect the SHAPE of each criterion separately.


**Question:** Do all three indicators tell the same story?

---

## 11. WHICH k "WINS" EACH STATISTIC?

**Objective:** Identify the best k according to each statistic.

```r
quality_table[
  which.max(quality_table$ASW),
]

quality_table[
  which.max(quality_table$CH),
]

quality_table[
  which.max(quality_table$HG),
]
```

`which.max()` returns the position of the largest value.

Do not think that:
"R says k=4, so k=4 is true."

If indicators disagree, that is useful information.
We now inspect what the competing partitions actually represent. 
The statistics support the decision; they do not make it for you


---

## 12. VISUALISE A FOUR-CLUSTER TEACHING SOLUTION

**Objective:** Visualise the four-cluster teaching solution.

```r
cluster4 <- factor(clusters_4)

seqdplot(
  emp_seq,
  group = cluster4,
  main = "Four-cluster solution: state distributions"
)
```

It creates one state-distribution plot per cluster.

**Question:** What is the main pattern in each cluster?

Check:
- dominant state(s)
- timing of entry into employment
- unemployment duration
- care interruptions
- continuing fragmentation

```r
seqIplot(
  emp_seq,
  group = cluster4,
  sortv = seqtransn(emp_seq),
  main = "Four-cluster solution: individual trajectories"
)
```

Distribution plots show the average structure of each group.
Index plots reveal whether the cluster is internally heterogeneous.


---

## 13. NUMERICAL CLUSTER SUMMARIES

**Objective:** Compare state durations and transitions across clusters.

```r
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

```

`across(ends_with("_years"), mean)`
applies mean() to every duration column ending in "_years".

Labels should be based on actual trajectory content, not merely on the
arbitrary cluster number.


**Question:** Do the summaries support what you saw in the plots?

---

## 14. REPRESENTATIVE SEQUENCES

**Objective:** Plot representative sequences.

```r
seqrplot(
  emp_seq,
  group = cluster4,
  diss = dist_om,
  coverage = 0.25,
  main = "Representative sequences by cluster"
)
```

`seqrplot()` selects a reduced, non-redundant set of representative observed
sequences and displays them.

`diss=dist_om`:
representativeness is evaluated using our sequence dissimilarities.

`coverage=0.25`:
ask the representative set to cover at least a specified proportion of the
sequences through their neighbourhoods.


**Important:** These are observed sequences selected to summarise each cluster.

---

## 15. COMPARE k=3, 4 AND 5 SUBSTANTIVELY

**Objective:** Compare k = 3, 4, and 5 visually.

```r
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
```


Complete this sentence for each move:

"Moving from 3 to 4 clusters adds the distinction between ______ and ______."

"Moving from 4 to 5 clusters adds the distinction between ______ and ______."

Then ask this question:
Is the added distinction central to our research question?


---

## 16. SAVE THE COMMON TEACHING TYPOLOGY

**Objective:** Save the common teaching typology for Lesson 4.

```r
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
```

**Important**:
Saving k=4 does NOT mean it is the universally correct answer.
We need one common solution for Lesson 4 so everyone can practise the same
second-stage analysis.

In the MINI-PROJECT, you should choose and justify you own solution.


---

## 17. END-OF-LAB QUESTIONS

Make sure you can answer:

- What does a linkage method decide?
- How do single, complete and average linkage differ?
- What does Ward clustering try to achieve?
- What do ASW, CH, and Hubert's Gamma each tell us?
- Why is there rarely one uniquely correct number of clusters? and why is the dendrogram not itself "the final clustering"?
- Why should cluster labels remain descriptive?

---

# MINI-PROJECT — TODAY'S CHECKPOINT

**Report section(s):** 3.3. Clustering; 4.2. Trajectory typology

1. Cluster your main distance matrix.
2. Compare several values of k (usually 3–6).
3. Use quality indicators, cluster sizes, and plots together.
4. Choose and label your preferred solution.
5. Write a short justification.

**Save before leaving:** your preferred cluster variable, quality table, and main cluster figure.
