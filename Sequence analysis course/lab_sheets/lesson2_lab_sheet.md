# Lesson 2 Lab Sheet — Measuring Similarity

**Use side by side with:** `guided_labs/lesson2_guided.R`

---

## 0. PACKAGES AND DATA

**Objective** Load packages and recreate the employment sequence object.
 ```r
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
```


This setup reproduces the sequence object from Lesson 1.
The new analytical object today will be a DISTANCE MATRIX.


---

## 1. START WITH TWO VERY SIMPLE TOY TRAJECTORIES

**Objective:** Look at the two toy trajectories before calculating a distance.

```r
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
```

The two trajectories are:

- : E E E U U E

- B: E E U U E E

They contain almost the same local pattern, shifted by one position.


**Question**: In what way are they similar, and in what way are they different? Should they be considered close or far apart?
Your answer depends on what "similarity" is supposed to mean.


---

## 2. HAMMING DISTANCE: EXACT POSITION-BY-POSITION COMPARISON

**Objective:** Calculate Hamming distance.
```r
toy_sm_ham <- seqsubm(
  toy_seq,
  method = "CONSTANT",
  cval = 1
)

toy_sm_ham
```


`seqsubm()` creates a substitution-cost matrix.

`method="CONSTANT"`:
every change from one different state to another gets the same cost.

`cval=1`:
one mismatch costs 1.

Check that:
diagonal = 0 because changing E into E costs nothing.
E <-> U = 1.

```r
toy_ham <- seqdist(
  toy_seq,
  method = "HAM",
  sm = toy_sm_ham,
  norm = "none"
)

toy_ham
```


`seqdist()` computes pairwise sequence dissimilarities.

`method="HAM"`:
compare the two states at the same position.
No insertion or deletion is allowed.

`norm="none"`:
retain the raw distance rather than dividing/rescaling it.

With substitution cost 1, this is simply the NUMBER OF POSITIONS at which the two sequences differ.

**Question**: At which positions do A and B differ? What kind of difference does Hamming emphasise?


---

## 3. OPTIMAL MATCHING: ALLOW EDIT OPERATIONS

**Objective:** Calculate Optimal Matching with several indel costs.

```r
toy_sm_om <- seqsubm(
  toy_seq,
  method = "CONSTANT",
  cval = 2
)
```


We now say that substituting E for U (or vice versa) costs 2.

```r
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
```


Optimal Matching finds the CHEAPEST series of edit operations needed to transform one sequence into the other. OM allows substitutions, insertions, and deletions.

`sm` = substitution costs.

`indel` = insertion/deletion cost.
"indel" means INsertion/DELetion.

IMPORTANT:

- Lower indel costs make it cheap to shift a pattern in time.

- High indel costs discourage shifting and make substitutions relatively more attractive.


**Note:** The DATA did not change.
The two trajectories did not change.
What changed was our DEFINITION OF SIMILARITY.

**Question**: Which cost setting says:
"I care strongly about exact timing"?
Which setting is more tolerant of a similar pattern occurring slightly earlier or later?


---

## 4. FULL DATA: CONSTANT-COST OPTIMAL MATCHING

**Objective:** Calculate the constant-cost OM matrix for the full dataset.

```r
sm_constant <- seqsubm(
  emp_seq,
  method = "CONSTANT",
  cval = 2
)

sm_constant
```

This is an intentionally transparent baseline:
every pair of different employment states is considered equally dissimilar.

Example: Full-time <-> Part-time costs exactly as much as Full-time <-> Unemployment.

Is that substantively convincing?
Maybe yes for a neutral baseline; maybe not as a final assumption.

```r
dist_om <- seqdist(
  emp_seq,
  method = "OM",
  sm = sm_constant,
  indel = 1,
  norm = "none",
  full.matrix = TRUE
)
```r

OBJECT:
`dist_om` is a 1000 x 1000 matrix.

**Question:** What does one cell in the matrix represent?
`Cell [i,j]` = dissimilarity between person i and person j.

Diagonal = 0 because every sequence is identical to itself.

Symmetric because distance(i,j) = distance(j,i).

```r
dim(dist_om)
dist_om[1:6, 1:6]
```


---

## 5. FULL DATA: HAMMING DISTANCE

**Objective:** Calculate the Hamming matrix for the full dataset.

```r
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
```

**IMPORTANT**: This uses a different definition of similarity. Here Hamming uses mismatch cost 1 whereas OM used substitution cost 2.
Do NOT interpret "OM is numerically larger/smaller" as inherently more or
less dissimilar. Cost scales are part of the definition.
Instead, compare the STRUCTURE or ranking of pairwise distances.


---

## 6. COMPARE THE STRUCTURE OF TWO DISTANCE MATRICES

**Objective:** Compare the OM and Hamming distance structures.

```r
om_vector <- as.vector(as.dist(dist_om))
ham_vector <- as.vector(as.dist(dist_ham))
```

WHAT:
`as.dist()` stores only the unique lower-triangle distances.
We do not need both [i,j] and [j,i], because they are identical.

`as.vector()` turns the dist object into an ordinary numerical vector.

```r
cor(om_vector, ham_vector)
```

`cor()` asks whether pairs judged similar by one metric also tend to be judged
similar by the other.


Check that

- A high correlation means the broad structures agree.

- A value below 1 means they do not rank every pair identically.


**Note**: The objective of a sensitivity analysis is not to have identical results, but to see if conclusions are robust and what assumptions are responsible for what differences. 


---

## 7. DATA-DRIVEN SUBSTITUTION COSTS: TRANSITION RATES

**Objective:** Calculate OM using transition-rate substitution costs.

```r
trate_costs <- seqcost(
  emp_seq,
  method = "TRATE"
)

trate_costs$sm
trate_costs$indel
```

`seqcost()` returns BOTH:

- a substitution matrix: `$sm`

- a suggested insertion/deletion cost: `$indel`

`method="TRATE"`:
substitution costs are derived from observed transitions between states.

**General idea**:
states with frequent transitions between them are treated as more similar
than states between which transitions are rare.

**Note**:
"Data-driven" does NOT mean "theory-free".
We are choosing to define similarity using observed transition behaviour.

```r
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
```

**Question**: If the correlation is high but not perfect, what does that tell us?

---

## 8. EXAMINE PARTICULAR PAIRS

**Objective:** Inspect a few pairs of sequences and their distances.

```r
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
```
A distance number is easier to understand when we return to the trajectories. 

**Question**: Can you explain why two methods judge one pair differently? More precisely, why one method considers a pair relatively closer than another method does? 

---

## 9. OPTIONAL: NORMALISATION

**Objective:** Run this section only if time allows.

```r
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
```

`norm="maxlength"` rescales distance relative to sequence length.

Normalisation rescales distances. Normalisation becomes particularly relevant when sequences differ in length.

In this dataset, our sequences all have the same length, so the substantive impact
here is limited. It is still important to know that normalisation is a
methodological choice that should be reported.


---

## 10. SAVE DISTANCES FOR LESSON 3

**Objective:** Save the distance matrices for Lesson 3.

```r
dir.create("outputs", showWarnings = FALSE)

saveRDS(dist_om, "outputs/dist_om_constant.rds")
saveRDS(dist_ham, "outputs/dist_hamming.rds")
saveRDS(dist_om_trate, "outputs/dist_om_trate.rds")
```
These matrices will become the input to clustering in Lesson 3.

---

## 11. END-OF-LAB QUESTIONS

Make sure you can answer:

- What does one cell in a distance matrix represent?

- What does Hamming distance emphasise? what does it privilege?

- What does Optimal Matching allow that Hamming does not?

- What do substitution and indel costs mean? Why does a low indel cost make timing shifts less expensive?

- Why should the distance measure follow the research question?

---

# MINI-PROJECT — TODAY'S CHECKPOINT

**Report section(s):** 3.2. Dissimilarity measure

1. Calculate one main distance specification.

2. Calculate one reasonable alternative.

3. Choose the one that best fits your research question.

4. Write 3–5 sentences justifying the choice.


**Save before leaving:** your main and alternative distance objects.
