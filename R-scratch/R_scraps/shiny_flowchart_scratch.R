data <- metadata

total <- nrow(data)
count_and_pct <- function(count) {
  pct <- ifelse(total > 0, round((count / total) * 100, 1), 0)
  paste0(count, " (", pct, "%)")
}


cultural_count <- sum(data[[grep("(?i)(cult).*?(barrier)(?!.*(?i)_desc)",
                                 names(data), value = TRUE, perl = TRUE)]] == 1,
                      na.rm = TRUE)
eng_learn_count <- sum(data[[grep("(?i)(english).*?(learn)(?!.*(?i)_desc)",
                                  names(data), value = TRUE, perl = TRUE)]] == 1,
                       na.rm = TRUE)
foster_count <- sum(data[[grep("(?i)(foster)(?!.*(?i)_desc)", names(data),
                               value = TRUE, perl = TRUE)]] == 1, na.rm = TRUE)

skills_def_count <- sum(data[[grep("(?i)(skills).*?(def)(?!.*(?i)_desc)",
                                   names(data), value = TRUE, perl = TRUE)]] == 1,
                        na.rm = TRUE)
low_inc_count <- sum(data[[grep("(?i)(low).*?(inc)(?!.*(?i)_desc)",
                                names(data), value = TRUE, perl = TRUE)]] == 1,
                     na.rm = TRUE)
homeless_count <- sum(data[[grep("(?i)(homeless)(?!.*(?i)_desc)",
                                 names(data), value = TRUE, perl = TRUE)]] == 1,
                      na.rm = TRUE)
tanf_count <- sum(data[[grep("(?i)(tanf.*?plan|plan.*?tanf)(?!.*(?i)_desc)",
                             names(data), value = TRUE, perl = TRUE)]] == 1,
                  na.rm = TRUE)
offender_count <- sum(data[[grep("(?i)(offender)(?!.*(?i)_desc)",
                                 names(data), value = TRUE, perl = TRUE)]] == 1,
                      na.rm = TRUE)

any_cled <- cultural_count + eng_learn_count
econ_marginalized <- low_inc_count + homeless_count + tanf_count

# flowchart_code <- paste0(
#   "graph TD;",
#   "A[Socioeconomic Barriers ", count_and_pct(any_cled + econ_marginalized + foster_count + skills_def_count + offender_count), "]",
#   "A -->|Social| B[Social ", count_and_pct(any_cled + foster_count + skills_def_count + offender_count), "]",
#   "A -->|Economic| C[Economic ", count_and_pct(econ_marginalized), "]",
#   "B -->|Cultural| D[Cultural ", count_and_pct(any_cled), "]",
#   "B -->|Circumstances| E[Circumstances ",
#   count_and_pct(foster_count + skills_def_count + offender_count), "]",
#   "D --> F[Cultural Barriers ", count_and_pct(cultural_count), "]",
#   "D --> G[English Language Learner ", count_and_pct(eng_learn_count), "]",
#   "E --> H[Foster Youth ", count_and_pct(foster_count), "]",
#   "E --> I[Offender ", count_and_pct(offender_count), "]",
#   "E --> J[Skills Deficient ", count_and_pct(skills_def_count), "]",
#   "C --> K[Low SES ", count_and_pct(low_inc_count), "]",
#   "C --> L[Homeless ", count_and_pct(homeless_count), "]",
#   "C --> M[TANF ", count_and_pct(tanf_count), "]"
# )
#
#
# DiagrammeR::grViz(flowchart_code)



# DiagrammeR::grViz("digraph {
#
# graph[layout = dot, rankdir = LR]
#
# a
# b
# c
#
# a -> b -> c
# }")
#
#
#
# library(DiagrammeR)
#
# library(DiagrammeR)
#
# flowchart_code <- "digraph TD {
#   A [label='Socioeconomic Barriers 449 (93%)']
#   A -> B [label='Social 239 (49.5%)']
#   A -> C [label='Economic 210 (43.5%)']
#   B -> D [label='Cultural 49 (10.1%)']
#   B -> E [label='Circumstances 190 (39.3%)']
#   D -> F [label='Cultural Barriers 30 (6.2%)']
#   D -> G [label='English Language Learner 19 (3.9%)']
#   E -> H [label='Foster Youth 35 (7.2%)']
#   E -> I [label='Offender 14 (2.9%)']
#   E -> J [label='Skills Deficient 141 (29.2%)']
#   C -> K [label='Low SES 193 (40%)']
#   C -> L [label='Homeless 3 (0.6%)']
#   C -> M [label='TANF 14 (2.9%)']
# }"
#
# DiagrammeR::grViz(flowchart_code)
#
#
# library(DiagrammeR)
#
# flowchart_code <- "digraph TD {
#   node [shape=rect, style=filled, fillcolor=lightblue]  # Set nodes to be rectangles
#   A [label='Socioeconomic Barriers 449 (93%)']
#   A -> B [label='Social 239 (49.5%)']
#   A -> C [label='Economic 210 (43.5%)']
#   B -> D [label='Cultural 49 (10.1%)']
#   B -> E [label='Circumstances 190 (39.3%)']
#   D -> F [label='Cultural Barriers 30 (6.2%)']
#   D -> G [label='English Language Learner 19 (3.9%)']
#   E -> H [label='Foster Youth 35 (7.2%)']
#   E -> I [label='Offender 14 (2.9%)']
#   E -> J [label='Skills Deficient 141 (29.2%)']
#   C -> K [label='Low SES 193 (40%)']
#   C -> L [label='Homeless 3 (0.6%)']
#   C -> M [label='TANF 14 (2.9%)']
# }"
#
# DiagrammeR::grViz(flowchart_code)



library(DiagrammeR)


flowchart_code <- "digraph TD {
  // Set graph layout to top-down and prevent curved edges
  graph [rankdir=TB, splines=false]  // TB (Top to Bottom) layout for decision tree

  // Define node styling (rectangular shape, Helvetica font)
  node [fontname=Helvetica, shape=rectangle, style=filled, fillcolor=lightblue, width=2]

  // Define the nodes with labels
  A [label='Socioeconomic Barriers 449 (93%)']
  B [label='Social 239 (49.5%)']
  C [label='Economic 210 (43.5%)']
  D [label='Cultural 49 (10.1%)']
  E [label='Circumstances 190 (39.3%)']
  F [label='Cultural Barriers 30 (6.2%)']
  G [label='English Language Learner 19 (3.9%)']
  H [label='Foster Youth 35 (7.2%)']
  I [label='Offender 14 (2.9%)']
  J [label='Skills Deficient 141 (29.2%)']
  K [label='Low SES 193 (40%)']
  L [label='Homeless 3 (0.6%)']
  M [label='TANF 14 (2.9%)']

  // Define edges without edge labels to avoid extra text under nodes
  A -> B;
  A -> C;
  B -> D;
  B -> E;
  D -> F;
  D -> G;
  E -> H;
  E -> I;
  E -> J;
  C -> K;
  C -> L;
  C -> M;

  // Optional: Set line style for clearer edges
  edge [style=dashed, arrowhead=none]
}"

DiagrammeR::grViz(flowchart_code)





flowchart_code <- "digraph LR {
  // Set graph layout to left-to-right and prevent curved edges
  graph [rankdir=LR, splines=false]  // LR (Left to Right) layout for flowchart

  // Define node styling (rectangular shape, Helvetica font)
  node [fontname=Helvetica, shape=rectangle, style=filled, fillcolor=lightblue, width=2]

  // Define the nodes with labels
  A [label='Socioeconomic Barriers 449 (93%)']
  B [label='Social 239 (49.5%)']
  C [label='Economic 210 (43.5%)']
  D [label='Cultural 49 (10.1%)']
  E [label='Circumstances 190 (39.3%)']
  F [label='Cultural Barriers 30 (6.2%)']
  G [label='English Language Learner 19 (3.9%)']
  H [label='Foster Youth 35 (7.2%)']
  I [label='Offender 14 (2.9%)']
  J [label='Skills Deficient 141 (29.2%)']
  K [label='Low SES 193 (40%)']
  L [label='Homeless 3 (0.6%)']
  M [label='TANF 14 (2.9%)']

  // Define edges without edge labels to avoid extra text under nodes
  A -> B;
  A -> C;
  B -> D;
  B -> E;
  D -> F;
  D -> G;
  E -> H;
  E -> I;
  E -> J;
  C -> K;
  C -> L;
  C -> M;

  // Optional: Set line style for clearer edges
  edge [style=dashed, arrowhead=none]
}"

DiagrammeR::grViz(flowchart_code)






# library(DiagrammeR)
#
# flowchart_code <- "digraph TD {
#   // Set graph layout to top-to-bottom and prevent curved edges
#   graph [rankdir=TB, splines=false]  // TB (Top to Bottom) layout for decision tree
#
#   // Define node styling (rectangular shape, Helvetica font)
#   node [fontname=Helvetica, style=filled, fillcolor=lightblue, width=2]
#
#   // Define the top-level node with larger text
#   A [label='Socioeconomic Barriers 449 (93%)', fontsize=18]
#
#   // Define the second-level nodes with smaller text
#   B [label='Social 239 (49.5%)', fontsize=16]
#   C [label='Economic 210 (43.5%)', fontsize=16]
#
#   // Define the third-level nodes with even smaller text
#   D [label='Cultural 49 (10.1%)', fontsize=14]
#   E [label='Circumstances 190 (39.3%)', fontsize=14]
#
#   // Define the fourth-level nodes with smallest text
#   F [label='Cultural Barriers 30 (6.2%)', fontsize=12]
#   G [label='English Language Learner 19 (3.9%)', fontsize=12]
#   H [label='Foster Youth 35 (7.2%)', fontsize=12]
#   I [label='Offender 14 (2.9%)', fontsize=12]
#   J [label='Skills Deficient 141 (29.2%)', fontsize=12]
#   K [label='Low SES 193 (40%)', fontsize=12]
#   L [label='Homeless 3 (0.6%)', fontsize=12]
#   M [label='TANF 14 (2.9%)', fontsize=12]
#
#   // Define edges without edge labels to avoid extra text under nodes
#   A -> B;
#   A -> C;
#   B -> D;
#   B -> E;
#   D -> F;
#   D -> G;
#   E -> H;
#   E -> I;
#   E -> J;
#   C -> K;
#   C -> L;
#   C -> M;
#
#   // Optional: Set line style for clearer edges
#   edge [style=dashed, arrowhead=none]
# }"
#
# DiagrammeR::grViz(flowchart_code)




library(DiagrammeR)

flowchart_code <- "digraph TD {
  // Set graph layout to top-to-bottom and prevent curved edges
  graph [rankdir=TB, splines=false]  // TB (Top to Bottom) layout for decision tree

  // Define node styling (rectangular shape, Helvetica font)
  node [fontname=Helvetica, shape=rectangle, style=filled, fillcolor=lightblue, width=2]

  // Define the top-level node with larger text
  A [label='Socioeconomic Barriers 449 (93%)', fontsize=18]

  // Define the second-level nodes with smaller text
  B [label='Social 239 (49.5%)', fontsize=16]
  C [label='Economic 210 (43.5%)', fontsize=16]

  // Define the third-level nodes with even smaller text
  D [label='Cultural 49 (10.1%)', fontsize=14]
  E [label='Circumstances 190 (39.3%)', fontsize=14]

  // Define the fourth-level nodes with smallest text
  F [label='Cultural Barriers 30 (6.2%)', fontsize=12]
  G [label='English Language Learner 19 (3.9%)', fontsize=12]
  H [label='Foster Youth 35 (7.2%)', fontsize=12]
  I [label='Offender 14 (2.9%)', fontsize=12]
  J [label='Skills Deficient 141 (29.2%)', fontsize=12]
  K [label='Low SES 193 (40%)', fontsize=12]
  L [label='Homeless 3 (0.6%)', fontsize=12]
  M [label='TANF 14 (2.9%)', fontsize=12]

  // Define edges without edge labels to avoid extra text under nodes
  A -> B;
  A -> C;
  B -> D;
  B -> E;
  D -> F;
  D -> G;
  E -> H;
  E -> I;
  E -> J;
  C -> K;
  C -> L;
  C -> M;

  // Optional: Set line style for clearer edges
  edge [style=dashed, arrowhead=none]
}"

DiagrammeR::grViz(flowchart_code)






flowchart_code <- "digraph TD {
  graph [rankdir=TB, splines=false]

  node [fontname=Helvetica, shape=rectangle, style=filled, fillcolor=lightblue, width=2]

  A [label='Socioeconomic Barriers 449 (93%)', fontsize=18]

  B [label='Social 239 (49.5%)', fontsize=16]
  C [label='Economic 210 (43.5%)', fontsize=16]

  D [label='Cultural 49 (10.1%)', fontsize=14]
  E [label='Circumstances 190 (39.3%)', fontsize=14]

  F [label='Cultural Barriers 30 (6.2%)', fontsize=12]
  G [label='English Language Learner 19 (3.9%)', fontsize=12]
  H [label='Foster Youth 35 (7.2%)', fontsize=12]
  I [label='Offender 14 (2.9%)', fontsize=12]
  J [label='Skills Deficient 141 (29.2%)', fontsize=12]
  K [label='Low SES 193 (40%)', fontsize=12]
  L [label='Homeless 3 (0.6%)', fontsize=12]
  M [label='TANF 14 (2.9%)', fontsize=12]

  { rank=same; K; L; M; I; J }

  A -> B;
  A -> C;
  B -> D;
  B -> E;
  D -> F;
  D -> G;
  E -> H;
  E -> I;
  E -> J;
  C -> K;
  C -> L;
  C -> M;

  edge [style=dashed, arrowhead=none]
}"

DiagrammeR::grViz(flowchart_code)



try <- 449
try_p <- 93

try2 <- 239
try2_p <- 49.5

flowchart_code <- "digraph TD {
  graph [rankdir=TB, splines=false]

  node [fontname=Helvetica, shape=rectangle, style=filled, fillcolor=lightblue, width=2]

  A [label='Socioeconomic Barriers 449 (93%)', fontsize=18]

  B [label='Social 239 (49.5%)', fontsize=16]
  C [label='Economic 210 (43.5%)', fontsize=16]

  D [label='Cultural 49 (10.1%)', fontsize=14]
  E [label='Circumstances 190 (39.3%)', fontsize=14]

  F [label='Cultural Barriers 30 (6.2%)', fontsize=12]
  G [label='English Language Learner 19 (3.9%)', fontsize=12]
  H [label='Foster Youth 35 (7.2%)', fontsize=12]
  I [label='Offender 14 (2.9%)', fontsize=12]
  J [label='Skills Deficient 141 (29.2%)', fontsize=12]
  K [label='Low SES 193 (40%)', fontsize=12]
  L [label='Homeless 3 (0.6%)', fontsize=12]
  M [label='TANF 14 (2.9%)', fontsize=12]

  { rank=same; B; C }  // Ensures Social and Economic are on the same level
  { rank=same; K; L; M; I; J } // Ensures the row of nodes at the bottom are aligned

  A -> B;
  A -> C;
  B -> D;
  B -> E;
  D -> F;
  D -> G;
  E -> H;
  E -> I;
  E -> J;
  C -> K;
  C -> L;
  C -> M;

  edge [style=dashed, arrowhead=none]
}"

DiagrammeR::grViz(flowchart_code)

