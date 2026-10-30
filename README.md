# Replication: The Institutional Order of Liberalization

Paper: Edgell, Boese, Maerz, Lindenfors, and Lindberg, "The Institutional Order of Liberalization," *British Journal of Political Science* (2022).

Data sources: V-Dem (V10), Episodes of Regime Transformation (ERT V2.2), and the authors' `eps_data.csv`.

R version: To be completed using `R.version.string`.

## Packages

The replication uses the following R packages:

- seqR
- vdemdata
- ERT
- dplyr
- tidyr
- purrr
- ggplot2
- ggrepel
- xtable
- stargazer
- psych
- model
-tibble

## Repository Structure

- `data/raw/`: Original data files.
- `data/clean/`: Cleaned analysis dataset.
- `scripts/`: R scripts.
- `output/`: Generated figures.
- `part1_replication.qmd`: Replication report.

## Scripts and Outputs

| Script | Output | Purpose |
|---|---|---|
| `01_clean.R` | `data/clean/eps_data.csv` | Prepares the analysis dataset. |
| `02_analyze.R` | `output/replication_graph.png` | Produces the replicated figure. |

`output/studies_graph.png` contains the published figure for comparison.

## Reproducing the Results

Run the scripts in order:

1. `Run Scripts Clean
2. `scripts/02_analyze.R`
3. Render the report:

```bash
quarto render part1_replication.qmd
```
