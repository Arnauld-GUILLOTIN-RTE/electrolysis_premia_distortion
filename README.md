# electrolysis_premia_distortion

This project supports a submitted paper, allowing reproduction of all this paper's figures. 
It processes output data from a set of [Antares Simulator](https://antares-simulator.org/) outputs which are available for download on this [Zenodo record](https://zenodo.org/records/20022572). 

This project is solely intended to reproduce figures of said data provided code execution complying with instructions below.



- [Reproduction & Execution](#reproduction-execution)
  - [Prerequisites](#prerequisites)
  - [Reproduce figures](#reproduce-figures)
  - [Memory Requirements](#memory-requirements)
- [Project Structure & Data Flow](#project-structure--data-flow)
- [Main Analysis Workflow (`main.R`)](#main-analysis-workflow-main-r)
  - [1. Environment Setup](#1-environment-setup)
  - [2. Antares Simulation Path Configuration](#2-antares-simulation-path-configuration)
  - [3. Figure Generation](#3-figure-generation)
  - [4. Storing output figures](#4-storing-output-figures)

---

## <a name="reproduction-execution"></a>Reproduction & Execution

### <a name="prerequisites"></a><a name="prerequisites"></a>Prerequisites
- R version 4.4.2 (as used in paper reproduction)
- `renv` managed environment with all dependencies (run `renv::restore()` in `main.R`). The `renv.lock` file and `renv` folder of the repository ensure packages used match those with which the code runs as intended
- Unzipped Antares simulation folders, at the root of `data/simulations/` (download from associated [Zenodo record](https://zenodo.org/records/20022572)). The structure of the folder should be, for a given simulation, e.g., `data/simulations/NT2040_0/`
- Note: Input parameters in `data/input/` and `src/config.R` match model inputs and should not be changed to reproduce paper figures

### <a name="reproduce-figures"></a>Reproduce figures
1. In Git bash: `git clone https://github.com/Arnauld-GUILLOTIN-RTE/electrolysis_premia_distortion.git`
2. Open `electrolysis_premia_distortion.Rproj` in RStudio
3. Run `main.R`, ensuring that environment restoration from `renv::restore()` worked correctly (it can take quite a while if it necessitates a large number of package downloads) and all data linking to simulation outputs works
4. Outputs automatically generated in `data/output/` as `.png` and `.xlsx` files and the Viewer panel


### <a name="memory-requirements"></a>Memory Requirements
- Full simulation data loading can exceed available RAM
- Intermediate `.rds` files (cached in `data/temp/`) can be used to avoid the most RAM-demanding step of the processing (Fig. 5a-b and 6)

---

## <a name="project-structure--data-flow"></a>Project Structure & Data Flow

```
data/
├── input/              # Static input parameters for calculations
│   ├── electrolysis_capacities.csv
│   └── gas_efficiencies.csv
├── output/             # Generated figures and summary files (.png, .xlsx)
├── simulations/        # Antares simulation folders (whole study) 
│   ├── NT2040_0/
│   ├── NT2040_1/
│   └── ...
└── temp/               # Intermediate processed data files (.rds)

src/
├── config.R            # Geographic and parameter definitions as per Antares model inputs
├── data_functions.R    # Data loading and processing functions
└── graph_functions.R   # Visualization and plotting functions
```

---

## <a name="main-analysis-workflow-main-r"></a>Main Analysis Workflow (`main.R`)

### <a name="1-environment-setup"></a><a name="1-environment-setup"></a>1. Environment Setup
- Restores R package environment via `renv::restore()` (ensures reproducibility)
- Sources configuration and utility functions
- Initializes paths for simulations, outputs, temporary files, and input data

### <a name="2-antares-simulation-path-configuration"></a>2. Antares Simulation Path Configuration
Creates a list of Antares simulation objects (`opts_list`) for each premium level:
```
opts_list$pr0 → NT2040_0 most recent output
opts_list$pr1 → NT2040_1 most recent output
opts_list$pr2 → NT2040_2 most recent output
opts_list$pr3 → NT2040_3 most recent output
```
These objects enable reading full simulation data (dispatch, prices, flows) via `antaresRead` package functions.

### <a name="3-figure-generation"></a>3. Figure Generation

| Figure | Function | Output Type | Description |
|--------|----------|-------------|-------------|
| Fig. 2a, 3a | `summarise_studies()` | Excel file | Study summary statistics across premium levels |
| Fig. 2b | `plot_h2_supply()` | PNG | Hydrogen supply across premium levels (annual average) |
| Fig. 3b | `plot_elec_supply()` | PNG | Electricity supply across premium levels (annual average) |
| Fig. 4a | `plot_stack()` | Interactive widget | Electricity mix in NL, premium 0 EUR/kgH2, example week |
| Fig. 4b | `plot_stack()` | Interactive widget | Electricity mix in NL, premium 3 EUR/kgH2, example week |
| Fig. 4c | `plot_electrolysis_cons()` | PNG | Electrolysis consumption over study scope and across premium levels |
| Fig. 5a | `plot_fscd_eu_agg_concat()` | PNG | FSCD (EU aggregated) for premium 0 EUR/kgH2 |
| Fig. 5b | `plot_fscd_change_with_pr()` | PNG | FSCD change as premium increases (EU aggregated) |
| Fig. 6 | `plot_fscd_change_heatmap()` | PNG | Average FSCD's change heatmap across countries and premium levels |
| Fig. 7a–c | `boxplot_opcosts/emissions/gas()` | PNG | Boxplots of operating costs, emissions, gas (distribution across weather years) |
| Appendix Fig. B.8 | `plot_heatmap_marginal_cost_gap()` | PNG | Marginal cost gap heatmap across premium and carbon price ranges |
| Appendix Fig. B.9 | `price_gap_plot()` | PNG | Price gap visualization |
| Appendix Fig. C.10a–c | | PNG | Equivalent to Fig. 7a–c, repeated using sensitivity scenario data |

### <a name="4-storing-output-figures"></a>4. Storing output figures

**Output Organization**:
- Figures saved as `.png` in `data/output/`
- Summary Excel files generated by plotting functions in `data/output/`
- Intermediate `.rds` files cached in `data/temp/` to avoid reprocessing data For Figures 5a-b and 6.
- Note: Figures 4a-b are `htmlwidget` objects to be viewed inside the Viewer panel of RStudio