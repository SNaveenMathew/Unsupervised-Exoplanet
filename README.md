# Unsupervised Exoplanet Detection using Deep Learning

## Introduction

I was very fortunate to explore ASTR 596 (AI in Astronomy) at UIUC in Spring 2019. I understood that machine learning can teach us a lot in scientific domains such as Physics and Astronomy when the problem is well structured.

The intent of this project is to continue my efforts in improving the unsupervised deep learning pipeline for exoplanet detection.

## Motivation to continue

[Geocentric model](https://en.wikipedia.org/wiki/Geocentric_model) was proposed by Ptolemy. Astonomical predictions of this model were used for over 1500 years. [Heliocentric model](https://en.wikipedia.org/wiki/Heliocentrism) came into picture in the late 16th century. But the biggest breakthrough came Newton's laws met Tycho Brahe's observations - when models were built to explain the orbits of planets, asteroids, comets, etc. and predictions were made, which were later confirmed (with negligible differences) through observations.

Models form an important component of Physics and Astronomy. The ultimate objective of a scientist is to build a model that allows causal inference. If the importance for accuracy outweighs the need for a rational explanation of the predictions, the machine learning approach is short sighted. This is because (accurate) pattern identification without (scientific / logical / causal) reasoning does not provide a reasonable forecast. The application of such an approach is limited to computational models that are based on estimates of some form correlation - not causation.

### Personal note

Currently reasoning is a very difficult task for AI. As a result, I'm not sure whether the long term goal of this project will be met, but I will keep trying.

## State at the end of STAT 430 and ASTR 596

At the end of STAT 430 (Data Science Programming Methods) and ASTR 596 (AI in Astronomy), this repository had:

- Code to download a subset of 3050 light curves out of 7491 light curves in [Kepler_KOI_DV_wget.bat](https://exoplanetarchive.ipac.caltech.edu/bulk_data_download/Kepler_KOI_DV_wget.bat)
- Preprocessing: Filling up missing values using [Stineman interpolation](https://cran.r-project.org/web/packages/stinepack/stinepack.pdf). The interpolation method is fixed and 'anomalies' in interpolated values are not treated.
- Modeling: Builds LSTM autoencoder with fixed hyperparameters.
- Postprocessing: Since large number of anomalies were identified using LSTM autoencoder, phase folding was applied to filter the anomalies and consider only the anomalies that fit the period
- Code [main.R](main.R) to invoke the whole pipeline. It is wrapped using docopt, so it can be executed directly using command line

There were several constraints during the execution of the project: in terms of resources and portability. Therefore, the code was tested only on 36 light curves. Manual observation of the light curves showed that the model had a good recall - it detected all exoplanet transits, but it had poor precision - there were many periodic false detections.

## Areas covered

### Initial Phase

- Shiny dashboard: [application link](https://snaveenmathew.shinyapps.io/unsupervised_exoplanet/).
- SQLite database containing start and end indices of identified transits, user base with hashed password.
- Customize button: creates a user-specific copy of the indices (blank by default). Editing option has not been added.
- Reset button: removes user-specific copy and uses default model based indices.

### Pipeline Modernization & Performance Upgrades

- **Gap-Aware Preprocessing**: Replaced linear gap interpolation with continuous segment splitting ($\Delta t > 0.5\text{ days}$ or cadence index jumps) to avoid artificial ramps across Kepler quarter boundaries and Earth downlinks.
- **Stellar Variability Detrending**: Added running median baseline filtering on continuous segments to flatten stellar rotation and starspot modulations.
- **Asymmetric Flare Clipping & One-Sided Transit Scoring**: Clipped positive $> +3\sigma$ spikes (stellar flares, cosmic rays) and switched anomaly residual scoring to $\max(0, \hat{X} - X)$ so only negative transit dips trigger candidate intervals.
- **GPU-Accelerated 1D-CNN Autoencoder**: Replaced the 1-step autoregressive LSTM with a symmetrical 1D Convolutional Autoencoder (`Conv1D` + `MaxPooling1D` $\to$ Bottleneck $\to$ `UpSampling1D` + `Conv1D`), optimized for modern GPUs with batch sizes up to 256–512 and sequence length $L = 128$ cadences (~2.6 days).
- **Cross-Platform Native R Execution**: Eliminated external shell script dependencies (`counts.sh`, `counts_df.sh`) for native Kepler star parsing and database population across Windows and Linux.
- **On-Demand Plotting & Model Inference**: The Shiny app generates and caches light curve plots directly from raw `.tbl` files upon user selection if not pre-generated, running inference via star-specific or global autoencoder models and storing the rendered plots in `plots/test_pred_plot/` for future instant loading.
- **Session-Scoped Database Lifecycle**: Unified SQLite connections into a single session-scoped connection with automatic cleanup on session termination.
- **Interactive Plotly Manual Tagging**: Replaced the old "Customize" (blank placeholder row, no editing) / "Reset" pair with a full tagging session workflow. "Edit Tags" pre-loads a star's saved windows and "Start Blank" begins empty; drag-selecting a range and clicking "Fix Brush as Window" adds a candidate interval, "Undo Last Window" removes the most recent one, and fixed windows render as translucent, edge-draggable/resizable shapes directly on the plot. Nothing is written to `user_star` until "Confirm Tags"; "Cancel" discards the session untouched. "Reset Tags to Model" still reverts a star to model-detected candidates immediately.
- **Zoom & Pan Controls**: Added "Zoom to Selection" (zoom into a drag-selected range), "Zoom In" / "Zoom Out" (±50% around the current center), and "Reset Zoom", with y-axis limits recomputed dynamically for whichever window is visible.
- **Global Model Fallback + `test_idx` Cache**: When a star has no dedicated `.hdf5` model, inference now falls back to a shared `global_conv1d_autoencoder.hdf5`. Model-detected candidate windows are persisted once per star into a new `test_idx` table and reused across sessions instead of being recomputed on every view; per-user overrides still live in `user_star`.
- **Thompson-Sampling Bandit Star Selector**: Added a live "Suggest Next Star" button (plus a sidebar priority badge) that recommends which star most needs another tagger, computed on demand from `user_star` and `test_idx` — no separate bandit-state table required. Each star is modeled as a Beta-Bernoulli arm whose "reward" is *disagreement* among taggers: stars with zero human tags get an uninformative Beta(1, 1) prior, stars with two or more taggers use the mean pairwise IoU (interval overlap) between their saved windows, and stars with exactly one tagger fall back to comparing that tagger against the model's own `test_idx` detection as a weaker second opinion. Thompson Sampling — drawing a fresh posterior sample per star on every request — was chosen over epsilon-greedy/UCB1 specifically because this is a multi-user dashboard: UCB1 is deterministic, so every concurrent session would get funneled to the exact same star, and epsilon-greedy can't tell "unlabeled" apart from "already agreed on." Random posterior draws naturally spread concurrent taggers across several high-priority stars while still concentrating attention on the ones that most need it.
- **Graceful Keras/Reticulate Degradation**: Keras/reticulate calls are now wrapped in `requireNamespace()` / `tryCatch()` guards so the dashboard still renders (falling back to a zero baseline) when the Python/Keras backend isn't available — a step toward reliable shinyapps.io deployment.
- **Dependency Cleanup**: Dropped an unused `imputeTS` import from `app.Rmd`'s global chunk — a leftover from the old Stineman-interpolation preprocessing that nothing in the current pipeline calls — so the dashboard no longer needs an extra, unused package installed just to start.
- **shinydashboard UI Rework**: Rebuilt the app on `shinydashboard` (collapsible sidebar for Kepler ID selection, dashboard header with a GitHub link) with `shinyBS` tooltips on every toolbar control.

## Areas to focus

### Immediate

- Phase folding / Box Least Squares (BLS) period verification on detected transit intervals to extract orbital periods ($P$), durations, and transit depths.
- Multi-channel input support (e.g. Centroid X/Y offsets) to automatically reject background eclipsing binaries (EBs) and instrumental jitter.

### Soon

- Create a platform independent package in R that can run automatically.
    - Add R vignettes.
- Figure out reticulate issue in deployment of application on shinyapps.io. (`app.Rmd` now guards all Keras/reticulate calls and pins an explicit `resource_files` manifest of the bundled `.tbl` / `.hdf5` files for shinyapps.io, but full Python backend support there is still unresolved.)

### Maybe later

- Global foundation model pre-training across the complete Kepler and TESS datasets.
- Crowdsource the manual tagging of identified candidates.

## Long term goal (needs no update)

- Build a platform that can identify patterns (exoplanets) and reason why those detections are true.
- Slowly improve the accuracy of detection.
- Apply unsupervised learning to other problems in Astronomy and try to reason why things are the way they are.

## Repository structure

```
Unsupervised-Exoplanet/
├── main.R                              # CLI entry point (docopt); runs the pipeline
├── pipeline.R                          # 1D-CNN autoencoder training, plotting, SQLite output
├── util.R                              # helpers (gap splitting, detrending, transit detection, DB)
├── download.R                          # downloads Kepler KOI light curves into data/
├── counts.sh / counts_df.sh            # legacy star / planet-count helpers
├── Kepler_KOI_DV_wget_remaining.bat
├── Unsupervised-Exoplanet.Rproj
├── _config.yml
├── data/
│   ├── Kepler_KOI_DV_wget.bat          # bulk download script for Kepler DV light curves
│   ├── remove_log.sh
│   └── *.tbl                           # Kepler light-curve tables (downloaded; gitignored)
├── plots/                              # generated by pipeline.R or on-demand by Shiny (gitignored)
│   ├── learning_curve/                 # Autoencoder training-history plots (*_learning.png)
│   ├── train_pred_plot/                # train-set prediction vs actual (*_train_plot.png)
│   └── test_pred_plot/                 # test-set prediction vs actual (*_test_plot.png)
├── trained_models/                     # saved Keras models (*.hdf5; gitignored)
├── shiny/
│   └── app.Rmd                         # Shiny dashboard
├── Report/                             # course report sources (gitignored)
└── Presentation/                       # course presentation sources (gitignored)
```

`pipeline.R` and `shiny/app.Rmd` create `plots/learning_curve`, `plots/train_pred_plot`, and `plots/test_pred_plot` (and `trained_models/`) if they do not already exist. Kepler `*.tbl` files are downloaded into `data/` by `download.R` / `data/Kepler_KOI_DV_wget.bat`.

### Large files (removed from git history)

Generated and course-material binaries were previously committed and bloated the repository. They have been purged from git history and are gitignored. Regenerate pipeline outputs locally by running `main.R`; do not commit them.

| Path | What was stored |
|------|-----------------|
| `plots/learning_curve/` | Autoencoder training-history plots (`*_learning.png`) |
| `plots/train_pred_plot/` | Training-set prediction plots (`*_train_plot.png`) |
| `plots/test_pred_plot/` | Test-set prediction plots (`*_test_plot.png`); this was the largest plot set |
| repository root | A stray test plot, `kplr002302548_q1_q16_tce_02_dvt_lc_test_plot.png` |
| `Presentation/` | Course slides, embedded HTML, GIFs, and example PNGs |
| `Report/` | Course report PDF/LaTeX sources and figure PNGs |
| `exoplanet_db.sqlite`, `shiny/exoplanet_db.sqlite` | SQLite databases of transit indices / users |
| `README.html` | Rendered copy of this README |

## Contributing

- Currently this idea is freely available to everyone! I want to make this big - not for my personal benefit, but for the benefit of the whole community of deep learning and astronomy enthusiasts.