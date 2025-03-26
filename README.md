Kala
================
Kala is an R package developed within the OHDSI framework that allows you to generate and explore temporal features for one or more target cohorts. By leveraging the FeatureExtraction package and its temporalCovariateSettings, Kala aggregates those features into intuitive time-window comparisons and (optionally) creates time-series analyses, including data decomposition, statistical summaries, and visualizations.

(Kala is not part of OHDSI HADES.)

Introduction
============
Researchers often need to compare features (e.g., comorbidities, drug exposures, or other clinical observations) across different time windows before or after a key event. Kala streamlines this process by:

1. Cohort Input: Taking one or more target cohorts (already instantiated in your OMOP CDM).
2. Temporal Feature Generation: Generating features across configurable time windows using temporalCovariateSettings.
3. Summarized Reporting: Producing tabular and graphical summaries of the features, allowing you to compare and interpret changes across time.
4. Time-Series Analyses: (Optionally) Creating time-series analyses—both for regular and irregular time windows—that provide decomposition and statistical summaries for trends, seasonality, and more.

The end result is a cohesive set of reports that simplify the understanding of temporal patterns in your data.

Features
========
- Time-Window-Based Feature Construction: Leverages FeatureExtraction to construct features for specified temporal windows (e.g., -365 to -1 days, 0 to 30 days, etc.).
- Intuitive Reporting: Outputs simple comparison tables with each time window as a column and features as rows, making it easier to identify trends and differences.
- Time-Series Visualization: Creates time-series plots (with or without interruptions) for your selected features, offering decompositions and highlighting patterns that might not be apparent in standard tables.

How Kala is Useful
========

Kala is particularly useful for observational health data research by:

- Facilitating Temporal Comparisons: It provides a systematic approach to extract and compare patient-level features before and after a key event, enabling deeper insights into patient trajectories and outcomes.
- Enhancing Visual Analysis: Through its time-series visualizations and decomposition analyses, users can quickly identify trends, seasonal effects, and irregular patterns that might be missed in static summaries.
- Improving Reproducibility and Standardization: Operating within the OHDSI framework and using the OMOP CDM, Kala ensures that feature extraction is consistent, reproducible, and scalable across different datasets and studies.
- Streamlining Reporting: The combination of tabular outputs and visual reports makes it easier for researchers to present and communicate their findings, supporting better decision-making in clinical research and observational studies.

Screenshot
==========


Technology
==========
The Kala package is an R package.

System Requirements
===================
R

Installation
=============
## R package

To install the latest development version, install from GitHub:

```r
remotes::install_github("OHDSI/Kala")
```

Kala will be submitted to Cran when ready.

User Documentation
==================
* Vignette: [Simple Time Series Analysis Using Kala](https://raw.githubusercontent.com/OHDSI/Kala/master/inst/doc/SimpleTimeSeriesAnalysisUsingKala.pdf)
* Package manual: [Kala manual](https://ohdsi.github.io/Kala/reference/index.html) 

Support
=======
* Developer questions/comments/feedback: <a href="http://forums.ohdsi.org/c/developers">OHDSI Forum</a>
* We use the <a href="https://github.com/OHDSI/Kala/issues">GitHub issue tracker</a> for all bugs/issues/enhancements

License
=======
Kala is licensed under Apache License 2.0

Development
===========
Kala is being developed in R Studio and follows best practices in R package development. The project is currently in beta testing, and contributions are welcome.

### Development status

Beta testing
