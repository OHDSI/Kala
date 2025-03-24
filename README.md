Kala
================
Kala is an R package developed within the OHDSI framework that allows you to generate and explore temporal features for one or more target cohorts. By leveraging the FeatureExtraction package and its temporalCovariateSettings, Kala aggregates those features into intuitive time-window comparisons and (optionally) creates time-series analyses, including data decomposition, statistical summaries, and visualizations.

(Kala is not part of OHDSI HADES.)

Introduction
============
Researchers often need to compare features (e.g., comorbidities, drug exposures, or other clinical observations) across different time windows before or after a key event. Kala streamlines this process by:

1. Taking one or more target cohorts (already instantiated in your OMOP CDM).
2. Generating features across configurable time windows using temporalCovariateSettings.
3. Producing tabular and graphical summaries of the features, allowing you to compare and interpret changes across time.
4. (Optionally) Creating time-series analyses—both regular and irregular time windows—providing decomposition and statistical summaries for trends, seasonality, and more.

The end result is a cohesive set of reports that simplify understanding of temporal patterns in your data.

Features
========
- Time-Window-Based Feature Construction: Leverages FeatureExtraction to construct features for specified temporal windows (e.g., -365 to -1 days, 0 to 30 days, etc.).
- Intuitive Reporting: Outputs simple comparison tables with each time window as a column and features as rows, making it easier to identify trends and differences.
- Time-Series Visualization: Creates time-series plots (with or without interruptions) for your selected features, offering decompositions and highlighting patterns that might not be apparent in standard tables.

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
