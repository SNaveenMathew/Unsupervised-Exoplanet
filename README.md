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

## Areas to focus

### Immediate (Updated 2019/05/07)

- Remove resource constraint - convert LSTM model training from CPU to GPU.
- Implement astronomy based ideas to filter out false detections - eg: remove detections above the mean.

### Soon (Updated 2019/05/07)

- Create a platform indepdent package in R that can run automatically.
    - Add R vignettes.

### Maybe later (Updated 2019/05/07)

- Hyperparameter turning.
- Download whole bulk data.
- Crowdsource the manual tagging of identified candidates.

## Long term goal (2019/05/07 - needs no update)

- Build a platform that can identify patterns (exoplanets) and reason why those detections are true.
- Slowly improve the accuracy of detection.
- Apply unsupervised learning to other problems in Astronomy and try to reason why things are the way they are.

## Contributing

- Currently this idea is freely available to everyone! I want to make this big - not for my personal benefit, but for the benefit of the whole community of deep learning and astronomy enthusiasts.