# Kepler

## Objective

The objective of this project is to identify planetary transit using data from Kepler telescope. Classical methods include BLS (Box Least Squares) and TLS (Transit Least Squares). Modern methods apply deep learning (supervised convolutional neural networks) to identify planetary transit. These modern methods take specific steps to remove eclipsing binary star systems from the transits.

This project treats planetary transits as anomalies and attempts to detect them in an unsupervised way. Therefore, the scope of this project is limited to applying unsupervised deep learning to identify candidate planetary transits and to plot them. As a conclusion, this project is expected to address cases of correct and incorrect detections and to propose ways to address them, which may develop into a long-term project.

## Platform and Tools

The code has been tested on the following platforms:

**Platform 1:**

- Ubuntu 16.04 (shell, sed, grep, top, wget, head, awk)
- R version 3.5.2

**Platform 2:**

- R Studio Cloud
- R version 3.5.1

## Roadmap (2019/04/14)

- Finalizing the problem (done)
- Downloading the data (done)
- Preprocessing the data to get number of planets per star (done)
- Translating the python code into R (done)
- Manual tagging and verification (done)
- Phase folding (tentative, depending on time)
- Scope for future (done)
