# An Evaluation of Model Predictions for COVID-19 in New York
Repository for data and code analysing the performance of different models in predicting COVID-19 daily deaths and ICU bed utilisation in New York.

## Installation

1) Install R.

    For Windows:<br/>
    Download the binary setup file for R [here](https://cran.r-project.org/bin/windows/base/) and open the downloaded .exe file.
    
    For MacOS:<br/>
    Download the appropriate version of .pkg file [here](https://cran.r-project.org/bin/macosx/) and open the downloaded .pkg file.
    
2) Install RStudio.
    Choose the appropriate installer file for your operating system [here](https://rstudio.com/products/rstudio/), download it and then run it to install RStudio.
    
3) Download all files in the repository to your working directory. The script analysing the performance of the models is given in ``analysis.R``.

## Description of Data

1) ``ny_data.csv`` contains the daily deaths predictions (point estimates and 95% prediction intervals) by [IHME](http://www.healthdata.org/covid/data-downloads), [LANL](https://covid-19.bsvgateway.org), [UT](https://covid-19.tacc.utexas.edu/projections/) and [YYG](https://covid19-projections.com/). The csv file also contains the source of training ground truth used by each model.

2) ``ny_gt.csv`` contains the observed daily deaths from 5 different sources, i.e. [Covid Tracking](https://github.com/COVID19Tracking/covid-tracking-data/raw/master/data/states_daily_4pm_et.csv), [The New York Times](https://github.com/nytimes/covid-19-data/raw/master/us-states.csv), [JHU raw data](https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_daily_reports_us), [JHU time series](https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_daily_reports_us) and [USAFacts](https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_deaths_usafacts.csv).

3) ``ny_icu.csv`` contains the daily ICU bed utilisation predictions (point estimates and 95% prediction intervals) by the IHME model.

4) ``ny_icu_gt.csv`` contains the observed daily ICU bed utilisation obtained from [The City](https://github.com/thecityny/covid-19-nyc-data/raw/master/beds.csv).

5) ``ny_icu_capacity.csv`` contains the maximum daily ICU bed capacity obtained from [The City](https://github.com/thecityny/covid-19-nyc-data/raw/master/state.csv).
