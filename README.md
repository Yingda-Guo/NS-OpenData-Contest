# NS-OpenData-Contest



## Description
We linked the Nova Scotia Open Datasets for Disease Prevalence over time (2005-2017) with Annual Weather statistics for Nova Scotia over 12 years. We then Created an Web Application that runs different machine learning models and selects the best solution to predict Disease Prevalence given we have an estimate for then upcoming winter conditions. This Web App can be used by any people who would like to apply the model to get an estimate of disease prevalence by disease category.

[App link](https://yingdaguo.shinyapps.io/NS-OpenData-Contest/)

![截屏2021-10-18 下午9 07 48](https://user-images.githubusercontent.com/13625416/137822779-46418e1c-9c89-4464-b35e-a418addcdbd8.png)

## DataSets
Notifiable Diseases Counts and Rates 2005-2017 

https://data.novascotia.ca/Health-and-Wellness/Notifiable-Diseases-Counts-and-Rates-2005-2017/mdfn-jkdg

Nova Scotia Monthly Climate Summaries from 2005 to 2017

http://climate.weather.gc.ca/prods_servs/cdn_climate_summary_e.html

## Description of the processes and algorithms 
We programmed in R,  used R library data.table to crunch, manipulate the data. We used the caret package to run several machine learning algorithms(K-Nearest Neighbors, Support Vector Machine, Random Forest, Generalized Linear Model and Neural Network) on the dataset. We used Shiny package to create a interactive Machine learning application dashboard that can be used on the web by users. We used the library ggplot to visualize the graphs.

## Application Link
https://yingdaguo.shinyapps.io/NS-OpenData-Contest/
