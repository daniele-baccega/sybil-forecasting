## BRANCHES
The **main** branch refers to the main results presented in [1] (together with the comparisons with other approaches).
There are different branches beyond the main branch:
- **sybil-v2.0**: this branch refers to the results presented in [1] using a fixed recovery rates, here, we also included a pre-processing step to fill missing data and to move from a weekly to a daily step in case of availability of data with a weekly step.
- **sybil-v2.5**: this branch refers to the results presented in [1] using data on USA (together with the comparison of Sybil's performance with the framework of Watson et al. [3]); here, we also separated the data pre-processing from Sybil and we tried to use estimates of real cases [4, 5, 6, 7] instead of working with data on confirmed cases; we are introducing the possibility to work with data on recoveries or with a fixed recovery rate.
- **sybil-v3.0**: this branch will host a more usable and cleaned version of Sybil.

## INTRODUCTION
The COVID-19 pandemic, caused by the SARS-CoV-2 virus, highlights the intricate challenges of addressing the most impactful global health crisis of the 21st century.
The rapid global spread of the virus has affected nearly every part of the world.
Consequently, healthcare systems worldwide are grappling with the significant challenge posed by COVID-19, requiring a continuous COVID-19 monitoring system that includes robust surveillance, widespread testing, contact tracing, and can be used to plan and deploy stringent infection control measures.

Establishing a continuous monitoring system aids policymakers in effectively managing the socio-health emergency brought about by the epidemic. Accurate forecasting is a fundamental element of such a system, and is crucial for efficient planning, resource allocation, and decision-making within public health authorities. It facilitates the development of proactive measures, such as vaccination campaigns, travel advisories, and community engagement programs, fostering public awareness and participation in disease control efforts. This proactive approach enhances preparedness and is critical in curbing the spread of infectious diseases and mitigating their impact on communities worldwide.

## WHAT IS THIS?
We propose **Sybil** [1], an integrated machine learning and variant-aware compartmental model framework capable of providing improved prediction accuracy and explainability.
Sybil exploits the relative stability of disease characteristics indices to project in the future and employs a simple and widely recognized analytical model to draw the infection dynamic.
Sybil’s strengths mark the difference with approaches present in the literature thanks to _i)_ its capability of providing accurate forecasts, even when there are relevant changes in the diffusion process, and _ii)_ reduced need for training data. 
Furthermore, the approach offers _iii)_ the possibility to study the evolution of the infection of several variants and _iv)_ the replicability of the results. Additionally, _v)_ the open-source code is available online.

## HOW IT WORKS?
Sybil combines a simple compartmental model (Susceptible - Infected - Recovered - Dead - Susceptible, SIRDS) with a machine learning-based predictive model (Prophet [2]) to forecast the future progression of infection, even in the presence of multiple virus strains. At the core of Sybil, there is a simple analytical model which has a dual functionality. In the first stage of Sybil’s operation, the analytical model is used to derive the value of critical model parameters from the surveillance data. Then, these parameters’ values---ascribable to the reproductive number over time, Rt---are used as training data for the ML component of Sybil. Based on that knowledge, it predicts the future values for the key parameters, which are then plugged back into the analytical model. Then, Sybil computes the future evolution of daily infections using the analytical model with these key parameters’ future values.

## REQUIREMENTS
You need to have docker installed on your machine, for more info see this document: https://docs.docker.com/engine/installation/.

Ensure your user has the rights to run docker (without the use of sudo). To create the docker group and add your user:

Create the docker group.
```
  $ sudo groupadd docker
 ```
 
Add your user to the docker group.
```
  $ sudo usermod -aG docker $USER
```

Log out and log back in so that your group membership is re-evaluated.

## HOW TO REPRODUCE THE RESULTS
To reproduce the results presented in the paper run:
```
./reproduce.sh
```

## REFERENCES
[1] Baccega, Daniele, et al. "Enhancing COVID-19 Forecasting Precision through the Integration of Compartmental Models, Machine Learning and Variants." medRxiv (2024): 2024-03. doi: https://doi.org/10.1101/2024.03.20.24304583

[2] Taylor, S.J., Letham, B.: Forecasting at scale. The American Statistician 72(1), 37–45 (2018). doi: https://doi.org/0.7287/peerj.preprints.3190v2

[3] Watson GL, Xiong D, Zhang L, Zoller JA, Shamshoian J, et al. (2021) Pandemic velocity: Forecasting COVID-19 in the US with a machine learning & Bayesian time series compartmental model. PLOS Computational Biology 17(3): e1008837. https://doi.org/10.1371/journal.pcbi.1008837

[4] Javier  ́Alvarez et al. “Estimating active cases of COVID-19”. In: MedRxiv (2021), pp. 2021–12.

[5] Christina M Astley et al. “Global monitoring of the impact of the COVID-19 pandemic through online surveys sampled from the Facebook user base”. In: Proceedings of the National Academy of Sciences 118.51 (2021), e2111455118.

[6] Jes ́us Rufino et al. “Using survey data to estimate the impact of the omicron variant on vaccine efficacy against COVID-19 infection”. In: Scientific Reports 13.1 (2023), p. 900.

[7]Joshua A Salomon et al. “The US COVID-19 Trends and Impact Survey: Continuous real-time measurement of COVID-19 symptoms, risks, protective behaviors, testing, and vaccination”. In: Proceedings of the National Academy of Sciences 118.51 (2021), e2111454118.

## COPYRIGHT AND LICENSE
Copyright _Daniele Baccega, Paolo Castagno, Antonio Fernández Anta, Matteo Sereno_

![CC BY-NC-SA 3.0](http://ccl.northwestern.edu/images/creativecommons/byncsa.png)

This work is licensed under the Creative Commons Attribution-NonCommercial-ShareAlike 3.0 License.  To view a copy of this license, visit https://creativecommons.org/licenses/by-nc-sa/3.0/ or send a letter to Creative Commons, 559 Nathan Abbott Way, Stanford, California 94305, USA.
