## WHAT'S NEW IN VERSION 2.0
- We now use a fixed recovery rate (same for each variant for now) to dispense with data on recoveries which are ofter not available. Now we only need data on cases and fatalities.
- We included a pre-processing step using splines to fill missing data and to move from a weekly to a daily step in case of availability of data with a weekly step (without alter too much the information present in the time series).

## INTRODUCTION
The COVID-19 pandemic, caused by the SARS-CoV-2 virus, highlights the intricate challenges of addressing the most impactful global health crisis of the 21st century. The rapid global spread of the virus has affected nearly every part of the world. Consequently, healthcare systems worldwide are grappling with the significant challenge posed by COVID-19, requiring robust surveillance, widespread testing, contact tracing, and stringent infection control measures. Forecasting the trajectory and impact of epidemics is crucial for efficient planning, resource allocation, and decision-making within public health authorities. Epidemic forecasting involves a multidisciplinary approach, integrating epidemiology, mathematical modeling, data analysis, and computational methods to gain insights into the future progression of outbreaks. Accurate epidemic forecasting is pivotal in early detection, prevention, and control of infectious diseases. By providing timely information on disease transmission dynamics, identifying potential high-risk areas, and evaluating the effectiveness of interventions, forecasting empowers public health agencies to implement targeted strategies, allocate resources efficiently, and alleviate strain on healthcare systems.
Establishing a continuous monitoring system aids policymakers in effectively managing the socio-health emergency brought about by the epidemic. Indeed, accurate forecasting facilitates the development of proactive measures, such as vaccination campaigns, travel advisories, and community engagement programs, fostering public awareness and participation in disease control efforts. This proactive approach enhances preparedness and is critical in curbing the spread of infectious diseases and mitigating their impact on communities worldwide.

## WHAT IS THIS?
We propose **Sybil**, an integrated machine learning and compartmental model framework capable of providing improved prediction accuracy and explainability. Sybil exploits the relative stability of disease characteristics indices to project in the future and employs a simple and widely recognized analytical model to draw the infection dynamic. Sybil marks the difference with other approaches in the literature thanks to _i)_ its capability of providing accurate forecasts, even when there are relevant changes in the diffusion process, _ii)_ replicability of the approach and _iii)_ reduced need for training data.

The evaluation of Sybil’s forecast is made by i) comparing it against the real data coming from the active surveillance of the pandemic situation in several European states and ii) comparing it against forecasts obtained by a ML approach. In this latter
case, we use Prophet [1] to predict the evolution of the different active variants in the considered period, representing a typical forecasting approach based on ML.

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
[1] Taylor, S.J., Letham, B.: Forecasting at scale. The American Statistician 72(1), 37–45 (2018) https://doi.org/0.7287/peerj.preprints.3190v2

## COPYRIGHT AND LICENSE
Copyright _Daniele Baccega, Paolo Castagno, Antonio Fernández Anta, Matteo Sereno_

![CC BY-NC-SA 3.0](http://ccl.northwestern.edu/images/creativecommons/byncsa.png)

This work is licensed under the Creative Commons Attribution-NonCommercial-ShareAlike 3.0 License.  To view a copy of this license, visit https://creativecommons.org/licenses/by-nc-sa/3.0/ or send a letter to Creative Commons, 559 Nathan Abbott Way, Stanford, California 94305, USA.
