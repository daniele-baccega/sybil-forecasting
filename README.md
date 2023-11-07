## INTRODUCTION
The ability to predict the trajectory and impact of epidemics is of utmost importance for efficient planning, allocation of resources, and decision-making within the realm of public health authorities. Epidemic forecasting relies on a multidisciplinary approach that integrates epidemiology, mathematical modeling, data analysis, and computational methods to gain insights into the future progression of outbreaks. Precise epidemic forecasting plays a pivotal role in early detection, prevention, and control of infectious diseases. By providing timely information on disease transmission dynamics, potential high-risk areas, and the effectiveness of interventions, forecasting empowers public health agencies to implement targeted strategies, allocate resources with efficiency, and alleviate the strain on healthcare systems. This establishment of a continuous monitoring system aids policymakers in managing the socio-health emergency engendered by the epidemic. Furthermore, forecasting facilitates the development of proactive measures, such as vaccination campaigns, travel advisories, and community engagement programs, fostering public awareness and participation in disease control endeavors.

## WHAT IS THIS?
We propose an integrated framework aimed at addressing and compensating for the limitations associated with the two types of models mentioned earlier. Our approach combines a compartmental model with a machine learning-based predictive model to forecast the future progression of infection, utilizing both COVID-19 data and variants data. 
Specifically, we have chosen to employ the Susceptible-Infected-Recovered-Deceased (SIRD) compartmental  model with reinfections due to its simplicity. 
For the prediction component, we utilize Prophet [1],  a machine learning-based decomposable time series model [2]. Prophet was selected for its flexibility, non-linearity, robustness, and user-friendliness, but other predictive models can be used.

The main novelty proposed by this study lies in the application of the predictive model to the rates (infection, recovery, and fatality rates) derived from the compartmental model, which are obtained from COVID-19 data, instead of directly on the infected population. As we will show, this approach results in enhanced accuracy in epidemic prediction due to the fact that the rates exhibit less dramatic changes compared to the infected population, leading to a smoother transition.

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
[2] Harvey, A.C., Peters, S.: Estimation procedures for structural time series models. Journal of forecasting 9(2), 89–108 (1990) https://doi.org/10.1002/for.3980090203 

## COPYRIGHT AND LICENSE
Copyright _Daniele Baccega, Paolo Castagno, Matteo Sereno, Antonio Fernández Anta_

![CC BY-NC-SA 3.0](http://ccl.northwestern.edu/images/creativecommons/byncsa.png)

This work is licensed under the Creative Commons Attribution-NonCommercial-ShareAlike 3.0 License.  To view a copy of this license, visit https://creativecommons.org/licenses/by-nc-sa/3.0/ or send a letter to Creative Commons, 559 Nathan Abbott Way, Stanford, California 94305, USA.