## INTRODUCTION
Forecasting the trajectory and impact of epidemics is crucial for efficient planning, resource allocation, and decision-making within public health authorities.
Epidemic forecasting involves a multidisciplinary approach, integrating epidemiology, mathematical modeling, data analysis, and computational methods to gain insights into the future progression of outbreaks.
Accurate epidemic forecasting is pivotal in early detection, prevention, and control of infectious diseases.
By providing timely information on disease transmission dynamics, identifying potential high-risk areas, and evaluating the effectiveness of interventions, forecasting empowers public health agencies to implement targeted strategies, allocate resources efficiently, and alleviate strain on healthcare systems.

## WHAT IS THIS?
we propose **Sybil**, an integrated machine learning and compartmental model framework capable of providing improved prediction accuracy and explainability.
Sybil exploits the relative stability of disease characteristics indices to project in the future and employs a simple and widely recognized analytical model to draw the infection dynamic.
Sybil marks the difference with other approaches in the literature thanks to _i)_ its capability of providing accurate forecasts, even when there are relevant changes in the diffusion process, _ii)_ replicability of the approach and _iii)_ reduced need for training data.

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
