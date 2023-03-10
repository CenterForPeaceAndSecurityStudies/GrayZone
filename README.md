## The Shadow of Deterrence: Why capable actors engage in contests short of war

Gannon, J Andr´es, Erik A. Gartzke, Jon R. Lindsay, and Peter Schram. “The Shadow of Deterrence: Why capable
actors engage in contests short of war.” Journal of Conflict Resolution, (Forthcoming).

## The Paper:

[Pre-print](https://github.com/CenterForPeaceAndSecurityStudies/GrayZone/blob/master/paper/Gannonetal_ShadowOfDeterrence.pdf).

[Appendix](https://github.com/CenterForPeaceAndSecurityStudies/GrayZone/blob/master/docs/07_Appendix.pdf).

## The Data:

The statistical analysis performed in the paper using the following data:
- [.csv](https://github.com/CenterForPeaceAndSecurityStudies/GrayZone/blob/master/data/grayzone_model.csv)

## The Authors:
[J Andrés Gannon](https://jandresgannon.com/), [Erik A Gartzke](http://erikgartzke.com/), [Jon R. Lindsay](https://www.jonrlindsay.com/), and [Peter Schram](https://peterschram.com/)

## Replication Code and Analysis
All coding scripts needed for replication and analysis are located in the /docs folder. They are described below.

### Helper files
[01 Data Load](https://github.com/CenterForPeaceAndSecurityStudies/GrayZone/blob/master/docs/01_DataLoad.rmd) - loads the data that were originally created in google docs.

[02 Distance](https://github.com/CenterForPeaceAndSecurityStudies/GrayZone/blob/master/docs/02_Distance.Rmd) - creates geographic distance numbers to be used for one of the deterrence variables.

### Data Preparation - External
03 series - we prepare data from existing sources, including [03a ICB](https://github.com/CenterForPeaceAndSecurityStudies/GrayZone/blob/master/docs/03a_PriorData_ICB.Rmd), [03b DCID](https://github.com/CenterForPeaceAndSecurityStudies/GrayZone/blob/master/docs/03b_PriorData_DCID.Rmd), and [03c REI](https://github.com/CenterForPeaceAndSecurityStudies/GrayZone/blob/master/docs/03b_PriorData_DCID.Rmd).

### Data Preparation - Aggregate
[04 Full Data](https://github.com/CenterForPeaceAndSecurityStudies/GrayZone/blob/master/docs/04_FullData.Rmd) - merge and quality check new, final dataset.

### Modeling
[05a Model Prep](https://github.com/CenterForPeaceAndSecurityStudies/GrayZone/blob/master/docs/05a_Model_Prep.Rmd) - add new variables to be used in the model and make necessary variable transformations.

[05b Model Results](https://github.com/CenterForPeaceAndSecurityStudies/GrayZone/blob/master/docs/05b_Model_Results.Rmd) - statistical models producing the results in the main paper and appendix.

### Visualization
[06a Intensity](https://github.com/CenterForPeaceAndSecurityStudies/GrayZone/blob/master/docs/06a_Figures_Intensity.Rmd) - produces Figure 1

[06b Map](https://github.com/CenterForPeaceAndSecurityStudies/GrayZone/blob/master/docs/06b_Figures_Map.Rmd) - produces Figure 2

