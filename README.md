Code used for analysis in my Evolution 2023 SSB Presidential Address

It is structured as a targets workflow. From within R, `source("run.R")` will make it all run. It requires that you have installed the packages "ggplot2", "tidyverse", "readxl", "magrittr", "tarchetypes", "stringr", "igraph", "ggplot2", "viridis", "tidygraph", "ggraph", "ngramr", "Matrix", "waffle", "gganimate", "animation", "gridExtra".

There is a `data` directory. This should have xls files output from Web of Science. As I'm not sure about the licensing for the raw files, I start from the processed files instead saved in RDS format (I include just a few of the files so you can understand the format), but if you want to run from scratch, download data for each the journal in chunks of 1000 articles, naming each `JournalNameNoSpaces_FirstArticleNumber_LastArticleNumber.xls`. For the analysis for the talk, the downloaded datasets were:

```
AmericanMidlandNaturalist_1001_2000.xls
AmericanMidlandNaturalist_1_1000.xls
AmericanMidlandNaturalist_2001_3000.xls
AmericanMidlandNaturalist_3001_4000.xls
AmericanMidlandNaturalist_4001_5000.xls
AmericanMidlandNaturalist_5001_end.xls
AmericanNaturalist_10001_11000.xls
AmericanNaturalist_1001_2000.xls
AmericanNaturalist_11001_11467.xls
AmericanNaturalist_1_1000.xls
AmericanNaturalist_2001_3000.xls
AmericanNaturalist_3001_4000.xls
AmericanNaturalist_4001_5000.xls
AmericanNaturalist_5001_6000.xls
AmericanNaturalist_6001_7000.xls
AmericanNaturalist_7001_8000.xls
AmericanNaturalist_8001_9000.xls
AmericanNaturalist_9001_10000.xls
AnnalsOfEugenics_all.xls
AnnalsOfHumanGenetics_1001_2000.xls
AnnalsOfHumanGenetics_1_1000.xls
AnnalsOfHumanGenetics_2001_3000.xls
AnnalsOfHumanGenetics_3001_end.xls
BiodemographyAndSocialBiology_1_end.xls
Copeia_1001_2000.xls
Copeia_1_1000.xls
Copeia_2001_3000.xls
Copeia_3001_4000.xls
Copeia_4001_5000.xls
Copeia_5001_6000.xls
Copeia_6001_7000.xls
Copeia_7001_8000.xls
Copeia_8001_end.xls
Ecology_10001_11000.xls
Ecology_1001_2000.xls
Ecology_11001_12000.xls
Ecology_12001_13000.xls
Ecology_13001_14000.xls
Ecology_14001_15000.xls
Ecology_15001_16000.xls
Ecology_16001_17000.xls
Ecology_17001_17704.xls
Ecology_1_1000.xls
Ecology_2001_3000.xls
Ecology_3001_4000.xls
Ecology_4001_5000.xls
Ecology_5001_6000.xls
Ecology_6001_7000.xls
Ecology_7001_8000.xls
Ecology_8001_9000.xls
Ecology_9001_10000.xls
EugenicsQuarterly_all.xls
EugenicsReview_1001_2000.xls
EugenicsReview_1_1000.xls
EugenicsReview_2001_3000.xls
EugenicsReview_3001_4000.xls
EugenicsReview_4001_end.xls
Evolution_10001_11000.xls
Evolution_1001_2000.xls
Evolution_11001_end.xls
Evolution_1_1000.xls
Evolution_2001_3000.xls
Evolution_3001_4000.xls
Evolution_4001_5000.xls
Evolution_5001_6000.xls
Evolution_6001_7000.xls
Evolution_7001_8000.xls
Evolution_8001_9000.xls
Evolution_9001_10000.xls
Genetics_10001_11000.xls
Genetics_1001_2000.xls
Genetics_11001_12000.xls
Genetics_12001_13000.xls
Genetics_13001_14000.xls
Genetics_14001_15000.xls
Genetics_15001_16000.xls
Genetics_16001_17000.xls
Genetics_17001_18000.xls
Genetics_18001_19000.xls
Genetics_19001_20000.xls
Genetics_1_1000.xls
Genetics_20001_21000.xls
Genetics_2001_3000.xls
Genetics_21001_22000.xls
Genetics_22001_23000.xls
Genetics_23001_24000.xls
Genetics_24001_25000.xls
Genetics_25001_26000.xls
Genetics_26001_end.xls
Genetics_3001_4000.xls
Genetics_4001_5000.xls
Genetics_5001_6000.xls
Genetics_6001_7000.xls
Genetics_7001_8000.xls
Genetics_8001_9000.xls
Genetics_9001_10000.xls
JournalOfBiosocialScience_1001_2000.xls
JournalOfBiosocialScience_1_1000.xls
JournalOfBiosocialScience_2001_3000.xls
JournalOfBiosocialScience_3001_end.xls
MolecularBiologyAndEvolution_1001_2000.xls
MolecularBiologyAndEvolution_1_1000.xls
MolecularBiologyAndEvolution_2001_3000.xls
MolecularBiologyAndEvolution_3001_4000.xls
MolecularBiologyAndEvolution_4001_5000.xls
MolecularBiologyAndEvolution_5001_6000.xls
MolecularBiologyAndEvolution_6001_7000.xls
MolecularBiologyAndEvolution_7001_8000.xls
MolecularBiologyAndEvolution_8001_end.xls
MolecularPhylogeneticsAndEvolution_1001_2000.xls
MolecularPhylogeneticsAndEvolution_1_1000.xls
MolecularPhylogeneticsAndEvolution_2001_3000.xls
MolecularPhylogeneticsAndEvolution_3001_4000.xls
MolecularPhylogeneticsAndEvolution_4001_5000.xls
MolecularPhylogeneticsAndEvolution_5001_6000.xls
MolecularPhylogeneticsAndEvolution_6001_7000.xls
MolecularPhylogeneticsAndEvolution_7001_end.xls
SocialBiology_1001_end.xls
SocialBiology_1_1000.xls
SystematicBiology_1001_2000.xls
SystematicBiology_1_1000.xls
SystematicBiology_2001_end.xls
SystematicBotany_1001_2000.xls
SystematicBotany_1_1000.xls
SystematicBotany_2001_3000.xls
SystematicBotany_3001_end.xls
SystematicZoology_1001_end.xls
SystematicZoology_1_1000.xls
TrendsInEcologyAndEvolution_1001_2000.xls
TrendsInEcologyAndEvolution_1_1000.xls
TrendsInEcologyAndEvolution_2001_3000.xls
TrendsInEcologyAndEvolution_3001_4000.xls
TrendsInEcologyAndEvolution_4001_5000.xls
TrendsInEcologyAndEvolution_5001_end.xls
```