# COVID-19-epidemic-trajectories
Objective: To discover different epidemic trajectories of COVID-19 using data from Korea, this project provided the related program and data of the research conducted by Yu et al[1]. In this study, Generalized Additive Model (GAM)[2] was employed to explore distinctive trajectories of COVID-19 epidemic based on gender and age.

Data description: The data were retrieved from the public source (accessed on May 2nd, 2020)–the daily report of Korea Center for Disease Control[3]. The patients' confirmed date, age, gender information were collected in the "PatientInfo" file. We observed the epidemic started on Feb 19, 2020 and therefore data prior to Feb 19, 2020 were excluded from the analysis. Also because almost all cases from Daegu province were missing due to the limited access, the cases from Daegu were excluded. Three cases without any information of confirmed date were also excluded. In sum, there were 3349 cases for the analysis of overall trajectory. To explore the different trajectories, we stratified our analyses by gender, and categorized age (in years) into four groups: 1) 0-19, 2) 20-39, 3) 40- 59, 4) 60 and above. Cases with missing information on gender (n=78) or age (n=86) were excluded.   

Program description: The first step is to discover overall epidemic pattern and select the best fitted model between Poisson model and Negative Binomial model. The next step is to investigate the distinct epidemic trajectories by gender and age. In both stages, The daily new cases were firstly fitted by model along with date, then prediction was made based on the fitted model. The observed counts and fitted curves were plotted for each group to better observe and understand the epidemic patterns. 

Software and Packages: The analysis was completed using R software[4], with the packages mgcv[2], foreign[5], dplyr[6], ggplot2[7], data.table[8], and incidence[9].

Reference:

[1] Yu, X., Duan, J., Jiang, Y., Zhang, H.(2020). Distinctive trajectories of COVID-19 epidemic by age and gender: a retrospective
modeling of the epidemic in South Korea. Manuscript sumbitted for publication.

[2] Wood, S. N. (2017). Generalized additive models: an introduction with R. CRC press.

[3] J. (2020). jihoo-kim/Data-Science-for-COVID-19. Retrieved May 2, 2020, from https://github.com/jihoo-kim/Data-Science-for-COVID-19

[4] R Core Team (2020). R: A language and environment for statistical computing. R Foundation for Statistical Computing, Vienna, Austria. URL https://www.R-project.org/.

[5] R Core Team (2020). foreign: Read Data Stored by 'Minitab', 'S', 'SAS', 'SPSS', 'Stata', 'Systat', 'Weka', 'dBase', .... R package version 0.8-75. https://CRAN.R-project.org/package=foreign

[6] Hadley Wickham, Romain François, Lionel Henry and Kirill Müller (2020). dplyr: A Grammar of Data Manipulation. R package version 0.8.5. https://CRAN.R-project.org/package=dplyr

[7] H. Wickham. ggplot2: Elegant Graphics for Data Analysis. Springer-Verlag New York, 2016.

[8] Matt Dowle and Arun Srinivasan (2019). data.table: Extension of `data.frame`. R package version 1.12.8. https://CRAN.R-project.org/package=data.table

[9] Kamvar ZN, Cai J, Pulliam JRC, Schumacher J, Jombart T. Epidemic curves made easy using the R package incidence [version 1; referees: awaiting peer review]. F1000Research 2019, 8:139. URL https://doi.org/10.12688/f1000research.18002.1.
