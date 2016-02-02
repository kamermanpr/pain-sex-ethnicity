# Random forest analysis of predictors of pain tolerance and beliefs

## Description
The primary content of this repository includes the data, codebook, and analysis scripts (with associated markdown outputs and plots) for random forest analyses of predictors of cold-pain tolerance, pressure-pain tolerance, and acceptance of pain behaviours in males (APBQ-M) in healthy black and white young adults of both sexes in South Africa. 

The repository also includes the data and codebook used in bivariate analyses. Analysis of these data were completed using GraphPad Prism 4.0, and the results have not been posted (please contact [antonia.wadley@wits.ac.za](mailto:antonia.wadley@wits.ac.za) or [peter.kamerman@wits.ac.za](mailto:peter.kamerman@wits.ac.za) for more information).

## Bibliographic information
[To be added when published]

## License
<a rel="license" href="http://creativecommons.org/licenses/by/4.0/"><img alt="Creative Commons License" style="border-width:0" src="https://i.creativecommons.org/l/by/4.0/88x31.png" /></a><br /><span xmlns:dct="http://purl.org/dc/terms/" property="dct:title">The 'pain-sex-race' respository</span> by <a xmlns:cc="http://creativecommons.org/ns#" href="https://github.com/kamermanpr/pain-sex-race.git" property="cc:attributionName" rel="cc:attributionURL">Peter Kamerman and Antonia Wadley</a> is licensed under a <a rel="license" href="http://creativecommons.org/licenses/by/4.0/">Creative Commons Attribution 4.0 International License</a>.

## Repository contents

## Overview of the study
### Background
Sex and race influence pain sensitivity and pain beliefs. However, studies on relationships between sex, race and pain largely have emanated from the US and Europe. 

### Objectives
To determine effects of sex and race on pain sensitivity and beliefs in a black African population we compared cold and pressure pain tolerance in male and female black and white South Africans. We also assessed whether psychosocial factors (including pain beliefs) predicted sex and race differences in pain sensitivity and beliefs. 

### Methods
We recruited 108 black (64 female, 44 male) and 108 white (64 female, 52 male) students. Participants underwent a cold-pressor test (hand submersion in ice-water) and pressure algometry (pressure applied to nail-bed of index finger) to determine tolerance to noxious thermal and mechanical stimuli. Socioeconomic status, catastrophizing (Pain Catastrophizing Scale), depression and anxiety (25-item Hopkins Symptom Checklist), and pain beliefs (Appropriate Pain Beliefs Questionnaire) were assessed as predictors of sex and race differences in pain tolerance. 

### Results
Cold pain tolerance was lower in black than white students (for both sexes), and pressure pain tolerance was lower in female than male students (for both races). Pain intensity at tolerance was similar for all groups in both tests. Men were less accepting of men expressing pain than were women, with black males being the least accepting. Multivariate analysis didn’t identify any psychosocial factors as predictors of pain tolerance independently of sex and race. 

### Conclusion
Despite a different cultural and social background from US and European cohorts, we saw similar patterns of sex and race differences in tolerance to cold and pressure stimuli in an African cohort. However, traditional psychosocial predictors of pain sensitivity were not apparent in this group. 

## Codebook

### Random forest analysis
[_**Data file:** random-forest.csv_](/data/random-forest.csv)   
[_**Codebook file:** codebook-random-forest.csv_](/data/codebook-random-forest.csv)

| Label      	| Notes                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         	|
|:------------	|:---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------	|
| ID         	| Participant identification code                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               	|
| CPT        	| Cold-pain tolerance: Time (in seconds) participants could retain their dominant hand in iced water (5oC). For safety, a maximum duration of 300s hand immersion was used.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     	|
| PPT        	| Pressure-pain tolerance: Pressure (in kPa) applied to the nail bed of the index finger using a pressure algometer with a 10mm^2 probe (Algometer, Somedic AB, Sweden). For safety, a maximum pressure of 1500kPa was used.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     	|
| Race       	| Self-identified race (B: Black, W: White)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     	|
| Sex        	| Self-identified biological sex (F: Female, M: Male)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           	|
| Anxiety    	| Anxiety was assessed using 10-item anxiety subscale of the Hopkins Symptom Checklist-25 (HSCL-25). Participants rated the extent to which they had experienced symptoms within the last week on a 4-point Likert scale. A mean subscale scores > 1.75 indicates clinically relevant levels of anxiety.                                                                                                                                                                                                                                                                                                                                                                                                                                                        	|
| Depression 	| Depression was measured using the 15-item depression subscale of the Hopkins Symptom Checklist-25 (HSCL-25). See Anxiety for details.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          	|
| PCS        	| Pain Catastrophizing Scale was used to assess catastrophic thinking related to pain. The PCS was administered before exposure to the experimental pain stimuli to determine trait catastrophizing: Assesses the general tendency to catastrophize. Participants rated each of the 13 items on the questionnaire on a 5-point Likert scale. PCS scores > 30 indicate a clinically significant level of catastrophizing.                                                                                                                                                                                                                                                                                                                                        	|
| APBQ-F     	| Appropriate Pain Behavior Questionnaire - Female (APBQ-F): Assesses pain beliefs regarding females. A 14-item questionnaire that measures beliefs about the appropriateness of expressing pain in the presence of others. Eight items on the ABPQ express a positive attitude to pain expression, and 6 express a negative attitude towards expressing pain. Participants rated the extent to which they agreed with each of the 14 statements by scoring them on a 7-point Likert scale. APBQ was scored by calculating the difference between the mean score of the eight positive statements and six negative statements. The final score has a bounded range -6 to +6, with negative values indicating a bias against females expressing pain behaviours. 	|
| APBQ-M     	| Appropriate Pain Behavior Questionnaire - Male (APBQ-M): Assesses pain beliefs regarding males. See ABPQ-F for details.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       	|
| Education  	| Highest level of education completed by a parent or guardian [0: none, 1: primary school (grades 1 - 7), 2:,secondary school (grades 8 - 12), 3: tertiary education (post-secondary school)].                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  	|
| Assets     	| Average number of household assets owned by parents or guardians (five assets were assessed: refrigerator, television, car, microwave oven, and washing machine; 0: not owned, 1: owned).                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     	|

