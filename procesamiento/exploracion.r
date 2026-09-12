library(haven)
library(labelled)
library(dplyr)
library(psych)

data <- read_sav("input/edumer_students_long.sav") %>%
	select(
		exp_t1_a, exp_t1_b, exp_t2_a, exp_t2_b,
		exp_t3_a, exp_t3_b, exp_t4_a, exp_t4_b,
		exp_t4_c, exp_t4_d
	)


summary(data)
diccionario <- generate_dictionary(data)
View(diccionario)

