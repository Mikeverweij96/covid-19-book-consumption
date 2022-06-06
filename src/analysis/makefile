DATA = ../../data
TEMP = ../../gen/temp
OUTP = ../../gen/output

all: $(OUTP)/fig12_percentage_change_proportion_of_books.png $(OUTP)/model1_recent.csv $(OUTP)/model1_gender_recent.csv $(OUTP)/model1_started_recent.csv $(OUTP)/model2__country_recent.csv $(OUTP)/model1_age_recent.csv



$(OUTP)/fig12_percentage_change_proportion_of_books.png : plots.R $(TEMP)/covid_stringency_prepared.csv $(TEMP)/users_cleaned.csv $(TEMP)/books_cleaned.csv
	R --vanilla < plots.R



$(OUTP)/model1_recent.csv : regression.R $(TEMP)/covid_stringency_prepared.csv $(TEMP)/users_cleaned.csv $(TEMP)/books_cleaned.csv
	R --vanilla < regression.R


$(OUTP)/model1_gender_recent.csv : regression_gender.R $(TEMP)/covid_stringency_prepared.csv $(TEMP)/users_cleaned.csv $(TEMP)/books_cleaned.csv
	R --vanilla <  regression_gender.R


$(OUTP)/model1_age_recent.csv : regression_age.R $(TEMP)/covid_stringency_prepared.csv $(TEMP)/users_cleaned.csv $(TEMP)/books_cleaned.csv
	R --vanilla <  regression_age.R


$(OUTP)/model2__country_recent.csv : regression_fanatic_country.R $(TEMP)/covid_stringency_prepared.csv $(TEMP)/users_cleaned.csv $(TEMP)/books_cleaned.csv
	R -- vanilla < regression_fanatic_country.R

$(OUTP)/model1_started_recent.csv : regression_robustness.R $(TEMP)/covid_stringency_prepared.csv $(TEMP)/users_cleaned.csv $(TEMP)/books_cleaned.csv
	R --vanilla < regression_robustness.R