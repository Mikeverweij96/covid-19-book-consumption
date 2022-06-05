DATA = ../../data
TEMP = ../../gen/temp
OUTP = ../../gen/output

all: $(DATA)/all_books.csv $(DATA)/all_users.csv $(DATA)/books_re_scrape.csv $(DATA)/stringency_index.csv $(DATA)/users_re_scrape.csv $(TEMP)/books_after_rescrape.csv $(TEMP)/users_after_rescrape.csv $(TEMP)/books_prepared.csv $(TEMP)/users_prepared.csv $(TEMP)/books_cleaned.csv $(TEMP)/users_cleaned.csv $(TEMP)/covid_stringency_prepared.csv


$(DATA)/all_books.csv $(DATA)/all_users.csv $(DATA)/books_re_scrape.csv $(DATA)/stringency_index.csv $(DATA)/users_re_scrape.csv: data_download.R
	R --vanilla < data_download.R


$(TEMP)/books_after_rescrape.csv $(TEMP)/users_after_rescrape.csv: add_re_scrape.R $(DATA)/all_books.csv $(DATA)/all_users.csv $(DATA)/books_re_scrape.csv $(DATA)/users_re_scrape.csv
	R --vanilla < add_re_scrape.R


$(TEMP)/books_prepared.csv: book_data_prepare.R $(TEMP)/books_after_rescrape.csv
	R --vanilla < book_data_prepare.R


$(TEMP)/users_prepared.csv $(TEMP)/active_users.csv: user_data_prepare.R $(TEMP)/users_after_rescrape.csv
	R --vanilla < user_data_prepare.R


$(TEMP)/books_cleaned.csv $(TEMP)/users_cleaned.csv: data_clean.R $(TEMP)/users_prepared.csv $(TEMP)/books_prepared.csv
	R --vanilla < data_clean.R


$(TEMP)/covid_stringency_prepared.csv $(TEMP)/users_cleaned.csv: covid_data_prepare.R $(TEMP)/users_cleaned.csv $(DATA)/stringency_index.csv
	R --vanilla < covid_data_prepare.R



