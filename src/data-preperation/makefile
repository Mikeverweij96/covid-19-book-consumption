DATA = ../../data
TEMP = ../../gen/temp
OUTP = ../../gen/output

all: $(DATA)/all_books.csv $(TEMP)/books_cleaned.csv

$(DATA)/all_books.csv: Thesis_data_combine.R
	R --vanilla < Thesis_data_combine.R

$(TEMP)/books_cleaned.csv: Thesis_data_clean.R $(DATA)/all_books.csv
	R --vanilla < Thesis_data_clean.R