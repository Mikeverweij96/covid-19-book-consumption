all: data-preperation analysis

data-preperation:
	make -C src/data-preperation

analysis: data-preperation
	make -C src/analysis