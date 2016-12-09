
rds/generation_data.rds: rds/operating_records.rds
	Rscript collect.generation_data.R

rds/operating_records.rds:
	Rscript collect.operating_records.R

clean:
	rm rds/*.rds
