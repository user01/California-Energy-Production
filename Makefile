
generation_data: rds/generation_data.rds
	Rscript collect.generation_data.R

operating_records: rds/operating_records.rds
	Rscript collect.operating_records.R
