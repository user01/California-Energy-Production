
rds/generation_data.rds: rds/operating_records.rds
	Rscript collect.generation_data.R

rds/operating_records.rds:
	Rscript collect.operating_records.R

rds/weather_stations.rds: misc/weather_stations.txt
	Rscript collect.weather_stations.R

misc/weather_stations.txt:
	wget -N http://weather.rap.ucar.edu/surface/stations.txt -O misc/weather.stations.txt
	touch misc/weather.stations.txt

clean:
	rm rds/*.rds
