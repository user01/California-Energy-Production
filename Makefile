
plots/full.plot_rf_inactive.png: plots/state.plants.png
	Rscript model.simple.R

plots/state.plants.png: rds/generator_data_daily.rds
	Rscript plot.basic.R

rds/generator_data_daily.rds: rds/generation_data.rds rds/weather_stations.rds
	Rscript collect.weather_data.R

rds/generation_data.rds: rds/operating_records.rds
	Rscript collect.generation_data.R

rds/operating_records.rds:
	Rscript collect.operating_records.R

rds/weather_stations.rds: misc/weather_stations.txt
	Rscript generate.weather_stations.R

misc/weather_stations.txt:
	wget -N http://weather.rap.ucar.edu/surface/stations.txt -O misc/weather.stations.txt
	touch misc/weather.stations.txt

clean:
	rm rds/*.rds
	rm plots/*.png
	rm *.html
