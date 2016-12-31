# Energy Load Forecasting

Data cleaning and model forecasting of energy generation.

### Data Sources

* Environmental Protection Agency - Emissions and Activity Data (AMPD)
* Energy Information Administration - Generator Metadata
* National Oceanic and Atmospheric Administration - Temperature Data

## California Power Plants

* Mostly Closed Energy Market
* Expansive Grid
* Publicly Available Data

 ![CA Power Plants](readme_assets/state.plants.png "CA Power Plants")

Focused on **Natural Gas** plants as they feature:

* Detailed Available Data
* Flexible Supply
* Not Directly Dependent on Environment Conditions

 ![Natural Gas Steamgraph 2010-2014](readme_assets/natural.gas.steamgraph.png "Natural Gas Steamgraph 2010-2014")

## Data Shape

From 2010 - 2014, for every Power Station, for every Day

* Active
* Operation Statistics, Past
* Location (Longitude/Latitude)
* Local Weather, Past and Forecast
* Production Capacity
* Date

![Processing Strategy](readme_assets/strategy.svg "Processing Strategy")

### Feature Engineering

Derived the following features about each plants' day:

* Day of the Week
* Weekend
* Season
* Recent Operating History
  * Past Day
  * Past Week

And based on geographic location:

* Local Neighborhood of nearest 3 Power Stations
  * Mean Production Capacity
  * Mean Production of Past Day
  * Mean Production of Past Week

### Random Forest Models

For every plant, the code trains a random forest model. This was done by best subset selection of the available features, judging the model quality with a 5 fold cross-validated balanced error rate between the two classes of _ACTIVE_ and _INACTIVE_.

![Common Features](readme_assets/model.results.png "Common Features")

| Name                          | Model                                                                     | Active Accuracy | Inactive Accuracy | Balanced Error Rate |
|-------------------------------|---------------------------------------------------------------------------|-----------------|-------------------|---------------------|
| Magnolia                      | ~ WEEKDAY + GRID_NEIGHBORS + OP_TIME_LAG + OP_TIME_MA                     | 0.988           | 0.97              | 0.02                |
| Coalinga Cogeneration Company | ~ WEEKEND + GRID_NEIGHBORS + OP_TIME_NEIGHBORS + OP_TIME_LAG + OP_TIME_MA | 0.996           | 0.943             | 0.03                |
| Average                       |                                                                           | 0.677           | 0.937             | 0.192               |


![Forecast Results](readme_assets/forecast.results.png "Forecast Results")
![Forecast Results Zoom](readme_assets/forecast.results.zoom.png "Forecast Results Zoom")
