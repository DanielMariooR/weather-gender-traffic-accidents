# weather-gender-traffic-accidents
Analysis of how weather conditions affect traffic accidents across genders in Taiwan.
## Research Background 
Previous studies have shown that weather conditions and driver gender both influence traffic accidents. However, these factors are often examined separately, and many studies rely on aggregated city-level weather data, implicitly assuming that accidents occurring within the same city experience similar weather conditions.

In reality, weather conditions can vary substantially across locations within the same city on the same day. To address these limitations, this study combines localized weather data matching with gender interaction analysis to examine whether rainfall affects traffic accident patterns differently across male and female drivers in Taiwan.
## Methods
To examine gender differences under varying rainfall conditions, this study applies statistical modeling with interaction terms between rainfall categories and driver gender. Localized weather data matching is used to reduce the limitations of aggregated city-level weather measurements and provide more location-specific weather conditions for accident observations.

### Dependent Variable

* Number of traffic accidents

### Independent Variables

#### Rainfall Categories

* No rain (reference group)
* Light rain
* Moderate rain
* Heavy rain

#### Gender Variable

* Female driver (reference group)
* Male driver

### Interaction Terms

* Light rain × Male
* Moderate rain × Male
* Heavy rain × Male

## Data Sources

### Traffic Accident Data

Source: [Taiwan Government Open Data Platform](https://data.gov.tw/)

The traffic accident dataset includes:

* Accident-level A1 and A2 records
* Date and time of accidents
* Latitude and longitude coordinates
* Party gender information
* Responsibility ranking

### Weather Data

Source: [Taiwan Climate Change Projection Information Platform (TCCIP)](https://tccip.ncdr.nat.gov.tw/)

The weather dataset includes:

* Rainfall (mm)
* Average temperature
* Date
* Latitude and longitude coordinates (0.01° grid)

### Dataset Coverage

* Coverage area: Taiwan nationwide
* Time period: 2022–2024
* Total observations: 600,000+ merged observations
