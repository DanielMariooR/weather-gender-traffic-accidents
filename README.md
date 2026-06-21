# weather-gender-traffic-accidents
Analysis of how weather conditions affect traffic accidents across genders in Taiwan.
## Research Background 
Previous studies have shown that weather conditions and driver gender both influence traffic accidents. However, these factors are often examined separately, and many studies rely on aggregated city-level weather data, implicitly assuming that accidents occurring within the same city experience similar weather conditions.

In reality, weather conditions can vary substantially across locations within the same city on the same day. To address these limitations, this study combines localized weather data matching with gender interaction analysis to examine whether rainfall affects traffic accident patterns differently across male and female drivers in Taiwan, while also comparing urban and rural areas to evaluate whether these patterns remain consistent across different regional contexts.

## Methods
To examine gender differences under varying rainfall conditions, this study applies statistical modeling with interaction terms between rainfall categories and driver gender. Localized weather data matching is used to reduce the limitations of aggregated city-level weather measurements and provide more location-specific weather conditions for accident observations. Additional subgroup analyses are also conducted separately for urban and rural areas.

### Dependent Variable

* Number of traffic accidents

### Independent Variables

#### Rainfall Categories

* No Rain = <1 mm/day (reference group)
* Light Rain = 1–30 mm/day
* Moderate Rain = 30–60 mm/day
* Heavy Rain = >60 mm/day

Reference: Adapted from Taiwan Central Weather Administration rainfall warning criteria.

#### Gender Variable

* Female driver (reference group)
* Male driver

### Interaction Terms

* Light Rain × Male
* Moderate Rain × Male
* Heavy Rain × Male

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

## Conclusion
Dry conditions and light rain are associated with marginally higher accident counts than heavy rain, although none of the rainfall categories are statistically significant (p > 0.05) after controlling for temperature and gender. This suggests that rainfall intensity alone is not a strong predictor of accident frequency in our model.

Male at-fault drivers exhibit consistently higher accident volumes than female drivers across all weather conditions (Estimate = 0.056, p < 0.001, IRR = 1.058), corresponding to approximately 6% higher accident counts.

Light rain is associated with a modest but statistically significant increase in accident counts among male drivers relative to female drivers (Interaction Estimate = 0.014, p = 0.025, IRR = 1.014). In contrast, no significant interaction is observed under heavy rain (p = 0.365), suggesting that the gender difference identified under light rain does not persist during heavy rainfall.
