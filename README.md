# stmInsights
A web application based on Shiny to analyze Structural Topic Models. An online demo is available [here](http://pathways.polsys.uni-bamberg.de:443/stmInsights).

### Setup

In order for ```stmInsights``` to work properly you need to provide an R-image which includes three objects:

- **model**: the estimated stm model generated with ```stm()```
- **out**: stm meta data generated with ```prepDocuments()```
- **prep**: effect estimates for all topics generated with ```estimateEffect()```

Object names must not be changed, otherwise the app will crash. Adjust the path for the ```load() function``` in the ```helper.R``` file for your own image. 
Afterwards you can upload all files to any shiny server, install the required packages and run the app.

### Restrictions

- At the moment this app only works for models without content covariates.
- In order to display labels for all graphs correctly, the ``Topics`` has to be clicked first. This is due to text fields being generated dynamically depending on the number of topics of the given model.
