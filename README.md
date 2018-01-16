# stminsights

<img src="/images/stminsights_interface.PNG" width="600" height="400">

This app enables interactive validation, interpretation and visualisation of [Structural Topic Models](http://structuraltopicmodel.com) (STM). In case you are not familiar with STM, the [package vignette](https://cran.r-project.org/web/packages/stm/vignettes/stmVignette.pdf) is an excellent starting point.


## How to Install

You can download and install the app by running ``devtools::install_github('methodds/stminsights')``.

## How to Use

After installing stminsights run ``stminsights::run_stminsights()`` to launch the shiny app in your browser. Afterwards you can upload an `.RData` file which should include:

- one or several stm objects.
- one or several estimateEffect objects.
- an object `out` which was used to fit your stm models

As an example, the following code fits two model and estimates effects for the stm corpus poliblog5k and saves all objects required for stminsights in `stm_poliblog5k.RData`. 


```
library(stm)

out <- list(documents = poliblog5k.docs,
            vocab = poliblog5k.voc,
            meta = poliblog5k.meta)

poli <- stm(documents = out$documents, 
            vocab = out$vocab,
            data = out$meta, 
            prevalence = ~ rating * s(day),
            K = 20)
prep_poli <- estimateEffect(1:20 ~ rating * s(day), poli,
                            meta = out$meta)

poli_content <-  stm(documents = out$documents, 
                     vocab = out$vocab,
                     data = out$meta, 
                     prevalence = ~ rating + s(day),
                     content = ~ rating,
                     K = 15)  
prep_poli_content <- estimateEffect(1:15 ~ rating + s(day), poli_content,
                                    meta = out$meta)

save.image('stm_poliblog5k.RData')
```

After launching stminsights and uploading the file, all objects are automatically imported  and you can select which models and effect estimates to analyze.

## Live Demo

In case you want to try stminsights before installing it,  download `stm_poliblog5k.RData` [here](http://polsoz.uni-bamberg.de:1337/data/poliblog/stm_poliblog5k.RData) and upload it at www.polsoz.uni-bamberg.de/stminsights.


