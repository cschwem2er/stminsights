# stminsights

<img src="/images/stminsights_interface.PNG" width="600" height="400">

This app enables interactive validation, interpretation and visualisation of [Structural Topic Models](http://structuraltopicmodel.com) (STM). In case you are not familiar with STM, the [package vignette](https://cran.r-project.org/web/packages/stm/vignettes/stmVignette.pdf) is an excellent starting point. In contrast to [stmgui](https://cran.r-project.org/web/packages/stmgui/index.html), which also allows to preprocess text data and fit models, stminsights is focused more on making your life easier after these steps.


## How to Install

You can download and install the app by running ``devtools::install_github('methodds/stminsights')``.    

For Windows users installing from github requires proper setup of [Rtools](https://cran.r-project.org/bin/windows/Rtools/), for which a tutorial is available [here](https://github.com/stan-dev/rstan/wiki/Install-Rtools-for-Windows).

## How to Use

After installing stminsights run ``stminsights::run_stminsights()`` to launch the shiny app in your browser. Afterwards you can upload an `.RData` file which should include:

- one or several stm objects.
- one or several estimateEffect objects.
- an object `out` which was used to fit your stm models

As an example, the following code fits two models and estimates effects for the [Political Blog Corpus](http://www.sailing.cs.cmu.edu/main/?page_id=710). Afterwards, all objects required for stminsights are stored in `stm_poliblog5k.RData`. 


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

In case you want to try stminsights before installing it, download `stm_poliblog5k.RData` [here](http://polsoz.uni-bamberg.de:1337/data/poliblog/stm_poliblog5k.RData) and upload it at www.polsoz.uni-bamberg.de/stminsights.

## Citation

To cite package `stminsights` in publications please use:
```
  Carsten Schwemmer (2018). stminsights: A Shiny Application for Inspecting
  Structural Topic Models. R package version 0.1.0.
  https://github.com/methodds/stminsights
```

A BibTeX entry for LaTeX users is:

```
  @Manual{,
    title = {stminsights: A Shiny Application for Inspecting Structural Topic Models},
    author = {Carsten Schwemmer},
    year = {2018},
    note = {R package version 0.1.0},
    url = {https://github.com/methodds/stminsights},
  }
```

