# text.alignment

This repository contains an R package for aligning texts using the [Smith-Waterman algorithm](https://en.wikipedia.org/wiki/Smith%E2%80%93Waterman_algorithm). This is especially usefull if you want to 

- **Find names in documents even if they are not correctly spelled**
- **Match 2 texts**
- **Find relevant sequences of texts in other texts**

### Installation

- For regular users, install the package from your local CRAN mirror `install.packages("text.alignment")`
- For installing the development version of this package: `remotes::install_github("DIGIT-VUB/text.alignment", build_vignettes = TRUE)`

Look to the vignette and the documentation of the functions

```
vignette("textalignment", package = "text.alignment")
help(package = "text.alignment")
```

### DIGI

By DIGI: Brussels Platform for Digital Humanities: https://digi.research.vub.be

![](vignettes/logo.png)
