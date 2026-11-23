# R for Microeconometrics

R code for the standard microeconometric models covered in the Microeconometrics course by S. Derya Uysal (LMU Munich).

**Book:** a short online book with explanations, code and output for all models is at <https://deryauysal.github.io/R-microeconometrics/>. Its source is in `book/` (Quarto); the rendered pages are in `docs/`.

| Script | Topic | Data |
|---|---|---|
| `01-binary-response-mroz.R` | Linear probability, logit and probit models | `mroz` (wooldridge) |
| `02-binary-response-bwght.R` | Binary response models, second example | `bwght` (wooldridge) |
| `03-multinomial-response.R` | Multinomial and conditional logit, multinomial probit | `Fishing`, `HC` (mlogit) |
| `04-ordered-response.R` | Ordered logit and probit | `pension` (wooldridge) |
| `05-count-data-fertil2.R` | Poisson, negative binomial, hurdle and zero-inflated models | `fertil2` (wooldridge) |
| `06-count-data-smoke.R` | Count data models, second example | `smoke` (wooldridge) |
| `07-censored-and-selection.R` | Tobit and Heckman selection models | `fringe` (wooldridge), `mus16data.dta` |

## Usage

Each script lists the packages it needs at the top. Install them once, for example

```r
install.packages(c("wooldridge", "car", "lmtest", "DescTools"))
```

and then run the script in R or RStudio.

Most data sets come with the R packages `wooldridge` and `mlogit`. The Heckman example in script 07 uses `mus16data.dta` from Cameron and Trivedi, *Microeconometrics Using Stata*. This file is not included here; download it from the book's website and put it in your working directory.

## Rebuilding the book

Install [Quarto](https://quarto.org), put `mus16data.dta` into `book/`, and run `quarto render` in `book/`. The output goes to `docs/`.

## License

MIT, see `LICENSE`.
