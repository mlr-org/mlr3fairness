# Adult Dataset

Dataset used to predict whether income exceeds \$50K/yr based on census
data. Also known as "Census Income" dataset Train dataset contains 13
features and 30178 observations. Test dataset contains 13 features and
15315 observations. Target column is "target": A binary factor where 1:
\<=50K and 2: \>50K for annual income. The column `"sex"` is set as
protected attribute.

## Source

Dua, Dheeru, Graff, Casey (2017). “UCI Machine Learning Repository.”
<http://archive.ics.uci.edu/ml/>. Ding, Frances, Hardt, Moritz, Miller,
John, Schmidt, Ludwig (2021). “Retiring adult: New datasets for fair
machine learning.” In *Thirty-fifth Conference on Neural Information
Processing Systems Datasets and Benchmarks Track (Round 1)*.

## Derived tasks

- `adult_train`: Original train split for the adult task available at
  UCI.

- `adult_test`: Original test split for the adult task available at UCI.

## Using Adult - Known Problems

The adult dataset has several known limitations such as its age, limited
documentation, and outdated feature encodings (Ding et al., 2021).
Furthermore, the selected threshold (income \<=50K) has strong
implications on the outcome of analysis, such that "In many cases, the
\$50k threshold understates and misrepresents the broader picture" (Ding
et al., 2021). As a result, conclusions w.r.t. real-world implications
are severely limited.

We decide to replicate the dataset here, as it is a widely used
benchmark dataset and it can still serve this purpose.

## Pre-processing

- `fnlwgt` Remove final weight, which is the number of people the census
  believes the entry represents

- `native-country` Remove Native Country, which is the country of origin
  for an individual

- Rows containing `NA` in workclass and occupation have been removed.

- Pre-processing inspired by article: @url
  https://cseweb.ucsd.edu//classes/sp15/cse190-c/reports/sp15/048.pdf

## Metadata

- (integer) age: The age of the individuals

- (factor) workclass: A general term to represent the employment status
  of an individual

- (factor) education: The highest level of education achieved by an
  individual.

- (integer) education_num: the highest level of education achieved in
  numerical form.

- (factor) marital_status: marital status of an individual.

- (factor) occupation: the general type of occupation of an individual

- (factor) relationship: whether the individual is in a relationship-

- (factor) race: Descriptions of an individual’s race

- (factor) sex: the biological sex of the individual

- (integer) captain-gain: capital gains for an individual

- (integer) captain-loss: capital loss for an individual

- (integer) hours-per-week: the hours an individual has reported to work
  per week

- (factor) target: whether or not an individual makes more than \$50,000
  annually

## Examples

``` r
library("mlr3")
data("adult_test", package = "mlr3fairness")
data("adult_train", package = "mlr3fairness")
```
