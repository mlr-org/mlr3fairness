# COMPAS Dataset

The COMPAS dataset includes the processed COMPAS data between 2013-2014.
The data cleaning process followed the guidance in the original COMPAS
repo. Contains 6172 observations and 14 features. The target column
could either be "is_recid" or "two_year_recid", but often
"two_year_recid" is prefered. The column `"sex"` is set as protected
attribute, but more often `"race"` is used.

## Source

ProPublica Analysis: <https://github.com/propublica/compas-analysis>

Bao, Michelle, Zhou, Angela, Zottola, A S, Brubach, Brian, Desmarais,
Sarah, Horowitz, Seth A, Lum, Kristian, Venkatasubramanian, Suresh
(2021). “It's COMPASlicated: The Messy Relationship between RAI Datasets
and Algorithmic Fairness Benchmarks.” In *Thirty-fifth Conference on
Neural Information Processing Systems Datasets and Benchmarks Track
(Round 1)*.

## Using COMPAS - Known Problems

The COMPAS dataset was collected as part of the ProPublica analysis of
machine bias in criminal sentencing. It is important to note, that using
COMPAS is generally discouraged for the following reasons:

- The prediction task derived from this dataset has little connection to
  actually relevant tasks in the context of risk assessment instruments.

- Collected data and labels suffer from disparate measurement bias.

The dataset should therefore not be used to benchmark new fairness
algorithms or measures. For a more in-depth treatment, see Bao et al.,
2021: It's COMPASlicated: The Messy Relationship between RAI Datasets
and Algorithmic Fairness Benchmarks. We replicate the dataset here to
raise awareness for this issue. Furthermore, similar issues exist across
a wide variety of datasets widely used in the context of fairness
auditing and we, therefore, consider issues, e.g. derived from disparate
measurement bias an important issue in the context of fairness audits.

## Pre-processing

- Identifying columns are removed

- Removed the outliers for abs(days_b_screening_arrest) \>= 30.

- Removed observations where is_recid != -1.

- Removed observations where c_charge_degree != "O".

- Removed observations where score_text != 'N/A'.

- Factorize the features that are categorical.

- Add length of stay (c_jail_out - c_jail_in) in the dataset.

- `Pre-processing Resource:` @url
  https://github.com/propublica/compas-analysis/blob/master/Compas%20Analysis.ipynb

Note: The 'is_recid' column was removed as it's a target column.

## Metadata

- (integer) age: The age of defendants.

- (factor) c_charge_degree : The charge degree of defendants. F: Felony
  M: Misdemeanor

- (factor) race: The race of defendants.

- (factor) age_cat: The age category of defendants.

- (factor) score_text: The score category of defendants.

- (factor) sex: The sex of defendants.

- (integer) priors_count: The prior criminal records of defendants.

- (integer) days_b_screening_arrest: The count of days between screening
  date and (original) arrest date. If they are too far apart, that may
  indicate an error. If the value is negative, that indicate the
  screening date happened before the arrest date.

- (integer) decile_score: Indicate the risk of recidivism (Min=1,
  Max=10)

- (factor) two_year_recid: Binary variable indicate whether defendant is
  rearrested at within two years.

- (numeric) length_of_stay: The count of days stay in jail.

## Examples

``` r
library("mlr3")
data("compas", package = "mlr3fairness")
```
