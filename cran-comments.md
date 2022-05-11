## Resubmission

I have updated DESCRIPTION, quoting 'mlr3' where required and included references according to the guide.
All exported functions have running examples, except for report_* functions. 
The latter create files on the filesystem and internally rely on rmarkdown::draft which uses the same \dontrun directive.

## R CMD check results

❯ checking CRAN incoming feasibility ... NOTE
  Maintainer: ‘Florian Pfisterer <pfistererf@googlemail.com>’
  
  New submission

0 errors ✔ | 0 warnings ✔ | 1 note ✖

- RHub shows a PREPERROR but all builds succeed.
- There are 3 false positives for misspelled words.


