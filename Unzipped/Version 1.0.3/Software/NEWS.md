# Anthro Survey Analyser v 1.0.3 (2020-10-26)
* Z-Score flag points now only considers the 4 relevant zscore columns in the dataset.
  Previously and under certain conditions it included columns that contained the word 'flag' as well.
* Fixed a bug where, observations where erroneously removed from prevalence
  computation. This happens if age in months was supplied, then values
  `> 59.992` months were considered `> 1826` days. Now anything above
  `60.009` months is considered `> 1826` days and thus excluded.
* The offline version now supports files of up to 500mb.


# Anthro Survey Analyser v 1.0.2 (2020-05-19)
* Changed the rounding procedure of age in days before matching the observations
with the growth standards in order to be consistent with previous implementations in certain edge cases (e.g. age is exactly 24 months).
* Fixed a link to an external page on the Data Quality Report module.

# Anthro Survey Analyser v 1.0.1 (2019-09-23)
* Fixed age distribution plots: frequencies for standardised age groups, age in years, and age in months now match. Grouping was changed to fixed age groups from rounding.
* Font size of age distribution plots reduced for greater readability.
* Blurb added to Data Quality Report page to explain what the Data Quality Report is for.
* Oedema coding corrected in the Quick Guide.

# Anthro Survey Analyser v 1.0.0 (2019-07-18)
* Initial release of the Anthro Survey Analyser.
