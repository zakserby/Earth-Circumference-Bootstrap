# Earth Circumference Monte Carlo

R script for AEM 6850 (Empirical Methods, Cornell, Fall 2025) that estimates the circumference of the Earth the way Eratosthenes did, from shadow measurements taken in Ithaca and Kingston, and compares a naive bootstrap with a block bootstrap.

script_files:
- earth_circumference_bootstrap.R : computes the sun angle in each city and a circumference estimate for each measurement, bootstraps the mean by resampling rows (naive) and by resampling whole teams (block), and plots both bootstrap distributions with their 95% intervals and the true value
- Earth-Circumference-Monte-Carlo.Rproj : RStudio project, open this first so the script finds data/ and output_figure/

data:
- Eratosthenes.csv : stick heights and shadow lengths measured by class teams in both cities on different days of the year

output_figure:
- bootstrap_distributions.png : naive and block bootstrap distributions

Other files:
- readme.rtf : full readme with general, methodological and data-specific information
