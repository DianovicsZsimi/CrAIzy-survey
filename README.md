Data wrangling, analysis, and visualisation code for the 'Who Will Conduct Science? Who Will Conduct Science? Researchers’ Expectations of AI Capabilities and the Future of Scientific Work
' project

Repo structure:
- Data - contains data file that is anonymised and processed
- Plots_png - figures rendered in .png
- Tables - tables for the manuscript and supplement

In order to recreate the results, you need:
- R and R Markdown
- The code was written on Windows, using R version 4.6.1

Steps to recreate the results:

1. Run data_wrangling.Rmd (only works on raw files, which are not included!)
2. Run data_analysis.Rmd
3. Run Supplementary_Information.Rmd

Other files include:
results_text.Rmd - Used for minimising mistakes in copying results to the manuscript
visualisation_helper.R - R helper to make the figures look nice
