# Default target: ensure all dependencies are built and run nba_app.R
all: nba_app.R

# Target to generate specific logo files dynamically
logo/%.gif: logo_scraping.R
	Rscript logo_scraping.R

# Target to build the project HTML from the Quarto file
project.html: project.qmd
	quarto render project.qmd

# Target to run the NBA app, depending on the logos and HTML
nba_app.R: logo/team1.gif logo/team2.gif project.html
	Rscript nba_app.R

