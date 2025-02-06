# Load required libraries
library(rvest)
library(dplyr)
library(purrr)

# URL of the website to scrape
url <- "https://www.sportslogos.net/teams/list_by_league/6/National_Basketball_Association/NBA/logos/" 

# Read the HTML content of the webpage
page <- read_html(url)

# Extract the required data
team_data <- page %>%
  html_elements("#team > ul > li > a") %>%  # Select the parent <a> tag
  map_df(~ {
    tibble(
      logo_url = .x %>% html_element("img") %>% html_attr("src"),  # Extract the image URL
      team_name = .x %>% html_text(trim = TRUE)                   # Extract the team name
    )
  })


past_team_data <- page %>%
  html_elements("#past > ul > li > a") %>%  # Select the parent <a> tag
  map_df(~ {
    tibble(
      logo_url = .x %>% html_element("img") %>% html_attr("src"),  # Extract the image URL
      team_name = .x %>% html_text(trim = TRUE)                   # Extract the team name
    )
  })

team_data = rbind(team_data,past_team_data)

# Print the extracted data
print(team_data)

# Download the logo images (optional)
output_folder <- "logos"
if (!dir.exists(output_folder)) {
  dir.create(output_folder)
  cat("Directory created:", output_folder, "\n")
} else {
  cat("Directory already exists:", output_folder, "\n")
}

for (i in seq_along(team_data$logo_url)) {
  file_name <- paste0(output_folder, "/", gsub(" ", "_", team_data$team_name[i]), ".gif")
  
  # Code from ChatGPT and HW4
  tryCatch({
    download.file(team_data$logo_url[i], file_name, mode = "wb")
    cat("Downloaded:", file_name, "\n")
  }, error = function(e) {
    cat("Failed to download:", team_data$logo_url[i], "\nError:", e$message, "\n")
  })
}

