# Indian Job Market Analysis Dashboard

## Overview
This project is an interactive **Shiny application** designed to provide exploratory data analysis (EDA) of the Indian job market. It visualizes key trends such as the most in-demand skills, the distribution of job opportunities across major Indian cities, and the top hiring companies.

The application allows users to filter the analysis by city and adjust the number of top items displayed, offering dynamic and actionable insights for job seekers, recruiters, and market analysts.

##  Features

* **Interactive Dashboard:** A dynamic UI built with `bslib` and `flatly` theme for a modern look.
* **Skill Demand Analysis:** Bar chart visualizing the **Top N required skills** in the market (filterable by city).
* **Geographic Distribution:** Bar chart showing job opportunities across the **Top N cities** (fixed to global data for overall context).
* **Hiring Trends:** Table showing the **Top N hiring companies** (filterable by city).
* **Role-Specific Insights:** Dedicated table showing the **Top 7 Job Profiles** and the **Top 3 required skills** for each.
* **Raw Data Viewer:** A separate tab to browse and inspect the original, cleaned dataset.
* **Dynamic Filtering:** Use a dropdown to filter all primary visuals and tables by a specific Indian city.

## Setup and Installation

### Prerequisites

You must have **R** and **RStudio** installed.

### 1. Install Required Packages

The following R packages are required to run the application. You can install them using the following command in your R console:

```R
install.packages(c("shiny", "tidyverse", "DT", "plotly", "stringr", "bslib"))