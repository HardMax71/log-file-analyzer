<div style="text-align:center;">
  # Log File Analyzer

  ![Log File Analyzer Icon](images/icon.png)

  A powerful Shiny application designed to parse, visualize, and analyze log files. It provides an intuitive interface for data exploration and visualization, making it easier to gain insights from complex log data.
</div>

## Features

- Parse various log file formats
- Interactive data visualization with multiple plot types
- Dynamic filtering and column selection
- Recent files quick access
- Responsive design with modern UI elements

## Screenshots

<div style="text-align:center;">
  | Main Interface | Plot Example | Filter Options |
  |:--------------:|:------------:|:--------------:|
  | ![Main Interface](screenshots/main_interface.png) | ![Plot Example](screenshots/plot_example.png) | ![Filter Options](screenshots/filter_options.png) |
</div>


## Installation

1.  Clone this repository:

```         
git clone https://github.com/HardMax71/log-file-analyzer.git
```

2.  Install the required R packages:

``` r
install.packages(c("shiny", "ggplot2", "dplyr", "lubridate", "shinydashboard", "shinyjs", "bslib", "plotly", "shinyWidgets"))
```

3.  Navigate to project directory:

```         
cd log-file-analyzer
```

4.  Run the Shiny app:

``` r
shiny::runApp()
```
