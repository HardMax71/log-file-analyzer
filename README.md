<div align="center">
<img src="https://raw.githubusercontent.com/HardMax71/log-file-analyzer/main/images/icon.png" alt="Logo" width="100" height="100">

# Log File Analyzer

A powerful Shiny application designed to parse, visualize, and analyze log files. It provides an intuitive interface for data exploration and visualization, making it easier to gain insights from complex log data.
</div>

## Features

- Parse various log file formats
- Interactive data visualization with multiple plot types
- Dynamic filtering and column selection
- Recent files quick access
- Responsive design with modern UI elements

## Screenshots

<div align="center">
  <table>
    <tr>
      <th>Main Interface</th>
      <th>Plot Example</th>
    </tr>
    <tr>
      <td><img src="https://raw.githubusercontent.com/HardMax71/log-file-analyzer/main/images/main_interface.png" alt="Main Interface" width="100%" style="display:block; margin:auto;"></td>
      <td><img src="https://raw.githubusercontent.com/HardMax71/log-file-analyzer/main/images/plot_example.png" alt="Plot Example" width="100%" style="display:block; margin:auto;"></td>
    </tr>
  </table>
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
