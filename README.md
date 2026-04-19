# missouri-deer-report
R Shiny dashboard used to show deer harvest numbers by county during Missouri's deer hunting season.

## Setup

1. Install R (>= 4.x)  
2. Install `renv` (if not already installed):

```r
install.packages("renv")
```

3. Clone this repository: 

```bash
git clone https://github.com/jbarry376/missouri-deer-report.git
```

4. Open the project in RStudio by clicking: 
```
missouri-deer-report.Rproj
```

5. Restore the project environment:
```r
renv::restore()
```

6. Run the app: 
```r
shiny::runApp()
```
