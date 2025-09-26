
<!-- README.md is generated from README.Rmd. Please edit that file -->

# bgmeter

<!-- badges: start -->

<!-- badges: end -->

The bgmeter package allows you to periodically collect measurements in
the background while your main R process continues running.

## Installation

You can install the development version of bgmeter from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("sales-lab/bgmeter")
```

## Example

This example shows how to start and stop background measurements using
`bgmeter`.

``` r
library(bgmeter)

# Define a dummy function to return some metrics.  Replace this with your desired function.
measure <- function() list(cpu = 75, memory = 1024)

# Create a temporary file to store the measurements.
filename <- tempfile("bgmeter", fileext = ".jsonl")

# Start the background measurement process, collecting metrics every second.
meter <- bgmeter_start(measure, 1L, filename)

# Let the process run for 2 seconds.
Sys.sleep(2L)

# Stop the background measurement process.
bgmeter_stop(meter)

# Load the metrics and print them.
metrics <- readLines(filename)
print(metrics)
#> [1] "{\"cpu\":[75],\"memory\":[1024]}" "{\"cpu\":[75],\"memory\":[1024]}"
#> [3] "{\"cpu\":[75],\"memory\":[1024]}"

# Clean up the temporary file.
unlink(filename)
```
