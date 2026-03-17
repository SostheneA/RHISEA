# Print HISEA Summary Report to Console and/or File

Print HISEA Summary Report to Console and/or File

## Usage

``` r
print_hisea_summary(
  summary_report,
  nsamps,
  actual_proportions,
  means_vars = NULL,
  sds_vars = NULL,
  standard_sizes = NULL,
  stocks_names,
  output_file = "hisea_summary_report.txt",
  header = NULL,
  run_type = "SIMULATION",
  nv_used = NULL,
  seed_val = NULL,
  nmix_simulated = NULL
)
```

## Arguments

- summary_report:

  Output from `create_hisea_summary_report`.

- nsamps:

  Number of simulation/bootstrap replicates.

- actual_proportions:

  True composition vector.

- means_vars:

  Matrix of mean variable values.

- sds_vars:

  Matrix of SD of variable values.

- standard_sizes:

  Vector of baseline sample sizes.

- stocks_names:

  Character vector of stock names.

- output_file:

  Path to the output text file.

- header:

  Optional custom header line.

- run_type:

  Character string of run type.

- nv_used:

  Integer, number of variables.

- seed_val:

  Integer, seed value.

- nmix_simulated:

  Integer, size of simulated mixture.

## Value

\`data.frame\` summary statistics (invisibly)

## Examples

``` r
# 1. Prepare dummy summary data
nsamps <- 10; np <- 2
est_array <- array(runif(nsamps * np * 2), dim = c(nsamps, np, 2))
dimnames(est_array) <- list(NULL, c("Stock1", "Stock2"), c("RAW", "ML"))
rep <- create_hisea_summary_report(est_array, run_type = "SIMULATION")

# 2. Print to console (using a temp file to avoid clutter)
print_hisea_summary(
  summary_report = rep,
  nsamps = 10,
  actual_proportions = c(0.5, 0.5),
  stocks_names = c("Stock1", "Stock2"),
  output_file = tempfile(fileext = ".txt"),
  run_type = "SIMULATION"
)
#> PROGRAM HISEA (R Version)...EXECUTION DATE: 17-MAR-26       13:07:58
#> FUNCTION OF THIS RUN IS.........SIMULATION
#> #STOCKS IN THE MODEL............2
#> THE STOCKS ARE..................Stock1      Stock2      
#> NUMBER OF RUNS ANALYZED.......10
#> ACTUAL COMPOSITION IS...........  0.500  0.500
#> =======================================================================
#> =======================================================================
#> TABLE OF COMPOSITION ESTIMATE MEANS.  NUMBER OF RUNS =    10
#> STOCK         RAW            ML             
#> Stock1        0.55571        0.39778        
#> Stock2        0.47489        0.64419        
#> 
#> =======================================================================
#> TABLE OF COMPOSITION ESTIMATE STANDARD DEVIATIONS OVER THE    10   RUNS
#> STOCK         RAW            ML             
#> Stock1        0.31301        0.29967        
#> Stock2        0.35999        0.22617        
#> 
#> =======================================================================
#> THE COVARIANCE MATRIX OF THE  10 MAXIMUM LIKELIHOOD COMPOSITION ESTIMATES IS:
#> (POPULATIONS 1-2 * 1-2)
#> Stock1         8.9802e-02
#> Stock2         3.5923e-02  5.1153e-02
#> 
#> THE CORRESPONDING CORRELATION MATRIX IS:
#> (POPULATIONS 1-2 * 1-2)
#> Stock1            1.00000
#> Stock2            0.53002     1.00000
#> 
#> DETERMINANT OF ML COVARIANCE MATRIX:  3.30318e-03
#> 
#> FORTRAN STOP
#> R HISEA       job terminated at 17-MAR-26       13:07:58
#> HISEA formatted summary written to: C:\Users\SOSTHE~1\AppData\Local\Temp\1\RtmpQLetJq\filedc83d2e3f2b.txt 
```
