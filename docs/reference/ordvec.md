# Generate Ordered Random Values (Order Statistics based)

Produces an ordered vector of \`num\` random values, scaled to be
approximately between 0 and \`upper\`. This function can be used as a
basis for certain types of random sampling schemes. The logic is similar
to generating order statistics from an exponential distribution.

## Usage

``` r
ordvec(num, upper)
```

## Arguments

- num:

  Integer, the number of ordered values to generate.

- upper:

  Numeric, the maximum value for the scaled output. The generated values
  will be in the approximate range (0, \`upper\`\].

## Value

A numeric vector of length \`num\`, containing ordered values scaled
approximately between 0 and \`upper\`.

## Examples

``` r
ordvec(5, 100) # Generates 5 ordered numbers, max value around 100
#> [1] 11.26068 12.84343 59.53728 60.45093 87.66352
ordvec(10, 10) # Generates 10 ordered numbers, max value around 10
#>  [1] 0.1673624 2.3265509 2.8496037 3.0698628 3.7485101 4.2327754 5.5300368
#>  [8] 8.3644747 8.3895806 9.7954295
```
