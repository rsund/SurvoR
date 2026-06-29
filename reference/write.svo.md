# Write R Data Frame into Survo Binary File

Writes a data frame into Survo (or Muste) binary format.

## Usage

``` r
write.svo(dataf,svofile)
```

## Arguments

- dataf:

  R data frame object

- svofile:

  name for output file

## Details

The R data frame is copied to the given Survo data set. Missing values
and encodings are correctly handled.

## Value

A Survo data file.

## Author

Reijo Sund
