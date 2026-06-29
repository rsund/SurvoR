# Read Survo Binary Files

Reads a file in Survo (or Muste) binary format into a data frame.

## Usage

``` r
read.svo(file)
```

## Arguments

- file:

  a filename or URL as a character string.

## Details

If the filename appears to be a URL (of schemes `http:`, `ftp:` or
`https:`) the URL is first downloaded to a temporary file and then read.
(`https:` is only supported on some platforms.)

The variables in the Survo data set become the columns of the data
frame. Missing values are correctly handled. The variable labels, types
and lengths as well as data description and info are stored as
attributes of the data frame.

## Value

A data frame with attributes. These will include `"status.info"`,
`"status.description"`, `"status.varname"`, `"status.vartype"` and
`"status.varlen"`.

## Author

Reijo Sund
