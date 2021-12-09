
## Part one

These caves seem to be lava tubes. Parts are even still volcanically
active; small hydrothermal vents release smoke into the caves that
slowly settles like rain.

If you can model how the smoke flows through the caves, you might be
able to avoid it and be that much safer. The submarine generates a
heightmap of the floor of the nearby caves for you (your puzzle input).

Smoke flows to the lowest point of the area it’s in. For example,
consider the following heightmap:

    2199943210
    3987894921
    9856789892
    8767896789
    9899965678

Each number corresponds to the height of a particular location, where 9
is the highest and 0 is the lowest a location can be.

Your first goal is to find the low points - the locations that are lower
than any of its adjacent locations. Most locations have four adjacent
locations (up, down, left, and right); locations on the edge or corner
of the map have three or two adjacent locations, respectively. (Diagonal
locations do not count as adjacent.)

In the above example, there are four low points, all highlighted: two
are in the first row (a 1 and a 0), one is in the third row (a 5), and
one is in the bottom row (also a 5). All other locations on the
heightmap have some lower adjacent location, and so are not low points.

The risk level of a low point is 1 plus its height. In the above
example, the risk levels of the low points are 2, 1, 6, and 6. The sum
of the risk levels of all low points in the heightmap is therefore 15.

Find all of the low points on your heightmap. What is the sum of the
risk levels of all low points on your heightmap?

``` r
library(data.table)
library(tinytest)

library(conflicted)
conflict_prefer('shift', 'data.table')
```

    ## [conflicted] Will prefer [34mdata.table::shift[39m over any other package

``` r
fill <- 10L

convert_to_DT <- function(text_file) {
    data.table(do.call(rbind, lapply(strsplit(
        readLines(text_file), ''
    ), as.integer)))
}

min_cols <- function(DT) {
    DT[, lapply(.SD, function(x) {
        x < shift(x, 1, fill = fill) & x < shift(x, -1, fill = fill)
    })]
}
min_rows <- function(DT) {
    t(data.table(t(DT))[, lapply(.SD, function(x) {
        x < shift(x, 1, fill = fill) & x < shift(x, -1, fill = fill)
    })])
}

find_low_points <- function(text_file) {
    DT <- convert_to_DT(text_file)
    min_c <- min_cols(DT)
    min_r <- min_rows(DT)
    data.table(height = unlist(DT)[which(min_c & min_r)],
                         which(min_c & min_r, arr.ind = TRUE))
}

expect_equal(
    find_low_points('data/day-9-example.txt')[, sum(height + 1)],
    15
)
```

    ## ----- PASSED      : <-->
    ##  call| eval(expr = expr, envir = envir)

``` r
low_heights <- find_low_points('data/day-9-heightmap.txt')
low_heights[, sum(height + 1)]
```

    ## [1] 489

## Part two

Next, you need to find the largest basins so you know what areas are
most important to avoid.

A basin is all locations that eventually flow downward to a single low
point. Therefore, every low point has a basin, although some basins are
very small. Locations of height 9 do not count as being in any basin,
and all other locations will always be part of exactly one basin.

The size of a basin is the number of locations within the basin,
including the low point. The example above has four basins.

The top-left basin, size 3:

    2199943210
    3987894921
    9856789892
    8767896789
    9899965678

The top-right basin, size 9:

    2199943210
    3987894921
    9856789892
    8767896789
    9899965678

The middle basin, size 14:

    2199943210
    3987894921
    9856789892
    8767896789
    9899965678

The bottom-right basin, size 9:

    2199943210
    3987894921
    9856789892
    8767896789
    9899965678

Find the three largest basins and multiply their sizes together. In the
above example, this is 9 \* 14 \* 9 = 1134.

What do you get if you multiply together the sizes of the three largest
basins?

``` r
library(terra)
```

    ## terra version 1.3.22

``` r
library(raster)
```

    ## Loading required package: sp

``` r
convert_to_terra <- function(text_file) {
    ls <- lapply(strsplit(readLines(text_file), ''), as.integer)
    r <- rast(nrow = length(ls),
                        res = c(1, 1),
                        ymin = 0, ymax = length(ls),
                        ncol = nchar(length(ls[[1]])),
                        xmin = 0, xmax = length(ls[[1]]))
    mat <- do.call(rbind, ls)
    values(r) <- mat
    r
}

area_of_clumps <- function(r) {
    r[r == 9] <- NA
    rc <- raster::clump(raster(r >= 0), directions = 4)
    DT <- data.table(values(rc))[, .N, V1][!is.na(V1)]
    setorder(DT, -N)
    DT
}

r <- convert_to_terra('data/day-9-example.txt')
area_of_clumps(r)[1:3][, Reduce(`*`, N)]
```

    ## [1] 1134

``` r
r <- convert_to_terra('data/day-9-heightmap.txt')
area_of_clumps(r)[1:3][, Reduce(`*`, N)]
```

    ## [1] 1056330

``` r
# lsm_p_area(landscape)
```
