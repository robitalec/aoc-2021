
## Part one

You come across a field of hydrothermal vents on the ocean floor! These
vents constantly produce large, opaque clouds, so it would be best to
avoid them if possible.

They tend to form in lines; the submarine helpfully produces a list of
nearby lines of vents (your puzzle input) for you to review. For
example:

    0,9 -> 5,9
    8,0 -> 0,8
    9,4 -> 3,4
    2,2 -> 2,1
    7,0 -> 7,4
    6,4 -> 2,0
    0,9 -> 2,9
    3,4 -> 1,4
    0,0 -> 8,8
    5,5 -> 8,2

Each line of vents is given as a line segment in the format x1,y1 ->
x2,y2 where x1,y1 are the coordinates of one end the line segment and
x2,y2 are the coordinates of the other end. These line segments include
the points at both ends. In other words:

-   An entry like 1,1 -> 1,3 covers points 1,1, 1,2, and 1,3.
-   An entry like 9,7 -> 7,7 covers points 9,7, 8,7, and 7,7.

For now, only consider horizontal and vertical lines: lines where either
x1 = x2 or y1 = y2.

So, the horizontal and vertical lines from the above list would produce
the following diagram:

    .......1..
    ..1....1..
    ..1....1..
    .......1..
    .112111211
    ..........
    ..........
    ..........
    ..........
    222111....

In this diagram, the top left corner is 0,0 and the bottom right corner
is 9,9. Each position is shown as the number of lines which cover that
point or . if no line covers that point. The top-left pair of 1s, for
example, comes from 2,2 -> 2,1; the very bottom row is formed by the
overlapping lines 0,9 -> 5,9 and 0,9 -> 2,9.

To avoid the most dangerous areas, you need to determine the number of
points where at least two lines overlap. In the above example, this is
anywhere in the diagram with a 2 or larger - a total of 5 points.

Consider only horizontal and vertical lines. At how many points do at
least two lines overlap?

``` r
library(data.table)
library(sf)
```

    ## Linking to GEOS 3.9.1, GDAL 3.3.1, PROJ 8.0.1

``` r
eg_DT <- fread('data/day-5-example-lines.txt', colClasses = 'character', 
                             sep = '', header = FALSE)
DT <- fread('data/day-5-lines.txt', colClasses = 'character', 
                             sep = '', header = FALSE)
point_cols <- c('start', 'end')

prep_text_lines <- function(DT, point_cols) {
    DT[, (point_cols) := tstrsplit(V1, ' -> ')]
    DT[, (paste0(point_cols[1], c('_X', '_Y'))) := 
                tstrsplit(.SD[[1]], ',', type.convert = TRUE), .SDcols = point_cols[1]]
    DT[, (paste0(point_cols[2], c('_X', '_Y'))) := 
                tstrsplit(.SD[[1]], ',', type.convert = TRUE), .SDcols = point_cols[2]]
    
    no_diag <- DT[start_X == end_X | start_Y == end_Y]
    
    coords <- grep('start_|end_', colnames(no_diag), value = TRUE)
    setorderv(no_diag, coords)
}

dt_to_lines <- function(DT) {
    coords <- grep('start_|end_', colnames(DT), value = TRUE)

    st_as_sf(do.call(
        st_sfc,
        lapply(seq.int(nrow(DT)), function(i) {
            st_linestring(
                matrix(
                    as.matrix(
                        DT[i, .SD, .SDcols = coords]), ncol = 2, byrow = TRUE)
            )
        })
    ))
}
count_overlap <- function(lines, min_n) {
    # grd <- st_as_sf(st_make_grid(lines, cellsize = 1))
    # inter <- st_join(lines, grd, join = st_intersects)
    # sum(vapply(st_intersects(grd, lines), length, 42)  >= min_n)
    sum(vapply(st_intersects(lines, lines), length, 42)  >= min_n)
}

prep_eg_DT <- prep_text_lines(eg_DT, point_cols)
eg_lines <- dt_to_lines(prep_eg_DT)
count_overlap(eg_lines, 2)
```

    ## [1] 5

``` r
prep_DT <- prep_text_lines(DT, point_cols)
DT_lines <- dt_to_lines(prep_DT)
count_overlap(DT_lines, 2)
```

    ## [1] 311
