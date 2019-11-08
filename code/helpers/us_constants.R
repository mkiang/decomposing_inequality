## Constants ----
## For convenience, make a dictionary that contain US divisions and
## regions. Note, D9 contains AK and HI, which we will remove to debug.
## Additionally, `us$remove` contains the FIPS of US territories we are not
## interested in.
us <- list(
    d1  = sprintf(fmt = '%02d', c(9, 23, 25, 33, 44, 50)),
    d2  = sprintf(fmt = '%02d', c(34, 36, 42)),
    d3  = sprintf(fmt = '%02d', c(18, 17, 26, 39, 55)),
    d4  = sprintf(fmt = '%02d', c(19, 20, 27, 29, 31, 38, 46)),
    d5  = sprintf(fmt = '%02d', c(10, 11, 12, 13, 24, 37, 45, 51, 54)),
    d6  = sprintf(fmt = '%02d', c(1, 21, 28, 47)),
    d7  = sprintf(fmt = '%02d', c(5, 22, 40, 48)),
    d8  = sprintf(fmt = '%02d', c(4, 8, 16, 35, 30, 49, 32, 56)),
    d9  = sprintf(fmt = '%02d', c(2, 6, 15, 41, 53)),
    d9b = sprintf(fmt = '%02d', c(6, 41, 53))
)   ## No AK or HI

us$r1 = c(us$d1, us$d2)
us$r2 = c(us$d3, us$d4)
us$r3 = c(us$d5, us$d6, us$d7)
us$r4 = c(us$d8, us$d9)

us$all    = c(us$r1, us$r2, us$r3, us$r4)
us$allb   = c(us$r1, us$r2, us$r3, us$d8, us$d9b)
us$remove = sprintf(fmt = '%02d', c(72, 78, 66, 69, 60))
