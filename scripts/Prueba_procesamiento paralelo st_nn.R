install.packages("nngeo")
require(nngeo)
require(parallel)


numCores <- detectCores()
numCores


data(cities)
data(towns)

cities = st_transform(cities, 32636)
towns = st_transform(towns, 32636)
water = st_transform(water, 32636)


# Large example - Polygons
set.seed(1)
n = 10000
x = data.frame(
  lon = (runif(n) * 2 - 1) * 70,
  lat = (runif(n) * 2 - 1) * 70
)
x = st_as_sf(x, coords = c("lon", "lat"), crs = 4326)
x = st_transform(x, 32630)
x = st_buffer(x, 1000000)
start_1 = Sys.time()
result1 = st_nn(x, x, k = 3)
end_1 = Sys.time()
end_1 - start_1


# Large example - Polygons - Parallel processing
start_2 = Sys.time()
result2 = st_nn(x, x, k = 3, parallel = 8)
end_2 = Sys.time()
end_2 - start_2
all.equal(result1, result2)