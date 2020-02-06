# Packages e funções para leitura e carregamento de imagens; 
# interpolações, predições, plots e etc.. 

# 1. Leitura/carregamento de imagens 
# 2. Summary of imagens
# 3. criação e manipulação de imagens 
# 4. Plots imagens 
# 5. geoStatistics 
# 6.

## 1. Leitura/carregamento de imagens


# rgdal (exemplos)
#Read/write between GDAL grid maps and Spatial objects
library(rgdal)
library(grid)
GDALinfo(system.file("external/test.ag", package="sp")[1]) # testar essa função 
x <- readGDAL(system.file("external/test.ag", package="sp")[1]) # read/load spatial data, o plot não funciona; spplot e image funcionam
class(x)
image(x)
summary(x)
x@data[[1]][x@data[[1]] > 10000] <- NA
summary(x)
image(x)


# readOGR {rgdal} (exemplos)
# Read OGR vector maps into Spatial objects

ogrDrivers() # informa quais são os formatos que podem ser lifod com a função OGR #reads the origin of the data source and creates an spatial object
dsn <- system.file("vectors", package = "rgdal")[1] # reads and stores the data file name in an object 
ogrListLayers(dsn) # reads and informs the names of layers  
ogrInfo(dsn=dsn, layer="cities") # information about a specific layer
owd <- getwd() # getwd retrieves the working directory ## importante para os testes 
setwd(dsn) # setwd sets the working directory ## importante para os testes
cities <- readOGR(dsn=dsn, layer="cities") # le os points da layer
plot(cities)
summary(cities)



ogrInfo(dsn=dsn, layer="kiritimati_primary_roads")
OGRSpatialRef(dsn=dsn, layer="kiritimati_primary_roads")# informa o sistema de referência de cordenadas
kiritimati_primary_roads <- readOGR(dsn=dsn, layer="kiritimati_primary_roads")
summary(kiritimati_primary_roads)
ogrInfo(dsn=dsn, layer="scot_BNG")
OGRSpatialRef(dsn=dsn, layer="scot_BNG")
scot_BNG <- readOGR(dsn=dsn, layer="scot_BNG")
summary(scot_BNG)
plot(scot_BNG)


#readShapeSpatial {maptools}	R Documentation
#Read shape files into Spatial*DataFrame objects

# melhor função para dados espaciais, por enquanto

library(maptools)
xx <- readShapeSpatial(system.file("shapes/sids.shp", package="maptools")[1],
                       IDvar="FIPSNO", proj4string=CRS("+proj=longlat +ellps=clrk66"))
summary(xx)
spplot(xx)

# raster {raster}	
# Create a RasterLayer object


# Create a RasterLayer object from a file
#   N.B.: For your own files, omit the 'system.file' and 'package="raster"' bits
#   these are just to get the path to files installed with the package

f <- system.file("external/test.grd", package="raster") # retrieving the filename from the file in the package dir
f
r <- raster(f)

logo <- raster(system.file("external/rlogo.grd", package="raster")) 

#from scratch
r1 <- raster(nrows=108, ncols=21, xmn=0, xmx=10)
summary(r1)
r1
#from an Extent object
e <- extent(r)
r2 <- raster(e)
r2
#from another Raster* object
r3 <- raster(r)
s <- stack(r, r, r)
r4 <- raster(s)
r5 <- raster(s, 3)
r3
r4
r5

# rasterFromXYZ {raster}	R Documentation
#Create a Raster* object from x, y, z values

#Create a Raster* object from x, y and z values. x and y represent spatial coordinates and must be on a regular grid

r <- raster(nrow=1000, ncol=1000, extent(t001), crs=NA)
r[] <- runif(ncell(r))
r[r<0.5] <- NA
xyz <- rasterToPoints(r)

r2 <- rasterFromXYZ(xyz) #Raster to point conversion. Cells with NA are not converted. A function can be used to select a subset of the raster cells (by their values).
plot(r2)
r4 <-dropLayer(r2,c(1))
## 3. criação/manipulação de imagens
# extent {raster}	
# Extent

r <- raster() # criou um raster com o default da função 
extent(r)
extent(c(0, 20, 0, 20))
#is equivalent to
extent(0, 20, 0, 20)
extent(matrix(c(0, 0, 20, 20), nrow=2))
x <- list(x=c(0,1,2), y=c(-3,5))
extent(x)
# gridded função para verificar se a matriz é grid ou não

x <- readGDAL(system.file("external/simple.ag", package="sp")[1])
class(x)
image(x)
summary(x)
x <- readGDAL(system.file("pictures/big_int_arc_file.asc", package="rgdal")[1])
summary(x)
cat("if the range is not 10000, 77590, your GDAL does not detect big\n")
cat("integers for this driver\n")
y = readGDAL(system.file("pictures/Rlogo.jpg", package = "rgdal")[1], band=1)
summary(y)
y = readGDAL(system.file("pictures/Rlogo.jpg", package = "rgdal")[1])
summary(y)
spplot(y, names.attr=c("red","green","blue"), 
       col.regions=grey(0:100/100),
       main="example of three-layer (RGB) raster image", as.table=TRUE)
data(meuse.grid)
gridded(meuse.grid) = ~x+y
proj4string(meuse.grid) = CRS("+init=epsg:28992")
fn <- tempfile()
writeGDAL(meuse.grid["dist"], fn)
GDALinfo(fn)
writeGDAL(meuse.grid["dist"], fn, setStatistics=TRUE)
GDALinfo(fn)
mg2 <- readGDAL(fn)
proj4string(mg2)
SP27GTIF <- readGDAL(system.file("pictures/SP27GTIF.TIF", 
                                 package = "rgdal")[1], output.dim=c(100,100))
summary(SP27GTIF)
image(SP27GTIF, col=grey(1:99/100))


# stack {raster}	R Documentation
# Create a RasterStack object

# file with one layer
fn <- system.file("external/test.grd", package="raster")
s <- stack(fn, fn)
r <- raster(fn)
extent(s)
s <- stack(r, fn)
nlayers(s)

# file with three layers
slogo <- stack(system.file("external/rlogo.grd", package="raster")) 
nlayers(slogo) # conta o número de layers
slogo


#brick {raster}	
#Create a RasterBrick object

b <- brick(system.file("external/rlogo.grd", package="raster"))
b
nlayers(b)
names(b)
extract(b, 870) # extrai valores de objetos dos layers

#rasterize {raster}	(exemplos)
#Rasterize points, lines, or polygons

#rasterize points
###############################
r <- raster(ncols=36, nrows=18)
n <- 1000
x <- runif(n) * 360 - 180 # These functions provide information about the uniform distribution on the interval from min to max. dunif gives the density, punif gives the distribution function qunif gives the quantile function and runif generates random deviates
y <- runif(n) * 180 - 90
xy <- cbind(x, y)
# get the (last) indices
r0 <- rasterize(xy, r)
plot(r0)
# prensence/absensce (NA) (is there a point or not?)
r1 <- rasterize(xy, r, field=1)
plot(r1)
# how many points?
r2 <- rasterize(xy, r, fun=function(x,...)length(x))
vals <- runif(n)
plot(r2)
# sum of the values associated with the points
r3 <- rasterize(xy, r, vals, fun=sum)


#subset Subset layers in a Raster* object

s <- stack(system.file("external/rlogo.grd", package="raster"))
sel <- subset(s, 2:3) # selecionando só o segundo e o terceiro layer

# Note that this is equivalent to
sel2 <- s[[2:3]]
# and in this particular case:
sel3 <- dropLayer(s, 1)
nlayers(s)
nlayers(sel)

# effect of drop=FALSE when selecting a single layer
sel <- subset(s, 2)
class(sel)
sel <- subset(s, 2, drop=FALSE)
class(sel)

#addLayer {raster} 
# Add or drop a layer

file <- system.file("external/test.grd", package="raster")
s <- stack(file, file, file)
r <- raster(file)
s <- addLayer(s, r/2, r*2) # add layers, in this case the layesr that are function of an raster
s
s <- dropLayer(s, c(3, 5)) # drop epecific layers 
nlayers(s)

#Unstack
#Description
#Create a list of RasterLayer objects from a RasterStack or RasterBrick
file <- system.file("external/test.grd", package="raster")
s <- stack(file, file)
list1 <- unstack(s)
b <- brick(s)
list2 <- unstack(b)

#merge {raster}	
#Merge Raster* objects

r1 <- raster(xmx=-150, ymn=60, ncols=30, nrows=30) # criou um raster 
r1[] <- 1:ncell(r1) # adiciona valores ás células 
r2 <- raster(xmn=-100, xmx=-50, ymx=50, ymn=30) # criou o segundo objeto 
res(r2) <- c(xres(r1), yres(r1)) # copiou a resolução do r1 para o r2
r2[] <- 1:ncell(r2) # adicionaou valores na célula
rm <- merge(r1, r2)
plot(r1)
plot(r2)
plot(rm)

# if you have many RasterLayer objects in a list
# you can use do.call:
x <- list(r1, r2) # create a list of raster objects 
# add arguments such as filename
# x$filename <- 'test.tif'
m <- do.call(merge, x) #do.call constructs and executes a function call from a name or a function and a list of arguments to be passed to it.

#mosaic {raster}	
# Merge Raster* objects using a function for overlapping areas

r <- raster(ncol=100, nrow=100)
r1 <- crop(r, extent(-10, 11, -10, 11)) # change the extent of the rasterfile
r2 <- crop(r, extent(0, 20, 0, 20))
r3 <- crop(r, extent(9, 30, 9, 30))

r1[] <- 1:ncell(r1) # adicionando valores ás células
r2[] <- 1:ncell(r2)
r3[] <- 1:ncell(r3)

m1 <- mosaic(r1, r2, r3, fun=mean)

s1 <- stack(r1, r1*2)
s2 <- stack(r2, r2/2)
s3 <- stack(r3, r3*4)
m2 <- mosaic(s1, s2, s3, fun=min)



# mask {raster}	
# Mask values in a Raster object


r <- raster(ncol=10, nrow=10)
m <- raster(ncol=10, nrow=10)
r[] <- runif(ncell(r)) * 10
m[] <- runif(ncell(r))
m[m < 0.5] <- NA
mr <- mask(r, m)
mr
plot(r)
plot(m)
plot(mr)
m2 <- m > .7
mr2 <- mask(r, m2, maskvalue=TRUE)
plot(mr2)

#  4. redict Spatial model predictions

# A simple model to predict the location of the R in the R-logo using 20 presence points 
# and 50 (random) pseudo-absence points. This type of model is often used to predict
# species distributions. See the dismo package for more of that.

# create a RasterStack or RasterBrick with with a set of predictor layers
logo <- brick(system.file("external/rlogo.grd", package="raster"))
names(logo)

## Not run: 
# the predictor variables

par(mfrow=c(2,2))
plotRGB(logo, main='logo')
plot(logo, 1, col=rgb(cbind(0:255,0,0), maxColorValue=255))
plot(logo, 2, col=rgb(cbind(0,0:255,0), maxColorValue=255))
plot(logo, 3, col=rgb(cbind(0,0,0:255), maxColorValue=255))
par(mfrow=c(1,1))
# End(Not run)

# known presence and absence of points
p <- matrix(c(48, 48, 48, 53, 50, 46, 54, 70, 84, 85, 74, 84, 95, 85, 
              66, 42, 26, 4, 19, 17, 7, 14, 26, 29, 39, 45, 51, 56, 46, 38, 31, 
              22, 34, 60, 70, 73, 63, 46, 43, 28), ncol=2) # matriz 2x2 

a <- matrix(c(22, 33, 64, 85, 92, 94, 59, 27, 30, 64, 60, 33, 31, 9,
              99, 67, 15, 5, 4, 30, 8, 37, 42, 27, 19, 69, 60, 73, 3, 5, 21,
              37, 52, 70, 74, 9, 13, 4, 17, 47), ncol=2) # matriz 2x2

# extract values for points
xy <- rbind(cbind(1, p), cbind(0, a))
v <- data.frame(cbind(pa=xy[,1], extract(logo, xy[,2:3]))) # criando um subset com dados da imagem para criar o modelo
v # as variáveis de respota têm o nome das layers
#build a model, here an example with glm 
model <- glm(formula=pa~., data=v)
summary(model)
#predict to a raster
r1 <- predict(logo, model, progress='text') # prediz valores para a layer baseado em dados prévios da imagem. 

plot(r1)
points(p, bg='blue', pch=21) # coloca os pontos utilizados para a predição 
points(a, bg='red', pch=21) 
r1 # raster resultado do modelo de predição 
plot(logo)

# principal components of a RasterBrick
# here using sampling to simulate an object too large
# too feed all its values to prcomp
sr <- sampleRandom(logo, 100) # legal isso, acho que vou precisar para os objetos de raster com muitas células
sr
pca <- prcomp(sr)
summary(pca)
library(pls)
loadings(pca)
biplot(pca)
# note the use of the 'index' argument
x <- predict(logo, pca, index=1:3) # colocou cada PC como uma layer 
plot(x)
# caralho, posso utilizar PCA para prever, muito bem!!
x
# partial least square regression
library(pls)
model <- plsr(formula=pa~., data=v)
summary(model)
# this returns an array:
predict(model, v[1:5,]) # interessante jeito de gerar dados para prever
# write a function to turn that into a matrix
pfun <- function(x, data) {
  y <- predict(x, data)
  d <- dim(y)
  dim(y) <- c(prod(d[1:2]), d[3])
  y
}

pp <- predict(logo, model, fun=pfun, index=1:3)
plot(pp) # muito bom também, mas muito complexo. 

# Random Forest

library(randomForest)
rfmod <- randomForest(pa ~., data=v)

## note the additional argument "type='response'" that is 
## passed to predict.randomForest
r3 <- predict(logo, rfmod, type='response', progress='window')


# com support vector machine
model <-ksvm(pa~red+green,data=v)
model
r4 <-predict(logo,model)
plot(r4)

model1 <-ksvm(pa~red+green,data=v,kernel="polydot")
model1
r5 <-predict(logo,model1)
spplot(r5)

model <-pcaNNet(v[,2:4],pa,data=v,size=3,decay=0.1)
model
r6 <-predict(logo,model)
plot(r6)
ktspplot(r6)

nnetFit <- train(v[,2:4],pa,
                 method = "pcaNNet",
                 preProcess = "range",
                 tuneLength = 2,
                 trace = FALSE,
                 maxit = 100)

nnetFit

model2 <-krls(X=v[,2:4],y=pa,sigma = 10)


model2
summary(model2)
r7 <-predict(logo,model2)
plot(r7)

model20 <- train(v[,2:4],pa,
                 method = "krlsRadial")
model20
warnings()
# Random Forest

library(randomForest)
rfmod <- randomForest(pa~., data=v)

## note the additional argument "type='response'" that is 
## passed to predict.randomForest
r3 <- predict(logo, rfmod, type='response', progress='window')




# interpolate {raster}	R Documentation
# Interpolate

# Make a RasterLayer with interpolated values using a fitted model object of classes such as 'gstat' (gstat package) or 'Krige' (fields package)

## Thin plate spline interpolation with x and y only
# some example data
r <- raster(system.file("external/test.grd", package="raster"))
ra <- aggregate(r, 10) # cria rasterlayer com menor número de células, maiores células
xy <- data.frame(xyFromCell(ra, 1:ncell(ra))) # extrai valores de coodenadas de cćlulas e cria um data.frame
v <- getValues(ra) # valores das células 

#### Thin plate spline model
library(fields) # package com os modelos de inerpolação
tps <- Tps(xy, v) # modelo de interpolação, input e output
p <- raster(r) # MUITO IMPORTANTE, criou um raster sem valores mas com as informações epaciais do raster original

# use model to predict values at all locations
p <- interpolate(p, tps) # usa o modelo de interpolação para criar os valores interpolados e colocalos como a layer 
p1 <- mask(p, r) # cria mask para plotar somoente os valores que estão no território original.

plot(p) # valores interpolados ocupando toda a extensão do raster
plot(p1)

## change the fun from predict to fields::predictSE to get the TPS standard error
se <- interpolate(p, tps, fun=predictSE) # standard error 
se <- mask(se, r)
plot(se)

## another variable; let's call it elevation
elevation <- (init(r, 'x') * init(r, 'y')) / 100000000
names(elevation) <- 'elev'
elevation <- mask(elevation, r)

z <- extract(elevation, xy)

# add as another independent variable
xyz <- cbind(xy, z)
tps2 <- Tps(xyz, v)
p2 <- interpolate(elevation, tps2, xyOnly=FALSE)

# as a linear coveriate
tps3 <- Tps(xy, v, Z=z)

# Z is a separate argument in Krig.predict, so we need a new function
# Internally (in interpolate) a matrix is formed of x, y, and elev (Z)

pfun <- function(model, x, ...) {
  predict(model, x[,1:2], Z=x[,3], ...)
}
p3 <- interpolate(elevation, tps3, xyOnly=FALSE, fun=pfun)
plot(p3)


# interpolate with inverse distance weighting 


r <- raster(system.file("external/test.grd", package="raster")) # sistema de cordenadas de referências em graus 
data(meuse) # data frame
mg <- gstat(id = "zinc", formula = zinc~1, locations = ~x+y, data=meuse,
nmax=13, set=list(idp = .5)) # locations são as colunas da longitude e latitude na forma de formula 
z <- interpolate(r, mg) # usou o raster carregado para interpolar os valores 
z <- mask(z, r)
plot(z)
r
head(r)


# interpolate with kriging
## kriging
coordinates(meuse) <- ~x+y
projection(meuse) <- projection(r)
## ordinary kriging
v <- variogram(log(zinc)~1, meuse)
m <- fit.variogram(v, vgm(1, "Sph", 300, 1))
gOK <- gstat(NULL, "log.zinc", log(zinc)~1, meuse, model=m)
OK <- interpolate(r, gOK)
# examples below provided by Maurizio Marchi
## universial kriging
vu <- variogram(log(zinc)~elev, meuse)
mu <- fit.variogram(vu, vgm(1, "Sph", 300, 1))
gUK <- gstat(NULL, "log.zinc", log(zinc)~elev, meuse, model=mu)
names(r) <- elev
UK <- interpolate(r, gUK, xyOnly=FALSE)




# rastertopoints
r <- raster(nrow=18, ncol=36)
r[] <- runif(ncell(r)) * 10
r[r>8] <- NA
p <- rasterToPoints(r)
p <- rasterToPoints(r, fun=function(x){x>6}) # pegou somente os pontos maiores que 6
plot(r) # plot raster
points(p) # plot the points retrivied before

## plot, raster package 

# RasterLayer
r <- raster(nrows=10, ncols=10)
r <- setValues(r, 1:ncell(r))
plot(r)

e <- extent(r)
plot(e, add=TRUE, col='red', lwd=4) # cria linh que contona o extent

e <- e / 2
plot(e, add=TRUE, col='red')

# Scatterplot of 2 RasterLayers
r2 <- sqrt(r)
plot(r, r2)
plot(r, r2, gridded=TRUE) # da plotar uma layer contra a outra

# Multi-layer object (RasterStack / Brick)
s <- stack(r, r2, r/r)
plot(s, 2)
plot(s)


# two objects, different range, one scale:
r[] <- runif(ncell(r))
r2 <- r/2
brks <- seq(0, 1, by=0.1) 
nb <- length(brks)-1 
cols <- rev(terrain.colors(nb))
par(mfrow=c(1,2))
plot(r, breaks=brks, col=cols, lab.breaks=brks, zlim=c(0,1), main='first') 
plot(r2, breaks=brks, col=cols, lab.breaks=brks, zlim=c(0,1), main='second') 

# breaks and labels, 
x <- raster(nc=10, nr=10)
x[] <- runif(ncell(x))
brk <- c(0, 0.25, 0.75, 1)
arg <- list(at=c(0.12,0.5,0.87), labels=c("Low","Med.","High"))
plot(x, col=terrain.colors(3), breaks=brk)
plot(x, col=terrain.colors(3), breaks=brk, axis.args=arg)
par(mfrow=c(1,1))

# color ramp
plot(x, col=colorRampPalette(c("red", "white", "blue"))(255))


# adding random points to the map
xy <- cbind(-180 + runif(10) * 360, -90 + runif(10) * 180)
points(xy, pch=3, cex=5)

# for SpatialPolygons do
 plot(pols, add=TRUE)

# adding the same points to each map of each layer of a RasterStack
fun <- function() {
  points(xy, cex=2)
  points(xy, pch=3, col='red')
}
plot(s, addfun=fun)


#spplot {raster}	
# Use spplot to plot a Raster* object

r <- raster(system.file("external/test.grd", package="raster"))
s <- stack(r, r*2)
names(s) <- c('meuse', 'meuse x 2')

spplot(s)

pts <- data.frame(sampleRandom(r, 10, xy=TRUE))
coordinates(pts) <- ~ x + y

spplot(s, scales = list(draw = TRUE), 
       xlab = "easting", ylab = "northing", 
       col.regions = rainbow(99, start=.1), 
       names.attr=c('original', 'times two'),
       sp.layout = list("sp.points", pts, pch=20, cex=2, col='black'),
       par.settings = list(fontsize = list(text = 12)), at = seq(0, 4000, 500))



#setValues {raster}	
#Set values of a Raster object

r <- raster(ncol=10, nrow=10)
vals <- 1:ncell(r)
r <- setValues(r, vals)
# equivalent to
r[] <- vals
r


#bind {raster}	
#Bind Spatial* objects

#Bind (append) Spatial* objects into a single object. All objects must be of the same vector type base class (SpatialPoints, SpatialLines, or SpatialPolygons)

if (require(rgdal) & require(rgeos)) {
  p <- shapefile(system.file("external/lux.shp", package="raster"))
  mersch <- p[p$NAME_2=='Mersch', ]
  diekirch <- p[p$NAME_2=='Diekirch', ]
  remich <- p[p$NAME_2=='Remich', ]
  remich$NAME_1 <- NULL
  x <- bind(mersch, diekirch, remich)
  plot(x)
  data.frame(x)
}



#compassRose compassRose Display a compass rose

compassRose(x,y,rot=0,cex=1)

# coordinates {sp}	R Documentation
# setdata(meuse.grid)

# data.frame
data(meuse.grid)
coordinates(meuse.grid) <- ~x+y
gridded(meuse.grid) <- TRUE
class(meuse.grid)
bbox(meuse.grid)
plot(meuse.grid)
head(meuse.grid)

data(meuse)
meuse.xy = meuse[c("x", "y")]
coordinates(meuse.xy) <- ~x+y
class(meuse.xy)
plot(meuse.xy)
meuse.rs <- raster(meuse.grid)
meuse.rs


## sp package

# gridded


# just 9 points on a grid:
x <- c(1,1,1,2,2,2,3,3,3)
y <- c(1,2,3,1,2,3,1,2,3)
xy <- cbind(x,y)
S <- SpatialPoints(xy) # tranformou matrix em spatial objetic
class(S)
plot(S)
gridded(S) <- TRUE # transformou em spatial pixels 
gridded(S) # verificou se é gridded
class(S)
summary(S)
plot(S) # diferença do primeiro plot, agora é um grid 
gridded(S) <- FALSE # transformou dde spatial pixels pra grid novamente 
gridded(S)
class(S)


# as.SpatialPolygons.GridTopology {sp}	R Documentation
# Make SpatialPolygons object from GridTopology object

data(meuse.grid)
gridded(meuse.grid)=~x+y
xx = spsample(meuse.grid,  type="hexagonal", cellsize=200)
xxpl = HexPoints2SpatialPolygons(xx)
image(meuse.grid["dist"])
plot(xxpl, add = TRUE)
points(xx, cex = .5)
## Not run: 
spplot(aggregate(meuse.grid[,1:3], xxpl), main = "aggregated meuse.grid")




data(meuse)
coordinates(meuse) <- c("x", "y") # promote to SpatialPointsDataFrame
bubble(meuse, "cadmium", maxsize = 2.5, main = "cadmium concentrations (ppm)",
       key.entries = 2^(-1:4))
bubble(meuse, "zinc", main = "zinc concentrations (ppm)",
       key.entries = 100 * 2^(0:4))

## gstat package 

# krige {gstat}
library(gstat)
library(sp)
data(meuse)
coordinates(meuse) = ~x+y
data(meuse.grid)
summary(meuse.grid)
gridded(meuse.grid) = ~x+y
meuse.grid
head(meuse.grid)
m <- vgm(.59, "Sph", 874, .04)
# ordinary kriging:
x <- krige(log(zinc)~1, meuse, meuse.grid, model = m)
spplot(x["var1.pred"], main = "ordinary kriging predictions")
spplot(x["var1.var"],  main = "ordinary kriging variance")
# simple kriging:
x <- krige(log(zinc)~1, meuse, meuse.grid, model = m, beta = 5.9)
# residual variogram:
m <- vgm(.4, "Sph", 954, .06)
# universal block kriging:
x <- krige(log(zinc)~x+y, meuse, meuse.grid, model = m, block = c(40,40))
spplot(x["var1.pred"], main = "universal kriging predictions")



library(maptools)
ago <- wrld_simpl[wrld_simpl@data$NAME == 'Angola',]

library(rgdal)
t1 <-readOGR("bacia_delim.shp",layer = "bacia_delim")
t2 <-raster(t1,ncol=25,nrow=25)
t3 <-rasterize(t1,t2)
t3
plot(t3)
head(coordinates(t3))
crs <-crs("+proj=longlat +datum=WGS84")

crs(t3) <-crs
# ajeitando um raster para fazer idw


rago <- raster(t3)
rago[] <- 1
rago
proj4string(rago) <- CRS(proj4string(t3))
r_ago <- mask(rago, t3)
r_ago[] <- 1
r_ago

plot(r_ago)
head(coordinates(r_ago))
df_r_ago <-as.data.frame(r_ago[,]) # transforma o raster em data frame
df_r_ago
grid_ago <-SpatialPointsDataFrame(
  coordinates(r_ago), data=df_r_ago,
  proj4string = CRS("+proj=longlat +datum=WGS84"))


grid_ago <- as(r_ago, 'SpatialPointsDataFrame')
grid_ago <- grid_ago[!is.na(grid_ago@data$layer), ]
gridded(grid_ago) <- TRUE
grid_ago

attach(MA)
df1 <-MA[,2:3]

tdata <- data.frame(x=rep(coordinates(r_ago)[,1], 3), 
                    y=rep(coordinates(r_ago)[,2], 3),
                    temp=runif(75, 12,35),
                    day = rep(1:3, each = 25))

coordinates(tdata) <- ~x+y 
proj4string(tdata) <- CRS(proj4string(t3))

idw_ago <- idw(temp ~ 1, tdata[tdata$day == 3, ], grid_ago, idp = 2.5,nmax=1)
spplot(idw_ago, "var1.pred")
points(temp)
