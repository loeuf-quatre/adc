# This analysis examines San Francisco police data:
#
# Step 1: Tidy data
# Step 2: EDA
# Step 3: Reduce dimensionality
# Step 4: Define and map hotspots i.e. clustering and shapefiles
# Step 5: Collect IVs from external docs, scraping
# Step 6: Structural and dynamic analysis

# Housekeeping ----------------------------------------------------------------

# Packages
library(data.table)
library(dplyr)
library(factoextra)
library(ggplot2)
library(googleway)
library(grid)
library(inTrees)
library(KernSmooth)
library(leaflet)
library(prophet)
library(randomForest)
library(reshape2)
library(rvest)
library(sp)
library(timeDate)

# Working directory
setwd('C:/Users/edwar/Documents/apple')

# Read in endpoint dataset
pdi <- read.csv('./data/police-department-incidents.csv')

# Tidy data -------------------------------------------------------------------

names(pdi) <- tolower(names(pdi))

pdi$date <- as.Date(pdi$date)

pdi$yr <- format(pdi$date, '%Y')
pdi$mth <- format(pdi$date, '%m')
pdi$day <- format(pdi$date, '%d')
pdi$dow <- strftime(pdi$date, '%a')

pdi$time.block <- as.numeric(gsub(':', '', pdi$time))
pdi$time.block <- .bincode(pdi$time.block, seq(0, 2400, by = 100))
pdi$time.block <- sprintf('%02d', pdi$time.block)

pdi <- pdi %>% select(-location)

# EDA -------------------------------------------------------------------------

# Frequency of crimes by category
freq <- pdi %>%
  group_by(category) %>%
	summarize(
	  count.incidents = length(unique(incidntnum))
	) %>%
	arrange(-count.incidents) %>%
	mutate(
	  category = abbreviate(category, minlength = 12),
		category = factor(category, category)
	) %>%
	mutate(
	  total.incidents = sum(count.incidents), 
		per = count.incidents / total.incidents,
		per = round(per, 2),
		per = ifelse(per < .01, '<1%', scales::percent(per)))
	
caption <- paste0('SF Open Data collected between ', 
  format(min(pdi$date), '%b %Y'), 
	' and ', format(max(pdi$date), '%b %Y'))

p1 <- ggplot(freq, aes(x = category, y = count.incidents, 
  label = per)) + 
  geom_bar(stat = 'identity') +
	geom_text(vjust = -.5, size = 3) +
	scale_y_continuous(labels = scales::comma) +
	labs(
	  title = "So Petty",
		subtitle = 'Street theft comprises about 25% of all police incidents',
		caption = caption,
		x = 'Crime Category',
		y = 'Incidents Involving') +
	theme_minimal(base_size = 12) +
	theme(
	  axis.text.x = element_text(angle = 45, hjust = 1),
	  panel.grid.major = element_blank(), 
		panel.grid.minor = element_blank(),
    panel.background = element_blank())

ggsave(paste0(getwd(), '/shiny/images/p1.png'), 
  p1, dpi = 300, width = 12, height = 7, 
	units = 'in')

# Frequency of crimes by subcategory
fs <- pdi %>% 
  group_by(category, descript) %>%
  summarize(
	  count.incidents = length(unique(incidntnum))
	) %>%
	group_by(category) %>%
	arrange(-count.incidents) %>%
	top_n(n = 5) %>%
	arrange(category) %>%
	group_split(category)

# These aren't really crimes
nc <- pdi[pdi$category == 'NON-CRIMINAL', 'descript']
nc <- factor(nc)
table(nc)[order(-table(nc))]

pdi <- pdi %>% 
  filter(category != 'NON-CRIMINAL')
	
# Frequency of crimes by DOW and time
fdt <- pdi %>%
  group_by(dow, time.block) %>%
	summarize(
	  incidents = length(unique(incidntnum))
	) %>%
	group_by() %>%
	mutate(
	  dow = factor(dow, 
		c('Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat', 'Sun')),
		time.block = factor(time.block, 
		  unique(sort(time.block, decreasing = T)))
	)
p1b <- ggplot(fdt) +
  geom_tile(aes(x = dow, y = time.block, fill = incidents)) +
	scale_fill_gradient2(
	  name = 'Incidents',
		low = 'white', 
		high = 'red') +
	labs(
	  title = 'High Noon Saloon',
		subtitle = 'Officers anchor on certain times of day',
		x = 'Day of Week',
		y = 'Hour') +
	theme_minimal(base_size = 14) +
	theme(
		axis.ticks = element_blank(),
		panel.grid.major = element_blank(), 
		panel.grid.minor = element_blank(),
    panel.background = element_blank()
	)
ggsave(paste0(getwd(), '/shiny/images/p1b.png'), 
  p1b, dpi = 300, width = 7, height = 7, 
	units = 'in')
	
# Total incidents by time
ti <- pdi %>% 
  group_by(date) %>% 
	summarize(
	  incidents.total = length(unique(incidntnum))
	) %>%
	rename(ds = date, y = incidents.total)

p2 <- ggplot(ti) +
  geom_line(aes(x = ds, y = y)) +
	labs(
	  title = 'Incidents Time Series',
		subtitle = 'Count of police incidents remarkably steady over 15 years',
		x = 'Date', 
		y = 'Incidents') +
	theme_minimal(base_size = 16) +
	theme(
	  panel.grid.major = element_blank(), 
		panel.grid.minor = element_blank(),
    panel.background = element_blank())
	
# Break into components
m <- prophet(ti)
future <- make_future_dataframe(m, periods = 180)
forecast <- predict(m, future)
components <- data.frame(
  ds = as.Date(forecast$ds),
	trend = forecast$trend,
	weekly = forecast$weekly,
	yearly = forecast$yearly)
components$week <- lubridate::isoweek(components$ds)
components$yr <- format(components$ds, '%Y')
components$dow <- strftime(components$ds, '%a')
weekly <- components %>% 
  group_by(week, yr) %>% 
	mutate(obs = length(ds)) %>% 
	filter(obs == 7) %>% 
	select(-obs) %>% 
	head(7) %>%
	mutate(
	  dow = factor(dow, 
		c('Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat')))
p3 <- ggplot(components) + 
  geom_line(aes(x = ds, y = trend), size = 1.5) +
	labs(
	  x = 'Day',
		y = 'Trend') +
	theme_minimal(base_size = 16) +
	theme(
	  panel.grid.major = element_blank(), 
		panel.grid.minor = element_blank(),
    panel.background = element_blank())
	
p4 <- ggplot(weekly) +
  geom_line(aes(x = dow, y = weekly, group = 1), size = 1.5) +
	labs(
	  x = 'Day', 
		y = 'Weekly') +
	theme_minimal(base_size = 16) +
	theme(
	  panel.grid.major = element_blank(), 
		panel.grid.minor = element_blank(),
    panel.background = element_blank())
	
yearly <- components %>% 
  group_by(yr) %>% 
	mutate(obs = length(ds)) %>% 
	filter(obs == 365) %>% 
	select(-obs) %>% head(365)
p5 <- ggplot(yearly) + 
  geom_line(aes(x = ds, y = yearly), size = 1.5) +
	scale_x_date(date_labels = '%b') +
	labs(
	  x = 'Day',
		y = 'Yearly') +
	theme_minimal(base_size = 16) +
	theme(
	  panel.grid.major = element_blank(), 
		panel.grid.minor = element_blank(),
    panel.background = element_blank())

pushViewport(viewport(layout = grid.layout(2, 3)))
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
print(p2, vp = vplayout(1, 1:3))
print(p3, vp = vplayout(2, 1))
print(p4, vp = vplayout(2, 2))
print(p5, vp = vplayout(2, 3))

# Export p6 as PNG

# Total incidents by time and category
tic <- pdi %>% 
  group_by(date, category) %>% 
	summarize(
	  incidents.total = length(unique(incidntnum))
	) %>%
	rename(ds = date, y = incidents.total)
	
trendy <- function(x) {
  df <- tic[tic$category == x, ]
	m <- prophet(df)
	future <- make_future_dataframe(m, periods = 180)
  forecast <- predict(m, future)
	data.frame(ds = forecast$ds, 
	  y = forecast$trend, 
		category = x)
}

trends <- lapply(unique(tic$category), trendy)
trends <- plyr::ldply(trends, data.frame)
trends$category <- abbreviate(trends$category, minlength = 14)

p7 <- ggplot(trends) +
  geom_line(aes(x = as.Date(ds), y = y)) + 
	facet_wrap(~category, scales = 'fixed') +
	labs(
	  title = 'Stealing time',
		subtitle = 'Larceny is the biggest consumer of PD resources and is growing',
		caption = '',
		x = 'Date',
		y = 'Daily Incidents') +
	theme_minimal() +
	theme(
	  panel.grid.major = element_blank(), 
		panel.grid.minor = element_blank(),
    panel.background = element_blank())

ggsave(paste0(getwd(), '/shiny/images/p7.png'), 
  p7, dpi = 300, width = 12, height = 7, 
	units = 'in')
	
p8 <- p7 + 
  labs(
	  title = 'Undercover', 
	  subtitle = 'Normalized trends give better sense of variation') +
  facet_wrap(~category, scales = 'free_y')

ggsave(paste0(getwd(), '/shiny/images/p8.png'), 
  p8, dpi = 300, width = 12, height = 7, 
	units = 'in') 

# Define hot spots ------------------------------------------------------------

# Reduce dimensionality -----------------------------------
reddim <- pdi %>%
  filter(category != 'TREA') %>% # "Trespassing near industrial property"
	filter(address != '800 Block of BRYANT ST') %>% # Police HQ
  group_by(address, category) %>%
	summarize(
	  obs = length(unique(incidntnum))
	)
reddim <- reshape2::dcast(reddim, address ~ category)
reddim[is.na(reddim)] <- 0

# Correlation matrix
core <- cor(reddim[, -1])
core <- melt(core)

p9 <- ggplot(core) +
  geom_tile(aes(x = Var1, y = Var2, fill = value)) +
	scale_fill_gradient2(
	  name = 'Cor Coef',
		low = 'white', 
		high = 'orange') +
	labs(
	  title = 'See One, See All',
		subtitle = 'Ample collinearity among crime sites',
		x = '',
		y = '') +
	theme_minimal(base_size = 10) +
	theme(
	  axis.text.x = element_text(angle = 45, hjust = 1),
		axis.ticks = element_blank(),
		panel.grid.major = element_blank(), 
		panel.grid.minor = element_blank(),
    panel.background = element_blank()
	)
	
ggsave(paste0(getwd(), '/shiny/images/p9.png'), 
  p9, dpi = 300, width = 10, height = 7, 
	units = 'in')

# Varimax PCA
pca <- psych::principal(reddim[, -1], 
  rotate = 'varimax', 
	nfactors = 6, 
	scores = T)

loads <- data.frame(unclass(pca$loadings))
loads <- loads[, names(loads)[order(names(loads))]]
loads$category <- rownames(loads)
loads <- melt(loads)
loads <- loads %>% 
  arrange(desc(category)) %>%
  mutate(
	  category = factor(category, unique(category))
	)

p10 <- ggplot(loads) +
  geom_tile(aes(x = variable, y = category, fill = value)) +
	scale_fill_gradient2(
	  name = 'Loading',
	  low = 'white', 
		high = 'blue') +
	labs(
	  title = '',
		subtitle = '',
		x = '',
		y = ''
	) +
	theme_minimal(base_size = 14) +
	theme(
	  axis.ticks = element_blank(),
		panel.grid.major = element_blank(), 
		panel.grid.minor = element_blank(),
    panel.background = element_blank())

broke <- c('RC1', 'RC2', 'RC3', 'RC4', 'RC5', 'RC6')
woke <- c('Homeless', 'Despair', 'Theft', 'Property',
	  'Runaway', 'Seedy')
loads$variable <- plyr::mapvalues(loads$variable,
  from = broke,
	to = woke)
p10 <- p10 %+% loads # Update plot
p10 <- p10 + 
  labs(
	title = 'Birds of a Feather',
	subtitle = 'City hot spots tend to see certain crime combinations',
	x = 'New Variables', 
	y = 'Old Variables')

ggsave(paste0(getwd(), '/shiny/images/p10.png'), 
  p10, dpi = 300, width = 10, height = 7, 
	units = 'in') 

scores <- data.frame(pca$scores)
scores <- scores[, names(scores)[order(names(scores))]]
names(scores) <- plyr::mapvalues(names(scores),
  from = broke,
	to = woke)
scores_print <- data.frame(address = reddim$address,
  round(scores, 2))
scores_print <- head(scores_print, 50)
names(scores_print) <- tolower(names(scores_print))

core_new <- cor(scores)
core_new <- melt(core_new)
p11 <- p9 %+% core_new # Update plot
p11 <- p11 + 
  geom_text(aes(x = Var1, y = Var2, label = round(value, 2))) +
	labs(
	  title = 'Much Better',
		subtitle = 'New non-correlated variables capture ~70% of variance',
		x = '',
		y = ''
	) +
	theme_minimal(base_size = 14) +
	theme(
	  axis.ticks = element_blank(),
		panel.grid.major = element_blank(), 
		panel.grid.minor = element_blank(),
    panel.background = element_blank())
	
ggsave(paste0(getwd(), '/shiny/images/p11.png'), 
  p11, dpi = 300, width = 10, height = 7, 
	units = 'in') 
	
# Cluster each address's component loadings aka our new variables

# k-means scree plot
dfs <- scale(scores)
wss <- (nrow(dfs) - 1) * sum(apply(dfs, 2, var))
for (i in 2:15) {
	set.seed(35)
	wss[i] <- sum(kmeans(dfs, centers = i)$withinss)
}
wss <- data.frame(cluster = seq_along(wss), wss)

p12 <- ggplot(wss) +
  geom_point(aes(x = cluster, y = wss)) + 
	geom_line(aes(x = cluster, y = wss)) +
	scale_y_continuous(labels = scales::comma) +
	geom_vline(xintercept = 8, linetype = 2) +
	labs(
	  title = 'Eight is Great',
		subtitle = paste0('Eight clusters on new variable set',
		  'explain ~60% of their variance'),
		caption = '',
		x = 'Number of Clusters',
		y = 'Within Groups Sum of Squares') +
	theme_minimal(base_size = 14)
	
ggsave(paste0(getwd(), '/shiny/images/p12.png'), 
  p12, dpi = 300, width = 12, height = 7, 
	units = 'in') 
	
set.seed(35)
k <- kmeans(scale(scores), 8)

# Visualize clusters
centers <- melt(k$centers)
centers <- centers %>% 
  group_by(Var2) %>% 
	mutate(
	  value = ecdf(value)(value) # Normalize between 0 and 1
	)
	
p13 <- ggplot(centers) + 
  geom_bar(width = 1, aes(x = Var2, y = value, fill = Var2), 
	  color = 'white', stat = 'identity') +
	scale_y_continuous(breaks = 0:nlevels(centers$Var2)) +
	facet_wrap(~Var1) +
	coord_polar() + 
	labs(
	  title = 'Mug Shots',
		subtitle = ,
		caption = '',
		x = '',
		y = '') +
	theme_minimal(base_size = 14) +
	theme(
	  axis.ticks = element_blank(),
		axis.text = element_blank(),
		axis.line = element_blank(),
		legend.title = element_blank())

profiles_old <- unique(centers$Var1)
profiles_new <- c(
  'Vandals',
	'Mischief', 
  'Underbelly', 
	'Tragic', 
	'Low Grade', 
	'Vulnerable', 
	'Fallout',
	'Thieves')
centers$Var3 <- plyr::mapvalues(centers$Var1, 
  from = profiles_old, 
	to = profiles_new)
centers <- centers[order(centers$Var1), ]
centers$Var3 <- factor(centers$Var3, unique(centers$Var3))

p13 <- ggplot(centers) + 
  geom_bar(width = 1, aes(x = Var2, y = value, fill = Var2), 
	  color = 'white', stat = 'identity') +
	scale_y_continuous(breaks = 0:nlevels(centers$Var2)) +
	facet_wrap(~Var3) +
	coord_polar() + 
	labs(
	  title = 'Mug Shots',
		subtitle = paste0('Potential cluster descriptors include',
		  'Underbelly (S3), Tragic (S4) and *Vulnerable (S6)'),
		caption = paste0("*For example, areas labeled 'Vulnerable'",
  		"have relatively \nhigh incidence of runaways and prostitution"),
		x = '',
		y = '') +
	theme_minimal(base_size = 14) +
	theme(
	  axis.ticks = element_blank(),
		axis.text = element_blank(),
		axis.line = element_blank(),
		legend.title = element_blank())

ggsave(paste0(getwd(), '/shiny/images/p13.png'), 
  p13, dpi = 300, width = 12, height = 7, 
	units = 'in') 
	
# Press on with k-means
reddim$cluster <- k$cluster

# Some addresses have multiple coordinates associated
addies <- pdi %>% 
  group_by(address) %>% 
	summarize(
	  x = mean(x), 
		y = mean(y)
	)
reddim <- left_join(reddim, addies) %>% 
  filter(y < 40) # Mis-coded coordinates

# Shapefiling contour map for each hot spot profile
sf <- function(x) {
  df <- reddim[reddim$cluster == x, ]
  d2d <- bkde2D(cbind(df$x, df$y), bandwidth = c(0.00225, 0.00225))
  lines <- contourLines(x = d2d$x1, y = d2d$x2, z = d2d$fhat, nlevels = 8)
	
	# Get most concentrated contour's centroid
	macks <- lapply(lines, function(x) x[[1]])
	top2 <- unlist(macks)
	top2 <- tail(sort(unique(top2)), 2)
	macks <- which(unlist(macks) %in% top2)
	macks <- lines[macks]
	macks <- lapply(macks, function(x) {
	  data.frame(center.x = mean(x$x), 
		center.y = mean(x$y))
	  }
	)
	macks <- plyr::ldply(macks, data.frame)
	
	dd1 <- sapply(1:length(lines), 
	  function(i) Polygon(as.matrix(cbind(lines[[i]]$x, lines[[i]]$y))))
	dd2 <- sapply(1:length(lines), function(i) Polygons(list(dd1[[i]]), i))
	poly_data <- data.frame(Value = sapply(1:length(lines), 
	  function(i) lines[[i]]$level))
	dd3 <- SpatialPolygonsDataFrame(SpatialPolygons(dd2), data = poly_data)
	proj4string(dd3) <- CRS('+proj=longlat +datum=WGS84')
	dd_json <- spTransform(dd3, CRS('+init=epsg:4326'))
	values <- unique(sapply(1:length(lines),function(i) lines[[i]]$level))
	list(dd_json, macks)
}

# Save to R object for leaflet app
layer1 <- sf(1)[[1]]
layer2 <- sf(2)[[1]]
layer3 <- sf(3)[[1]]
layer4 <- sf(4)[[1]]
layer5 <- sf(5)[[1]]
layer6 <- sf(6)[[1]]
layer7 <- sf(7)[[1]]
layer8 <- sf(8)[[1]]

export <- list(layer1, layer2, layer3, layer4, 
  layer5, layer6, layer7, layer8, scores_print)
save(export, file = './shiny/export.RData')

cols <- c(
  'darkgreen',
	'red',
	'chartreuse',
	'orange',
	'cyan',
	'blue',
	'violet',
	'deeppink')

leaflet() %>%
	addTiles(group = 'Default') %>%
  addProviderTiles(providers$Stamen.TonerLite, group = 'Toner Lite') %>%
	addPolygons(data = layer1, stroke = F, color = cols[1], 
	  group = 'Vandals') %>%
	addPolygons(data = layer2, stroke = F, color = cols[2], 
	  group = 'Mischief') %>%
	addPolygons(data = layer3, stroke = F, color = cols[3], 
	  group = 'Underbelly') %>%
	addPolygons(data = layer4, stroke = F, color = cols[4], 
	  group = 'Tragic') %>%
	addPolygons(data = layer5, stroke = F, color = cols[5], 
	  group = 'Low Grade') %>%
	addPolygons(data = layer6, stroke = F, color = cols[6], 
	  group = 'Vulnerable') %>%
	addPolygons(data = layer7, stroke = F, color = cols[7], 
	  group = 'Fallout') %>%
	addPolygons(data = layer8, stroke = F, color = cols[8], 
	  group = 'Thieves') %>%
	addLayersControl(
    baseGroups = c('Toner Lite', 'Default'),
    overlayGroups = c(
		  "Vandals", 
			"Mischief", 
		  "Underbelly", 
			"Tragic", 
			"Low Grade", 
      "Vulnerable", 
			"Fallout",
			"Thieves"),
    options = layersControlOptions(collapsed = F)
  ) %>%
	hideGroup(c('Vandals', 'Tragic', 'Low Grade', 'Fallout', 'Thieves'))

# Revisit EDA ---------------------------------------------

# Trend by hot spot
# Join cluster data to incident data
dv <- inner_join(pdi, reddim[, c('address', 'cluster')], 
  by = 'address')
dv <- dv %>%
  group_by(cluster, date) %>%
	summarize(
	  activity = length(unique(incidntnum))
	) %>%
	rename(ds = date, y = activity) %>%
	filter(ds < '2018-05-01')

trendyNew <- function(x) {
  df <- dv[dv$cluster == x, ]
	m <- prophet(df)
	future <- make_future_dataframe(m, periods = 180)
  forecast <- predict(m, future)
	data.frame(ds = forecast$ds, 
	  y = forecast$trend, 
		cluster = x)
}
	
trends <- lapply(unique(dv$cluster), trendyNew)
trends <- plyr::ldply(trends, data.frame)
trends$cluster.name <- plyr::mapvalues(trends$cluster, 
  profiles_old, profiles_new)

p14 <- ggplot(trends) +
  geom_line(aes(x = as.Date(ds), y = y), size = 1.25) + 
	facet_wrap(~cluster.name, scales = 'fixed') +
	labs(
	  title = 'Cluster Trends',
		subtitle = "'Low Grade' crimes are plausibly the most numerous",
		caption = '',
		x = 'Date',
		y = 'Daily Incidents') +
	theme_minimal(base_size = 16) +
	theme(
	  panel.grid.major = element_blank(), 
		panel.grid.minor = element_blank(),
    panel.background = element_blank())

ggsave(paste0(getwd(), '/shiny/images/p14.png'), 
  p14, dpi = 300, width = 12, height = 7, 
	units = 'in')
	
p15 <- p14 + 
  facet_wrap(~cluster.name, scales = 'free_y')

ggsave(paste0(getwd(), '/shiny/images/p15.png'), 
  p15, dpi = 300, width = 12, height = 7, 
	units = 'in') 

# Collect independent variables -----------------------------------------------

# R -------------------------------------------------------

# Holidays ----------------------------
us <- listHolidays()[grepl('US', listHolidays())]

holidays <- lapply(us, function(x) {
  paste0(x, '(', format(min(pdi$date), '%Y'), 
	  ':', format(max(pdi$date), '%Y'), ')')
		})
holidays <- lapply(holidays, function(x) eval(parse(text = x)))
names(holidays) <- us
holidays <- plyr::ldply(holidays, data.frame, .id = 'holiday')
names(holidays)[2] <- 'date'
holidays <- dcast(holidays, date ~ holiday, length)
names(holidays) <- tolower(names(holidays))

# Moon phase --------------------------
moon_phase <- data.frame(date = pdi$date, 
  lunar.phase = lunar::lunar.phase(pdi$date, name = 4))
moon_phase <- unique(moon_phase)
moon_phase <- moon_phase %>%
  arrange(date)

# Docs ----------------------------------------------------

# Calls for service -------------------
pdc <- read.csv('./data/police-department-calls-for-service.csv')

# Unemployment rate
u3 <- read.csv('./data/sf-unemployment-rate.csv')
names(u3) <- c('date', 'rate')

u3$yr <- format(as.Date(u3$date), '%Y')
u3$mth <- format(as.Date(u3$date), '%m')
u3$mom <- c(NA, diff(u3$rate))
u3$yoy <- lag(u3$rate, 12)
u3$yoy <- u3$rate - u3$yoy

# Weather
wx <- read.csv('./data/sf-wx-daily.csv', header = T)

# API -----------------------------------------------------

# Google Maps -------------------------

# Zip codes of each hot spot's centroids
layers <- unique(reddim$cluster)
zips <- lapply(layers, function(x) paste0("sf(", x, ")[[2]]"))
zips <- lapply(zips, function(x) eval(parse(text = x)))
names(zips) <- layers
zips <- plyr::ldply(zips, data.frame, .id = 'hot.spot')
vips <- apply(zips, 1, function(x) c(x[[3]], x[[2]]))

googZips <- function(x) {
	g <- google_reverse_geocode(location = as.numeric(x),
		result_type = c('postal_code'),
		key = 'AIzaSyAwZZtedMUbn77Ivst1bn0vkyxp6PTGMJs',
		simplify = TRUE)
	gsub('[^0-9]', '', g$results$formatted_address)
}

zips$zip <- apply(vips, 2, googZips)

# Potential structural crime determinants
points_interest <- c('bar', 'bus stop', 'homeless shelter', 
  'hospital', 'hotel', 'mall', 'park', 'pawn shop', 
	'police station', 'place of worship', 'school'
	)
	
nearest <- expand.grid(unique(zips$zip), points_interest)
nearest <- apply(nearest, 1, paste, collapse = ' ')

sites <- function(x) {
  tryCatch({
		Sys.sleep(5)
		goog <- google_places(
			search_string = x,
			key = 'shhhhh')
		types <- goog$results$types
		types <- lapply(types, function(x) paste0(x, collapse = ' | '))
		data.frame(
			address = goog$results$formatted_address,
			lat = goog$results$geometry$location$lat,
			lng = goog$results$geometry$location$lng,
			name = goog$results$name,
			type = unlist(types),
			total.ratings = goog$results$user_ratings_total,
			search.string = x)}, error = function(c) NA)
}

latlngs <- lapply(nearest, sites)
latlngs <- plyr::ldply(latlngs, data.frame)
latlngs <- latlngs %>% filter(total.ratings > 0)
latlngs <- unique(latlngs)

# Distance in km between centroids and determinants
dm <- geosphere::distm(zips[, c('center.x', 'center.y')], 
  latlngs[, c('lng', 'lat')]) / 1000
dm <- data.frame(dm)
names(dm) <- paste0(gsub('[0-9]', '', latlngs$search.string), 
  ' | ', latlngs$name)
dm$hot.spot <- zips$hot.spot
dm <- melt(dm, id.vars = 'hot.spot')
dm$determ <- gsub(' \\|.*', '', dm$variable)

dm <- dm %>% 
  group_by(hot.spot, determ) %>% 
	mutate(rnk = rank(value)) %>% 
	filter(rnk <= 3) %>% 
	arrange(hot.spot) %>% 
	summarize(m = median(value))

goog <- dm %>%
  mutate(determ = gsub(' ', '.', trimws(determ))) %>%
  tidyr::spread(determ, m) %>%
	rename_all(tolower) %>%
	data.frame
	
# Scraping ------------------------------------------------
	
# Giants games ------------------------
bb <- 'https://www.baseball-reference.com/teams/SFG/'

years <- seq(format(min(pdi$date), '%Y'), 
  format(max(pdi$date), '%Y'), by = 1)

giants <- function(x) {
	query <- paste0(bb, x, '-schedule-scores.shtml#all_team_schedule')
	query <- url(query, 'rb')
	results <- read_html(query)
	xp <- '//td'
	nodes <- results %>% 
		html_nodes(xpath = xp)

	# Index of variables
	date_game <- which(grepl('date_game', nodes %>% html_attrs()))
	home <- which(grepl('homeORvis', nodes %>% html_attrs()))
	ranking <- which(grepl('rank', nodes %>% html_attrs()))
	result <- which(grepl('win_loss_result', nodes %>% html_attrs()))
	day_night <- which(grepl('day_or_night', nodes %>% html_attrs()))

	raw_text <- nodes %>% html_text
	data.frame(
		dg = raw_text[date_game],
		home = raw_text[home],
		ranking = raw_text[ranking],
		wl = raw_text[result],
		dn = raw_text[day_night])
}

sfg <- lapply(years, giants)
names(sfg) <- years
sfg <- plyr::ldply(sfg, data.frame, .id = 'year')
sfg$dg <- gsub(' \\(.*', '', sfg$dg)
sfg$dg <- as.Date(paste0(sfg$dg, ',', sfg$year), '%A, %b %d,%Y')

# Zip code demos ----------------------
usps <- 'https://www.unitedstateszipcodes.org/'

z <- unique(zips$zip)

zeeps <- function(x) {
	Sys.sleep(5)
	query <- paste0(usps, x)
	query <- url(query, 'rb')
	results <- read_html(query)
	xp <- '//th | //td | //*[contains(concat( " ", @class, " " ), concat( " ", "value", " " ))]'
	nodes <- results %>% 
		html_nodes(xpath = xp)
	rendered <- nodes %>%
	  html_text() %>% 
		trimws
	pop <- rendered[14]
	home.value <- rendered[23]
	area <- rendered[26]
	age.m <- rendered[41]
	age.f <- rendered[44]
	children <- rendered[104]
	work.ft <- rendered[592]
	df <- data.frame(
	  zip = x,
		pop,
		home.value,
		area,
		age.m,
		age.f,
		children,
		work.ft)
	df <- sapply(df, function(x) as.numeric(tidyr::extract_numeric(x)))
}

locales <- lapply(z, zeeps)
locales <- do.call(rbind, locales)
locales <- data.frame(locales)
locales$pop.density <- locales$pop / locales$area
locales$children.p <- locales$children / locales$pop
locales$work.ft.p <- locales$work.ft / locales$pop
locales <- locales %>%
  select(-c(pop, children, work.ft, area))

# Synthesize analytical dataset ---------------------------

# Structural variables ----------------
sv <- left_join(goog, zips, by = 'hot.spot')
sv$zip <- as.numeric(sv$zip)
sv <- left_join(sv, locales, by = 'zip')
sv <- sv %>% 
  select(-c(center.x, center.y, zip))
sv <- melt(sv)
sv <- sv %>% 
	group_by(hot.spot, variable) %>% 
	summarize(
		m = median(value)
	)

diss <- sv %>% 
  group_by(variable) %>% 
	mutate(a.m = median(m), rel = m - a.m) %>% 
	arrange(hot.spot, -abs(rel)) %>% 
	filter(variable %in% names(goog)) %>%
	group_by()

vars <- as.character(diss$variable)
vars <- unique(rev(vars[order(vars)]))
diss$variable <- factor(diss$variable, vars)
diss$hot.spot.name <- plyr::mapvalues(diss$hot.spot, 
  profiles_old, profiles_new)
diss$hot.spot.name <- as.character(diss$hot.spot.name)
diss <- diss[order(diss$hot.spot.name), ]
diss$hot.spot.name <- factor(diss$hot.spot.name, 
  unique(diss$hot.spot.name))

p16 <- ggplot(diss, aes(x = variable, y = rel)) + 
  geom_point(size = 3) + 
	geom_hline(yintercept = 0, linetype = 2) +
	scale_y_continuous(labels = scales::unit_format(unit = 'km')) +
	labs(
	  title = 'Landmarks',
		subtitle = paste0('Vulnerable zones are close to everything,',
		  'Underbelly are intriguingly closer to hotels'),
		caption = 'Dashed line indicates average distance across zones',
		x = '',
		y = 'Closer \u2190 \u0394 Average Distance \u2192 Farther') +
	coord_flip() + 
	facet_wrap(~hot.spot.name) + 
	theme_minimal(base_size = 14) +
	theme(panel.spacing.x = unit(7, "mm"))
	
ggsave(paste0(getwd(), '/shiny/images/p16.png'), 
  p16, dpi = 300, width = 12, height = 10, 
	units = 'in')
	
demo <- sv %>% 
  filter(!variable %in% names(goog)) %>%
	arrange(variable)
	
vars <- as.character(demo$variable)
vars <- unique(rev(vars[order(vars)]))
demo$variable <- factor(demo$variable, rev(vars))
demo$hot.spot.name <- plyr::mapvalues(demo$hot.spot, 
  profiles_old, profiles_new)
demo$hot.spot.name <- as.character(demo$hot.spot.name)
demo <- demo[order(demo$hot.spot.name), ]
demo$hot.spot.name <- factor(demo$hot.spot.name, unique(demo$hot.spot.name))
	
p17 <- ggplot(demo, aes(x = hot.spot.name, y = m)) + 
  geom_bar(stat= 'identity') + 
	facet_wrap(~variable, scales = 'free') + 
	labs(
	  title = 'Demos',
		subtitle = paste0('Children overrepresented in Tragic,', 
		  'Underbelly and Vulnerable zones'),
		x = 'Cluster',
		y = 'Median Value') +
	theme_minimal(base_size = 16) +
	theme(axis.text.x = element_text(angle = 45, hjust = 1))
	
ggsave(paste0(getwd(), '/shiny/images/p17.png'), 
  p17, dpi = 300, width = 12, height = 10, 
	units = 'in')

# Dynamic variables -------------------

# Join cluster data to incident data
dv <- inner_join(pdi, reddim[, c('address', 'cluster')], 
  by = 'address')
dv <- dv %>%
  group_by(cluster, date) %>%
	summarize(
	  activity = length(unique(incidntnum))
	)
	
all_dates <- expand.grid(
  cluster = unique(dv$cluster), 
	date = unique(dv$date))
dv <- left_join(all_dates, dv)
dv[is.na(dv)] <- 0
dv$yr <- format(dv$date, '%Y')
dv$mth <- format(dv$date, '%m')
dv$day <- format(dv$date, '%d')
dv$day <- ifelse(dv$day < 15, 'front half', 'back half')
dv$dow <- strftime(dv$date, '%a')

# Holidays
holidays$date <- as.Date(holidays$date)
dv <- left_join(dv, holidays)
dv[is.na(dv)] <- 0

# Moon phase
dv <- left_join(dv, moon_phase)

# Unemployment
dv <- left_join(dv, 
  u3[, c('yr', 'mth', 'mom', 'yoy')], 
	by = c('yr', 'mth'))

# Weather
wx$dates <- as.Date(as.character(wx$dates))
wx$station <- as.character(wx$station)

dv <- left_join(dv, wx, by = c('date' = 'dates'))
dv[is.na(dv)] <- 'Unknown'
dv$temps <- as.numeric(dv$temps)
dv$precip <- as.numeric(dv$precip)

dv <- dv %>%
  data.frame

# Giants
sfg <- sfg %>% 
  group_by(dg, home) %>% 
	summarize( # Double-headers
	  ranking = max(as.character(ranking)), 
		wl = max(as.character(wl)), 
		dn = max(as.character(dn))
	) %>% 
		filter(home != '@') %>% # Home games
		select(-home)
dv <- left_join(dv, sfg, by = c('date' = 'dg'))

dv[is.na(dv$ranking), 'ranking'] <- 'No home game'
dv[is.na(dv$wl), 'wl'] <- 'No home game'
dv[is.na(dv$dn), 'dn'] <- 'No home game'

# Model -----------------------------------------------------------------------

rafo <- function(y) {
		x <- dv[dv$cluster == y, ]

		indies <- names(dv)[-c(1:4)]
		indies <- indies[!indies %in% 'station']
		chars <- names(x[, sapply(x, class) == 'character'])
		x[chars] <- lapply(x[chars], factor)
		f <- as.formula(paste0('activity ~ ', 
			paste0(indies, collapse = ' + ')))
		fit <- randomForest(f, x[!is.na(x$precip), ])

		treeList <- RF2List(fit)
		rules <- extractRules(treeList,
			X = x[, indies],
			digits = 2,
			maxdepth = 6)
		rules <- unique(rules)

		rulesOutcome <- getRuleMetric(rules,
			x[, indies],
			x$activity)

		rulesPruned <- pruneRule(rulesOutcome, x[, indies], x$activity)

		rulesReadable <- presentRules(rulesPruned, paste0('x$', indies))

		rr <- rulesReadable

		rr <- data.frame(rr)

		rr$freq <- as.numeric(as.character(rr$freq))
		rr$pred <- as.numeric(as.character(rr$pred))

		rr <- rr %>% arrange(-pred)

		rr %>% 
			filter(freq >= (5 / nrow(x)))
}

rules <- lapply(unique(dv$cluster), rafo)
names(rules) <- unique(dv$cluster)
rules <- plyr::ldply(rules, data.frame)
