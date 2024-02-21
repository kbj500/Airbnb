---
title: "AirbnbData"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```



RUN ALL CHUNKS IN CHRONOLOGICAL ORDER!!



```{r data_read}
#read in data
air <- read.csv("listings 2.csv")
```
```{r data_clean}
#separate data for cleaning
air_clean <- air

# change host-since to years
host_years <- as.numeric((as.Date(as.character(air_clean$last_scraped), format="%Y-%m-%d")- as.Date(as.character(air_clean$host_since), format="%Y-%m-%d"))/365) # convert dates to days then divide by years set as numeric

air_clean$host_since <- host_years

names(air_clean)[names(air_clean) == 'host_since'] <- 'host_years'

air_clean <- air_clean[-c(1:11,14,19:20,22,27,29,35,49,50,55)] #drop web scraping ids + urls + host about + misc 

#convert host response/acceptance percentages to numeric
air_clean$host_response_rate <- as.numeric(sub("%", "", air_clean$host_response_rate))
air_clean$host_acceptance_rate <- as.numeric(sub("%", "", air_clean$host_acceptance_rate))

#convert price to numeric
air_clean$price <- as.numeric(gsub("\\$", "",gsub("\\..*","",air_clean$price)))

#remove NAs
air_clean <- na.omit(air_clean)

# change location of host to binary in or out of SF

air_clean$host_location[air_clean$host_location=="San Francisco, California, United States"] <- 1
air_clean$host_location[!(air_clean$host_location==1)] <- 0
air_clean$host_location <- as.numeric(air_clean$host_location)



# change first + last review to single column years between first and last review

yearsbetwn_firstlast_review <- as.numeric((as.Date(as.character(air_clean$last_review), format="%Y-%m-%d")- as.Date(as.character(air_clean$first_review), format="%Y-%m-%d"))/365)

air_clean$first_review <- yearsbetwn_firstlast_review

names(air_clean)[names(air_clean) == 'first_review'] <- 'yearsbetwn_firstlast_review'
air_clean = air_clean[-c(39)]
```

```{r dataconvert}

# change to has/pending license or doesnt have 

air_clean$license[air_clean$license==""] <- 0
air_clean$license[!(air_clean$license=="0")] <- 1
air_clean$license <- as.numeric(air_clean$license)

# change true falses to 1s and 0s for is_superhost, has_pic, _verified
boolean.integer <- function(arg1) {
  arg1[arg1=='t'] <- 1
  arg1[arg1=='f'] <- 0
  arg1 <- as.numeric(arg1)}

air_clean$host_is_superhost <- boolean.integer(air_clean$host_is_superhost)

air_clean$host_has_profile_pic <- boolean.integer(air_clean$host_has_profile_pic)

air_clean$host_identity_verified <-boolean.integer(air_clean$host_identity_verified)

air_clean$instant_bookable <-boolean.integer(air_clean$instant_bookable)


```
```{r data_dealingNAs}
library(gamlr)

air_clean <- naref(air_clean)

```
```{r data_amenities}
#converting each text in amenities column to a list with individual items list
air_clean$amenities <- lapply(strsplit(as.character(air_clean$amenities),
               "[][']|,\\s*"), function(x) x[nzchar(x)])

#creating the aggregated amenity list
am <- (air_clean$amenities[1])[[1]]
'%nin%' = Negate('%in%')
for(a in air_clean$amenities){
 nvars <- a %nin% am
 am <- c(am,a[nvars])
}

# removing everything but text fromm list
am_clean <- c()

for(i in am){
  i <- gsub('\"',"",gsub('\"',"",i))
  am_clean <- c(am_clean, i)
}

```
```{r data_hostverifications}
#converting each text in host verification to individual items list
air_clean$host_verifications <- lapply(strsplit(as.character(air_clean$host_verifications),
                "[][']|,\\s*"), function(x) x[nzchar(x)])

hv <- (air_clean$host_verifications[1])[[1]]
#'%nin%' = Negate('%in%')
for(a in air_clean$host_verifications){
 nvars <- a %nin% hv
 hv <- c(hv,a[nvars])
}

```
```{r amenities3}
genam <- c("Wifi", "Essentials", "Kitchen", "Heating", "Hangers", "TV", "Shampoo", "Hair dryer", "Air conditioning", "Iron", "Laptop friendly workspace","Washer","Dryer","Hot water","Bed linens","Private entrance","Lock on bedroom door","Cable TV","Extra pillows and blankets", "Private living room","Indoor fireplace", "Ethernet connection","Shower gel", "Pocket wifi")

t <- genam %in% am_clean

genam_new <- c(genam[t])

genam1 <- c("Refrigerator","Dishes and silverware","Microwave","Coffee maker","Free parking on premises","Stove","Cooking basics","Oven","Free street parking","Dishwasher","Patio or balcony","Garden or backyard","Pool","BBQ grill","Gym","Hot tub","Single level home","Breakfast","Paid parking off premises","Beach essentials","Paid parking on premises","Baking sheet")

genam_new <- c(genam_new,genam1)

t <- genam_new %in% am_clean
genam_new <- c(genam_new[t])



genam2 <- c("Bathtub","Room-darkening shades","High chair","Crib","Babysitter recommendations","Window guards","Baby bath","Pack ’n Play/travel crib","Children’s books and toys")

genam_new <- c(genam_new,genam2)

t <- genam_new %in% am_clean
genam_new <- c(genam_new[t])



genam3 <- c("Smoke detector","Carbon monoxide detector","Fire extinguisher","First aid kit","Lock on bedroom door","Cleaning before checkout")
genam_new <- c(genam_new,genam3)

t <- genam_new %in% am_clean
genam_new <- c(genam_new[t])

for (g in genam_new) {
  air_clean <- cbind(air_clean, g)
  names(air_clean)[names(air_clean) == "g"] <- g
}


```
```{r amenities4}

air_clean[53:106] <- 0
ac <- c()
acf <- list()

for (v in air_clean$amenities) {
  v <- gsub('\"',"",gsub('\"',"",v))
  acf <- append(acf,list(v))
}

air_clean$amenities <- acf
```
```{r amenities5}
x<-0
for (n in names(air_clean[53:106])) {
  for (v in air_clean$amenities) {
    x<-x+1
    #print(v)
    #print(n %in% v)
    if(n %in% v)
    {
      #print(air_clean[x,which(colnames(air_clean)==n)])
      air_clean[x,which(colnames(air_clean)==n)] <- 1
      #print(air_clean[x,which(colnames(air_clean)==n)])
    }
  }
  x <- 0
}




```
```{r verification 1}

for (h in hv) {
  air_clean <- cbind(air_clean, h)
  names(air_clean)[names(air_clean) == "h"] <-h
}
air_clean[107:122]<-0
```
```{r verification 2}
x<-0
for (n in names(air_clean[107:122])) {
  for (v in air_clean$host_verifications) {
    x<-x+1
    if(n %in% v)
    {
      air_clean[x,which(colnames(air_clean)==n)] <- 1
    }
  }
  x <- 0
}

#removing verification and amenities column lists since all converted
air_clean <- air_clean[-c(which(names(air_clean)=="amenities"), which(names(air_clean)=="host_verifications"))]
```

ALL CHUNKS ABOVE ARE FOR CLEANING DATA

ALL CHUNKS BELOW ARE FOR ANALYSIS AND MODELS

```{r boxplot main vars}
par(mfrow=c(2,3))
boxplot(air_clean$host_neighbourhood, main="Neighbourhood",col="red")
boxplot(air_clean$bedrooms,main="Bedrooms",col = "blue")
boxplot(air_clean$bathrooms_text,main="Bathrooms",col="green")
boxplot(air_clean$minimum_nights_avg_ntm,main="Avg Nights in Year", col="orange")
boxplot(air_clean$property_type,main="Property",col="brown")
                    
```



```{r analysis4}
library(ggplot2)

set.seed(321)

##create histograms 

datp <- data.frame(dens = c(air_clean$price), lines =rep(c("price")), each=3192)
ggplot(datp, aes(x = dens, fill = lines)) + geom_density(alpha = 0.5)+geom_vline(xintercept=mean(air_clean$price), size=1.5, color="red")

dathy <- data.frame(dens = c(air_clean$host_years), lines =rep(c("host years")), each=3192)
ggplot(dathy, aes(x = dens, fill = lines)) + geom_density(alpha = 0.5)+geom_vline(xintercept=mean(air_clean$host_years), size=1.5, color="red")



dat <- data.frame(dens = c(air_clean$host_response_rate, air_clean$host_acceptance_rate)
                   , lines = rep(c("host_response_rate","host_acceptance_rate"), each = 3192))
#Plot.
ggplot(dat, aes(x = dens, fill = lines)) + geom_density(alpha = 0.5)

```
```{r amenities distribution}
c <- colSums(air_clean[51:104])
barplot(c, main= "Amenities count",las=2)
```
```{r verification distribution}
w <- colSums(air_clean[105:120])
barplot(w,main= "Verification count", las=2)
```

```{r simple regression}
library(lattice)
par(mfrow=c(8,14),mai=c(rep(0.3,4)))
sr <- glm(price ~ review_scores_rating, data=air_clean)
summary(sr)
xyplot(price ~ review_scores_rating|host_neighbourhood, data=air_clean, 
  panel = function(x, y, ...) {
       panel.xyplot(x, y, ...)
       panel.lmline(x, y, col = "red")
  })
```
```{r simpleregression2}
sr2 <-glm(price~host_years+host_total_listings_count, data=air_clean)
summary(sr2)
library(lattice)
par(mfrow=c(8,14),mai=c(rep(0.3,4)))
xyplot(price ~ host_years+host_total_listings_count|property_type, data=air_clean, 
  panel = function(x, y, ...) {
       panel.xyplot(x, y, ...)
       panel.lmline(x, y, col = "red")
  })
```

```{r model}
#set 10% size of test set
test_set_size = floor(0.1*nrow(air_clean))

#set deterministic randomized number
set.seed(1234)

#filter data to sample
test_set_sample = sample(seq_len(nrow(air_clean)), size = test_set_size)

#assign test and training data removing country column
test_set =air_clean[test_set_sample,]
training_set =air_clean[-test_set_sample,]

X <- model.matrix(~., data=training_set)[,-1]

X =subset(X, select=c( -price))
```

```{r model1}
lass_price <- gamlr(X, training_set$price)

plot(lass_price)
```
```{r model2}
#set seed 0
set.seed(0)

#construct cv lasso with 10-fold cvs
cv.lass_price <- cv.gamlr(X, training_set$price, nfold = 10)

#plot cv lasso
plot(cv.lass_price)
```

```{r modelpredict}
#create another matrix for predicted values from test set
Xtest <- model.matrix(~., data=test_set)[,-1]
Xtest =subset(Xtest, select=c( -price))

#predict values from model with test set matrix
Xtest_pred_oos <- predict(cv.lass_price, newdata=Xtest)
```
```{r coefficients}
lass_price_coef <- drop(coef(cv.lass_price, select="min"))
l <- lass_price_coef[which(lass_price_coef!=0)]

#extract minimum and maximum coefficients
lcmax <- order(l, decreasing=TRUE)[1:5]
lcmin <- order(l, decreasing=FALSE)[1:5]

l[lcmax]
l[lcmin]
```
```{r rsquared1}
1 - cv.lass_price$cvm[cv.lass_price$seg.min]/cv.lass_price$cvm[1]
```
```{r rsquared12}
X_oosr2 <- 1-sum((test_set$price-Xtest_pred_oos)^2)/sum(((test_set$price-mean(test_set$price)))^2)

cat("The OOS R2 is",X_oosr2,".")
```
```{r modelbetter}
#create new matrix with training data including interaction from team and all other variables
Xnew <- model.matrix(~host_neighbourhood*.+property_type*., data=training_set)[,-1]
Xnew =subset(Xnew, select=c( -price))
#create corresponding model
new_las_price <- cv.gamlr(Xnew, training_set$price, nfold = 10)
plot(new_las_price)
```
```{r coefficients2}
newlass_price_coef <- drop(coef(new_las_price, select="min"))
l <- newlass_price_coef[which(newlass_price_coef!=0)]

#extract minimum and maximum coefficients
lcmax <- order(l, decreasing=TRUE)[1:5]
lcmin <- order(l, decreasing=FALSE)[1:5]

l[lcmax]
l[lcmin]
```
```{r plotnew}
1 - new_las_price$cvm[new_las_price$seg.min]/new_las_price$cvm[1]
```
```{r rsq2}

Xnewtest <- model.matrix(~host_neighbourhood*.+property_type*., data=test_set)[,-1]
Xnewtest =subset(Xnewtest, select=c( -price))
Xnewtest_pred_oos <- predict(new_las_price, newdata=Xnewtest)

Xnewtest_oosr2 <- 1-sum((test_set$price-Xnewtest_pred_oos)^2)/sum(((test_set$price-mean(test_set$price)))^2)

cat("The OOS R2 is",Xnewtest_oosr2,".")
```

