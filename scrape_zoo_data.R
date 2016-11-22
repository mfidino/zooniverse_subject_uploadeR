
dat <- read.csv("chicago-wildlife-watch-classifications.csv", header = TRUE,
                stringsAsFactors = FALSE)

dim(dat)

library(jsonlite)
library(parallel)

test <- dat$subject_data[1000]




# function to apply to the subject data
sdat_fn <- function(x) {
  return(t(data.frame(unlist(fromJSON(x)[[1]], use.names= TRUE))))
}


a2 <- Sys.time()
test <- mclapply(dat$subject_data, sdat_fn)
b2 <- Sys.time() - a2
longshot <- data.frame(plyr::rbind.fill.matrix(test), stringsAsFactors = FALSE)

metadat <- vector("list", length = 10)
metadat <- foreach(i = c(6676:6686), .packages = 'jsonlite') %dopar% {
  
  # change this bit here!
  
metadat<- vector("list", length = nrow(dat))
for(i in 1:nrow(dat)){
  slist <- fromJSON(dat$annotations[[i]], simplifyVector = FALSE)
  b <- unlist(lapply(slist[[1]]$value, function(x) x$choice))
  if(is.null(b)) b <- NA
  a <- sapply(lapply(slist[[1]]$value, function(x) x$answers), function(x) as.numeric(as.character(x[1])))
  a[is.na(a)] <- 1
  if(length(a)==0) a <- NA
  metadat[[i]] <- data.frame(species = b, n = a, id = longshot$id[i], 
             nviews = longshot$retired.classifications_count[i],
             usrnm = dat$user_name[i], usrid = dat$user_id[i],
             gs = dat$gold_standard[i], stringsAsFactors = FALSE)
}


test <- plyr::rbind.fill(metadat)

t2 <- test[-which(is.na(test$species) | is.na(test$id)),]
t2$id <- factor(t2$id)
nvotes <- t2 %>% group_by(id) %>% summarise(nv = length(unique(usrnm)))

s1 <- t2 %>% group_by(id, usrnm) %>% summarise(sp = unique(species))


ack <- s1 %>% group_by(id) %>% summarise(ml = names(sort(table(sp), decreasing = TRUE))[1],
                                                  times = sort(table(sp), decreasing = TRUE)[1] ) %>% 
  data.frame

joined.dat <- left_join(nvotes, ack, by = "id")
joined.dat

length(unique((t2$usrnm[t2$id=="9074"])))
nrow(t2[t2$id=="9074",])
sapply(a, function(x) x[1])
lapply(slist, function(x) x$value$answers)

t(unlist(unlist(slist, recursive = FALSE)))

tc <- sapply(metadat, ncol)


hist(t(t(sort(table(t2$usrnm)))))

mm <- plyr::rbind.fill.matrix(metadat)
mm <- data.frame(mm)
# get only T0
mm <- mm[grep("T0", mm$task),]
usre <- t(t(sort(table(t2$usrnm))))

nen <- table(usre)

plot(c(cumsum(nen)/3426) ~ as.numeric(names(nen)), type = "p",
     xlab = "number of photos tagged",
     ylab = "cumulative proportion of users")
abline(v = 15)
table(usre)
ack2 <- Sys.time() - ack2

longshot <- plyr::rbind.fill.matrix(hunch)
longshot <- plyr::rbind.fill(lapply(slist[[1]], function(x) do.call("data.frame", as.list(x))))
