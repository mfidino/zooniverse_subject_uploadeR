
dat <- read.csv("chicago-wildlife-watch-classifications.csv", header = TRUE,
                stringsAsFactors = FALSE)

dim(dat)

library(jsonlite)

test <- dat$subject_data[1000]







slist <- vector("list", length = length(dat$subject_data))
no_cores <- detectCores() - 1
cl<-makeCluster(no_cores)
registerDoParallel(cl)

ln <- nchar(dat$annotations)

lotsa <- which(ln > 200)

fromJSON(dat$annotations[lotsa[9]], simplifyDataFrame = TRUE)

grep("T2", dat$annotations)

subject_data <- foreach(i = 1:nrow(dat), .packages = 'jsonlite') %dopar% {
  slist <- fromJSON(dat$subject_data[[i]], simplifyVector = TRUE, flatten = TRUE)
  slist <- t(data.frame(unlist(slist[[1]], use.names = TRUE)))
  slist
  
}
metadat <- vector("list", length = 10)
metadat <- foreach(i = c(6676:6686), .packages = 'jsonlite') %dopar% {
  
  # change this bit here!
for(i in 1:10){
  slist <- fromJSON(dat$annotations[[i]], simplifyVector = FALSE)
  b <- unlist(lapply(slist[[1]]$value, function(x) x$choice))
  a <- sapply(lapply(slist[[1]]$value, function(x) x$answers), function(x) as.numeric(as.character(x[1])))
  a[is.na(a)] <- 1
  metadat[[i]] <- list(species = b, n = a, id = longshot$id[i], 
             nviews = longshot$retired.classifications_count[i],
             usrnm = dat$user_name[i], usrid = dat$user_id[i],
             gs = dat$gold_standard[i])
}
}

b <- unlist(lapply(slist[[1]]$value, function(x) x$choice))
a <- sapply(lapply(slist[[1]]$value, function(x) x$answers), function(x) as.numeric(as.character(x[1])))
a[is.na(a)] <- 1
data.frame(b, a)


sapply(a, function(x) x[1])
lapply(slist, function(x) x$value$answers)

t(unlist(unlist(slist, recursive = FALSE)))

tc <- sapply(metadat, ncol)




mm <- plyr::rbind.fill.matrix(metadat)
mm <- data.frame(mm)
# get only T0
mm <- mm[grep("T0", mm$task),]


ack2 <- Sys.time() - ack2

longshot <- plyr::rbind.fill.matrix(hunch)
longshot <- plyr::rbind.fill(lapply(slist[[1]], function(x) do.call("data.frame", as.list(x))))
