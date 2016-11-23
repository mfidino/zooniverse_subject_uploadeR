
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
             gs = dat$gold_standard[i],
             retired_id = longshot$retired.subject_id[i],
             fp = longshot$file_path[i],
             stringsAsFactors = FALSE)
}


test <- plyr::rbind.fill(metadat)

t2 <- test[-which(is.na(test$species) | is.na(test$id)),]
t2 <- t2[-which(is.na(t2$retired_id)),]
t2 <- t2[-which(is.na(t2$fp)),]
t2$id <- factor(t2$id)

nvotes <- t2 %>% group_by(id) %>% summarise(nv = length(unique(usrnm)))
to_go <- which(duplicated(t2)==TRUE)
s1 <- t2[-to_go,]



ack <- s1 %>% group_by(id) %>% summarise(ml = names(sort(table(species), decreasing = TRUE))[1],
                                                  times = sort(table(species), decreasing = TRUE)[1],
                                         fp = unique(fp), retired_id = unique(retired_id)) %>% 
  data.frame

joined.dat <- left_join(nvotes, ack, by = "id")
joined.dat

nohmn <- which(joined.dat$ml=="HMN")

joined.dat <- joined.dat[-nohmn,]

probs <- with(joined.dat, times / nv)
probs[probs>1] <- 1


joined.dat$agreement_confidence <- qbinom(0.5, joined.dat$times, probs)/joined.dat$times

joined.dat$site <- substr(joined.dat$fp, 35, 42)

covdat <- read.csv("C:/Users/mfidino/Documents/R/Transect/SummaryFigures/covariate_data.csv",
                   header = TRUE, stringsAsFactors = FALSE)
tk <- c("STATIONID", "EASTING", "NORTHING", "DIST2CENT", "HOUS_AVG1K", "IMPV_AVG1K")
cvt <- covdat[,which(colnames(covdat) %in% tk)]
colnames(cvt) <- tolower(colnames(cvt))
colnames(cvt)[1] <- "site"


# combined cvt with joined.dat

ttt <- left_join(joined.dat, cvt, by = "site")
ttt$season <- "FA14"

aa <- which(d2$subject_id %in% joined.dat$retired_id)
dd <- d2[aa,]
colnames(dd)[1] <- "retired_id"
dd$retired_id <- as.character(dd$retired_id)
ttt2 <- left_join(ttt, dd[,c(1, 6)], by = "retired_id")

final <- ttt2[,-c(2,4)]
colnames(final) <- c("id", "species", "file_path", "retired_id",
                     "agreement_confidence", "site", "easting", "northing",
                     "dist2centerofcity", "avg_housing_density_1k_buffer",
                     "avg_imperv_surface_1k_buffer", "season", "locations")

# remove nthnghr
togo <- which(final$species=="NTHNGHR")
write.csv(final[-togo,], "cww_species_data.csv", row.names = FALSE)
# only keep what we need
ls <- longshot[-which(is.na(longshot$id)),]
ls <- ls[-which(is.na(ls$file_path)),]
# remove humans from qq and joined.dat

d2 <- read.csv("chicago-wildlife-watch-subjects.csv", header = TRUE,
               stringsAsFactors = FALSE)

dd <- which(d2$subject_id %in% as.numeric(as.character(joined.dat$id)))
ttt <- joined.dat
head(joined.dat)

joined.dat$agreement_confidence <- agreement.confidence

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
