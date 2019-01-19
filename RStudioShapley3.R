# ##########################################################
# #####     AAVSO Sunspot Counts
# ##########################################################
# 
# Created  30 June 2017, Jamie Riggs
# Modified
#          01 Jul 2014, Jamie Riggs, changed sidc to silso
# 
# 
# ##########################################################
# #####     Initialization
# ##########################################################

Sys.setenv(TZ="America/Denver")

library(xtable)
library(ggplot2)
library("reshape2")
library(RMySQL)
library(data.table)


# Path <- "C:/Users/Howe/Desktop/SPESI/SSN/"
Path <- "C:/Users/davidjayjackson/Documents/GitHub/"
setwd(Path)
WD <- getwd()
# 
# Load data from Oberver and daily(sun_data) tables
mydb <- dbConnect(MySQL(),user='root',password='dJj12345',dbname="gn",
                  host='localhost')
#
dbListTables(mydb)
#
ROD <- dbGetQuery(mydb, "SELECT * FROM daily
                  WHERE Year=2018 AND mon=12")
ROD$Ymd <- as.Date(ROD$Ymd)

##########################################################
#####     Functions     ##################################
##########################################################

fetch <- function(fn,ext) {# fn <- Ex
	infile <- paste0(WD, "/", fn, ".",ext)
     X <- data.frame(read.csv(infile, header=TRUE))
	}

FreqProp <- function(factor, factorLabel, dump) {
	table.x <- as.data.frame(table(factor),exclude=c(NA,dump))
	names(table.x)[1] <- c(factorLabel)
	prop <- table.x$Freq / sum(table.x$Freq)
	table.x <- data.frame(table.x, prop)
	sum.x <- colSums(table.x[,2:3])
	new.row <- table.x[1,]
	new.row[1] <- "Total"
	new.row[2:3] <- sum.x
	table.x <- rbind(table.x, new.row)
	}
  
WriteCSV <- function(RdataSet, CSVfileName) {# RdataSet <- A; CSVfileName <- loc
	outfile <- paste(WD, CSVfileName, sep="/")
	write.csv(RdataSet, file=outfile, row.names=F)
	}

##########################################################
#####     Initialization     #############################
##########################################################

 yr <- format(Sys.Date(), format="%Y")
 mo <- as.character(as.numeric(format(Sys.Date(), format="%m"))-1)
 
#   check if December's data and correct year and month
if (mo=="0") { 
	yr <- as.numeric(yr)-1
	mo <- 12
	}
(Ex <- ifelse(nchar(mo)==1,paste0(yr, "0", mo), paste0(yr,mo)))
(ver <- "00")


path <- paste0(Path, "/Data")
setwd(path)
(WD <- getwd())
(infile <- paste0("Shapley", Ex, ver, ".RData"))
load(infile)
summary(X)
nrow(X)

setwd(Path)
(WD <- getwd())

H <- X     # hold original data
# X <- H   # restore if needed


# subset for AAVSO bulletin

X <- H[, c("obs","day","see","w","NumObs","k","ow","name")]
summary(X)
nrow(X)


##########################################################
     part <- "Observers"
##########################################################

A <- X[,c("obs","NumObs","name")]
names(A) <- c("Obs","NumObs","Name")
A <- A[!duplicated(A),]
summary(A)
nrow(A)

# sum submissions

(add <- sum(A$NumObs))      # total number of observations submitted
(add <- as.character(add))

# append to A

label <- "Totals"
(a <- as.character(nrow(A)))       # number of observers that submitted
(Tots <- data.frame(Obs=label,NumObs=add,Name=a))

A <- data.frame(lapply(A,as.character))   # make numeric matrix as characters
str(A)

A <- rbind(A,Tots)     # attach sum and average

# print as latex table

(loc <- paste0("Reports/Bulletins/",Ex,part,".tex"))
addtorow          <- list()
addtorow$pos      <- list()
addtorow$pos[[1]] <- c(0)
addtorow$command  <- c(paste0("\\hline \n",
                             "\\endhead \n",
                             "\\hline \n",
                             "{\\footnotesize Continued on next page} \n",
                             "\\endfoot \n",
                             "\\endlastfoot \n"))
print(xtable(A,digits=0,caption=paste(Ex,"Number of observations by observer"),label="tab:obs"), caption.placement="top", include.rownames=F, tabular.environment="longtable", add.to.row=addtorow, hline.after=c(-1,(nrow(A)-1)), floating=F, file=loc)

rm(A)


##########################################################
     part <- "Soldata"   # 
##########################################################

#  ow is a substitute for Shapley's w. Only ow=1 used.
A <- subset(X, (see %in% c("E","G") & ow == 1 & !(obs %in% "SDOH")), select = c(day,see,NumObs,w,k,ow))
names(A) <- c("Day","Seeing","NumObs","Raw","k","w")
summary(A)
nrow(A)

#  Shapley sums the product of k, w, and Raw, then divides by sum w for a daily RA.
#  When w=1, Shapley's RA is the k, w, Raw product by the number of observations
#  per day. This is the average kRaw. This is what follows.
A$kRawi <- A$Raw * A$k     #  adjust each observer (i) by respective k
B <- aggregate(A$kRawi, list(Day=A$Day), "mean")   #  as per above, average kRaw
names(B)[2] <- "Ra"     #  rename
B$Ra <- round(B$Ra)     # round to nearest integer
B
Daily <- B

path <- paste0(Path,"/Reports/spesi")
setwd(path)
(WD <- getwd())
#(C <- fetch(paste0("20170500RawMinAvgMax"),"txt"))
# (C <- fetch(paste0(Ex,ver,"RawMinAvgMax"),"txt"))
# C <- dbGetQuery(mydb,"SELECT * FROM minmax
#                 WHERE year=2018  AND mon=12")
#Pull data from mimmax table
C <- dbGetQuery(mydb,"SELECT year,mon,day,count(*) as Count,min(W) as Minimum,avg(W) as Average,max(W) as Maximun 
                  FROM daily WHERE year=2018 AND mon=12
                  group by year,mon,day 
                   ORDER BY year,mon,day")

setwd(Path)
(WD <- getwd())

#  average the daily averages to make average monthly, an average of averages
G <- aggregate(A$Raw, list(Day=A$Day), "mean")
names(G)[2] <- "Raw"
G$Raw <- round(G$Raw)
G

(Y <- data.frame(Day=C[,1], NumObs=C[,2], Raw=G$Raw, Ra=B$Ra))
Y$Day <- as.character(Y$Day)
(y <- round(apply(Y[,2:(ncol(Y))],2,mean),1))
(y <- c(Day="Averges",y))

(sol <- rbind(Y,y))
rm(A,B,C,G,Y,y)


(loc <- paste0("Tables/",Ex,ver,part,".csv"))
WriteCSV(sol, loc)

(loc <- paste0("Reports/Bulletins/",Ex,part,".tex"))
addtorow          <- list()
addtorow$pos      <- list()
addtorow$pos[[1]] <- c(0)
addtorow$command  <- c(paste0("\\hline \n",
                             "\\endhead \n",
                             "\\hline \n",
                             "{\\footnotesize Continued} \n",
                             "\\endfoot \n",
                             "\\endlastfoot \n"))
print(xtable(sol, digits=1, caption=paste(Ex,"American Relative Sunspot Numbers (Ra)"), label="tab:soldata", align="ccccc"), caption.placement="top", include.rownames=F, tabular.environment="longtable", add.to.row=addtorow, hline.after=c(-1,0,(nrow(sol)-1),nrow(sol)), floating=F, file=loc)

A <- sol[-nrow(sol),-2]
A <- as.data.frame(sapply(A, as.numeric))
M <- melt(A, id.vars="Day")#, value.name="Counts", variable.name="Stat")
names(M)[2] <- "Legend"
M$Legend <- relevel(M$Legend,ref="Ra")

   yl <- "Raw and Ra Numbers"
   xl <- "Day"
(main <- paste(yl, "vs", xl, "for", Ex))
 (loc <- paste0("Reports/Bulletins/", Ex, part, xl, ".png"))
   gp <- ggplot(M, aes(x=Day, y=value, colour = Legend, linetype= Legend)) +
         geom_line() +
         geom_point(col='grey45') + 
         ggtitle(main) + 
         xlab(xl) + 
         ylab(yl) +
         theme(legend.position = "bottom") +
         theme(plot.title = element_text(hjust = 0.5)) +
         ggsave(loc, width=8, height=4.5)
gp


##########################################################
     part <- "NOAA"   # 
##########################################################

path <- paste0(Path,"/Reports/NOAA")
setwd(path)
(WD <- getwd())
(fn <- Ex)
(Ex <- as.character(as.numeric(Ex)-1))
A <- fetch(paste0(Ex,part,"daily"),"txt")
B <- fetch(paste0(Ex,part,"monthly"),"txt")
(Ex <- fn)
setwd(Path)
(WD <- getwd())
tail(A)
tail(B)


# NOAA daily

(a <- cbind(Year=yr,Month=mo,Daily))
A <- rbind(A,a)
(loc <- paste0("Tables/",Ex,ver,part,"daily.csv"))
WriteCSV(A, loc)
(loc <- paste0("Reports/NOAA/",Ex,part,"daily.txt"))
WriteCSV(A, loc)
(loc <- paste0("Reports/NOAA/daily.csv"))
WriteCSV(A, loc)
(loc <- paste0("Reports/Bulletins/",Ex,part,"daily.csv"))
WriteCSV(A, loc)


# NOAA monthly

(b <- c(Year=as.numeric(yr),Month=as.numeric(mo),Ra=as.numeric(sol$Ra[nrow(sol)])))
(B <- rbind(B,b))
(loc <- paste0("Tables/",Ex,ver,part,"monthly.csv"))
WriteCSV(B, loc)
(loc <- paste0("Reports/NOAA/",Ex,part,"monthly.txt"))
WriteCSV(B, loc)
(loc <- paste0("Reports/NOAA/monthly.csv"))
WriteCSV(B, loc)
(loc <- paste0("Reports/Bulletins/",Ex,part,"monthly.csv"))
WriteCSV(B, loc)



