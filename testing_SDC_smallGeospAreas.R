
### testing several methods from SDC-tools on census data of high spatial resolution (small areas)

library(sdcTable)
#packageVersion("sdcTable")
library(sdcHierarchies)
#packageVersion("sdcHierarchies")

### assume we start with microdata

### from vignette/manual:
#example of creating hierarchies for each variable and all its levels
# say we have a variable V1
#dimV1 <- hier_create(root = "Tot", nodes = LETTERS[1:4])
#dimV1 <- hier_add(dimV1, root = "B", nodes = c("Ba","Bb","Bc"))
#hier_display(dimV1)
# or a simpler one, V2
#dimV2 <- hier_create(root = "Tot", nodes = c("m", "w"))
#hier_display(dimV2)

################# our data hierarchies #########################################################
## for the geospatial census project, we chose:
# Attribute dimensions: age, sex, edu, cas, coc, pob
# Geospatial dimension: smsv (smasvaedi, i.e. small areas)

# age #--------------------------------------------------------------------
dim_age <- hier_create(root="Tot", nodes=
                         c(
                           "Y_LT1",
                           "Y1", 
                           "Y10", "Y11","Y12","Y13","Y14","Y15","Y16","Y17","Y18","Y19",
                           "Y2",
                           "Y20","Y21","Y22","Y23","Y24","Y25","Y26","Y27","Y28","Y29",
                           "Y3",
                           "Y30","Y31","Y32","Y33","Y34","Y35","Y36", "Y37","Y38","Y39",
                           "Y4",
                           "Y40","Y41","Y42","Y43","Y44","Y45","Y46","Y47","Y48","Y49",
                           "Y5",
                           "Y50","Y51","Y52","Y53","Y54","Y55","Y56","Y57","Y58","Y59",
                           "Y6",
                           "Y60","Y61","Y62","Y63","Y64","Y65","Y66","Y67","Y68","Y69",
                           "Y7",
                           "Y70","Y71","Y72","Y73","Y74","Y75","Y76","Y77","Y78","Y79",
                           "Y8",
                           "Y80","Y81","Y82","Y83","Y84","Y85","Y86","Y87","Y88","Y89",
                           "Y9",
                           "Y90","Y91","Y92","Y93","Y94","Y95","Y96","Y97","Y98","Y99",
                           "Y_GE100") )

hier_display(dim_age)
dim(dim_age)

# sex #----------------------------------------------------------------
dim_sex <- hier_create(root="Tot", nodes=c("F", "M") )
hier_display(dim_sex)

# edu #-----------------------------------------------------------------
dim_edu <- hier_create(root="Tot", nodes=
                         c("ED1" ,
                           "ED2" ,
                           "ED3" ,
                           "ED4" ,
                           "ED5" ,
                           "ED6" ,
                           "NAP" ,
                           "NONE") )
hier_display(dim_edu)

# cas #-----------------------------------------------------------------
dim_cas <- hier_create(root="Tot", nodes=
                         c(
                           "EDUC",   
                           "EMP",    
                           "HOME",    
                           "INAC_OTH",
                           "INC",     
                           "LT_MWA",  
                           "UNE_NW",  
                           "UNE_W" ))   
hier_display(dim_cas)

# coc #-----------------------------------------------------------------
dim_coc <- hier_create(root="Tot", nodes=
                         c(
                           "AF","AL","AO","AR",
                           "ASI_OTH",
                           "AT","AU","AZ",
                           "BA","BB","BD","BE","BF","BG","BJ","BO","BR","BS","BY",
                           "CA","CD","CH","CI","CL","CM","CN","CO","CR","CU","CV","CY","CZ",
                           "DE","DJ","DK","DO","DZ",
                           "EC","EE","EG","EL","ER","ES","ET",
                           "EUR_OTH",
                           "FI","FJ","FR","GE",
                           "GH","GM","GN","GQ","GT","GY",
                           "HN","HR","HT","HU",
                           "ID","IE","IL","IN","IQ","IR","IT",
                           "JM","JO","JP",
                           "KE","KR","KZ",
                           "LB","LK","LR","LT","LU","LV","LY",
                           "MA","MD","ME","MK","ML","MN","MR","MT","MU","MX","MY","MZ",
                           "NA","NAT","NE","NG","NI","NL","NO","NP","NZ",
                           "OM","PA","PE","PH","PK","PL","PT","PY",
                           "RO","RS","RU","RW",
                           "SA","SD","SE","SG","SI","SK","SL","SN","SO","STLS","SV","SY",
                           "TG","TH","TN","TR","TT","TW","TZ",
                           "UA","UG","UK","UNK","US","UY","UZ",
                           "VE","VN",
                           "YE",
                           "ZA","ZM"))
hier_display(dim_coc)
dim(dim_coc)

# pob #-----------------------------------------------------------------
dim_pob <- hier_create(root="Tot", nodes=
                         c(
                           "AD","AE","AF","AL","AM","AO","AR",
                           "ASI_OTH",
                           "AT","AU","AW","AZ",
                           "BA","BB","BD","BE","BF","BG","BH","BJ","BM","BO","BR","BS","BY",
                           "CA","CD","CH","CI","CL","CM","CN","CO","CR","CU","CV","CY","CZ","CZ_SK",
                           "DE","DJ","DK","DM","DO","DZ","EC","EE","EG","EL","ER","ES","ET",
                           "EUR_OTH",
                           "EX_SU","EX_YU",
                           "FI","FJ","FO","FR",
                           "GA","GE","GH","GL","GM","GN","GQ","GT","GW","GY",
                           "HN","HR","HT","HU",
                           "ID","IE","IL","IN","IQ","IR","IT",
                           "JM","JO","JP",
                           "KE","KG","KH","KR","KW","KZ",
                           "LA","LB","LK","LR","LT","LU","LV","LY",
                           "MA","MC","MD","ME","MG","MK","ML","MM","MN","MR","MT","MU","MW","MX","MY","MZ",
                           "NA","NAT","NE","NG","NI","NL","NO","NP","NZ",
                           "OM",
                           "PA","PE","PH","PK","PL","PT","PY",
                           "QA",
                           "RO","RS","RU","RW",
                           "SA","SB","SE","SG","SI","SK","SL","SN","SO","SV","SY",
                           "TG","TH","TJ","TM","TN","TO","TR","TT","TW","TZ",
                           "UA","UG","UK","UNK","US","UY","UZ",
                           "VE","VN",
                           "XK",
                           "ZA","ZM","ZW"))
hier_display(dim_pob)
dim(dim_pob)

# smsv #-------------------------------------------------------------------
dim_smsv <- hier_create(root="Tot", nodes=
                          c(
                            "0101","0102","0103",
                            "0201","0202","0203","0204","0205","0206","0207",
                            "0301","0302","0303","0304","0305",
                            "0401","0402","0403","0404","0405",
                            "0501","0502","0503",
                            "0601","0602","0603","0604","0605",
                            "0701","0702","0703","0704","0705","0706","0707","0708",
                            "0801","0802","0803","0804","0805","0806","0807",
                            "0901","0902","0903","0904",
                            "1001","1002","1003","1004","1005","1006","1007",
                            "1101","1102","1103","1104","1105",
                            "1201","1202","1203","1204","1205","1206",
                            "1301","1302","1303","1305","1306",
                            "1401","1402","1403","1404",
                            "1501","1502","1503","1504","1505",
                            "1601","1602","1603",
                            "1701","1702","1703","1704","1705","1706",
                            "1801","1802","1803","1804","1805","1806","1807","1808","1809",
                            "1901","1902",
                            "2001","2002","2003","2004","2005","2006","2007",
                            "2101","2102","2103",
                            "2104","2105",
                            "2201","2202","2203","2204","2205",
                            "2301","2302","2303",
                            "2401","2402","2403","2405","2406","2407",
                            "2501","2502","2503","2504",
                            "2601","2602",
                            "2701","2702","2703","2704","2705",
                            "2801","2802","2803","2804",
                            "2901","2902","2903","2904","2905",
                            "3001","3002","3004","3005","3006",
                            "3101","3102","3103","3104","3105",
                            "3201","3202","3203","3204",
                            "3301","3302","3303","3304","3305",
                            "3401","3402","3403","3404",
                            "3501","3502","3503","3504",
                            "3601","3602","3603",
                            "3701","3702","3703",
                            "3801","3802","3803","3804",
                            "3901","3902","3903","3904",
                            "4001","4002","4003",
                            "4101","4102","4103",
                            "4201","4202","4203","4204"
                          ))
hier_display(dim_smsv)

###-----------------------------------------------------------------------------

### reading the data from database (using linux server)

library(odbc)
library(DBI)
con_dev <- dbConnect(odbc(),
                     Driver = "FreeTDS",
                     Server = "XXX\\dev",
                     Database = "YYYY",
                     UID = "ZZZZZZ",
                     PWD = rstudioapi::askForPassword("Database password")
)

res <- dbSendQuery(con_dev, "SELECT edu, sex, smsv FROM dbo.test_censusGeosp")


########---- only when using SWAP method: ------------------------------------------
### we could have the household identifiers, if we want to test SWAP_method:
# res <- dbSendQuery(con_dev, "SELECT edu, sex, smsv, household, svf FROM dbo.censusGeosp")
# microData<- dbFetch(res)  
# str(microData)
# go now directly to swapping


#--------------------SDCTABLE: supression method------------------------------------

### creating objects sdcproblem for analysis #########################

dimList <- list(edu = dim_edu, sex = dim_sex, smsv = dim_smsv)
# choosing from:            
##age = dim_age, sex = dim_sex, edu = dim_edu, cas = dim_cas, coc= dim_coc, pob= dim_pob, smsv = dim_smsv)

names(dimList)
names(microData)

prob.microDat <- makeProblem(
  data = microData,
  dimList = dimList,
  dimVarInd = 1:3,
  freqVarInd = NULL,
  numVarInd = NULL,
  weightInd = NULL,
  sampWeightInd = NULL)

counts <- getInfo(prob.microDat, type = "freq")

## identifying sensitive cells can be done according to several criteria as seen at:
## https://sdctools.github.io/sdcTable/reference/primarySuppression.html 

prob.microDat_1 <- primarySuppression(prob.microDat, type = "freq", maxN = 5)
print(table(getInfo(prob.microDat_1, type = "sdcStatus")))
summary(prob.microDat_1)

## status of cells can be:
# u: cell is primary suppressed and needs to be protected
# x: cell has been secondary suppressed
# s: cell can be published
# z: cell must not be suppressed

## protecting the primary sensitive cells, several possible methods:

# OPT: protect the complete hierarchical, multidimensional table at once. 
#      This algorithm is however only suitable for small problem instances.
# HITAS: solving the secondary cell suppression problem by applying a cut and branch algorithm to 
#        subtables that are protected in specific order
# HYPERCUBE: solving the problem using a heuristic that is based on finding geometric hypercubes 
#            that are required to protect primary sensitive table cells
# SIMPLEHEURISTIC: solving the problem using a fast heuristic approach that only protects 
#                  against exact recalculation of values
### HOWEVER!!!: 
# The implemented methods may have bugs that yield in not-fully protected tables. 
# Especially the usage of "OPT", "HITAS" and "HYPERCUBE" in production is NOT suggested 
# as these methods may eventually be removed completely. 
# In case you encounter any problems, please report it or use 
# Tau-Argus (http://research.cbs.nl/casc/tau.htm).

## by using the function:
# https://sdctools.github.io/sdcTable/reference/protectTable.html

# resHITAS <- protectTable(prob.microDat_1, method = "HITAS")
# resOPT <- protectTable(prob.microDat_1, method = "OPT")
# resHYPER <- protectTable(prob.microDat_1, method = "HYPERCUBE")

# the SIMPLE-option worked fast
resSIMPLE <- protectTable(prob.microDat_1, method = "SIMPLEHEURISTIC")
finalData <- getInfo(resSIMPLE, type = "finalData")
print(head(finalData))
summary(resSIMPLE)




################### record swapping method and package ##################------------------

## rm(list=ls())
#install.packages("devtools")  
#devtools::install_github("sdcTools/recordSwapping", build_vignette=TRUE)
library(recordSwapping)
#vignette("recordSwapping")
## or as seen at: https://rdrr.io/github/sdcTools/recordSwapping/f/vignettes/recordSwapping.Rmd 

# then apply
# note that the "hierarchy" here means: which variables of the data-frame define geographical levels
# for us there is only one variable, "smsv"
## note that C++ indexing starts with 0, this is why our variables are now seen as: 0,1,2,...

## example data -------------------------------------
dat <- recordSwapping:::create.dat(N=250000)
str(dat)
hierarchy <- 0:2
risk_variables <- 5:7
k_anonymity <- 3
swaprate <- .05
hid <- 4
similar <- c(5)

dat_swapped <- recordSwap(dat,similar,hierarchy,risk_variables,hid,k_anonymity,swaprate)
dat_swapped

swaprate*nrow(dat[!duplicated(hid)])

# comparing 
dat_compare <- merge(dat[,.(paste(geo=nuts1[1],nuts2[1],nuts3[1],municipality[1],sep="_")),by=hid],
                     dat_swapped[,.(paste(geo=nuts1[1],nuts2[1],nuts3[1],municipality[1],sep="_")),by=hid],by="hid")

# number of swapped households
nrow(dat_compare[V1.x!=V1.y])

# or calculate like
swaprate*nrow(dat[!duplicated(hid)])

## real data now -------------------------------------

hsizes <- dplyr::count(microData, microData$household)
names(hsizes) <- c("mDh", "n")

## *** ## in order for the swapping function to work, the values of all variables should be integers!!!
edu_recoded <- as.numeric(factor(microData$edu))
sex_recoded<- as.numeric(factor(microData$sex))
svf_recoded <- as.numeric(factor(microData$svf))

microData_recoded <- data.frame(edu_recoded, sex_recoded, svf_recoded, microData$smsv, microData$household)
names(microData_recoded) <- c("edu", "sex", "svf", "smsv", "household")

mdrr <- sqldf::sqldf("select aa.*, bb.n from microData_recoded as aa, hsizes as bb where bb.mDh=aa.household")

hierarchy <- 2:3           # geospatial hierarchy smsv and svf
risk <- 0:1                # we say that the risk variables are edu, sex
hid <- 4                  # swapping household identifiers 
k_anonymity <- 3
# th <- 2
swaprate <- .05
similar <- c(5)   # similarity profile: size of household
#similar <- c(5, 0, 1)      # similarity profile, some choice: size household, sex, edu

md_swapped <- recordSwap(mdrr,similar,hierarchy,risk,hid,k_anonymity,swaprate, seed =123456L)

md_swapped

## check that overall frequencies remain the same
diff_counts_edu <- (dplyr::count(md_swapped, md_swapped$edu))$n-(dplyr::count(mdrr, mdrr$edu))$n

A <-as.numeric(md_swapped$edu)
B <- as.numeric(mdrr$edu)

library(tidyr)
library(ggplot2)
ddff <- data.frame(A,B)
names(ddff) <- c("swapped", "original")
ddff %>% 
  gather(key=Type, value=Value) %>% 
  ggplot(aes(x=Value,fill=Type)) + 
  geom_histogram(position="dodge")


# swaprate times number of households in data
swaprate*nrow(microData_recoded[!duplicated(hid)])  # 15777.8 when similar


####### overview of data ###############
sqldf::sqldf("select count(distinct(household)) from mdrr" ) 
sqldf::sqldf("select count(distinct(smsv)) from mdrr" )
sqldf::sqldf("select count(distinct(edu)) from mdrr" ) 
sqldf::sqldf("select count(distinct(svf)) from mdrr" ) 
################################################



################### cellKey method and package ##########################

## rm(list=ls())
## update.packages(ask=FALSE)
### 



#devtools::install_github(
#  repo = "sdcTools/ptable",
#  dependencies = TRUE,
#  build_opts = "--no-resave-data",
#  force = TRUE)
library(ptable)

#remotes::install_github(
#  repo = "sdcTools/cellKey",
#  dependencies = TRUE,
#  build_opts = "--no-resave-data",
#  force = TRUE)
# If you experience a timeout due to a proxy server while downloading, 
# one can work around this issue by specifying the proxy-server using the httr package:
#library(httr)
#httr::set_config(use_proxy(url = "xxx.xxx.xxx.xxx, port = yy))
# see more info at https://github.com/sdcTools/cellKey 
# then apply as shown at: https://sdctools.github.io/cellKey/articles/introduction.html 
library(cellKey)
##vignette(cellKey)
?cellKey::cellkey_pkg
#ck_vignette()

### https://rdrr.io/github/sdcTools/cellKey/f/vignettes/introduction.Rmd ### vvvv good

## some theory, short and correct, at: https://ec.europa.eu/eurostat/cros/system/files/methods_for_protecting_census_data.pdf 


x <- microData
x$rkey <- ck_generate_rkeys(dat=x, seed = 1234)

tab <- ck_setup( x = x, rkey = "rkey", dims = dimList)

print(tab)
#cat(tab$print())
tab$hierarchy_info()
tab$allvars()
# count variables
tab$cntvars()
# continuous variables
tab$numvars()


#----- perturbation table for count variable---------------------
# the example ptable provided directly
ptab1 <- ptable::pt_ex_cnts()   
## D=2, V=1.05; js=1; ncat=4; pstay=NA,NA,NA,NA; optim=1; mono=True,True, False,True
## worked better it seems

# creating a ptable by specifying parameters
para2 <- ptable::create_cnt_ptable(
  D = 8, V = 3, js = 2, pstay = 0.5, 
  optim = 1, mono = TRUE)

p_cnts1 <- ck_params_cnts(ptab = ptab1)
p_cnts2 <- ck_params_cnts(ptab = para2)

# apply perturbation; if we do not specify variable v to be applied to, the function uses all it finds
tab$params_cnts_set(val = p_cnts1)
tab$perturb(v="total")
#show results
tab$freqtab(v = c("total"))  #### nice info
tail(tab$freqtab(v="total"), 30)
head(tab$freqtab(v="total"), 20)
dim(tab$freqtab(v="total"))  # 5319 and 8


# aa <- tab$freqtab(v="total")
# bb <- as.factor(subset(aa, aa$edu != "Tot")$edu) 
# hist(as.numeric(bb))

### so the table of counts is: 
rrr <- tab$freqtab(v="total")
# how many cell counts are perturbed: 
dim(rrr[rrr$uwc != rrr$puwc])  ## 3053 and 8
# how many cell counts are not changed:
dim(rrr[rrr$uwc == rrr$puwc])  ## 2266  and 8


## trying some plots, although not so interesting----------
xx <- as.vector( tab$freqtab()$uwc )
yx <- as.vector( tab$freqtab()$puwc )
df <- data.frame(xx,yx)
ggplot2::ggplot(df, aes(df$xx, df$yx)) + 
  geom_point()
hist(df$xx-df$yx)
##-----------------------

# utility measures
tab$measures_cnts(v = "total", exclude_zeros = TRUE)   ###good
## here, they denote
# overview, about
## noise: amount of noise computed as orig - pert
## cnt: number of cells perturbed with the value given in column noise
## pct: percentage of cells perturbed with the value given in column noise

#measures: ... (see the pdf of the theoretical explanations in the link above)
## d1: absolute distance between original and masked values
## d2: relative absolute distance between original and masked values
## d3: absolute distance between square-roots of original and perturbed values
#cumulative distributions of distances d1-d3, with:
## cat: a specific value (for d1) or interval (for distances d2 and d3)
## cnt: number of records smaller or equal the value in column cat for the given distance
## pct: proportion of records smaller or equal the value in column cat for the selected distance

#plot.default(tab$measures_cnts(v = "total", exclude_zeros = TRUE)$cumdistr_d1, ylab="d1, d2, d3")
#plot.default(tab$measures_cnts(v = "total", exclude_zeros = TRUE)$cumdistr_d2, ylab="d2", lty="dotted")
#plot.default(tab$measures_cnts(v = "total", exclude_zeros = TRUE)$cumdistr_d3, ylab="d3", 
#      lty="dashed", pch = 19, col = "red")


# modifications for perturbed count variables
tab$mod_cnts()

xxx <- tab$mod_cnts()$smsv 
yyy <- tab$mod_cnts()$pert

#chisq.test(xxx, yyy, correct = FALSE)
# the cross-tabulated
totest <- xtabs(~ xxx + yyy)
library(DescTools)
GoodmanKruskalGamma(totest, conf.level=0.95)

tab$print() # same as (print(tab))
tab$summary()

#################################################################################
