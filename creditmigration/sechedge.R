rm(list=ls(all=TRUE));

# just download the files  
		rm(list=ls())
		setwd("/Users/gliao/Documents/sec")
			
		# get a comprehensive list of tickers for sp500
			sp500<-fread('sp500.csv',sep=',')[!is.na(CIK),.(CIK)]
			setnames(sp500,'CIK','cik')
			setkey(sp500,cik)
			sp500<-unique(sp500)
		# create path from query
			con <- dbConnect(drv=SQLite(), dbname="edgar_idx.db")
			res<-dbGetQuery(con,"select * from idx where (type='10-K' or type='10-Q') and date>='2007-01-01' and date<='2016-12-31'")
			dbDisconnect(con)
			dtind<-as.data.table(res)
			# merge with sp500
			dtind[,cik:=as.numeric(cik)]
			setkey(dtind,cik)
			dtind<-dtind[sp500]
			dtind[,downloaded:=0]
			dtind[,hedge:=0]
			# save(dtind,file='index160902.RData')		
		load('index160902.RData')
		setwd("/Users/gliao/Documents/sec")
		downloaded.sec(dtind)
# analyze the file
rm(list=ls())
setwd("/Users/gliao/Documents/sec")
load("/Users/gliao/Documents/sec/index160902.RData")
source("/Users/gliao/Dropbox/Research/ccy basis/creditmigration/util.R")
source("/Users/gliao/Dropbox/Research/ccy basis/creditmigration/utilsec.R")
# mark which ones are downloaded

dtind[,downloaded:=0]		
for (i in 1:nrow(dtind)){
	fn=str_c('sp500v3/',str_replace_all(dtind[i,path],'/','_'))
	if (file.exists(fn) & file.info(fn)$size>0){
		dtind[i,downloaded:=1]		
	}
}
dtind[,ioriginal:=rownames(dtind)]


dtind %>% setkey(path)
dtind<-unique(dtind)

dtindout<-dtind[downloaded==1]
dtindout[,parsed:=0]
dtindout[,error:=0]

for (i in 1:nrow(dtindout)){
	tryCatch({
		print(i)
		filename=str_c('sp500v3/',str_replace_all(dtindout[i,path],'/','_'))
		dthedged<-gen.hedge(filename)
		dtindout[i,sentshedged:=dthedged[,sentshedged]]
		dtindout[i,parashedged:=dthedged[,parashedged]]
		dtindout[i,parsed:=1]
	},error=function(e){dtindout[i,error:=1]})
	if (mod(i,60)==0) save(dtindout,file='dtindout160910.RData')
}

dtindout[error==1]

i.=1194
fn=str_c('sp500v3/',str_replace_all(dtindout[ioriginal==i.,path],'/','_'))
fn

gen.hedge(fn)
get.sec.unfiltered(fn)
sec<-scan(file = fn, what = "character", sep ="\n", allowEscapes = TRUE) 
sec


# sec<-scan(file = fn, what = "character", sep ="\n", allowEscapes = TRUE) 
# 			startn=suppressWarnings(grep("<html>|<HTML>", sec))[1]
# 			endn=suppressWarnings(grep("</html>|</HTML>", sec))[1]

# convert_text_to_sentences('happy is my name. gordon is fun. i loves sarah!')

# if (length(grep('<p>|<br>|</font>',sentences))+ length(grep('<p>|<br>|</font>',paragraphs))>0) print('yes')
# if (length(grep('<p>|<br>|</font>','na'))+ length(grep('<p>|<br>|</font>','<p>'))>0) print('yest')


## computing grid
setwd('/mnt/disks/xccy/sec')
source('/mnt/disks/xccy/creditmigration/util.r')
source('/mnt/disks/xccy/creditmigration/utilsec.R')
load('/mnt/disks/xccy/sec/index160902.RData')
		
	dtind[,downloaded:=0]		
	for (i in 1:nrow(dtind)){
		fn=str_c('sp500v3/',str_replace_all(dtind[i,path],'/','_'))
		if (file.exists(fn) & file.info(fn)$size>0){
			dtind[i,downloaded:=1]		
		}
	}
	dtind[,ioriginal:=rownames(dtind)]

	# dtind[type=='10-K',.N,downloaded]
	dt2download<-copy(dtind[type=='10-K'])
	dt2download
	# download.sec(dt2download,pathprefix='/mnt/glbucket/sec/sp500v4')
	# reverse
	download.sec(dt2download,pathprefix='/mnt/glbucket/sec/sp500v4',startn=nrow(dt2download),endn=1)



######################
# wrds index
	rm(list=ls())
	setwd('/mnt/disks/xccy/creditmigration')
	source('/mnt/disks/xccy/creditmigration/util.r')
	source('/mnt/disks/xccy/creditmigration/utilsec.R')

	wrdsind<-fread('wrdsindex.csv',sep=',')
	wrdsind<-read.dta('db/cikcusip.dta') %>% as.data.table()
	# wrdsind2 %>% str
	wrdsind
	spind<-wrdsind[conm=='S&P 500 Comp-Ltd']
	spind[,from.date:=ymd(from)]
	spind[,thru.date:=ymd(thru)]
	spind %>% ds
	spind %>% setkey(co_cusip,co_cik)
	spind %>% showdups() # firms that drop out and reenter the index
	cik_cusip<-unique(spind[,.(co_cusip,co_cik,co_conm,co_tic)])
	save(cik_cusip,file='/Users/gliao/Dropbox/Research/ccy basis/creditmigration/db/cik_cusip.RData')
	
	# cik_cusip[,str_length(co_cusip)]
	# spind[,.N,thru.date]
	# spind[is.na(thru.date)][order(-from.date)] %>% View
	# spind
	# spind[order(-from.date),co_conm] %>% head(30)

	spind<-spind[order(from.date)]
	spind %>% setkey('co_cik')


	# get the file path for each individual filing
		con <- dbConnect(drv=SQLite(), dbname="edgar_idx.db")
		res<-dbGetQuery(con,"select * from idx where (type='10-K') and date>='2004-01-01' and date<='2016-12-31'")
		dbDisconnect(con)
		dtind<-as.data.table(res)
		# merge with sp500
		dtind[,cik:=as.numeric(cik)]
		setkey(dtind,cik)

		dtindsp500_10k<-dtind[unique(spind)[!is.na(co_cik),.(co_cik)]]

		dtindsp500_10k %>% setkey(path)
		dt2download10kall<-unique(dtindsp500_10k)[!is.na(path)]

		rm(dtind)
		# download.sec2(dt2download10kall,pathprefix='/mnt/disks/xccy/sec/sp500v3/')
		# download.sec2(dt2download10kall,pathprefix='/mnt/disks/xccy/sec/sp500v3/',startn=nrow(dt2download10kall),endn=1)
		# download.sec2(dt2download10kall,pathprefix='/mnt/disks/xccy/sec/sp500v3/',startn=3000)
		# download.sec2(dt2download10kall,pathprefix='/mnt/disks/xccy/sec/sp500v3/',startn=1)
		download.sec2(dtind2[parsed==0],pathprefix='/mnt/disks/xccy/sec/sp500v3/',startn=1)

		i=1
		for(i in 1:9){
			cmdstr=(str_c('rm ', '/mnt/disks/xccy/sec/sp500v3/',str_replace_all(dtind2[parsed==0][i,.(path)],'/','_')))
			system(cmdstr)
		}

		dtind<-dt2download10kall %>% markdownloaded(prefix='/mnt/disks/xccy/sec/sp500v3/')

		
		# dthedge<-gen.hedge.all2(dtind,startn=1)

		dtind2<-mark.parsed(dtind)

		# try to parse one that's not parseable
		fn.1=dtind2[parsed==0][3,path]
		filename=str_c('/mnt/disks/xccy/sec/sp500v3/',str_replace_all(fn.1,'/','_'))

		aa<-dthedged<-gen.hedge(filename,'')
		bb<-gen.hedge.all2(dtind2[parsed==0],startn=1)


		setwd('/mnt/disks/xccy/creditmigration')
		load('temp.RData')
		source('/mnt/disks/xccy/creditmigration/util.r')
		source('/mnt/disks/xccy/creditmigration/utilsec.R')
		dtind2<-mark.parsed(dtind)


		bb<-gen.hedge.all2(dtind2[parsed==0],startn=2)
		# roll everything together
		rm(list=ls())
		setwd('/mnt/disks/xccy/creditmigration')
		load('temp.RData')
		source('/mnt/disks/xccy/creditmigration/util.r')
		source('/mnt/disks/xccy/creditmigration/utilsec.R')
		# dtind2<-mark.parsed(dtind)
		fn.1=dtind2[parsed==0][1,path]
		filename=str_c('/mnt/disks/xccy/sec/blocktext_sp500v3/',str_replace_all(fn.1,'/','_'),'.RData')
		load(filename)
		textblock %>% sents 

		i=1
		dtin.<-dtind2[parsed==1]
#################
		setwd('/mnt/disks/xccy/creditmigration')
		load('temp.RData')
		source('/mnt/disks/xccy/creditmigration/util.r')
		source('/mnt/disks/xccy/creditmigration/utilsec.R')
		dtind2<-mark.parsed(dtind)
		hedge.result.all<-construct.hedge.result.all(dtind2[parsed==1],30)
		dt.hedge.result<-rbindlist(hedge.result.all)
		save(dt.hedge.result,file='sec_hedge_result_10k.RData')		
		dt.hedge.result[,para:=NULL]
		dt.hedge.result.notxt<-dt.hedge.result
		save(dt.hedge.result.notxt,file='sec_hedge_result_10k_notxt.RData')		

		dt.hedge.result.notxt
aa<-construct.hedge.result.single(filename,cik)
# gen para and sentence filess and hedge ratio

#count
	dtind %>% ds


	dtout[,.N,parsed]



		dtind[,.N]
		dthedge<-gen.hedge.all.par(dtind,startn=1)
		# dthedge<-gen.hedge.all2(dtind,startn=1)
		# dthedge<-gen.hedge.all2(dtind,startn=4000)
		# dthedge<-gen.hedge.all2(dtind,startn=6000)

		# setwd('../sec')
		# i=1
		# aa<-gen.hedge(diag=T)






# require('doParallel')
# getDoParWorkers()
# getDoParRegistered()
# getDoParName()
# getDoParVersion()




# max.eig <- function(N, sigma) {
#      d <- matrix(rnorm(N**2, sd = sigma), nrow = N)
#      #
#      E <- eigen(d)$values
#      #
#      abs(E)[[1]]
#  }
# max.eig(5, 1)
# foreach(n = 1:5) %dopar% max.eig(n, 1)


# # install.packages('doMC')
# # install.packages('rbenchmark')
# library(doMC)
# library(rbenchmark)

# registerDoMC(cores=4)
# benchmark(
#      foreach(n = 1:50) %do% max.eig(n, 1),
#      foreach(n = 1:50) %dopar% max.eig(n, 1)
#  )



####

require(doParallel)
registerDoParallel(4)
library(rbenchmark)
benchmark(
     foreach(n = 1:50) %do% max.eig(n, 1),
     foreach(n = 1:50) %dopar% max.eig(n, 1)
 )


require(doParallel)
registerDoParallel(4)
getDoParWorkers()



########################
### download quarterly
rm(list=ls())
setwd('/mnt/disks/xccy/creditmigration')
source('/mnt/disks/xccy/creditmigration/util.r')
source('/mnt/disks/xccy/creditmigration/utilsec.R')

	wrdsind<-fread('wrdsindex.csv',sep=',')
	spind<-wrdsind[conm=='S&P 500 Comp-Ltd']
	spind[,from.date:=ymd(from)]
	spind[,thru.date:=ymd(thru)] 

	spind<-spind[order(from.date)]
	spind %>% setkey('co_cik')

	# get the file path for each individual filing
		con <- dbConnect(drv=SQLite(), dbname="edgar_idx.db")
		res<-dbGetQuery(con,"select * from idx where (type='10-Q') and date>='2004-01-01' and date<='2016-12-31'")
		dbDisconnect(con)
		dtind<-as.data.table(res)
		# merge with sp500
		dtind[,cik:=as.numeric(cik)]
		setkey(dtind,cik)
		dtindsp500_10q<-dtind[unique(spind)[!is.na(co_cik),.(co_cik)]]
		dtindsp500_10q %>% setkey(path)
		dt2download10qall<-unique(dtindsp500_10q)[!is.na(path)]
		rm(dtind)
		for (t=1:3){
		download.sec.par(dt2download10qall[order(-path)],pathprefix='/mnt/glbucket/sec/sp50010q/')
		}

