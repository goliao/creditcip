ymd('3/10/2016'),
ymd('4/21/2016'),
ymd('6/8/2016'),
ymd('6/2/2016'),


c(mdy('5/2/2013'),mdy('11/7/2013'),mdy('6/5/2014'),mdy('9/4/2014'),mdy('1/22/2015'),mdy('12/3/2015'),mdy('3/10/2016'),mdy('4/21/2016'),mdy('6/8/2016'),mdy('6/2/2016'))

c(ymd('1/22/2015'),ymd('3/10/2016'),ymd('4/21/2016'),ymd('6/8/2016'),ymd('6/2/2016'))





testfun<-function(){
  for (i in 1:5) {
    print(str_c('start: ', i))
    tryCatch({
		if (i==3) stop('glerror')
    }, error=function(err){
        print(err)
        stop(err)
    })
    print(str_c('end: ', i))
    }
 }

 testfun()