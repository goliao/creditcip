
sprd[!(date>=ymd('2014-09-22') & docclause=='MM')]
sprd[!(date>=ymd('2014-09-22') & docclause=='MR')]
sprd[!(date>=ymd('2014-09-22') & docclause=='XR')]
sprd[!(date>=ymd('2014-09-22') & docclause=='CR')]


sprd[date>=ymd('2014-09-22') & docclause=='MM14',docclause:='MM']
sprd[date>=ymd('2014-09-22') & docclause=='MR14',docclause:='MR']
sprd[date>=ymd('2014-09-22') & docclause=='XR14',docclause:='XR']
sprd[date>=ymd('2014-09-22') & docclause=='CR14',docclause:='CR']
