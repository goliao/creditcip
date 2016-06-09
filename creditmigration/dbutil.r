#dbutil


require('RMySQL')


DBimport<-function(name='sdc'){
con <- dbConnect(MySQL(), dbname="gldb", host="localhost", user='root')
# dtout<-dbGetQuery(con,'select * from sdc') %>% as.data.table()
dtout<-dbReadTable(con,name) %>% as.data.table()
RMySQL::dbDisconnect(con)
dtout
}

DBwrite<-function(name='',dfin,bool_overwrite=FALSE){
con <- dbConnect(MySQL(), dbname="gldb", host="localhost", user='root')
# dtout<-dbGetQuery(con,'select * from sdc') %>% as.data.table()
dtout<-dbWriteTable(con,name,dfin,overwrite=bool_overwrite) %>% as.data.table()
RMySQL::dbDisconnect(con)
dtout
}


DBsql<-function(sql=''){
con <- dbConnect(MySQL(), dbname="gldb", host="localhost", user='root')
dtout<-dbGetQuery(con,sql) %>% as.data.table()
RMySQL::dbDisconnect(con)
dtout
}


#dbListTables(con)
#vignette('databases',package='dplyr')


# gldb<-src_mysql('gldb','localhost')
# test<-tbl(gldb,sql('select * from sdc'))
# gldb<-src_sqlite('gldb.sqlite3',create=T)
# copy_to(gldb,sdc[,matdiff:=as.numeric(matdiff)],name='sdc2',temporary = FALSE)
# test<-tbl(gldb,sql('select * from sdc2'))

resave <- function(..., list = character(), file) {
   previous  <- load(file)
   var.names <- c(list, as.character(substitute(list(...)))[-1L])
   for (var in var.names) assign(var, get(var, envir = parent.frame()))
   save(list = unique(c(previous, var.names)), file = file)
}