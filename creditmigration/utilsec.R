# utilsec
# sechedge
require(dplyr); require(data.table); require(stringr);require('RCurl')
require('RSQLite'); require(XML); require(readr)
require(htmltools)
require(NLP);
require(tm);
require(openNLP);
require('doParallel')
# library(openNLPmodels.en);
	# functions
yrqrt2date<-function(yq){
	yq<-copy(as.character(yq))
	year.chr<-str_sub(yq,1,4)
	q.chr<-str_sub(yq,5)
	ymd(str_c(year.chr,'-',as.numeric(q.chr)*3-2,'-01'))
}
download.file.wrap<-function(urlstr=str_c('ftp://ftp.sec.gov/',dtind[i,path]),fileout=str_replace_all(dtind[1,path],'/','_')){
	tryCatch({download.file(urlstr,destfile = fileout)},error=function(e){print(str_c('skipped',e));return(NA)})
}


htmlToText <- function(input, ...) {

	  ###---PACKAGES ---###
	require(RCurl)
	require(XML)


	  ###--- LOCAL FUNCTIONS ---###
	  # Determine how to grab html for a single input element
	evaluate_input <- function(input) {    
	    # if input is a .html file
		if(file.exists(input)) {
			char.vec <- readLines(input, warn = FALSE)
			return(paste(char.vec, collapse = ""))
		}

	    # if input is html text
		if(grepl("</html>", input, fixed = TRUE,useBytes=T)) return(input)

	    # if input is a URL, probably should use a regex here instead?
			if(!grepl(" ", input)) {
	      # downolad SSL certificate in case of https problem
				if(!file.exists("cacert.perm")) download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.perm")
					return(getURL(input, followlocation = TRUE, cainfo = "cacert.perm"))
			}

	    # return NULL if none of the conditions above apply
			return(NULL)
		}

	  # convert HTML to plain text
		convert_html_to_text <- function(html) {
			doc <- htmlParse(html, asText = TRUE)
			text <- xpathSApply(doc, "//text()[not(ancestor::script)][not(ancestor::style)][not(ancestor::noscript)][not(ancestor::form)]", xmlValue)
			return(text)
		}

	  # format text vector into one character string
		collapse_text <- function(txt) {
			return(paste(txt, collapse = " "))
		}

	  ###--- MAIN ---###
	  # STEP 1: Evaluate input

	  # input  %>% enc2native() %>% enc2utf8() %>% lapply(.,evaluate_input)
	  # input %>% enc2utf8() %>% Encoding


		html.list <- lapply(input, evaluate_input)

	  # STEP 2: Extract text from HTML
		text.list <- lapply(html.list, convert_html_to_text)

	  # STEP 3: Return text
		text.vector <- sapply(text.list, collapse_text)
		return(text.vector)
	}
	convert_text_to_sentences <- function(text, lang = "en") {
		  # Function to compute sentence annotations using the Apache OpenNLP Maxent sentence detector employing the default model for language 'en'. 
			# install.packages(c('tm','NLP','openNLP'))
		require(tm)
		require(NLP)
		require(openNLP)
		sentence_token_annotator <- Maxent_Sent_Token_Annotator(language = lang)

		  # Convert text to class String from package NLP
		text <- as.String(text)

		  # Sentence boundaries in text
		sentence.boundaries <- NLP::annotate(text, sentence_token_annotator)

		  # Extract sentences
		sentences <- text[sentence.boundaries]

		  # return sentences
		return(sentences)
	}

	get.sec.unfiltered<-function(filename.,diag=F){
		sec<-scan(file = filename., what = "character", sep ="\n", allowEscapes = TRUE)  %>% str_to_lower()
		startn=suppressWarnings(grep("<html>", sec))[1]
		endn=suppressWarnings(grep("</html>", sec))[1]
		if (is.na(startn) | is.na(endn)){
			if(is.na(startn) & is.na(endn)){
				startn=suppressWarnings(grep("<document>", sec))[1]
				endn=suppressWarnings(grep("</document>", sec))[1]
			}
			if (!is.na(startn) & is.na(endn)) {
				message('no html tag: file not finished')	
				return (NA)}
				if (is.na(startn) | is.na(endn))
				 message('no html tag;no document tag')
				return(NA)
			} 
		secmainhtml <- sec[startn:endn] 
		if (diag) save_html(HTML(secmainhtml),str_c('/Users/gliao/Documents/sec/test.html'))
			secmainhtml
	}
	gen.text.sec<-function(filename.){
	# filename.=filename
# generate sentences and paragraphs from filename.
		secmainhtml<-get.sec.unfiltered(filename.)
		if(is.na(secmainhtml)){return(NA)}
# save_html(HTML(secmainhtml),str_c('/Users/gliao/Documents/sec/sp500v3html/',i,'.html'))

# html to text: method 1: genete vector of paragraphs
# Read and parse HTML file
		doc.html = htmlTreeParse(secmainhtml,useInternal = TRUE)
# Extract all the paragraphs (HTML tag is p, starting at
# the root of the document). Unlist flattens the list to
# create a character vector.
		doc.text = unlist(xpathApply(doc.html, '//text()', xmlValue))

# Replace all \n by spaces
		doc.text = gsub('\\n', ' ', doc.text)

		doc.text<-str_to_lower(doc.text)
# only keeping the long strings

		dtparas<-data.table('paras'=iconv(doc.text, "latin1", "ASCII", sub=""))[str_length(paras)>=50]
				# paragraphs<-list()
				# for (para in doc.text){
				# 	lpara<-str_length(para)
				# 	if (lpara<100) next
		  # #print(lpara)
				# 	paragraphs[length(paragraphs)+1]<-para
				# }
				# paragraphs %>% length

# html to text: method 2: generates full text & sentences
		secmainhtml<-str_to_lower(secmainhtml)
		if (length(grep('</p>|<br>|</font>|</dt>',secmainhtml))>0){
			text <- htmlToText(paste(secmainhtml,collapse=' '))
		} else { # if html is already text
			text<-secmainhtml
		}
		# write(text,'test.txt')

	sentences<-iconv(text, "latin1", "ASCII", sub="") %>% convert_text_to_sentences(.)
	dtsents<-data.table('sents'=sentences)[str_length(sents)>30]

		# if (length(grep('</p>|<br>|</font>|</dt>',sentences))+ length(grep('</p>|<br>|</font>|</dt>',paragraphs))>0) stop('parsing error')
	list('dtparas'=dtparas,'dtsents'=dtsents)
	
}


gen.dt.para<-function(textin){
# given textblocks (sentences or paragraphs), generate table for hedge count, 
dtpara<-data.table('para'=as.character(textin),'xhedg'=0,'xdebt'=0,'xcurr'=0,'xeur'=0,'xdeno'=0,'xcswp'=0,'xrisk'=0,'hedged'=0)
dtpara[para %like% 'hedg',xhedg:=1]
dtpara[para %like% 'debt|issu|borrow|bond|notes',xdebt:=1]
dtpara[para %like% 'currenc|exchang|fx',xcurr:=1]
dtpara[para %like% 'eur|dollar|aud|gbp|stering|cad|franc|yen|jpy|chf',xeur:=1]
dtpara[para %like% 'denominate',xdeno:=1]
dtpara[para %like% 'currency swap|forward contract|derivative',xcswp:=1]
dtpara[para %like% 'risk',xrisk:=1]
dtpara[,xsum:=xhedg+xdebt+xcurr+xeur+xdeno+xcswp+xrisk]
dtpara[xhedg & (xdebt | xdeno) & xcurr,hedged:=1]
dtpara[,length:=length(para)]
dtpara<-dtpara[order(-hedged,-xsum)][xsum>1]
# dtpara[hedged==1,para] %>% head(4)
dtpara
}


gen.hedge<-function(filename.in=str_c('sp500v3/',str_replace_all(dtind[i,path],'/','_')),fileout='',diag=F){
	# given filename.in, generate sentences and paragraphs, saving them, and given hedge count.
	textblock<-gen.text.sec(filename.in)
	dtsents<-gen.dt.para(textblock$sents)
	dtparas<-gen.dt.para(textblock$paras)
	if (fileout!='') save(textblock,file=str_c(fileout))
	dtout<-data.table('sentshedged'=dtsents[,sum(hedged)],'parashedged'=dtparas[,sum(hedged)])
	if (diag)
		list('result'=dtout,'dtsents'=dtsents,'dtparas'=dtparas)
	else
		dtout
}
gen.hedge.all<-function(dtin., fnindout='/mnt/disks/xccy/creditmigration/secdtindout160911.RData'){
	dtin<-copy(dtin.)
	dtin[,ioriginal:=rownames(dtin)]
	dtin %>% setkey(path)
	dtin<-unique(dtin)
	dtout<-dtin[downloaded==1]
	dtout[,parsed:=0]
	dtout[,error:=0]

	for (i in 1:nrow(dtout)){
		tryCatch({
			print(i)
			filename=str_c('/mnt/disks/xccy/sec/sp500v3/',str_replace_all(dtout[i,path],'/','_'))
			fnout=str_c('/mnt/disks/xccy/sec/blocktext_sp500v3/',str_replace_all(dtout[i,path],'/','_'),'.RData')
			dthedged<-gen.hedge(filename,fnout)
			dtout[i,sentshedged:=dthedged[,sentshedged]]
			dtout[i,parashedged:=dthedged[,parashedged]]
			dtout[i,parsed:=1]
		},error=function(e){dtout[i,error:=1]})
		if (mod(i,60)==0) save(dtout,file=fnindout)
	}
	dtout
}
gen.hedge.all2<-function(dtin.,startn=1){
	dtin<-copy(dtin.)
	dtin[,ioriginal:=rownames(dtin)]
	dtin %>% setkey(path)
	dtin<-unique(dtin)
	dtout<-dtin[downloaded==1]
	dtout[,parsed:=0]
	dtout[,error:=0]
	for (i in startn:nrow(dtout)){
		tryCatch({
			print(i)
			filename=str_c('/mnt/disks/xccy/sec/sp500v3/',str_replace_all(dtout[i,path],'/','_'))
			fnout=str_c('/mnt/disks/xccy/sec/blocktext_sp500v3/',str_replace_all(dtout[i,path],'/','_'),'.RData')
			if(!file.exists(fnout) | file.info(fnout)$size==0)
				dthedged<-gen.hedge(filename,fnout)
				dtout[i,sentshedged:=dthedged[,sentshedged]]
				dtout[i,parashedged:=dthedged[,parashedged]]
				dtout[i,parsed:=1]
		},error=function(e){dtout[i,error:=1]})
	}
	dtout
}
extract.text.all.par<-function(dtin.,startn=1,pathinprefix='/mnt/disks/xccy/sec/sp500v3/',pathoutprefix='/mnt/disks/xccy/sec/blocktext_sp500v3/',parcore=30){
  # previously known as gen.hedge.all.par
  # dtin.=dtdownloaded
  # startn=62
  # pathinprefix='/mnt/disks/xccy/sec/nonsp500v3/'
  # pathoutprefix='/mnt/disks/xccy/sec/blocktext_nonsp500v3/'
  # parcore=1
  
  require('doParallel')
  registerDoParallel(parcore)
  dtin<-copy(dtin.)
  dtin[,ioriginal:=rownames(dtin)]
  dtin %>% setkey(path)
  dtin<-unique(dtin)
  dtout<-dtin[downloaded==1]	
  # i=62
  foreach (i=startn:nrow(dtout)) %dopar% {
    # tryCatch({
      print(i)
      filename=str_c(pathinprefix,str_replace_all(dtout[i,path],'/','_'))
      fnout=str_c(pathoutprefix,str_replace_all(dtout[i,path],'/','_'),'.RData')
      if(!file.exists(fnout) | file.info(fnout)$size==0){
        textblock<-gen.text.sec(filename)
        if (!is.na(textblock)){ 
        	save(textblock,file=str_c(fnout))
        }
      }
    # },error=function(e){
    #   print(e)
    #   NA
    #   })
  }
}

download.sec<-function(dtindex.in,pathprefix='sp500v3/',startn=1,endn=nrow(dtindex.in)){	
	for (i in startn:endn){
			fn=str_c(pathprefix,str_replace_all(dtindex.in[i,path],'/','_'))
			if (!file.exists(fn) | file.info(fn)$size==0){
				print(str_c(i,' out of ',nrow(dtindex.in)))
				# Sys.sleep(1)
				download.file.wrap(urlstr=str_c('ftp://ftp.sec.gov/',dtindex.in[i,path]),fileout=str_c(pathprefix,str_replace_all(dtindex.in[i,path],'/','_')))
			}
		}
}

download.sec2<-function(dtindex.in,pathprefix='sp500v3/',startn=1,endn=nrow(dtindex.in)){
	for (i in startn:endn){
			fn=str_c(pathprefix,str_replace_all(dtindex.in[i,path],'/','_'))
			if (!file.exists(fn) | file.info(fn)$size==0){
				print(str_c(i,' out of ',nrow(dtindex.in)))
				urlstr=str_c('ftp://ftp.sec.gov/',dtindex.in[i,path])
				fileout=str_c(pathprefix,str_replace_all(dtindex.in[i,path],'/','_'))
				# secfile<-getURL(urlstr, ssl.verifyhost=FALSE, ssl.verifypeer=FALSE)
				# write(secfile,fileout)
				print(str_c('url: ',urlstr))
				print(str_c(' fileout: ',fileout))
				# cmdstr<-str_c('wget -O ', fileout, ' ',urlstr)
				cmdstr<-str_c('curl -u anonymous:liao@fas.harvard.edu -o ', fileout, ' ',urlstr)
				print(cmdstr)
				system(cmdstr)
			}
		}
}
download.sec.par<-function(dtindex.in,pathprefix='sp500v3/',startn=1,endn=nrow(dtindex.in)){
	require('doParallel')
	registerDoParallel(4)
	foreach (i=startn:endn) %dopar% {
			fn=str_c(pathprefix,str_replace_all(dtindex.in[i,path],'/','_'))
			if (!file.exists(fn) | file.info(fn)$size==0){
				# print(str_c(i,' out of ',nrow(dtindex.in)))
				urlstr=str_c('ftp://ftp.sec.gov/',dtindex.in[i,path])
				fileout=str_c(pathprefix,str_replace_all(dtindex.in[i,path],'/','_'))
				# print(str_c('url: ',urlstr))
				# print(str_c(' fileout: ',fileout))
				cmdstr<-str_c('curl -u anonymous:liao@fas.harvard.edu -o ', fileout, ' ',urlstr)
				# print(cmdstr)
				system(cmdstr)
			}
		}
}
markdownloaded<-function(dtin.,prefix='sp500v3/',parcore=30){
	dtin<-copy(dtin.)
	dtin[,downloaded:=0]
	require('doParallel')
	registerDoParallel(parcore)

	outlist<-foreach (i=1:nrow(dtin)) %dopar% {
		dtout<-copy(dtin[i])
		fn=str_c(prefix,str_replace_all(dtout[,path],'/','_'))
		if (file.exists(fn) & file.info(fn)$size>0){
			dtout[,downloaded:=1]			
		} else {
			dtout
		}
	}
	out<-rbindlist(outlist)
	print(out[,.N,downloaded])
	out
}
mark.parsed<-function(dtin.){
	dtin<-copy(dtin.)
	dtin[,ioriginal:=rownames(dtin)]
	dtin %>% setkey(path)
	dtin<-unique(dtin)
	dtout<-dtin[downloaded==1]
	dtout[,parsed:=0]
	for (i in 1:nrow(dtout)) {
			# print(i)
			filename=str_c('/mnt/disks/xccy/sec/sp500v3/',str_replace_all(dtout[i,path],'/','_'))
			fnout=str_c('/mnt/disks/xccy/sec/blocktext_sp500v3/',str_replace_all(dtout[i,path],'/','_'),'.RData')
			if(file.exists(fnout) & file.info(fnout)$size>0){
				dtout[i,parsed:=1]
			}
	}
	print(dtout[,.N,parsed])	
	dtout
}
construct.hedge.result.single<-function(filename){
# load a single file, used for construct.hedge.result.all
	load(filename)
	dtsents<-gen.dt.para(textblock$sents)
	dtparas<-gen.dt.para(textblock$paras)
	
	dtsents[,'type':='sents']
	dtparas[,'type':='paras']
	dtsp<-rbind(dtsents,dtparas)
	# dtsp[,'filename':=filename]
	# dtsp[,'cik':=cik]
	dtsp
}

construct.hedge.result.all<-function(dtin.,parcore=24, fileprefix='/mnt/disks/xccy/sec/blocktext_sp500v3/'){
	require('doParallel')
	registerDoParallel(parcore)
	outlist<-foreach(i=1:nrow(dtin.)) %dopar% {
		filename=str_c(fileprefix,str_replace_all(dtin.[i,path],'/','_'),'.RData')
		
		dtoutsingle<-construct.hedge.result.single(filename)
		dtoutsingle[,'cik':=dtin.[i,cik]]
		dtoutsingle[,'filename':=dtin.[i,filename]]
		dtoutsingle[,'conm':=dtin.[i,conm]]
		dtoutsingle[,'date':=dtin.[i,date]]
		dtoutsingle
	}
	outlist
}

recalc.hedge<-function(dtin){
	dtin[para %like% 'hedge',xhedg:=1]
	dtin[para %like% 'debt|issuance|bond',xdebt:=1]
	dtin[para %like% 'currenc|foreign.+exchange|fx|exchange+.rate',xcurr:=1]
	dtin[para %like% 'eur|dollar|aud|gbp|stering|cad|franc|yen|jpy|chf',xeur:=1]
	dtin[para %like% 'denominate',xdeno:=1]
	dtin[para %like% 'currency swap|forward contract|derivative',xcswp:=1]
	dtin[para %like% 'risk',xrisk:=1]
	dtin[,xsum:=xhedg+xdebt+xcurr+xeur+xdeno+xcswp+xrisk]
	dtin[xhedg & xdebt & xdeno & xcurr,hedged:=1]
}