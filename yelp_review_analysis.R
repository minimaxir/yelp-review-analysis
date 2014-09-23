source("Rstart.R")
library(caret)

rank_colors = c(brewer.pal(9, "Reds")[c(8,7,6)],brewer.pal(9, "Greens")[c(7,8)])

data_reviews <- droplevels(tbl_df(read.csv("yelp_reviews.csv",header=T)) %>% arrange(business_id))
data_reviews$is_positive = as.factor(ifelse(data_reviews$stars >= 4, "1", "0"))

# Linear Regression
reviews_reg = lm(stars ~ review_length + pos_words + neg_words, data=data_reviews)

# Logistic Regression + Cross-Validation
trainIndex <- unlist(createDataPartition(data_reviews$is_positive, p = 0.8))
reviews_reg_logit = glm(is_positive ~ review_length + pos_words + neg_words, family = "binomial", data=data_reviews[trainIndex,])
predicted_prob_pos = predict(reviews_reg_logit,data_reviews[-trainIndex,])

# Truth Table
threshold <- 0.50 # if predicted probability is greater than 0.5, say that the review is positive.
accuracy <- sum(ifelse(predicted_prob_pos > threshold,1,0)==data_reviews$is_positive[-trainIndex]) / length(data_reviews$is_positive[-trainIndex]) # 0.75

### Wordclouds

stop_words <- unlist(strsplit("a,able,about,across,after,all,almost,also,am,among,an,and,any,are,as,at,be,because,been,but,by,can,cannot,could,dear,did,do,does,either,else,ever,every,for,from,get,got,had,has,have,he,her,hers,him,his,how,however,i,if,in,into,is,it,its,just,least,let,like,likely,may,me,might,most,must,my,neither,no,nor,not,of,off,often,on,only,or,other,our,own,rather,said,say,says,she,should,since,so,some,than,that,the,their,them,then,there,these,they,this,tis,to,too,twas,us,wants,was,we,were,what,when,where,which,while,who,whom,why,will,with,would,yet,you,your,1,2,3,4,5,6,7,8,9,ii", ","))

# 2-gram

count_stop_words <- function(x) {
	word_array <- strsplit(as.character(x)," ")[[1]]
	 
	return (sum(word_array %in% stop_words))
}

words_2gram <- tbl_df(read.csv("yelp_words_by_stars_2gram.csv", header=T)) %>% filter(lapply(word, count_stop_words) < 1)
words_2gram_1star <- words_2gram %>% filter(stars == 1)
words_2gram_5star <- words_2gram %>% filter(stars == 5)


pal <- brewer.pal(9, "Reds")
pal <- pal[-c(1:3)]
png(filename = "yelp-words-2gram-1star.png", width = 3000, height = 3000, res= 300)

wordcloud(toupper(words_2gram_1star$word), words_2gram_1star$count, scale=c(11,.1), random.order=F, rot.per=.10, max.words=5000, colors=pal, family=fontFamily, random.color=T)

dev.off()

pal <- brewer.pal(9, "Greens")
pal <- pal[-c(1:3)]
png(filename = "yelp-words-2gram-5star.png", width = 3000, height = 3000, res= 300)

wordcloud(toupper(words_2gram_5star$word), words_2gram_5star$count, scale=c(9,.1), random.order=F, rot.per=.10, max.words=5000, colors=pal, family=fontFamily, random.color=T)

dev.off()

words_3gram <- tbl_df(read.csv("yelp_words_by_stars_3gram.csv", header=T)) %>% filter(lapply(word, count_stop_words) < 2)
words_3gram_1star <- words_3gram %>% filter(stars == 1)
words_3gram_5star <- words_3gram %>% filter(stars == 5)

pal <- brewer.pal(9, "Reds")
pal <- pal[-c(1:3)]
png(filename = "yelp-words-3gram-1star.png", width = 3000, height = 3000, res= 300)

wordcloud(toupper(words_3gram_1star$word), words_3gram_1star$count, scale=c(7,.1), random.order=F, rot.per=.10, max.words=5000, colors=pal, family=fontFamily, random.color=T)

dev.off()

pal <- brewer.pal(9, "Greens")
pal <- pal[-c(1:3)]
png(filename = "yelp-words-3gram-5star.png", width = 3000, height = 3000, res= 300)

wordcloud(toupper(words_3gram_5star$word), words_3gram_5star$count, scale=c(7,.1), random.order=F, rot.per=.10, max.words=5000, colors=pal, family=fontFamily, random.color=T)

dev.off()

### Charts

# Positivity Histogram

 ggplot(aes(x=(pos_words+0.0001)/review_length, fill=as.factor(stars), color=as.factor(stars)), data=data_reviews) +
  geom_bar() +
  scale_x_continuous(limits=c(0,0.25), label = percent) +
  scale_y_continuous(label = comma) +
  theme_custom() + 
  theme(legend.title = element_blank(), legend.position="top", legend.direction="horizontal", legend.key.width=unit(0.5, "cm"), legend.key.height=unit(0.25, "cm"), legend.margin=unit(-0.5,"cm"), panel.margin=element_blank()) +
  labs(title=paste("Yelp Review Positivity, by # of Stars for", format(nrow(data_reviews),big.mark=","),"Reviews"), x="% Review Positivity (# Positive Words : # Words)", y="Total # of Reviews") +
  scale_fill_manual(values=rank_colors, labels = c("1 Star", "2 Stars", "3 Stars", "4 Stars", "5 Stars")) +
  scale_color_manual(values=rank_colors, labels = c("1 Star", "2 Stars", "3 Stars", "4 Stars", "5 Stars")) +
  annotate("text", x = Inf, y = -Inf, label = "max woolf — minimaxir.com",hjust=1.1, vjust=-0.5, col="#1a1a1a", family=fontFamily, alpha = 0.10, size=2)
  
  ggsave("yelp-review-positivity.png", dpi=300, height=3, width=4)
  
# Positivity Density
  
ggplot(aes(x=(pos_words+0.0001)/review_length, fill=as.factor(stars), color=as.factor(stars), y=..density..), data=data_reviews) +
  geom_density(alpha = 1, position="fill") +
  scale_x_continuous(limits=c(0,0.25), label = percent) +
  scale_y_continuous(label = percent) +
  theme_custom() + 
  theme(legend.title = element_blank(), legend.position="top", legend.direction="horizontal", legend.key.width=unit(0.5, "cm"), legend.key.height=unit(0.25, "cm"), legend.margin=unit(-0.5,"cm"), panel.margin=element_blank()) +
  labs(title=paste("Yelp Positivity Proportion, by # Stars for", format(nrow(data_reviews),big.mark=","),"Reviews"), x="% Review Positivity (# Positive Words : # Words)", y="Proportion of Reviews") +
  scale_fill_manual(values=rank_colors, labels = c("1 Star", "2 Stars", "3 Stars", "4 Stars", "5 Stars")) +
  scale_color_manual(values=rank_colors, labels = c("1 Star", "2 Stars", "3 Stars", "4 Stars", "5 Stars")) +
  annotate("text", x = Inf, y = -Inf, label = "max woolf — minimaxir.com",hjust=1.1, vjust=-0.5, col="#1a1a1a", family=fontFamily, alpha = 0.10, size=2)
  
 ggsave("yelp-review-positivity-density.png", dpi=300, height=3, width=4)

# Negativity Histogram
 
 ggplot(aes(x=(neg_words+0.0001)/review_length, fill=as.factor(stars), color=as.factor(stars)), data=data_reviews) +
  geom_bar() +
  scale_x_continuous(limits=c(0,0.25), label = percent) +
  scale_y_continuous(label = comma) +
  theme_custom() + 
  theme(legend.title = element_blank(), legend.position="top", legend.direction="horizontal", legend.key.width=unit(0.5, "cm"), legend.key.height=unit(0.25, "cm"), legend.margin=unit(-0.5,"cm"), panel.margin=element_blank()) +
  labs(title=paste("Yelp Review Negativity, by # of Stars for", format(nrow(data_reviews),big.mark=","),"Reviews"), x="% Review Negativity (# Negative Words : # Words)", y="Total # of Reviews") +
  scale_fill_manual(values=rank_colors, labels = c("1 Star", "2 Stars", "3 Stars", "4 Stars", "5 Stars")) +
  scale_color_manual(values=rank_colors, labels = c("1 Star", "2 Stars", "3 Stars", "4 Stars", "5 Stars")) +
  annotate("text", x = Inf, y = -Inf, label = "max woolf — minimaxir.com",hjust=1.1, vjust=-0.5, col="#1a1a1a", family=fontFamily, alpha = 0.10, size=2)
  
 ggsave("yelp-review-negativity.png", dpi=300, height=3, width=4)
  
 # Negativity Density
  
 ggplot(aes(x=(neg_words+0.0001)/review_length, fill=as.factor(stars), color=as.factor(stars), y=..density..), data=data_reviews) +
  geom_density(alpha = 1, position="fill") +
  scale_x_continuous(limits=c(0,0.25), label = percent) +
  scale_y_continuous(label = percent) +
  theme_custom() + 
  theme(legend.title = element_blank(), legend.position="top", legend.direction="horizontal", legend.key.width=unit(0.5, "cm"), legend.key.height=unit(0.25, "cm"), legend.margin=unit(-0.5,"cm"), panel.margin=element_blank()) +
  labs(title=paste("Yelp Negativity Proportion, by # Stars for", format(nrow(data_reviews),big.mark=","),"Reviews"), x="% Review Negativity (# Negative Words : # Words)", y="Proportion of Reviews") +
  scale_fill_manual(values=rank_colors, labels = c("1 Star", "2 Stars", "3 Stars", "4 Stars", "5 Stars")) +
  scale_color_manual(values=rank_colors, labels = c("1 Star", "2 Stars", "3 Stars", "4 Stars", "5 Stars")) +
  annotate("text", x = Inf, y = -Inf, label = "max woolf — minimaxir.com",hjust=1.1, vjust=-0.5, col="#1a1a1a", family=fontFamily, alpha = 0.10, size=2)
  
 ggsave("yelp-review-negativity-density.png", dpi=300, height=3, width=4)
  
### Time Series
 
reviews_monthly_stars <- data_reviews %>% group_by(date=substr(date,1,7), stars) %>% summarize(count=n()) %>%
arrange(desc(date))
reviews_monthly_stars <- droplevels(reviews_monthly_stars)

# Time Series Stacked

 ggplot(aes(x=as.POSIXct(paste(date,"-01",sep="")), y=count, fill=as.factor(stars)), data=reviews_monthly_stars) +
  geom_area(position = "stack") +
  scale_x_datetime(breaks = date_breaks("1 year"), labels = date_format("%Y")) +
  scale_y_continuous(label = comma) +
  theme_custom() + 
  theme(legend.title = element_blank(), legend.position="top", legend.direction="horizontal", legend.key.width=unit(0.25, "cm"), legend.key.height=unit(0.25, "cm"), legend.margin=unit(-0.5,"cm"), panel.margin=element_blank()) +
  labs(title=paste("# of Yelp Reviews by Month, by # Stars for", format(nrow(data_reviews),big.mark=","),"Reviews"), x="Date of Review Submission", y="Total # of Review Submissions (by Month)") +
  scale_fill_manual(values=rank_colors, labels = c("1 Star", "2 Stars", "3 Stars", "4 Stars", "5 Stars")) +
  scale_color_manual(values=rank_colors, labels = c("1 Star", "2 Stars", "3 Stars", "4 Stars", "5 Stars"))
  
  ggsave("yelp-review-time-series.png", dpi=300, height=3, width=4)
  
# Time Series Density
 
ggplot(aes(x=as.POSIXct(paste(date,"-01",sep="")), y=count, fill=as.factor(stars)), data=reviews_monthly_stars) +
  geom_area(position = "fill") +
  scale_x_datetime(breaks = date_breaks("1 year"), labels = date_format("%Y")) +
  scale_y_continuous(label = percent) +
  theme_custom() + 
  theme(legend.title = element_blank(), legend.position="top", legend.direction="horizontal", legend.key.width=unit(0.25, "cm"), legend.key.height=unit(0.25, "cm"), legend.margin=unit(-0.5,"cm"), panel.margin=element_blank()) +
  labs(title=paste("Rating Proportion Over Time, by # Stars for", format(nrow(data_reviews),big.mark=","),"Reviews"), x="Date of Review", y="Proportion of All Yelp Reviews") +
  scale_fill_manual(values=rank_colors, labels = c("1 Star", "2 Stars", "3 Stars", "4 Stars", "5 Stars")) +
  scale_color_manual(values=rank_colors, labels = c("1 Star", "2 Stars", "3 Stars", "4 Stars", "5 Stars"))
  
  ggsave("yelp-review-time-proportion.png", dpi=300, height=3, width=4)
  
  
 reviews_monthly_positivity <- data_reviews %>% group_by(date=substr(date,1,7)) %>%
	summarize(count=n(),
	positivity = mean(pos_words) / mean(review_length),
	negativity = mean(neg_words) / mean(review_length)) %>%
	arrange(desc(date))
	
# Monthly Positivity
  
    ggplot(aes(x=as.POSIXct(paste(date,"-01",sep="")), y=positivity), data=reviews_monthly_positivity) +
geom_line(color = rank_colors[5]) +
 scale_x_datetime(breaks = date_breaks("1 year"), labels = date_format("%Y")) +
  scale_y_continuous(label = percent) +
  theme_custom() + 
  labs(title=paste("Positivity of Yelp Reviews by Month for", format(nrow(data_reviews),big.mark=","),"Reviews"), x="Date of Review Submission", y="Average % Review Positivity (by Month)")
  
ggsave("yelp-review-time-series-positivity.png", dpi=300, height=3, width=4)
  
# Monthly Negativity
 
ggplot(aes(x=as.POSIXct(paste(date,"-01",sep="")), y=negativity), data=reviews_monthly_positivity) +
  geom_line(color = rank_colors[1]) +
  scale_x_datetime(breaks = date_breaks("1 year"), labels = date_format("%Y")) +
  scale_y_continuous(label = percent) +
  theme_custom() + 
  labs(title=paste("Negativity of Yelp Reviews by Month for", format(nrow(data_reviews),big.mark=","),"Reviews"), x="Date of Review Submission", y="Average % Review Negativity (by Month)")
  
ggsave("yelp-review-time-series-negativity.png", dpi=300, height=3, width=4)
  
### Year Donut Charts
# Methodology is unusual: Must create each rectangle manually then warp to polar coordinates

# 2005

 data_reviews_agg_year <- data_reviews %>%
 	filter(substr(date,1,4) == 2005) %>%
 	group_by(stars) %>%
 	summarize(count = n()) %>%
 	mutate(fraction = count / sum(count),
 			ymax = cumsum(fraction),
 			ymin = c(0, head(ymax, n=-1)))
 
ggplot(aes(fill=as.factor(stars), color=as.factor(stars), ymax=ymax, ymin=ymin, xmax=5, xmin=4), data=data_reviews_agg_year) +
     geom_rect(color="white") +
     coord_polar(theta="y") +
     annotate("text", label = paste(format(data_reviews_agg_year$fraction * 100, digits=2),"%",sep=''), x=rep(6,5), y=(data_reviews_agg_year$ymin + data_reviews_agg_year$ymax)/2, col=rank_colors, size=3, family=fontTitle) +
     xlim(c(0, 6)) +
     theme_custom() +
     theme(panel.grid=element_blank(), axis.text=element_blank(), axis.ticks=element_blank(), panel.background=element_blank(), axis.title.x = element_blank(), axis.title.y=element_blank(),legend.title = element_blank(), legend.position="top", legend.direction="horizontal", legend.key.width=unit(0.5, "cm"), legend.key.height=unit(0.25, "cm"), legend.margin=unit(-0.5,"cm"), plot.margin = unit(c(-0.5,-0.5,-0.5,-0.5), "cm"), panel.border= element_blank()) +
     labs(title="Customized ring plot") +
     scale_fill_manual(values=rank_colors, labels = c("1 Star", "2 Stars", "3 Stars", "4 Stars", "5 Stars")) +
  scale_color_manual(values=rank_colors, labels = c("1 Star", "2 Stars", "3 Stars", "4 Stars", "5 Stars")) +
     annotate("text", x = 0, y = 0, label = "2005", col="#1a1a1a", family="Source Sans Pro Light", size=11)
     
ggsave("yelp-proportion-2005.png", dpi=300, width=3, height=3)
  
  
 # 2014

 data_reviews_agg_year <- data_reviews %>%
 	filter(substr(date,1,4) == 2014) %>%
 	group_by(stars) %>%
 	summarize(count = n()) %>%
 	mutate(fraction = count / sum(count),
 			ymax = cumsum(fraction),
 			ymin = c(0, head(ymax, n=-1)))
 
ggplot(aes(fill=as.factor(stars), color=as.factor(stars), ymax=ymax, ymin=ymin, xmax=5, xmin=4), data=data_reviews_agg_year) +
     geom_rect(color="white") +
     coord_polar(theta="y") +
     annotate("text", label = paste(format(data_reviews_agg_year$fraction * 100, digits=2),"%",sep=''), x=rep(6,5), y=(data_reviews_agg_year$ymin + data_reviews_agg_year$ymax)/2, col=rank_colors, size=3, family=fontTitle) +
     xlim(c(0, 6)) +
     theme_custom() +
     theme(panel.grid=element_blank(), axis.text=element_blank(), axis.ticks=element_blank(), panel.background=element_blank(), axis.title.x = element_blank(), axis.title.y=element_blank(),legend.title = element_blank(), legend.position="top", legend.direction="horizontal", legend.key.width=unit(0.5, "cm"), legend.key.height=unit(0.25, "cm"), legend.margin=unit(-0.5,"cm"), plot.margin = unit(c(-0.5,-0.5,-0.5,-0.5), "cm"), panel.border= element_blank()) +
     labs(title="Customized ring plot") +
     scale_fill_manual(values=rank_colors, labels = c("1 Star", "2 Stars", "3 Stars", "4 Stars", "5 Stars")) +
  scale_color_manual(values=rank_colors, labels = c("1 Star", "2 Stars", "3 Stars", "4 Stars", "5 Stars")) +
     annotate("text", x = 0, y = 0, label = "2014", col="#1a1a1a", family="Source Sans Pro Light", size=11)
     
ggsave("yelp-proportion-2014.png", dpi=300, width=3, height=3)
  