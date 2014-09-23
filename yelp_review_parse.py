import json
import datetime
import csv
import datetime
from nltk import word_tokenize
from string import punctuation
import re

# From http://locallyoptimal.com/blog/2013/01/20/elegant-n-gram-generation-in-python/

def find_ngrams(input_list, n):
  return zip(*[input_list[i:] for i in range(n)])

filename = "/Users/maxwoolf/Downloads/yelp_dataset_challenge_academic_dataset/yelp_academic_dataset_review.json"
count = 0
pattern = re.compile("[^\w']")

# words_1gram =  {1: {}, 2: {}, 3: {}, 4: {}, 5: {}}
# words_2gram =  {1: {}, 2: {}, 3: {}, 4: {}, 5: {}}
# words_3gram =  {1: {}, 2: {}, 3: {}, 4: {}, 5: {}}

with open("positive-words.txt") as f:
	pos_words = f.read().split()[213:]

with open("negative-words.txt") as f:
	neg_words = f.read().split()[213:]

with open('yelp_reviews.csv', 'wb') as file:
	w = csv.writer(file)
	w.writerow(["user_id","business_id","date","stars","review_length","votes_funny","votes_useful","votes_cool", "votes_total", "pos_words", "neg_words", "net_sentiment"])
	with open(filename) as f:
		for line in f:
			data = json.loads(line)
			votes = data['votes']
			text = data['text'].encode('utf-8').lower()
			text_tokens = pattern.sub(' ', text).split()
			stars = data['stars']
			
			#review_length = sum(1 for c in text if c.isalpha())
			review_length = len(text_tokens)
			votes_total = votes['funny'] + votes['useful'] + votes['cool']
			num_positive = sum([r in pos_words for r in text_tokens])
			num_negative = sum([r in neg_words for r in text_tokens])
			net_sentiment = num_positive - num_negative
				
# 			text_tokens_2gram = find_ngrams(text_tokens, 2)
# 			text_tokens_3gram = find_ngrams(text_tokens, 3)
			
# 			for word in text_tokens:
# 				if word not in words_1gram[stars]:
# 					words_1gram[stars][word] = 1
# 				else:
# 					words_1gram[stars][word] += 1
# 					
# 			for word in text_tokens_2gram:
# 				if word not in words_2gram[stars]:
# 					words_2gram[stars][word] = 1
# 				else:
# 					words_2gram[stars][word] += 1	
# 					
# 			for word in text_tokens_3gram:
# 				if word not in words_3gram[stars]:
# 					words_3gram[stars][word] = 1
# 				else:
# 					words_3gram[stars][word] += 1		
	

			
			w.writerow([data['user_id'], data['business_id'], data['date'], data['stars'], review_length, votes['funny'], votes['useful'], votes['cool'], votes_total, num_positive, num_negative, net_sentiment])
			count = count + 1
			if count % 1000 == 0:
  				print "%s: %s" % (count, datetime.datetime.now())
  				

# with open('yelp_words_by_stars.csv', 'wb') as file:
# 	w = csv.writer(file)
# 	w.writerow(['word','count', 'stars'])
# 	for stars, wordcount in words_1gram.items():
# 		for word, count in wordcount.items():
# 			if count > 100:
# 				w.writerow([word, count, stars])
# 				
# with open('yelp_words_by_stars_2gram.csv', 'wb') as file:
# 	w = csv.writer(file)
# 	w.writerow(['word','count', 'stars'])
# 	for stars, wordcount in words_2gram.items():
# 		for word, count in wordcount.items():
# 			if count > 100:
# 				w.writerow([' '.join(word), count, stars])
# 				
# with open('yelp_words_by_stars_3gram.csv', 'wb') as file:
# 	w = csv.writer(file)
# 	w.writerow(['word','count', 'stars'])
# 	for stars, wordcount in words_3gram.items():
# 		for word, count in wordcount.items():
# 			if count > 100:
# 				w.writerow([' '.join(word), count, stars])