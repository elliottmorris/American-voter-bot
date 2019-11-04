# bot that tweets random lines from CCES for all eternity
import tweepy, time, random, sys, csv, pandas


### globals:
consumer_key = ""
consumer_secret = ""
auth = tweepy.OAuthHandler(consumer_key, consumer_secret)
access_token = ""
access_token_secret = ""
auth.set_access_token(access_token, access_token_secret)
api = tweepy.API(auth)


### get row index from file, then write the iteration
# read
file = open("AmericanVoterBot/index.txt","r")
index = file.read()
file.close()

index = int(index) +1
# write & closed
file = open("index.txt","w")
file.write(str(index))
file.close()

### get row it
ccesv = pandas.read_csv('AmericanVoterBot/CCES_ANNOTATED.csv')
sentence = ccesv.iloc[index,0]

sentence = str(sentence)
print(str(sentence))
 
api.update_status(sentence)
