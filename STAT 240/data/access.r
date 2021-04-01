library(twitteR)
consumer_key = ' ... '
consumer_secret = ' ... '
access_token = ' ... '
access_secret = ' ... '

setup_twitter_oauth(consumer_key,
  consumer_secret,
  access_token,
  access_secret)

rm(list = c(
    'consumer_key',
    'consumer_secret',
    'access_token',
    'access_secret'
  )
)