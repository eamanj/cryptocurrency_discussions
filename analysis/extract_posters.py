#!/usr/bin/python

import argparse
import csv
import os
import sys
import utils
import pandas as pd
import numpy as np

parser = argparse.ArgumentParser(
    description='This script extracts coin introducers and their stats from '
                'a file that contains coin introduction URLs in the forum.')
parser.add_argument("forum_input",
                    help="The location of CSV file containing posts from all forums "
                    "sorted by date and time. The last two columns of file should "
                    "contain the list of modified and unmodified coins mentions in the "
                    "post.")
parser.add_argument("coins_earliest_trade_dates",
                    help="A CSV file containing the earliest possible introduction date "
                    "for each coin. The first line is the header and each "
                    "following line looks like 'coin-name,YYYY/MM/DD' where the date is "
                    "the earliest possible introduction date for that coin.")
parser.add_argument("coins_introduction_urls",
                   help="A CSV file mapping coin symbol to its (multiple) announcement "
                   "urls.")
parser.add_argument("output_dir",
                    help="The directory where the users lists will be written to.")
args = parser.parse_args()


if __name__ == '__main__':
  if os.path.isfile(args.output_dir):
    sys.exit("Output dir " + args.output_dir + " is an existing file.")
  if not os.path.isdir(args.output_dir):
    os.makedirs(args.output_dir)
  coins_earliest_trade_dates = pd.read_csv(args.coins_earliest_trade_dates, index_col=0,
                                           parse_dates = ['earliest_trade_date'])
  coins_earliest_trade_dates.index.str.upper()

  coins_urls = pd.read_csv(args.coins_introduction_urls, index_col=0)
  coins_urls.index.str.upper()
  url_columns = [col for col in coins_urls.columns if 'url' in col]
  coins_urls = coins_urls.fillna('')

  # write invalid coin introduction urls
  invalid_mask1 = np.logical_and.reduce([~coins_urls[col].str.contains('bitcointalk')
                                         for col in url_columns])
  invalid_mask2 = np.logical_and.reduce([~coins_urls[col].str.contains('index\.php\?topic')
                                         for col in url_columns])
  invalid_mask3 = coins_urls['name'] == ''
  invalid_coins = coins_urls.loc[invalid_mask1 | invalid_mask2 | invalid_mask3]
  invalid_coins.index.name = coins_urls.index.name
  invalid_coins_filename = os.path.join(args.output_dir, "invalid_introductionurls.csv")
  invalid_coins.to_csv(invalid_coins_filename)
  
  # write coin introduction urls that appear in our trade dates
  matching_coins_filename = os.path.join(args.output_dir,
                                         "matching_between_coinlist_introductionurls.csv")
  matching_coins = coins_urls.loc[coins_urls.index.intersection(coins_earliest_trade_dates.index)]
  matching_coins.index.name = coins_urls.index.name
  matching_coins.to_csv(matching_coins_filename)
  
  # write coin introduction urls that do not appear in our trade dates but appear in
  # introduction urls
  missing_price_coins_filename = os.path.join(args.output_dir,
                                              "missing_from_coinlist.csv")
  missing_price_coins = coins_urls.loc[coins_urls.index.difference(coins_earliest_trade_dates.index)]
  missing_price_coins.index.name = coins_urls.index.name
  missing_price_coins.to_csv(missing_price_coins_filename)

  # write coins that do not appear in our url list but we have their trade dates
  missing_url_coins_filename = os.path.join(args.output_dir,
                                            "missing_from_introductionurls.csv")
  missing_url_coins = coins_earliest_trade_dates.loc[coins_earliest_trade_dates.index.difference(coins_urls.index)]
  missing_url_coins.index.name = coins_earliest_trade_dates.index.name
  missing_url_coins.to_csv(missing_url_coins_filename)

  posts = utils.read_forum_data(args.forum_input)

  # find the info on the first post of each url for each coin
  missing_urls = dict()
  coins_info = list()
  for symbol, row in matching_coins.iterrows():
    coin_info = dict()
    earliest_trade_date = coins_earliest_trade_dates.loc[symbol, 'earliest_trade_date']
    coin_info['symbol'] = symbol
    coin_info['name'] = row['name']
    coin_info['earliest_trade_date'] = earliest_trade_date.strftime('%Y-%m-%d')
    for i, url in enumerate(row[url_columns], start=1):
      if url == '':
        continue
      url_posts = posts.loc[posts['url'] == url]
      if url_posts.empty:
        missing_urls[url] = symbol
        continue
      url_first_post_id = url_posts['date_time'].idxmin()
      url_first_post = url_posts.loc[url_first_post_id]
      user = url_first_post['user']
      date = url_first_post['date']
      user_posts = posts.loc[posts['user'] == user]
      user_subjects = posts.loc[posts['started_by'] == user]

      coin_info['user{}'.format(i)] = user
      coin_info['user{}_url'.format(i)] = url
      coin_info['user{}_date'.format(i)] = date.strftime('%Y-%m-%d')
      # number of posts made by the user before the earliest trade date
      coin_info['user{}_num_posts'.format(i)] = user_posts.loc[
          user_posts['date'] <= earliest_trade_date].shape[0]
      # number of subjects started by the user before the earliest trade date
      coin_info['user{}_num_subjects'.format(i)] = user_subjects.loc[
          user_subjects['date'] <= earliest_trade_date, 'thread_id'].unique().size
      # number of days the user has been on the thread until the earliest trade date
      coin_info['user{}_days_since_first_post'.format(i)] = (earliest_trade_date - user_posts['date'].min()).days

    # write if at least one url was present
    if any(url not in missing_urls.keys() for url in row[url_columns] if url != ''):
      coins_info.append(coin_info)

  # write forum info on coins for which we have forum-data, introduction url info and
  # price data
  fields = ['symbol', 'name', 'earliest_trade_date']
  for i in range(1, len(url_columns)+1):
    fields.extend(['user{}'.format(i),
                   'user{}_url'.format(i),
                   'user{}_date'.format(i),
                   'user{}_num_posts'.format(i),
                   'user{}_num_subjects'.format(i),
                   'user{}_days_since_first_post'.format(i)])
  coins_info_filename = os.path.join(args.output_dir, "coin_announcement.csv")
  writer = csv.DictWriter(open(coins_info_filename, 'w'), fields)
  writer.writeheader()
  for coin_info in coins_info:
    writer.writerow(coin_info)

  # write coins that appear in our introduction list, but their url is missing from forum
  # data
  missing_url_forums_filename = os.path.join(args.output_dir,
                                             "missing_url_from_forums.csv")
  with open(missing_url_forums_filename, 'w') as outfile:
    csv_out=csv.writer(outfile)
    csv_out.writerow(['symbol','url'])
    for url, symbol in missing_urls.iteritems():
      csv_out.writerow([symbol, url])
