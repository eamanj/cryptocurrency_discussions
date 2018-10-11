#!/usr/bin/python

import argparse
import os
import sys
import csv
import pandas as pd
import numpy as np
import utils
from collections import defaultdict
from sklearn.cluster import *
from sklearn.preprocessing import *
from sklearn.decomposition import PCA, TruncatedSVD
from sklearn.metrics import silhouette_samples, silhouette_score
import matplotlib
import matplotlib.pyplot as plt
import matplotlib.cm as cm

parser = argparse.ArgumentParser(
    description="This script clusters users based on number of posts they made in each "
                "announcment thread and computes several stats for each cluster.")
parser.add_argument("forum_input",
                    help="The location of CSV file containing posts from all forums "
                    "sorted by date and time. The last two columns of file should "
                    "contain the list of modified and unmodified coins mentions in the "
                    "post.")
parser.add_argument("price_input",
                    help="The location of CSV file containing price stats, including "
                    "volatility measures per coin.")
parser.add_argument("thread_input",
                    help="The location of CSV file containing threads stats, including "
                    "earliest mention and trade dates per coin.")
parser.add_argument("-c", dest="num_clusters", type=int, default=2,
                    help="The number of clusters to use in kmeans")
parser.add_argument("-e", dest="end_date", default=None,
                    help="If provided, it will be the end date (YYYY-MM-DD) of the "
                    "period during which we will compute thread metrics. If not "
                    "provided, the default would use the earliest trade date as the end "
                    "date. As opposed to default behavior, when end_date is specified "
                    "this period will be the same for all coins and the date should be "
                    "specified in YYYY-MM-DD format. The length of the period, which "
                    "will determine the start date can be  "
                    "specified seperately")
parser.add_argument("-l", dest="period_length", type=int, default=600,
                    help="The length of the period (days) during which we will compute "
                    "thread metrics. Depending on whether end_date is specified, the "
                    "period will end in end_date or earliest_trade_date. This argument "
                    "will determine the length of period, or effectively its start date. "
                    "Default is 600 days.")
parser.add_argument("-t", dest="num_posts_threshold", type=int, default=0,
                    help="Defines the threshold for the minimum number of posts a user "
                    "should have made in a thread before it's considered the user has "
                    "actually contributed to the thread. Default is 0.")
parser.add_argument("-v", dest="valid_coin_symbols",
                    help="If provided, it has to be a file with the symbol of valid "
                    "coins, one per line. If provided, the metrics only for these "
                    "coins will be computed. Also all the metrics that relate the coin "
                    "to other announcement threads will be measured based on the coins "
                    "present in this set and not all the coins present in coins_info "
                    "For example, num_unique_announcement_threads will contain the "
                    "number announcement threads belonging to these limited coins that "
                    "are connected to the coin of interest.")
args = parser.parse_args()


def hierarchical(data):
  cl = AgglomerativeClustering(n_clusters=args.num_clusters,
                               affinity='euclidean',
                               #affinity='cosine',
                               #linkage='average')
                               linkage='ward')
  cl.fit(data)
  labels = cl.labels_
  results = pd.DataFrame(data=labels, columns=['cluster'], index=data.index)
  #silhouette_plot(data, labels)
  return results
  

def dbscan(data):
  db = DBSCAN(eps=0.3, min_samples=100)
  db.fit(data)
  labels = db.labels_
  results = pd.DataFrame(data=labels, columns=['cluster'], index=data.index)
  return results


def kmeans(data):
  ''' all_announcement_posts should be a dataframe with rows as users and columns as coin
  symbols, values will be number of user posts in each symbol thread.
  '''
  km = KMeans(n_clusters=args.num_clusters, n_init=75)
  km.fit(data)
  cluster_distances = km.transform(data)
  labels = km.labels_
  results = pd.DataFrame(data=labels, columns=['cluster'], index=data.index)
  distances = pd.DataFrame(data=cluster_distances,
                           columns=['distance_{}'.format(i) for i in range(args.num_clusters)],
                           index=data.index)
  results = pd.concat([results, distances], axis=1, join='inner')
  print 'k-means inertia {}'.format(km.inertia_)
  
  #silhouette_plot(data, labels)
  return results


def silhouette_plot(data, cluster_labels):
  silhouette_avg = silhouette_score(data, cluster_labels)
  # Compute the silhouette scores for each sample
  sample_silhouette_values = silhouette_samples(data, cluster_labels)

  fig = plt.figure(figsize=(8,8))
  y_lower = 10

  # Number of clusters in labels, ignoring noise if present.
  num_clusters = len(set(cluster_labels)) - (1 if -1 in cluster_labels else 0)
  for i in range(num_clusters):
    # Aggregate the silhouette scores for samples belonging to cluster i, and sort them
    ith_cluster_silhouette_values = sample_silhouette_values[cluster_labels == i]
    ith_cluster_silhouette_values.sort()
    size_cluster_i = ith_cluster_silhouette_values.shape[0]
    y_upper = y_lower + size_cluster_i
    color = cm.spectral(float(i) / num_clusters)
    plt.fill_betweenx(np.arange(y_lower, y_upper),
                      0, ith_cluster_silhouette_values,
                      facecolor=color, edgecolor=color, alpha=0.7)

    # Label the silhouette plots with their cluster numbers at the middle
    plt.text(-0.05, y_lower + 0.5 * size_cluster_i, str(i))
    # Compute the new y_lower for next plot
    y_lower = y_upper + 10  # 10 for the 0 samples

  plt.xlabel("The silhouette coefficient values")
  plt.ylabel("Cluster label")

  # The vertical line for average silhouette score of all the values
  plt.axvline(x=silhouette_avg, color="red", linestyle="--")
  plt.yticks([])  # Clear the yaxis labels / ticks
  plt.xticks([-0.1, 0, 0.1, 0.2, 0.4, 0.8])
  plt.show()


def generate_feature_matrix(announcement_thread_ids_by_symbol, coin_data,
                            posts):
  # get number of posts made by each user in each coin thread, before end date and between
  # start and end dates (lim version)
  symbols_by_thread_id = {thread_id : symbol
                          for symbol, thread_ids in announcement_thread_ids_by_symbol.items()
                          for thread_id in thread_ids}
  all_announcement_thread_ids = [id for ids in announcement_thread_ids_by_symbol.values() for id in ids]
  all_announcement_posts = posts.loc[posts['thread_id'].isin(all_announcement_thread_ids)]
  all_announcement_posts['symbol'] = all_announcement_posts['thread_id'].map(symbols_by_thread_id)

  # add columns for posts end-date and start-dates that might be different for each coin
  # and then select posts within the date range
  if args.end_date:
    all_announcement_posts['end_date'] = pd.to_datetime(args.end_date)
    all_announcement_posts['start_date'] = (all_announcement_posts['end_date'] -
                                            pd.to_timedelta(args.period_length, unit='d'))
  else:
    # earliest trade date as end date
    earliest_trade_dates = coin_data['earliest_trade_date'].to_dict()
    all_announcement_posts['end_date'] = all_announcement_posts['symbol'].map(earliest_trade_dates)
    all_announcement_posts['end_date'] = pd.to_datetime(all_announcement_posts['end_date'])
    all_announcement_posts['end_date'] = (all_announcement_posts['end_date'] +
                                          pd.to_timedelta(args.earliest_trade_date_shift, unit='d'))
    all_announcement_posts['start_date'] = (all_announcement_posts['end_date'] -
                                            pd.to_timedelta(args.period_length, unit='d'))
  
  # filter posts to be within the respective date range of each coin
  all_announcement_posts = all_announcement_posts.loc[all_announcement_posts['date'] <=
                                                      all_announcement_posts['end_date']]
  all_announcement_posts = all_announcement_posts.groupby(['user', 'symbol']).size()
  all_announcement_posts = all_announcement_posts[
      all_announcement_posts >= args.num_posts_threshold]

  all_announcement_posts = all_announcement_posts.unstack('symbol', fill_value=0)
  return all_announcement_posts


def get_thread_data():
  # read coins data and index by coin symbol
  coin_data = pd.read_csv(args.thread_input, header = 0, index_col = 0,
                                    parse_dates= ['earliest_trade_date', 'user1_date',
                                                  'user2_date', 'user3_date', 'user4_date'])

  user_dates = ['user{}_date'.format(i) for i in range(1,5)]
  coin_data['earliest_mention_date'] = coin_data[user_dates].min(axis=1)
  threads_start_date = pd.to_datetime(args.end_date) - pd.to_timedelta(args.period_length, unit="D")
  coin_data['days_on_forum'] = (threads_start_date - coin_data['earliest_mention_date']).dt.days

  return coin_data


def get_announcement_thread_ids(posts, coin_data):
  # mapping from coin symbol to announcement thread ids
  announcement_thread_ids_by_symbol = defaultdict(list)
  url_columns = [col for col in coin_data.columns if 'url' in col]
  for coin_symbol, row in coin_data.iterrows():
    for announcement_url in row[url_columns]:
      if not announcement_url:
        continue
      url_posts = posts.loc[posts['url'] == announcement_url, ['url', 'thread_id']]
      if url_posts.empty:
        print 'Coin {} announcement url {} not in the forums.'.format(coin_symbol, announcement_url)
        continue

      thread_ids = url_posts['thread_id'].unique()
      assert len(thread_ids) == 1, 'URL {} has multiple thread ids {}'.format(announcement_url, thread_ids)
      announcement_thread_ids_by_symbol[coin_symbol].append(thread_ids[0])

  return announcement_thread_ids_by_symbol


def reduce_dimensions(data, num_comp):
  #dim_reducer = PCA(n_components=0.3, svd_solver='full')
  dim_reducer = PCA(n_components=num_comp)
  #dim_reducer = TruncatedSVD(n_components = num_comp)
  reduced_data = dim_reducer.fit_transform(data)
  print 'Dimensionality reduction explains {} of variance'.format(sum(dim_reducer.explained_variance_ratio_))
  reduced_data = pd.DataFrame(reduced_data,
                              columns=['DIM%i'%i for i in range(dim_reducer.components_.shape[0])],
                              index=data.index)
  return reduced_data


def normalize_data(data): 
  norm_data = normalize(data)
  norm_data = pd.DataFrame(norm_data, columns = data.columns.values, index=data.index)
  return norm_data


def scale_data(all_announcement_posts): 
  scaler = StandardScaler(copy=False, with_mean=True, with_std=True)
  data = scaler.fit_transform(all_announcement_posts)
  data = pd.DataFrame(data, columns = all_announcement_posts.columns.values,
                      index=all_announcement_posts.index)
  return data


def main():
  price_stats = pd.read_csv(args.price_input, index_col='symbol')
  coin_data = get_thread_data()
  coin_data = pd.concat([price_stats, coin_data], join='inner', axis=1)
  all_announcement_posts = pd.read_csv('./data/clustering/data_features.csv', index_col='user')
  
  print 'Shape of raw data {}'.format(all_announcement_posts.shape)
  
  # Limit the coins and their announcement thread ids only to the coins that are provided
  # in the valid_coin_symbols_file. This will only compute metrics for such coins and
  # those metrics that rely on their connection to other announcement threads will change
  # too, as their connection only with this limited announcement threads will be measured
  if args.valid_coin_symbols:
    with open(args.valid_coin_symbols) as f:
      valid_coins = f.read().splitlines()
    valid_coin_symbols = list(set(valid_coins))
    all_announcement_posts = all_announcement_posts[valid_coin_symbols]
  else:
    coin_columns = all_announcement_posts.columns.values
    coin_data = coin_data[coin_data['normalized_price_mean_std_fixed_50days'].notnull()]
    coin_data = coin_data[coin_data['volume_mean_mean_fixed_200days'].notnull()]
    coin_data = coin_data[(coin_data['volume_mean_mean_fixed_200days'] /
                               coin_data['price_mean_mean_fixed_200days']) >= 500]
    coin_data = coin_data[coin_data['volume_mean_25_fixed_200days'].notnull()]
    coin_data = coin_data[coin_data['volume_mean_25_fixed_200days'] >= 50]
    coin_data = coin_data[coin_data['volume_mean_mean_fixed_200days'] >= 50]
    coin_data = coin_data[coin_data['user_num_posts'] >= 50]
    coin_data = coin_data[coin_data['days_on_forum'] >=  -75]
    valid_coin_symbols = np.intersect1d(coin_data.index.values, coin_columns)
    all_announcement_posts = all_announcement_posts[valid_coin_symbols]
    # coins with min number of posts made by all users
    all_announcement_posts = all_announcement_posts[
        all_announcement_posts.columns[all_announcement_posts.sum(axis=0) >= 10]]
  
  # users with min required number of posts
  all_announcement_posts = all_announcement_posts.loc[all_announcement_posts.sum(axis=1) >= 10]
  print 'Shape of filtered data {}'.format(all_announcement_posts.shape)

  data = all_announcement_posts
  data = np.log1p(data)

  scaled_data = data
  scaled_data = scale_data(data)
  
  reduced_data = scaled_data
  reduced_data = reduce_dimensions(scaled_data, 10)
  reduced_data = normalize_data(reduced_data)
  print 'Shape of reduced data {}'.format(reduced_data.shape)
  
  results = kmeans(reduced_data)
  #results = dbscan(reduced_data)
  #results = hierarchical(reduced_data)
 
  # average silhouette score gives a perspective into the density and separation of the
  # formed clusters
  silhouette_avg = silhouette_score(reduced_data, results['cluster'])
  print("For {} clusters, average silhouette_score is {}".format(args.num_clusters, silhouette_avg))
  silhouette_plot(reduced_data, results['cluster'])

  
  volatility = price_stats["price_mean_volatility_fixed_200days"]
  volatility = volatility.rename('volatility')
  volatility = volatility.dropna()
  total_num_posts = all_announcement_posts.sum().rename('total_num_posts')

  print 'Cluster sizes:'
  print results.groupby('cluster').size()
  print '****************************'
 
  clrs = results['cluster'].unique()
  clustering_data = volatility.copy() 
  clrs = results['cluster'].unique()
  for i in clrs:
    print 'Cluster {}:'.format(i)
    cluster = all_announcement_posts.loc[results['cluster'] == i]
    cluster = cluster.sum().rename('num_posts')
    cluster = pd.concat([cluster, volatility, total_num_posts], axis=1, join='inner')
    cluster['fraction_posts'] = cluster['num_posts'] / cluster['total_num_posts']
    #print cluster.sort_values(by='fraction_posts', ascending=False).head(10)
    #print cluster.sort_values(by='num_posts', ascending=False).head(10)
    print 'Total number of posts {}'.format(cluster['num_posts'].sum())
    cluster = cluster.dropna(axis=0, how='any')
    print 'Averge volatility of cluster weighted by num posts {}'.format(np.average(
      cluster['volatility'], weights=cluster['num_posts']))
    print 'Averge volatility of cluster weighted by average posts {}'.format(np.average(
      cluster['volatility'], weights=cluster['fraction_posts']))
    print '********************************************'
    fraction_data = cluster['fraction_posts'].rename('fraction_posts_cl{}'.format(i))
    clustering_data = pd.concat([clustering_data, fraction_data], axis=1, join='inner')
 
  print 'All coins sorted by volatility and fraction in each cluster'
  clustering_data = clustering_data.sort_values(by='volatility', ascending=True)
  tmp = clustering_data.iloc[:15, clustering_data.columns != 'volatility']
  tmp = tmp.reindex_axis(tmp.sum().sort_values(ascending=False).index, axis=1)
  tmp = pd.concat([clustering_data['volatility'], tmp], join='inner', axis=1)
  print tmp.iloc[:,0:4].sort_values(by='volatility', ascending=True).head(20)
  tmp = clustering_data.iloc[-15:, clustering_data.columns != 'volatility']
  tmp = tmp.reindex_axis(tmp.sum().sort_values(ascending=False).index, axis=1)
  tmp = pd.concat([clustering_data['volatility'], tmp], join='inner', axis=1)
  print tmp.iloc[:, 0:4].sort_values(by='volatility', ascending=True).head(20)


if __name__ == '__main__':
  main()
