#!/usr/bin/python

import csv
import operator
import os
import json
import argparse
import sys
import numpy as np
import pandas as pd
from datetime import datetime

parser = argparse.ArgumentParser(
    description="This script computes the price volatility measures right after each "
    "coin hits the market and during a fixed period for all coins whose start date will "
    "be speicified in command line.")
parser.add_argument("prices_dir",
                    help="The directory that contains the price data for coins "
                    "from a previous crawl.")
parser.add_argument("markets_dir",
                    help="The directory that contains the list of markets for "
                    "each coin, one file per coin.")
parser.add_argument("coins_file",
                    help="The json file which contains list of coin names and their "
                    "sybmols. The coins we analyze price data are the ones in "
                    "this file.")
parser.add_argument("end_date",
                    help="The end date of the period during which we will compute "
                    "price and volume metircs such as volaitility or mean. This period "
                    "will be the same for all coins and the date should be specified in "
                    "YYYY-MM-DD format. The period will be 5,10,20,30 ... days, each "
                    "with start dates, but all ending in the same end_date.")
parser.add_argument("-s", dest="earliest_trade_date_shift", type=int, default=0,
                    help="The number of days to shift the earliest trade date to the "
                    "future. The first few days of trading might not have any volume, "
                    "be noisy or simply wrong. So we can shift the earliest trade date "
                    "a bit into the future, which effectively changes the period over "
                    "which means and volatilities are computed. This option only affects "
                    "means and volatilities, and not the bubble severity measures. "
                    "Default is zero which means no shift.")
args = parser.parse_args()

# The number of days over which to measure mean and volatilities
PERIODS=[5,8,10,15,20,30,50,100,200,300]
#METRICS = ['mean', 'open', 'close', 'low', 'high']
METRICS = ['mean']
  
def load_prices(filename):
  '''
  It loads the price data from the file for the coin with the given filename.  It then
  downsamples the prices so that only the last price available for each day is kept (so
  one record per day), and finally interpolates the data for missing days.

  Returns dataframe with date as index and price,volume,... as columns.
  '''
  prices = pd.read_json(filename, orient='records', typ='frame', date_unit='ms',
                        convert_dates = ['date'],
                        dtype={'btc': np.float64,
                               'date': np.datetime64,
                               'market_cap_usd': np.float64,
                               'usd': np.float64,
                               'volume_btc': np.float64,
                               'volume_orig': np.float64,
                               'volume_usd': np.float64})
  # set datetime as index
  prices = prices.set_index('date').sort_index()
  # downsample to periods of 1-day, and keep the last price/volume/marketcap available in
  # each day. Then for missing days, interpolate between the available days with max 10
  # days for interpolation
  aggs = {'open': 'first',
          'high': 'max',
          'low': 'min',
          'close': 'last',
          'mean': 'mean',
          'median': 'median'}
  # some old data don't have the market cap column
  if 'market_cap_usd' in prices.columns:
    prices = prices.resample('D').agg({'btc': aggs, 'market_cap_usd': aggs, 'usd': aggs,
                                       'volume_btc': aggs, 'volume_orig': aggs,
                                       'volume_usd': aggs})
  else:
    prices = prices.resample('D').agg({'btc': aggs, 'usd': aggs,
                                       'volume_btc': aggs, 'volume_orig': aggs,
                                       'volume_usd': aggs})
  prices = prices.interpolate(limit=10)
  prices = prices.sort_index()
  return prices

def weighted_avg_and_std(values, weights):
  """
  Return the weighted average and standard deviation.
  values, weights -- Numpy ndarrays with the same shape.
  """
  try:
    average = np.average(values, weights=weights)
    variance = np.average((values-average)**2, weights=weights)
    std = np.sqrt(variance)
  except ZeroDivisionError, UnboundLocalError:
    average = np.nan
    std = np.nan
  return (average, std)


def volatility(values):
  """
  Returns volatility as the standard deviation of logarithmic returns
  """
  # remove zeros
  vals = np.array(values)
  vals = 1.0 * vals[np.nonzero(vals)]
  returns = np.diff(vals, n=1) / vals[:-1]
  log_returns = np.diff(np.log(vals), n=1)
  try:
    volatility = np.std(returns)
  except ZeroDivisionError, UnboundLocalError:
    volatility = np.nan
  try:
    volatility_log = np.std(log_returns)
  except ZeroDivisionError, UnboundLocalError:
    volatility_log = np.nan
  return (volatility, volatility_log)


def compute_price_volume_stats(prices, pair, metric, coin):
  ''' This function computes the volume stats such as total volume overall or min/max
  price and fills in the relevent field in coin dict()

  coin: the dictionary to be updated with volume stats
  pair: determines which price/volume scale to analyze, either usd or btc.
  metric: can be either open/close/high/low/mean which determines the metric to choose
  for the given pair.
  '''
  max_price = prices[pair, metric].max()
  min_price = prices[pair, metric].min()
  active_days = len(prices.index)

  volume_key = 'volume_{}'.format(pair)
  # volume in destination currency, pair: either btc or usd
  total_volume = prices[volume_key, metric].sum()
  # volume in the currency itself
  total_volume_orig = prices['volume_orig', metric].sum()
  
  try:
    normalized_total_volume = total_volume/active_days
  except ZeroDivisionError, UnboundLocalError:
    normalized_total_volume = np.nan
  try:
    normalized_total_volume_orig = total_volume_orig/active_days
  except ZeroDivisionError, UnboundLocalError:
    normalized_total_volume_orig = np.nan

  coin['max_price_{}'.format(metric)] = max_price
  coin['min_price_{}'.format(metric)] = min_price
  coin['total_volume_{}'.format(metric)] = total_volume
  coin['normalized_total_volume_{}'.format(metric)] = normalized_total_volume
  coin['total_volume_orig_{}'.format(metric)] = total_volume_orig
  coin['normalized_total_volume_orig_{}'.format(metric)] = normalized_total_volume_orig

def compute_means_stds_volatilities(prices, pair, metric, coin, prefix):
  ''' This function computes volatilities over certain period after initial trade dates
  and fills in the relevent field in coin dict()

  pair: determines the unit of prices (either in btc or usd)
  metric: can be either open/close/high/low/mean which determines the metric to choose
  for the given pair.
  coin: the dictionary to be updated with mean and volatility of volume and price
  '''
  volume_key = 'volume_{}'.format(pair)
  for plength in PERIODS:
    for ptype in ["initial", "fixed"]:
      # get price and volume lists for the first/fixed *plength* days. 
      if ptype == "initial":
        # shifting the start by the value specified in earliest_trade_date shift
        start = prices.index.min() + pd.DateOffset(days=args.earliest_trade_date_shift)
        end = start + pd.DateOffset(days=plength)
      else:
        end = pd.to_datetime(args.end_date, infer_datetime_format=True)
        start = end - pd.DateOffset(days=plength)
        if not prices.index.contains(end) or not prices.index.contains(start):
          # prices does not contain either end or start date
          start = prices.index.min()
          end = start - pd.DateOffset(days=1)
     
      price_list = prices.loc[start:end][pair, metric]
      volume_list = prices.loc[start:end][volume_key, metric]
      
      (vw_price_mean, vw_price_std) = weighted_avg_and_std(price_list, volume_list)
      (price_volatility, price_volatility_log) = volatility(price_list)
      
      price_std = np.std(price_list) if len(price_list.index) else np.nan
      volume_weighted_price_std = vw_price_std
      volume_std = np.std(volume_list) if len(volume_list.index) else np.nan
      price_mean = np.mean(price_list) if len(price_list.index) else np.nan
      volume_weighted_price_mean = vw_price_mean
      volume_mean = np.mean(volume_list) if len(volume_list.index) else np.nan

      try:
        normalized_price_std = np.std(price_list)/np.mean(price_list)
      except ZeroDivisionError, UnboundLocalError:
        normalized_price_std = np.nan
      try:
        normalized_volume_weighted_price_std = vw_price_std / vw_price_mean
      except ZeroDivisionError, UnboundLocalError:
        normalized_volume_weighted_price_std = np.nan
      try:
        normalized_volume_std = np.std(volume_list)/np.mean(volume_list)
      except ZeroDivisionError, UnboundLocalError:
        normalized_volume_std = np.nan
      
      coin['{}price_{}_std_{}_{}days'
           .format(prefix, metric, ptype, plength)] = price_std
      coin['{}price_{}_volatility_{}_{}days'
           .format(prefix, metric, ptype, plength)] = price_volatility
      coin['{}price_{}_volatility_log_{}_{}days'
           .format(prefix, metric, ptype, plength)] = price_volatility_log
      coin['{}volume_weighted_price_{}_std_{}_{}days'
           .format(prefix, metric, ptype, plength)] = volume_weighted_price_std
      coin['{}volume_{}_std_{}_{}days'
           .format(prefix, metric, ptype, plength)] = volume_std
      coin['{}price_{}_mean_{}_{}days'
           .format(prefix, metric, ptype, plength)] = price_mean
      coin['{}volume_weighted_price_{}_mean_{}_{}days'
           .format(prefix, metric, ptype, plength)] = volume_weighted_price_mean
      coin['{}volume_{}_mean_{}_{}days'
           .format(prefix, metric, ptype, plength)] = volume_mean
      coin['{}normalized_price_{}_std_{}_{}days'
           .format(prefix, metric, ptype, plength)] = normalized_price_std
      coin['{}normalized_volume_weighted_price_{}_std_{}_{}days'
           .format(prefix, metric, ptype, plength)] = normalized_volume_weighted_price_std
      coin['{}normalized_volume_{}_std_{}_{}days'
           .format(prefix, metric, ptype, plength)] = normalized_volume_std
    
    
def compute_beta(prices, btc_prices, pair, metric, coin):
  '''
  This function computes the beta coefficient of the raw and logarithmic returns as
  compared with BTC prices as the benchmark.
  
  pair: determines the unit of prices (either in btc or usd)
  metric: can be either open/close/high/low/mean which determines the metric to choose
  for the given pair.
  coin: the dictionary to be updated with mean and volatility of volume and price
  '''
  for plength in PERIODS:
    for ptype in ["initial", "fixed"]:
      # get price and volume lists for the first/fixed *plength* days. 
      if ptype == "initial":
        # shifting the start by the value specified in earliest_trade_date shift
        start = prices.index.min() + pd.DateOffset(days=args.earliest_trade_date_shift)
        end = start + pd.DateOffset(days=plength)
      else:
        end = pd.to_datetime(args.end_date, infer_datetime_format=True)
        start = end - pd.DateOffset(days=plength)
        if not prices.index.contains(end) or not prices.index.contains(start):
          # prices does not contain either end or start date
          start = prices.index.min()
          end = start - pd.DateOffset(days=1)
      
      price_list = 1.0 * prices.loc[start:end][pair, metric]
      btc_price_list = 1.0 * btc_prices.loc[start:end][pair, metric]
      
      returns = np.diff(price_list, n=1) / price_list[:-1]
      btc_returns = np.diff(btc_price_list, n=1) / btc_price_list[:-1]
      log_returns = np.diff(np.log(price_list), n=1)
      btc_log_returns = np.diff(np.log(btc_price_list), n=1)
    
      # the except should catch division by zero and cases in which time duration of
      # price_list and btc_price_list are different
      try:
        beta_returns = np.cov(returns, btc_returns)[0][1] / np.var(btc_returns)
      except: 
        beta_returns = np.nan
      
      try:
        beta_log_returns = np.cov(log_returns, btc_log_returns)[0][1] / np.var(btc_log_returns)
      except: 
        beta_log_returns = np.nan
      
      coin['beta_{}_returns_{}_{}days'.format(metric, ptype, plength)] = beta_returns
      coin['beta_{}_log_returns_{}_{}days'.format(metric, ptype, plength)] = beta_log_returns


def compute_percentiles(prices, pair, metric, coin):
  ''' This function computes min/max/25/75 percentiles over certain period after initial
  trade dates and fills in the relevent field in coin dict()

  coin: the dictionary to be updated with percentiles of volume and price
  '''

  volume_key = 'volume_{}'.format(pair)
  for plength in PERIODS:
    for ptype in ["initial", "fixed"]:
      # get price and volume lists for the first/fixed *plength* days. 
      if ptype == "initial":
        # shifting the start by the value specified in earliest_trade_date shift
        start = prices.index.min() + pd.DateOffset(days=args.earliest_trade_date_shift)
        end = start + pd.DateOffset(days=plength)
      else:
        end = pd.to_datetime(args.end_date, infer_datetime_format=True)
        start = end - pd.DateOffset(days=plength)
        if not prices.index.contains(end) or not prices.index.contains(start):
          # prices does not contain either end or start date
          start = prices.index.min()
          end = start - pd.DateOffset(days=1)
      
      price_list = 1.0 * prices.loc[start:end][pair, metric]
      volume_list = 1.0 * prices.loc[start:end][volume_key, metric]

      price_min = np.amin(price_list) if len(price_list.index) else np.nan
      price_max = np.amax(price_list) if len(price_list.index) else np.nan
      price_median = np.median(price_list) if len(price_list.index) else np.nan
      price_25 = np.percentile(price_list, 25) if len(price_list.index) else np.nan
      price_75 = np.percentile(price_list, 75) if len(price_list.index) else np.nan

      volume_min = np.amin(volume_list) if len(volume_list.index) else np.nan
      volume_max = np.amax(volume_list) if len(volume_list.index) else np.nan
      volume_median = np.median(volume_list) if len(volume_list.index) else np.nan
      volume_25 = np.percentile(volume_list, 25) if len(volume_list.index) else np.nan
      volume_75 = np.percentile(volume_list, 75) if len(volume_list.index) else np.nan
      
      coin['price_{}_min_{}_{}days'.format(metric, ptype, plength)] = price_min
      coin['price_{}_max_{}_{}days'.format(metric, ptype, plength)] = price_max
      coin['price_{}_median_{}_{}days'.format(metric, ptype, plength)] = price_median
      coin['price_{}_25_{}_{}days'.format(metric, ptype, plength)] = price_25
      coin['price_{}_75_{}_{}days'.format(metric, ptype, plength)] = price_75

      coin['volume_{}_min_{}_{}days'.format(metric, ptype, plength)] = volume_min
      coin['volume_{}_max_{}_{}days'.format(metric, ptype, plength)] = volume_max
      coin['volume_{}_median_{}_{}days'.format(metric, ptype, plength)] = volume_median
      coin['volume_{}_25_{}_{}days'.format(metric, ptype, plength)] = volume_25
      coin['volume_{}_75_{}_{}days'.format(metric, ptype, plength)] = volume_75
      

def analyze_prices(prices, markets, pair, btc_prices):
  '''
  prices: the list containing price and volume. Its a list of dicts for each day, the keys
  in the dicts are usd, btc, date, volume_btc, volume_usd and volume_orig.
  
  pair: determines which origin currency to use for measuring price, usd or btc?
  '''
  coin = {}
  if len(prices.index) > 0:
    first_trade_date = prices.index.min()
    overlapping_btc_prices = btc_prices[first_trade_date:]
   
    for metric in METRICS:
      compute_price_volume_stats(prices, pair, metric, coin)
      # get coin price volatility and btc volatility in the same period as baseline
      compute_means_stds_volatilities(prices, pair, metric, coin, '')
      compute_means_stds_volatilities(overlapping_btc_prices, pair, metric, coin, 'btc_')
      compute_beta(prices, overlapping_btc_prices, pair, metric, coin)
      compute_percentiles(prices, pair, metric, coin)

    coin['first_trade'] = first_trade_date
    coin['market_num'] = len(markets)
    if 'BTC-E' in markets:
      coin['BTC-E'] = True
    else:
      coin['BTC-E'] = False
    if 'Kraken' in markets:
      coin['Kraken'] = True
    else:
      coin['Kraken'] = False
    if 'Poloniex' in markets:
      coin['Poloniex'] = True
    else:
      coin['Poloniex'] = False
    if 'Cryptsy' in markets:
      coin['Cryptsy'] = True
    else:
      coin['Cryptsy'] = False
    if 'BTC38' in markets:
      coin['BTC38'] = True
    else:
      coin['BTC38'] = False
    if 'BTER' in markets:
      coin['BTER'] = True
    else:
      coin['BTER'] = False
    if 'Bittrex' in markets:
      coin['Bittrex'] = True
    else:
      coin['Bittrex'] = False

  return coin


def main():
  if not os.path.isdir(args.prices_dir):
    sys.exit('Could not find prices directory %s.' % args.prices_dir)

  if not os.path.isdir(args.markets_dir):
    sys.exit('Could not find markets directory %s.' % args.markets_dir)
  
  fieldnames = ['symbol',
                'slug',
                'coin_name',
                'market_num',
                'BTC-E',
                'Kraken',
                'Poloniex',
                'Cryptsy',
                'BTC38',
                'BTER',
                'Bittrex',
                'first_trade']
  for plength in PERIODS:
    for metric in METRICS:
      fieldnames.extend(['max_price_{}'.format(metric),
                         'min_price_{}'.format(metric),
                         'total_volume_{}'.format(metric),
                         'normalized_total_volume_{}'.format(metric),
                         'total_volume_orig_{}'.format(metric),
                         'normalized_total_volume_orig_{}'.format(metric)])
    
      # the type of period: initial (right after ICO) or during the fixed time for all coins
      for ptype in ['initial', 'fixed']:
        fieldnames.extend(['price_{}_min_{}_{}days'.format(metric, ptype, plength),
                           'price_{}_max_{}_{}days'.format(metric, ptype, plength),
                           'price_{}_median_{}_{}days'.format(metric, ptype, plength),
                           'price_{}_25_{}_{}days'.format(metric, ptype, plength),
                           'price_{}_75_{}_{}days'.format(metric, ptype, plength)])
        fieldnames.extend(['volume_{}_min_{}_{}days'.format(metric, ptype, plength),
                           'volume_{}_max_{}_{}days'.format(metric, ptype, plength),
                           'volume_{}_median_{}_{}days'.format(metric, ptype, plength),
                           'volume_{}_25_{}_{}days'.format(metric, ptype, plength),
                           'volume_{}_75_{}_{}days'.format(metric, ptype, plength)])
        fieldnames.extend(['beta_{}_returns_{}_{}days'.format(metric, ptype, plength),
                           'beta_{}_log_returns_{}_{}days'.format(metric, ptype, plength)])
        for prefix in ['', 'btc_']:
          fieldnames.extend(['{}price_{}_std_{}_{}days'.format(prefix, metric, ptype, plength),
                             '{}price_{}_volatility_{}_{}days'.format(prefix, metric, ptype, plength),
                             '{}price_{}_volatility_log_{}_{}days'.format(prefix, metric, ptype, plength),
                             '{}volume_weighted_price_{}_std_{}_{}days'.format(prefix, metric, ptype, plength),
                             '{}volume_{}_std_{}_{}days'.format(prefix, metric, ptype, plength)])
          fieldnames.extend(['{}price_{}_mean_{}_{}days'.format(prefix, metric, ptype, plength),
                             '{}volume_weighted_price_{}_mean_{}_{}days'.format(prefix, metric, ptype, plength),
                             '{}volume_{}_mean_{}_{}days'.format(prefix, metric, ptype, plength)])
          fieldnames.extend(['{}normalized_price_{}_std_{}_{}days'.format(prefix, metric, ptype, plength),
                             '{}normalized_volume_weighted_price_{}_std_{}_{}days'.format(prefix, metric, ptype, plength),
                             '{}normalized_volume_{}_std_{}_{}days'.format(prefix, metric, ptype, plength)])


  # read coins from coins_file, these will be the coins we will analyze
  coins = dict()
  with open(args.coins_file, 'r') as f:
    coins = json.load(f)

  # a list of dictionaries. One dictionary per coin, with fields corresponding to those
  # above in fieldnames array
  btc_analysis = []
  usd_analysis = []

  # first get btc prices as baseline to all coins
  filename = os.path.join(args.prices_dir, 'BTC')
  btc_prices = load_prices(filename)

  for coin in coins:
    filename = os.path.join(args.prices_dir, coin)
    if not os.path.isfile(filename):
      continue

    print 'Analyzing coin {}'.format(coin)
    prices = load_prices(filename)

    market_file = os.path.join(args.markets_dir, coin)
    with open(market_file, 'r') as f:
      markets = json.load(f)

    for pair in ['usd','btc']:
      analysis = analyze_prices(prices, markets, pair, btc_prices)
      result = analysis.copy()
      result.update({'symbol': coin})
      result.update(coins[coin])
      if pair == 'btc':
        btc_analysis.append(result)
      if pair == 'usd':
        usd_analysis.append(result)

  usd_filename = 'price_volume_usd_enddate{}_s{}.csv'.format(args.end_date,
                                                             args.earliest_trade_date_shift)
  with open(usd_filename, 'wb') as csvfile:
    writer = csv.DictWriter(csvfile, fieldnames)
    writer.writeheader()
    for coin in usd_analysis:
      writer.writerow(coin)
  
  btc_filename = 'price_volume_btc_enddate{}_s{}.csv'.format(args.end_date,
                                                             args.earliest_trade_date_shift)
  with open(btc_filename, 'wb') as csvfile:
    writer = csv.DictWriter(csvfile, fieldnames)
    writer.writeheader()
    for coin in btc_analysis:
      writer.writerow(coin)

if __name__ == '__main__':
  main()
