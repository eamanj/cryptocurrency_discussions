#!/usr/bin/python

import argparse
import os
import sys
import pickle

parser = argparse.ArgumentParser(
    description="reads list of users per each coin and computes the number of all users "
                "(union")
parser.add_argument("users_input_file",
                    help="input file containing users per coin")
parser.add_argument("-v", dest="valid_coin_symbols",
                    help="If provided, it has to be a file with the symbol of valid "
                    "coins, one per line.")
args = parser.parse_args()

def main():
  with open(args.users_input_file, "rb") as f:
    users_per_symbol = pickle.load(f)
  
  valid_coin_symbols = users_per_symbol.keys()
  if args.valid_coin_symbols:
    with open(args.valid_coin_symbols) as f:
      valid_coins = f.read().splitlines()
    valid_coin_symbols = list(set(valid_coins))
  users_per_symbol = {symbol: users_per_symbol[symbol] for symbol in valid_coin_symbols}

  all_users = set().union(*(users_per_symbol.values()))
  print 'There are {} users in {} valid coins.'.format(len(all_users), len(valid_coin_symbols))



if __name__ == '__main__':
  main()
