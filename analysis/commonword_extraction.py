import argparse
from datetime import datetime, timedelta
import os

from nltk.tokenize import word_tokenize, RegexpTokenizer
from nltk.corpus import stopwords

import pandas as pd

"""
>> Could you write a python script that outputs the count of top used words
within each announcement thread? we should be able to say give me the top used
words in the announcement thread for the x number of days before an end period
y date. x and y should be command line flags, so we can change it.
"""


def run(days,
        enddate,
        inputfile,
        introducerfile,
        outputdir,
        minposts=5,
        stopword_filepath=None,
        multifile_output=False,
        topk=None):

    customized_stopwords = set({})
    if stopword_filepath is not None:
        with open(stopword_filepath, "r") as fin:
            for line in fin:
                word = line.rstrip()
                customized_stopwords.add(word)

    dirname = "E-{}_D-{}_M-{}".format(enddate, days, minposts)
    dirpath = os.path.join(outputdir, dirname)
    if not os.path.exists(dirpath):
        print("Create {}".format(dirpath))
        os.makedirs(dirpath)

    stopwords_set = set(stopwords.words('english')) | customized_stopwords
    tokenizer = RegexpTokenizer(r'\w+')

    df = pd.read_csv(inputfile)

    df.loc[:, "dt"] = pd.to_datetime(df["date"])
    df.loc[:, "thread_id"] = df["forum_page"].astype('str') + "-" + df[
        "subject_id"].astype('str')
    yyyy, mm, dd = enddate.split('-')
    end_dt = datetime(int(yyyy), int(mm), int(dd))
    start_dt = end_dt - timedelta(days=days)
    filtered_df = df[(df["dt"] <= end_dt) & (df["dt"] > start_dt)]
    filtered_df.loc[:, "topicid"] = filtered_df["url"].apply(
        lambda x: x.split("=")[1].split(".")[0])

    if not multifile_output:
        # single file
        filepath = os.path.join(outputdir, "{}_all.csv".format(dirname))
        fout = open(filepath, "w")

    idf = pd.read_csv(introducerfile)
    for index, row in idf.iterrows():
        symbol = row["symbol"]
        urls = row[["user1_url", "user2_url"]].dropna().tolist()
        topicid_list = list(map(lambda x: x.split("=")[1].split(".")[0], urls))
        topic_df_list = []
        for topicid in topicid_list:
            topic_df = filtered_df[filtered_df["topicid"] == topicid]
            topic_df_list.append(topic_df)
        concat_topic_df = pd.concat(topic_df_list, axis=0)
        print(type(concat_topic_df))
        print(concat_topic_df.columns)
        if len(concat_topic_df) < minposts:
            continue
        texts = " ".join(concat_topic_df["content"].fillna("").apply(
            lambda x: x.decode("utf-8")).tolist())
        # tokens = word_tokenize(texts)
        tokens = tokenizer.tokenize(texts)
        words = list(filter(lambda x: x not in stopwords_set,
                            map(lambda x: x.lower(), tokens)))
        count_s = pd.Series(words).value_counts()

        if topk is not None:
            count_s = count_s.head(topk)

        if multifile_output:
            filepath = os.path.join(dirpath, "{}.csv".format(symbol))
            count_s.to_csv(filepath, encoding="utf-8")
        else:
            fout.write("{},".format(symbol))
            pairs = zip(count_s.index.tolist(), count_s.tolist())
            pairs_str = "{}".format(pairs).replace(
                "[", "").replace("]", "").replace(" ", "").replace("(u'", "('")
            fout.write(pairs_str)
            fout.write("\n")

    if not multifile_output:
        fout.close()

    """
    for threadid, group_df in filtered_df.groupby("thread_id"):
        if len(group_df) < minposts:
            continue
        texts = " ".join(group_df["content"].fillna("").apply(
            lambda x: x.decode("utf-8")).tolist())
        #tokens = word_tokenize(texts)
        tokens = tokenizer.tokenize(texts)
        words = list(filter(lambda x: x not in stopwords_set,
                            map(lambda x: x.lower(), tokens)))
        count_s = pd.Series(words).value_counts()
        filepath = os.path.join(dirpath, "{}.csv".format(threadid))
        count_s.to_csv(filepath, encoding="utf-8")
    """

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='description')
    parser.add_argument('-E', '--enddate',
                        type=str,
                        #required=True,
                        default="2016-09-30",
                        help='End period yyyy-mm-dd')
    parser.add_argument('-D', '--days',
                        type=int,
                        #required=True,
                        default=30,
                        help='Number of days')
    parser.add_argument('-I', '--inputfile',
                        type=str,
                        #required=True,
                        default="/Users/suhara/Dropbox/Bitcoin (2)/bitcointalk_sep2016/announcements-data.csv",
                        help='Input filepath')
    parser.add_argument('-i', '--introducerfile',
                        type=str,
                        #required=True,
                        default="/Users/suhara/research/Bitcoin/active-users-introducers/introducers/dec_2016/all_introducers_concat.csv",
                        help='Input filepath')
    parser.add_argument('-O', '--outputdir',
                        type=str,
                        required=True,
                        help='Output dirpath')
    parser.add_argument('-M', '--minposts',
                        type=int,
                        default=5,
                        help='Minimum post in a thread.')
    parser.add_argument('-S', '--stopword_filepath',
                        type=str,
                        default=None,
                        help='Stop word filepath (one word per line)')
    parser.add_argument('-m', '--multifile_output',
                        action='store_true',
                        default=False,
                        help='multiple file output (default: single)')
    parser.add_argument('-k', '--topk',
                        type=int,
                        default=None,
                        help="Top-k value for common word extraction.")


    args = parser.parse_args()
    days = args.days
    enddate = args.enddate
    inputfile = args.inputfile
    introducerfile = args.introducerfile
    outputdir = args.outputdir
    minposts = args.minposts
    stopword_filepath = args.stopword_filepath
    multifile_output = args.multifile_output
    topk = args.topk

    run(days,
        enddate,
        inputfile,
        introducerfile,
        outputdir,
        minposts=minposts,
        stopword_filepath=stopword_filepath,
        multifile_output=multifile_output,
        topk=topk)
