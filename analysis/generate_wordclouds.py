import argparse
from datetime import datetime, timedelta
import os
import re
import sys

from nltk.tokenize import word_tokenize, RegexpTokenizer
from nltk.corpus import stopwords, words, brown
import numpy as np
import pandas as pd
import six
from textstat.textstat import textstat
from wordcloud import WordCloud
from matplotlib import pyplot as plt

nn_words = {word for word, pos in brown.tagged_words() if pos.startswith('NN')}

class PMI:
    def __init__(self,
                 stopwords=None,
                 smoothing=1e-4,
                 min_tf=5):
        self.stopwords = stopwords
        self.smoothing = smoothing
        self.min_tf = min_tf

    def calculate_pmi_texts(self, texts):
        """
        texts = [["a", "b", "c"], ["b", "c"], ["a", "c"]]
        """

        word_count_df = pd.concat(
            list(map(lambda x: pd.Series(x).value_counts(),
                     texts)),
            axis=1).fillna(0.0).astype('float')

        if self.stopwords is not None:
            vocabs = list(
                filter(lambda x: x not in self.stopwords,
                       word_count_df.index.tolist()))
            word_count_df = word_count_df.loc[vocabs]

        return self.calculate_pmi(word_count_df)

    def calculate_pmi(self, word_count_df):

        if self.min_tf is not None:
            word_count_df = word_count_df[
                word_count_df.sum(axis=1) > self.min_tf]

        word_count_df += self.smoothing
        total_count = word_count_df.sum().sum()
        doc_freq_s = word_count_df.sum(axis=0)
        word_freq_s = word_count_df.sum(axis=1)

        prob_df = word_count_df.div(doc_freq_s,
                                    axis=1).div(word_freq_s,
                                                axis=0)
        prob_df *= total_count
        pmi_df = np.log2(prob_df)

        return pmi_df


def load_volatilityfile(filepath,
                        days=100):
    df = pd.read_csv(filepath)
    coin_df = df[["name",
                  "symbol",
                  "log_price_mean_volatility_fixed_{}days".format(days),
                  "price_mean_volatility_fixed_{}days".format(days)]]
    coin_df.columns = ["name", "symbol", "log_volatility", "volatility"]
    coin_df.loc[:, "highvol_label_log"] = 0
    coin_df.loc[coin_df["log_volatility"] > coin_df["log_volatility"].median(), "highvol_label_log"] = 1

    coin_df.loc[:, "highvol_label"] = 0
    coin_df.loc[coin_df["volatility"] > coin_df["volatility"].median(), "highvol_label"] = 1

    coin_df.loc[:, "highvol_label_025q"] = np.nan
    coin_df.loc[coin_df["volatility"] >= coin_df["volatility"].quantile(0.75), "highvol_label_025q"] = 1
    coin_df.loc[coin_df["volatility"] <= coin_df["volatility"].quantile(0.25), "highvol_label_025q"] = 0

    return coin_df


def load_texts(days,
               enddate,
               inputfile,
               introducerfile,
               minposts=5,
               stopword_filepath=None,
               normalize=True,
               filter_rule=None):

    customized_stopwords = set({})
    if stopword_filepath is not None:
        with open(stopword_filepath, "r") as fin:
            for line in fin:
                word = line.rstrip()
                customized_stopwords.add(word)

    stopwords_set = set(stopwords.words('english')) | customized_stopwords
    tokenizer = RegexpTokenizer(r'\w+')

    df = pd.read_csv(inputfile)

    df.loc[:, "date_time"] = df["date"] + " " + df["time"]
    df.loc[:, "date_time_dt"] = df["date_time"].apply(
        lambda x: datetime.strptime(x, "%Y-%m-%d %H:%M:%S"))
    # Sorted by post-time (to make it easy to pick up the announcer)
    df = df.sort_values("date_time_dt")

    df.loc[:, "dt"] = pd.to_datetime(df["date"])
    df.loc[:, "thread_id"] = df["forum_page"].astype('str') + "-" + df[
        "subject_id"].astype('str')
    yyyy, mm, dd = enddate.split('-')
    end_dt = datetime(int(yyyy), int(mm), int(dd))
    start_dt = end_dt - timedelta(days=days)
    filtered_df = df[(df["dt"] <= end_dt) & (df["dt"] > start_dt)]
    filtered_df.loc[:, "topicid"] = filtered_df["url"].apply(
        lambda x: x.split("=")[1].split(".")[0])

    symbol_count_dict = {}
    symbol_fre_score_dict = {}
    symbol_fkg_score_dict = {}
    idf = pd.read_csv(introducerfile)
    for index, row in idf.iterrows():
        symbol = row["symbol"]
        urls = row[["user1_url", "user2_url"]].dropna().tolist()
        topicid_list = list(map(lambda x: x.split("=")[1].split(".")[0], urls))
        topic_df_list = []
        for topicid in topicid_list:
            # Filter out only the introducer's post
            topic_df = filtered_df[filtered_df["topicid"] == topicid]
            # Rules
            if filter_rule is None:
                topic_df_list.append(topic_df)
            elif filter_rule == "announcement":
                # Only announcement post
                topic_df_list.append(topic_df.head(1))
            elif filter_rule == "announcer":
                try:
                    announcer_uname = topic_df.head(1)["user"].values[0]
                    topic_df_list.append(
                        topic_df[topic_df["user"] == announcer_uname])
                except:
                    topic_df_list.append(topic_df.head(1))
            elif filter_rule == "not_announcement":
                topic_df_list.append(topic_df.tail(-1))
            elif filter_rule == "not_announcer":
                try:
                    announcer_uname = topic_df.head(1)["user"].values[0]
                    topic_df_list.append(
                        topic_df[topic_df["user"] != announcer_uname])
                except:
                    topic_df_list.append(topic_df.head(1))
            else:
                print("Invalid filter_rule: {}".format(filter_rule))
                sys.exit(1)

        concat_topic_df = pd.concat(topic_df_list, axis=0)
        print(type(concat_topic_df))
        print(concat_topic_df.columns)
        if filter_rule is None and len(concat_topic_df) < minposts:
            continue
        texts = " ".join(concat_topic_df["content"].fillna("").apply(
            #lambda x: x.decode("utf-8")).tolist())
            lambda x: six.u(x)).tolist())

        if len(texts) > 10:
            fre_score = textstat.flesch_reading_ease(texts)
            fkg_score = textstat.flesch_kincaid_grade(texts)
        else:
            fre_score = np.nan
            fkg_score = np.nan

        # tokens = word_tokenize(texts)
        tokens = tokenizer.tokenize(texts)
        words = list(filter(lambda x: x not in stopwords_set,
                            map(lambda x: x.lower(), tokens)))
        count_s = pd.Series(words).value_counts()
        if normalize:
            count_s = (count_s / count_s.sum()) * 1000
        symbol_count_dict[symbol] = count_s
        symbol_fre_score_dict[symbol] = fre_score
        symbol_fkg_score_dict[symbol] = fkg_score

    return symbol_count_dict, symbol_fre_score_dict, symbol_fkg_score_dict


if __name__ == "__main__":

    parser = argparse.ArgumentParser(description='description')
    parser.add_argument('-I', '--inputfile',
                        type=str,
                        default="/Users/suhara/Dropbox/Bitcoin (2)/bitcointalk_sep2016/announcements-data.csv",
                        help='Input filepath')
    parser.add_argument('-v', '--volatilityfile',
                        type=str,
                        default="results/final_prices_2016_11_100days_threads_2016_11_200days.csv",
                        help='Volatility filepath')
    parser.add_argument('-E', '--enddate',
                        type=str,
                        #required=True,
                        default="2016-11-01",
                        help='End period yyyy-mm-dd')
    parser.add_argument('-D', '--days',
                        type=int,
                        #required=True,
                        #default=600,
                        default=200,
                        help='Number of days')
    parser.add_argument('-i', '--introducerfile',
                        type=str,
                        #required=True,
                        default="/Users/suhara/research/Bitcoin/active-users-introducers/introducers/dec_2016/all_introducers_concat.csv",
                        help='Input filepath')
    parser.add_argument('-O', '--outputdir',
                        type=str,
                        #required=True,
                        #required=False,
                        default="output",
                        help='Output dirpath')
    parser.add_argument('-M', '--minposts',
                        type=int,
                        default=5,
                        help='Minimum post in a thread.')
    parser.add_argument('-S', '--stopword_filepath',
                        type=str,
                        default=None,
                        help='Stop word filepath (one word per line)')
    parser.add_argument('-W', '--worddict_filepath',
                        type=str,
                        default=None,
                        help='Word dictionary filepath (one word per line)')
    parser.add_argument('-N', '--normalize',
                        type=bool,
                        default=True,
                        help='Normalize word count in each digital coin')
    parser.add_argument('-f', '--filter_rule',
                        type=str,
                        default=None,
                        help='Filter rule {announcement, announcer, not_announcement, not_announcer}')
    parser.add_argument('-w', '--max_words',
                        type=int,
                        default=500,
                        help='max_words value for wordcloud')
    parser.add_argument('-T', '--trivialcoin-file',
                        type=str,
                        default="results/trivial-coins.csv",
                        help='filepath for trivial coin info')

    args = parser.parse_args()
    inputfile = args.inputfile
    volatilityfile = args.volatilityfile
    days = args.days
    enddate = args.enddate
    introducerfile = args.introducerfile
    outputdir = args.outputdir
    minposts = args.minposts
    stopword_filepath = args.stopword_filepath
    worddict_filepath = args.worddict_filepath
    normalize = args.normalize
    filter_rule = args.filter_rule
    if filter_rule in ["None", "none"]:
        filter_rule = None
    max_words = args.max_words
    trivialcoin_file = args.trivialcoin_file

    worddict_set = set()
    if worddict_filepath is not None:
        with open(worddict_filepath) as fin:
            for line in fin:
                line = line.rstrip()
                worddict_set.add(line)

    if not os.path.exists(outputdir):
        print("{} not exist. Create.".format(outputdir))
        os.makedirs(outputdir)

    coin_df = load_volatilityfile(volatilityfile)

    if trivialcoin_file is not None:
        # Overload trivialcoin if exists
        coin_df = pd.read_csv(trivialcoin_file)
        coin_df = coin_df.rename(
            columns={"coin": "symbol", "trivial-completed": "highvol_label"})

    dict_list = load_texts(days=days,
                           enddate=enddate,
                           inputfile=inputfile,
                           introducerfile=introducerfile,
                           minposts=minposts,
                           stopword_filepath=stopword_filepath,
                           normalize=normalize,
                           filter_rule=filter_rule)

    symbol_count_dict, symbol_fre_score_dict, symbol_fkg_score_dict = dict_list
    scores_df = pd.concat(
        [pd.Series(symbol_fkg_score_dict), pd.Series(symbol_fre_score_dict)],
        axis=1)
    scores_df.columns = ["fkg", "fre"]
    merge_df = pd.merge(coin_df, scores_df, left_on="symbol", right_index=True)
    agg_df = merge_df.groupby("highvol_label").agg(
        {"fkg": ["mean", "std"], "fre": ["mean", "std"]})
    agg_df.to_csv(os.path.join(outputdir,
                               "readability_scores.csv"))

    filtered_merge_df = merge_df[merge_df["fre"] > 0]
    filtered_agg_df = filtered_merge_df.groupby("highvol_label").agg(
        {"fkg": ["mean", "std"], "fre": ["mean", "std"]})
    filtered_agg_df.to_csv(os.path.join(outputdir,
                                        "readability_scores_filtered.csv"))


    #label_col = "highvol_label_025q"
    label_col = "highvol_label"
    label0_symbols = coin_df.query("{} == 0".format(label_col))["symbol"].tolist()
    label0_symbols = list(filter(lambda x: x in symbol_count_dict,
                          label0_symbols))
    label1_symbols = coin_df.query("{} == 1".format(label_col))["symbol"].tolist()
    label1_symbols = list(filter(lambda x: x in symbol_count_dict,
                          label1_symbols))

    # TODO: This concatenation should take into account "weight"
    label0_wordcount_s = pd.concat(
        list(map(lambda x: symbol_count_dict[x], label0_symbols)),
        axis=1).sum(axis=1) / len(label0_symbols)

    label1_wordcount_s = pd.concat(
        list(map(lambda x: symbol_count_dict[x], label1_symbols)),
        axis=1).sum(axis=1) / len(label1_symbols)

    wordcount_df = pd.concat([label0_wordcount_s, label1_wordcount_s],
                             axis=1).fillna(0)
    """
    wordcount_df = wordcount_df[
        wordcount_df.index.map(lambda x: re.match("^(\d+)|(_+)", x) is None)]
    """

    if len(worddict_set) > 0:
        words_set = worddict_set
    else:
        # English words
        words_set = set(words.words())
        # English nouns (from brown corpus)
        words_set = nn_words & words_set
    wordcount_df = wordcount_df[wordcount_df.index.map(lambda x: x in words_set)]

    wordcount_df[0].sort_values(ascending=False).to_csv(
        os.path.join(outputdir,
                     "lowvol_word_s.csv"))
    wordcount_df[1].sort_values(ascending=False).to_csv(
        os.path.join(outputdir,
                     "highvol_word_s.csv"))

    for label in [0, 1]:
        texts = []
        for w, count in wordcount_df.sort_values(label,
                                                 ascending=False).head(50)[label].iteritems():
            texts += [w] * int(count)
        texts = " ".join(texts)
        wc = WordCloud(background_color="white",
                       max_words=max_words,
                       collocations=False)
        wordcloud = wc.generate(texts)
        wc.to_file(os.path.join(outputdir,
                                "volatility_{}.png".format(label)))

        """
        # Display the generated image:
        # the matplotlib way:
        plt.imshow(wordcloud, interpolation='bilinear')
        plt.axis("off")
        plt.show()
        """