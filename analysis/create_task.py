if __name__ == "__main__":
    # base_cmd = "python analysis/generate_wordclouds.py -f {} -w {} -O {}"
    base_cmd = "python analysis/generate_wordclouds.py -W analysis/corpus.txt -f {} -w {} -O {}"
    for f in ["None", "announcement", "announcer", "not_announcement", "not_announcer"]:
        for w in [100, 500, 2000, 3000, 4000, 5000]:
            outputdir = "output/f-{}_w-{}".format(f, w)
            cmd = base_cmd.format(f, w, outputdir)
            print(cmd)
