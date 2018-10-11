
cmd = "python analysis/worddist_test_diff.py output/f-{}_w-5000/keywords_dist_diff.csv {} fig/{}_w-5000_{}"
for name in ["None", "not_announcer"]:
    for n in [10, 20, 30, 40, 50, 70, 75, 80, 100]:
        print(cmd.format(name, n, name, n))


