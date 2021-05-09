#to find Trials having duplicate data for Ethics Committees
import pandas as pd 
ec = pd.read_csv("ec.csv", encoding="unicode_escape")
ids = list(ec["Trial ID"].unique())
repeat_ec = []
for i in ids:
    if (len(ec.loc[ec["Trial ID"]==i]["EC Name"])==len(ec.loc[ec["Trial ID"]==i]["EC Name"].unique())):
        pass
    else:
        repeat_ec.append(i)
