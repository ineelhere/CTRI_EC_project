#python code to match Sites' data with respective EC data
#works with best accuracy when raw data has no duplicate entries
#data deduplication and preprocessing is required before final approach
#importing the libraries
import pandas as pd 
import fuzzywuzzy as fuzz
from fuzzywuzzy import fuzz
#importing the Sites and EC data stored in csv files (which were extracted from the in-house database)
sos = pd.read_csv("sos.csv", encoding="unicode_escape")
sos_data = sos[['Trial ID', 'CTRI Number', 'Number of Sites', 'Site Name']].reset_index()
ec = pd.read_csv("ec.csv", encoding="unicode_escape")
ec_data = ec[['Trial ID', 'CTRI Number','Number of Committees', 'EC Name']].reset_index()
#preprocessing the strings
def str_prep(data):
  data = data.lower()
  to_remove = ['hospital','medical','institute','research','committee','ethics','ethical','institutional','committe','review','board','institutional','instituational','!','”','#','$','%','&','’','(',')','*','+',',','-','.','/',':',';','<','=','>','?','@','[',']','^','_','`','{','|','}','~']
  for txt in to_remove:
    data = data.replace(txt,'')
  data = data.strip()
  return (data)
#scoring every combination of datapoints in terms of best matches
trial_id = []
ctri_number = []
sosno = []
sos = []
sosmod = []
ecno = []
ec = []
ecmod = []
score = []
for i in range(0, len(sos_data)):
    print(len(sos_data)-i)
    Str1=sos_data["Site Name"][i]
    Str1=str_prep(Str1)
    for j in range(0, len(ec_data)):
        if (ec_data["Trial ID"][j]==sos_data["Trial ID"][i]):
            Str2 = ec_data["EC Name"][j]
            Str2=str_prep(Str2)
            Ratio = fuzz.ratio(Str1.lower(),Str2.lower())
            trial_id.append(sos_data["Trial ID"][i])
            ctri_number.append(sos_data["CTRI Number"][i])
            sosno.append(sos_data["Number of Sites"][i])
            sos.append(sos_data["Site Name"][i])
            sosmod.append(Str1)
            ecno.append(ec_data["Number of Committees"][j])
            ec.append(ec_data["EC Name"][j])
            ecmod.append(Str2)
            score.append(Ratio)
result = pd.DataFrame({'trial id' : trial_id,
                       'ctri number' : ctri_number, 
                        'number of sites' : sosno, 
                        'sites of study' : sos,
                        'sites of study modified' : sosmod,
                        'number of ec' : ecno,
                        'ethics committee' : ec,
                        'ethics committee modified' : ecmod,
                        'score' : score })        
result.to_csv("IC_res_all.csv")
#to screen out best unique matches per Site with EC data having best scores
ids = list(result["trial id"].unique())
for i in range(0,len(ids)):
    temp = result.loc[result["trial id"]==ids[i]]
    temp.sort_values(by=['score'], inplace=True, ascending=False)
    temp = temp.reset_index()
    eclist = list(temp["ethics committee"].unique())
    temp2 = temp.loc[temp["ethics committee"]==eclist[0]].reset_index(drop=True).head(1)
    for j in eclist:
        if (temp.empty or temp2.empty):
            break
        temp2 = temp.loc[temp["ethics committee"]==j].reset_index(drop=True).head(1)
        temp = temp[temp["ethics committee"]!=j]
        sostemp = str(temp2["sites of study"][0])
        temp = temp[temp["sites of study"]!=sostemp]
        temp = temp.reset_index(drop=True)
        temp2.to_csv("IC_res_unique.csv", index = False, header = False, mode = 'a')
