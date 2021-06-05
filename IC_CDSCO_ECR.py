#download ECRs from CDSCO
import pandas as pd
import requests
url = "https://cdsco.gov.in/opencms/opencms/en/Clinical-Trial/Ethics-Committee/Ethics-Committee-Re-Registration/"
r = requests.get(url)
df_list = pd.read_html(r.text) # this parses all the tables in webpages to a list
df = df_list[0]
df.to_csv("IC_CDSCO_ECR_5June2021.csv")
