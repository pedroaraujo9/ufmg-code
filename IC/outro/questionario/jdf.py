import pandas as pd
df = pd.read_json('C://Users//Pedro//Desktop//quest2-40492-export.json', orient='index')
#print(df)
df.to_csv('c:/users/pedro/desktop/teste.csv')
