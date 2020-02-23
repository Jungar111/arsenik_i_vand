import pandas as pd

path = 'Data/wells.txt'

df = pd.read_csv(path, sep=" ")
df.columns = ['village','nwell','conc']

print(df)