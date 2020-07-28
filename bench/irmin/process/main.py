import json
import pandas as pd

data = open('data.json', 'r')
data = json.dumps(json.load(data)['results'])

df = pd.read_json(data, orient='index')
print(df)
