import json
import sys,os
import pandas as pd
def remove_first_col(csv_path,output_path):
    df = pd.read_csv(csv_path)
    first_column = df.columns[0]
    df = df.drop([first_column], axis=1)
    df.to_csv(output_path, index=False)

def sanitize(translate, input_path, output):
    temp_path = '__temp__csvthingy.csv'
    remove_first_col(input_path,temp_path)
    with open(temp_path) as i:
        text = i.read()
        transdict = json.load(translate)
        for (k,v) in transdict.items():
            text = text.replace(v,k)
        with open(output,'w') as o:
            o.write(text)
    os.remove(temp_path) 
if __name__ == "__main__":
    print('todo test this')
    print('make this at leas functional?')
    print('todo consider replacing pandas with pure python')
    if len(sys.argv) ==4:
        _, translate, input_path, output_path = sys.argv
        sanitize(translate,input_path,output_path)
    else:
        print('invalid number of args')