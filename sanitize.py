import json
import sys,os
import pandas as pd
def remove_first_col(csv_path,output_path):
    with open(csv_path) as i:
        with open(output_path,'w') as o:
            for line in i.readlines():
                line = line.split(',')
                o.write(','.join(line[1:]))

def sanitize(translate, input_path, output):
    temp_path = '__temp__csvthingy.csv'
    remove_first_col(input_path,temp_path)
    with open(temp_path) as i:
        with open(translate) as t:
            text = i.read()
            transdict = json.load(t)
            for (k,v) in transdict.items():
                text = text.replace(v,k)
            with open(output,'w') as o:
                o.write(text)
    os.remove(temp_path) 

if __name__ == "__main__":
    if len(sys.argv) ==4:
        _, translate, input_path, output_path = sys.argv
        sanitize(translate,input_path,output_path)
    else:
        print('usage: python3 sanitize.py [translate.json] [input.csv] [output.csv]')