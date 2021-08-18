# this programs import diary contents to a pandas dataframe
import glob
import argparse
import os
import re
import pandas as pd
from pathlib import Path
import matplotlib.pyplot as plt

# arguments
parser = argparse.ArgumentParser()
parser.add_argument("-l",
                    "--location",
                    help="The path to the diary folder",
                    dest="diary_loc",
                    metavar="DIARY_PATH")
args = parser.parse_args().__dict__

# basic checks
dir_loc = args['diary_loc']
if dir_loc is None:
    print("Invalid argument.")
    raise SystemExit()

try:
    os.chdir(dir_loc)
except FileNotFoundError:
    print("Invalid argument.")
    raise SystemExit()

# getting file list
file_list = glob.glob('**/*.*', recursive=True)
if len(file_list) == 0:
    print("Empty folder.")
    raise SystemExit()

# creating the dataframe
df = pd.DataFrame()
for i in file_list:
    splitted = i.split("/")

    # extracting timestamp
    year = splitted[0]
    month = splitted[1]
    day = splitted[2]
    if ".txt" in day:
        day = day[:-4]
    elif ".md" in day:
        day = day[:-3]
    else:
        print("Invalid text format?")
        breakpoint()
    tcont = Path(i).read_text()


    dosages = [0]
    drug_type = []
    for l in tcont.split("\n"):
        # extracting drug type
        drug_type = drug_type + re.findall("Ritaline LP|Ritaline|Concerta|Quasym", l)

        if drug_type != []:
            # extracting drug dosage
            match = re.findall(r"(\d{1,})mg", l)
            if "" in match:
                match.remove("")
            if match != []:
                dosages = dosages + [int(x) for x in match]


    dic = {"year": year,
           "month": month,
           "day": day,
           "date": pd.to_datetime(f"{month}-{day}-{year}"),
           "text_content": tcont,
           "daily_dosage": sum(dosages),
           "drug_type": drug_type,
           "dosages": dosages
           }
    df = df.append(dic, ignore_index=True)


# get day of the week
df["day_otw"] = df["date"].dt.day_name()

# getting first day of treatment:
for i in df.index:
    if df.loc[i, "drug_type"] != []:
        first_day = df.loc[i, "date"]
        break
elapsed = (df.loc[len(df.index)-1, "date"] - df.loc[0, "date"]).days

# calculating exponential smoothing
l_smooth = [0]
for i in df.daily_dosage:
    l_smooth.append(l_smooth[-1]*0.5 + i*0.5)
l_smooth.remove(0)
df["exp_smoothing"] = l_smooth

# calculating moving average
df["mov_avg"] = df["daily_dosage"]
for i in df.index[0:-7]:
    df.loc[i+7, "mov_avg"] = sum([df.loc[i,   "daily_dosage"],
                                  df.loc[i+1, "daily_dosage"], 
                                  df.loc[i+2, "daily_dosage"],
                                  df.loc[i+3, "daily_dosage"],
                                  df.loc[i+4, "daily_dosage"],
                                  df.loc[i+5, "daily_dosage"],
                                  df.loc[i+6, "daily_dosage"]])/7




# output
print(f"Total dosage over lifetime : {df['daily_dosage'].sum()/1000}g")
print(f"First day of treatment is: {first_day}")
print(f"Average dosage since first day of treatment is: {round(df['daily_dosage'].sum()/elapsed, 2)}mg")
df.plot.line(x="date", y="mov_avg")
df.plot.line(x="date", y="exp_smoothing")
df.plot.line(x="date", y="daily_dosage")
df.plot.scatter(x="date", y="daily_dosage")
plt.show()
breakpoint()
