from jpype import *
import numpy as np
import sys
# Our python data file readers are a bit of a hack, python users will do better on this:
sys.path.append("/Users/simongimmini/forks/jidt/demos/python")
import readFloatsFile
from pprint import pprint
import pandas as pd
import re
import os
from os import path as osp, stat
from tqdm import tqdm
import matplotlib.pyplot as plt
from statsmodels.graphics.tsaplots import plot_acf


# plot autocorrelation function for one column of a file that is read in as a pandas df
def plot_acf_for_file(file_path, lags=100):
    # print all column names
    df = pd.read_csv(file_path, sep=";")
    print(df.columns)

    # let the user choose the column to plot
    column = input("Please enter the column name to plot the acf for: ")

    df = pd.read_csv(file_path)
    plot_acf(df[column], lags=lags)
    plt.show() 

def get_year_month_day(file):
    try:    
        # get day, month and year from file name in pattern dd-mm-yyyy with regex
        day, month, year = re.findall(r'\d+', file.split("/")[-1])
    # except value error 
    except ValueError:
        try:
            # get month and year from file name in pattern mm-yyyy with regex
            month, year = re.findall(r'\d+', file.split("/")[-1])
            day = np.nan
        except ValueError:
            # get year from file name in pattern yyyy with regex
            year = re.findall(r'\d+', file.split("/")[-1])[0]
            month = np.nan
            # dict of weekday names to numbers 
            # this is for having a structure for a better analysis
            weekdays = {"monday": 1, "tuesday": 2, "wednesday": 3, "thursday": 4, "friday": 5, "saturday": 6, "sunday": 7}

            # check if file name contains a weekday
            for weekday in weekdays:
                if weekday in file:
                    day = weekdays[weekday]
                    break
                else:
                    day = np.nan

    return year, month, day



# function to calculate the mutual information 
def mutal_information_calculation(file_root, outfile_name, verbose=False, stat_signif=False, time_lag_max=10, dyn_corr_excl=0):

    # array with all files in file_root with os.path
    if ".csv" in file_path:
        files = [file_path]
    else:
        files = [osp.join(file_path, f) for f in os.listdir(file_path) if osp.isfile(osp.join(file_path, f))]


    # debug:
    #files = [os.listdir(file_root)[:2]]

    # pandas df with columns Year, Month, Day, Sensor1, Sensor2, Time_lag, MI and Stat_sig
    df = pd.DataFrame(columns=["Year", "Month", "Day", "Sensor1", "Sensor2", "Time_lag", "MI", "Stat_Sig"])

    for file in tqdm(files, position=0, desc="Processing files"):

        #file_path = osp.join(file_root, file)
        # print("----------------------------------")
        tqdm.write("Processing file: \"" + file + "\"")

        year, month, day = get_year_month_day(file)

        if verbose:
            print("Year: " + str(year) + ", Month: " + str(month) + ", Day: " + str(day))

        # read first line of file to get column names
        with open(file, 'r') as f:
            column_names = f.readline().split(',')
            # remove any non digit characters from column names
            column_names = [re.sub(r'\D', '', column_name) for column_name in column_names]

        # 0. Load/prepare the data:
        dataRaw = readFloatsFile.readFloatsFile(file)

        # print column names if verbose
        if verbose:
            print("Column names: " + str(column_names))

        # As numpy array:
        data = np.array(dataRaw)
        # 1. Construct the calculator:
        calcClass = JPackage("infodynamics.measures.continuous.kraskov").MutualInfoCalculatorMultiVariateKraskov1
        calc = calcClass()

        for time_lag in tqdm(range(0, time_lag_max+1), position=1, leave=False, desc="Time lag"):
            # 2. Set any properties to non-default values:
            calc.setProperty("TIME_DIFF", str(time_lag))
            calc.setProperty("DYN_CORR_EXCL", str(dyn_corr_excl))

            # Compute for all pairs:
            for s in tqdm(range(data.shape[1]), position=2, leave=False, desc="Sensor 1"):
                for d in tqdm(range(data.shape[1]), position=3, leave=False, desc="Sensor 2"):
                    # For each source-dest pair:
                    if (s == d):
                        continue
                    source = JArray(JDouble, 1)(data[:, s].tolist())
                    destination = JArray(JDouble, 1)(data[:, d].tolist())

                    # 3. Initialise the calculator for (re-)use:
                    calc.initialise()
                    # 4. Supply the sample data:
                    calc.setObservations(source, destination)
                    # 5. Compute the estimate:
                    result = calc.computeAverageLocalOfObservations()

                    if stat_signif:
                        # 6. Compute the (statistical significance via) null distribution empirically (e.g. with 100 permutations):
                        measDist = calc.computeSignificance(100)
                        nulldist = measDist.getMeanOfDistribution()
                        std = measDist.getStdOfDistribution()
                        p_value = measDist.pValue
                    else: 
                        p_value = np.nan

                    # save results in df with pd.concat
                    df = pd.concat([df, pd.DataFrame([[year, month, day, column_names[s], column_names[d], time_lag, result, p_value]], columns=["Year", "Month", "Day", "Sensor1", "Sensor2", "Time_lag", "MI", "Stat_Sig"])], ignore_index=True)
 
                    # print result for each sensor pair with 4 decimal places, nulldist, std, p_value and time lag using f-string
                    if verbose:
                        if stat_signif:
                            tqdm.write(f"MI({column_names[s]} -> {column_names[d]}) = {result:.4f} nulldist = {nulldist:.4f} std = {std:.4f} p_value = {p_value:.4f} time lag = {time_lag}")
                        else:
                            print(f"MI_Kraskov for sensor {column_names[s]} to sensor {column_names[d]} = {result:.4f} nats, time lag: {time_lag}")

                    
        # save df to csv
        if stat_signif:
            if outfile_name.endswith("_stat_sig.csv"):
                df.to_csv(outfile_name, index=False)
            else:
                outfile_name = outfile_name.split(".")[0] + "_stat_sig.csv"
                df.to_csv(outfile_name, index=False)
        else:
            df.to_csv(outfile_name, index=False)



# function to calculate the active information storage
def active_information_storage_calculation(file_path, outfile_name, verbose=False, stat_signif=False, dyn_corr_excl=0, split_observations=False, split_length=None):

    # array with all files in file_root with os.path
    if ".csv" in file_path:
        files = [file_path]
    else:
        files = [osp.join(file_path, f) for f in os.listdir(file_path) if osp.isfile(osp.join(file_path, f))]

   
    # debug:
    #files = [os.listdir(file_root)[:2]]
    
    # pandas df with columns Year, Month, Day, Sensor, AIS and Stat_sig
    df = pd.DataFrame(columns=["Year", "Month", "Day", "Sensor", "AIS", "Stat_Sig"])
    
    for file in tqdm(files, position=0, desc="Processing files"):
    
        tqdm.write("Processing file: \"" + file + "\"")
    
        year, month, day = get_year_month_day(file)

        if verbose:
            print("Year: " + str(year) + ", Month: " + str(month) + ", Day: " + str(day))

        # read first line of file to get column names
        with open(file, 'r') as f:
            column_names = f.readline().split(',')
            # remove any non digit characters from column names
            column_names = [re.sub(r'\D', '', column_name) for column_name in column_names]
    
        # 0. Load/prepare the data:
        dataRaw = readFloatsFile.readFloatsFile(file)
    
        # print column names if verbose
        if verbose:
            print("Column names: " + str(column_names))
    
        # As numpy array:
        data = np.array(dataRaw)
        # 1. Construct the calculator:
        calcClass = JPackage("infodynamics.measures.continuous.kraskov").ActiveInfoStorageCalculatorKraskov
        calc = calcClass()
    
        # 2. Set any properties to non-default values:
        calc.setProperty("k_History", "2")
        calc.setProperty("TAU", "5")
                
        # Compute for all columns:
        for v in tqdm(range(data.shape[1]), position=2, leave=False, desc="Sensor 1"):

            # set properties
            # FIXME: Addition of multiple observation sets is not currently supported with property DYN_CORR_EXCL set
            if not split_observations:
                calc.setProperty("DYN_CORR_EXCL", str(dyn_corr_excl)) 
                calc.setProperty("AUTO_EMBED_METHOD", "MAX_CORR_AIS")
                calc.setProperty("AUTO_EMBED_K_SEARCH_MAX", "10")
                calc.setProperty("AUTO_EMBED_TAU_SEARCH_MAX", "10")

            # 3. Initialise the calculator for (re-)use:
            calc.initialise()

            if split_observations:
                calc.startAddObservations()

                if split_length == 31:
                    # if month is 1, 3, 5, 7, 8, 10, 12
                    if month in [1, 3, 5, 7, 8, 10, 12]:
                        split_length = 31
                    # if month is 4, 6, 9, 11
                    elif month in [4, 6, 9, 11]:
                        split_length = 30
                    # if month is 2
                    elif month == 2:
                        # I removed 29th in all datasets
                        split_length = 28

                # split every column to oberservations of length 24 for every day
                for i in range(0, data.shape[0], split_length):
                    observations = JArray(JDouble, 1)(data[i:i+split_length, v].tolist())
                    calc.addObservations(observations)

                # 4. Finalise adding observations:
                calc.finaliseAddObservations()

            else: 
                variable = JArray(JDouble, 1)(data[:, v].tolist())
    
                # 4. Supply the sample data:
                calc.setObservations(variable)

            result = calc.computeAverageLocalOfObservations()
            if stat_signif:
                # 6. Compute the (statistical significance via) null distribution empirically (e.g. with 100 permutations):
                measDist = calc.computeSignificance(100)
                nulldist = measDist.getMeanOfDistribution()
                std = measDist.getStdOfDistribution()
                p_value = measDist.pValue
            else: 
                p_value = np.nan

            # save results in df with pd.concat
            df = pd.concat([df, pd.DataFrame([[year, month, day, column_names[v], result, p_value]], columns=["Year", "Month", "Day", "Sensor", "AIS", "Stat_Sig"])], ignore_index=True)

            # print result for each sensor pair with 4 decimal places, nulldist, std, p_value and time lag using f-string
            if verbose:
                if stat_signif:
                    tqdm.write(f"AIS({column_names[v]}) = {result:.4f} nulldist = {nulldist:.4f} std = {std:.4f} p_value = {p_value:.4f}")
                else:
                    print(f"AIS_Kraskov for sensor {column_names[v]} = {result:.4f} nats")

    
        # save df to csv every file iteration
        if stat_signif:
            if outfile_name.endswith("_stat_sig.csv"):
                df.to_csv(outfile_name, index=False)
            else:
                outfile_name = outfile_name.split(".")[0] + "_stat_sig.csv"
                df.to_csv(outfile_name, index=False)
        else:
            df.to_csv(outfile_name, index=False)


# function to calculate the transfer entropy for all sensor pairs
def transfer_entropy_calculation(file_root, outfile_name, verbose=False, stat_signif=False, time_lag_max=10, dyn_corr_excl=0):

    # array with all files in file_root with os.path
    if ".csv" in file_path:
        files = [file_path]
    else:
        files = [osp.join(file_path, f) for f in os.listdir(file_path) if osp.isfile(osp.join(file_path, f))]


    # pandas df with columns Year, Month, Day, Sensor1, Sensor2, Time_lag, TE and Stat_sig
    df = pd.DataFrame(columns=["Year", "Month", "Day", "Sensor1", "Sensor2", "Time_lag", "TE", "Stat_sig"])

    for file in tqdm(files, position=0, desc="Processing files"):

        tqdm.write("Processing file: \"" + file + "\"")

        year, month, day = get_year_month_day(file)

        # read first line of file to get column names
        with open(file, 'r') as f:
            column_names = f.readline().split(',')
            # remove any non digit characters from column names
            column_names = [re.sub(r'\D', '', column_name) for column_name in column_names]

        # 0. Load/prepare the data:
        dataRaw = readFloatsFile.readFloatsFile(file)

        # print column names if verbose
        if verbose:
            print("Column names: " + str(column_names))

        # As numpy array:
        data = np.array(dataRaw)
        
        # 1. Construct the calculator:
        calcClass = JPackage("infodynamics.measures.continuous.kraskov").TransferEntropyCalculatorKraskov
        calc = calcClass()
        # 2. Set any properties to non-default values:
        calc.setProperty("k_HISTORY", "2")
        calc.setProperty("k_TAU", "3")
        calc.setProperty("l_HISTORY", "2")
        calc.setProperty("l_TAU", "6")
        calc.setProperty("DYN_CORR_EXCL", str(dyn_corr_excl))
        calc.setProperty("AUTO_EMBED_METHOD", "MAX_CORR_AIS")
        calc.setProperty("AUTO_EMBED_K_SEARCH_MAX", "10")
        calc.setProperty("AUTO_EMBED_TAU_SEARCH_MAX", "10")

        for time_lag in tqdm(range(1, time_lag_max+1), position=1, leave=False, desc="Processing time lags"):
            calc.setProperty("DELAY", str(time_lag))
            # Compute for all pairs:
            for s in tqdm(range(data.shape[1]), position=2, leave=False, desc="Processing sensor 1"):
                for d in tqdm(range(data.shape[1]), position=3, leave=False, desc="Processing sensor 2"):
                    # For each source-dest pair:
                    if (s == d):
                        continue
                    source = JArray(JDouble, 1)(data[:, s].tolist())
                    destination = JArray(JDouble, 1)(data[:, d].tolist())

                    # 3. Initialise the calculator for (re-)use:
                    calc.initialise()
                    # 4. Supply the sample data:
                    calc.setObservations(source, destination)
                    # 5. Compute the estimate:
                    result = calc.computeAverageLocalOfObservations()

                    if stat_signif:
                        # 6. Compute the (statistical significance via) null distribution empirically (e.g. with 100 permutations):
                        measDist = calc.computeSignificance(100)
                        nulldist = measDist.getMeanOfDistribution()
                        std = measDist.getStdOfDistribution()
                        p_value = measDist.pValue
                    else:
                        p_value = np.nan

                    # save results in df with pd.concat
                    df = pd.concat([df, pd.DataFrame([[year, month, day, column_names[s], column_names[d], time_lag, result, p_value]], columns=["Year", "Month", "Day", "Sensor1", "Sensor2", "Time_lag", "TE", "Stat_sig"])], ignore_index=True)

                    # print result for each sensor pair with 4 decimal places, null distribution, std, p-value and time lag using f-string
                    if verbose:
                        if stat_signif:
                            print(f"TE_Kraskov for sensor {column_names[s]} to sensor {column_names[d]} = {result:.4f} nats, null distribution: {nulldist}, std: {std}, p-value: {p_value}, time lag: {time_lag}")
                        else:
                            print(f"TE_Kraskov for sensor {column_names[s]} to sensor {column_names[d]} = {result:.4f} nats, time lag: {time_lag}")

            # save df to csv every time lag
            if stat_signif:
                outfile_name = outfile_name.split(".")[0] + "_stat_sig.csv"
                df.to_csv(outfile_name, index=False)
            else:
                df.to_csv(outfile_name, index=False)

  

# main function
def main():

    # Add JIDT jar library to the path
    jarLocation = "/Users/simongimmini/forks/jidt/infodynamics.jar"

    # file root 
    file = "/Users/simongimmini/Documents/Studium/CSYS5030 Information Theory/assignments/3/pedestrians/data/one_week"

    # Start the JVM (add the "-Xmx" option with say 1024M if you get crashes due to not enough memory space)
    startJVM(getDefaultJVMPath(), "-ea", "-Djava.class.path=" + jarLocation)

    active_information_storage_calculation(file, outfile_name="week_daily_hourly_AIS.csv", verbose=False, stat_signif=True)

    #mutal_information_calculation(file, outfile_name="week_daily_hourly_MI_TL10.csv", verbose=False, stat_signif=False, time_lag_max=10)

    # transfer_entropy_calculation(file_root, outfile_name=f"{interval}_TE_TL5_dyncorr55.csv", verbose=False, stat_signif=False)

# main
if __name__ == "__main__":
    main()