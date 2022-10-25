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


# function to calculate the mutual information 
def mutal_information_calculation(file_root, outfile_name, verbose=False, stat_signif=False, time_lag_max=10):

    # array with all files in file_root with os.path
    if ".csv" in file_root:
        files = [file_root]
    else:
        files = [f for f in os.listdir(file_root) if osp.isfile(osp.join(file_root, f))]

    # debug:
    #files = [os.listdir(file_root)[:2]]

    # pandas df with columns Year, Month, Day, Sensor1, Sensor2, Time_lag, MI and Stat_sig
    df = pd.DataFrame(columns=["Year", "Month", "Day", "Sensor1", "Sensor2", "Time_lag", "MI", "Stat_Sig"])

    for file in tqdm(files, position=0, desc="Processing files"):

        file_path = osp.join(file_root, file)
        # print("----------------------------------")
        tqdm.write("Processing file: \"" + file + "\"")

        # get day, month and year from file name in pattern dd-mm-yyyy with regex
        day, month, year = re.findall(r'\d+', file)

        # read first line of file to get column names
        with open(file_path, 'r') as f:
            column_names = f.readline().split(',')
            # remove any non digit characters from column names
            column_names = [re.sub(r'\D', '', column_name) for column_name in column_names]

        # 0. Load/prepare the data:
        dataRaw = readFloatsFile.readFloatsFile(file_path)

        # print column names if verbose
        if verbose:
            print("Column names: " + str(column_names))

        # As numpy array:
        data = np.array(dataRaw)
        # 1. Construct the calculator:
        calcClass = JPackage("infodynamics.measures.continuous.kraskov").MutualInfoCalculatorMultiVariateKraskov1
        calc = calcClass()

        for time_lag in tqdm(range(0, time_lag_max), position=1, leave=False, desc="Time lag"):
            # 2. Set any properties to non-default values:
            calc.setProperty("TIME_DIFF", str(time_lag))
            #calc.setProperty("DYN_CORR_EXCL", "24")
            
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
def active_information_storage_calculation(file_root, outfile_name, verbose=False, stat_signif=False):
    # array with all files in file_root with os.path
    if ".csv" in file_root:
        files = [file_root]
    else:
        files = [f for f in os.listdir(file_root) if osp.isfile(osp.join(file_root, f))]
    
    # debug:
    #files = [os.listdir(file_root)[:2]]
    
    # pandas df with columns Year, Month, Day, Sensor, AIS and Stat_sig
    df = pd.DataFrame(columns=["Year", "Month", "Day", "Sensor", "AIS", "Stat_Sig"])
    
    for file in tqdm(files, position=0, desc="Processing files"):
    
        file_path = osp.join(file_root, file)
        tqdm.write("Processing file: \"" + file + "\"")
    
        # get day, month and year from file name in pattern dd-mm-yyyy with regex
        day, month, year = re.findall(r'\d+', file)
    
        # read first line of file to get column names
        with open(file_path, 'r') as f:
            column_names = f.readline().split(',')
            # remove any non digit characters from column names
            column_names = [re.sub(r'\D', '', column_name) for column_name in column_names]
    
        # 0. Load/prepare the data:
        dataRaw = readFloatsFile.readFloatsFile(file_path)
    
        # print column names if verbose
        if verbose:
            print("Column names: " + str(column_names))
    
        # As numpy array:
        data = np.array(dataRaw)
        # 1. Construct the calculator:
        calcClass = JPackage("infodynamics.measures.continuous.kraskov").ActiveInfoStorageCalculatorKraskov
        calc = calcClass()
    
        # 2. Set any properties to non-default values:
        #calc.setProperty("DYN_CORR_EXCL", "24")
        calc.setProperty("k_History", "1")
        
        # Compute for all pairs:
        for v in tqdm(range(data.shape[1]), position=2, leave=False, desc="Sensor 1"):
            variable = JArray(JDouble, 1)(data[:, v].tolist())
    
            # 3. Initialise the calculator for (re-)use:
            calc.initialise()
            # 4. Supply the sample data:
            calc.setObservations(variable)
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
            df = pd.concat([df, pd.DataFrame([[year, month, day, column_names[v], result, p_value]], columns=["Year", "Month", "Day", "Sensor", "AIS", "Stat_Sig"])], ignore_index=True)
    
            # print result for each sensor pair with 4 decimal places, nulldist, std, p_value and time lag using f-string
            if verbose:
                if stat_signif:
                    tqdm.write(f"AIS({column_names[v]}) = {result:.4f} nulldist = {nulldist:.4f} std = {std:.4f} p_value = {p_value:.4f}")
                else:
                    tqdm.write(f"AIS({column_names[v]}) = {result:.4f}")


        # save df to csv every file iteration
        if stat_signif:
            if outfile_name.endswith("_stat_sig.csv"):
                df.to_csv(outfile_name, index=False)
            else:
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