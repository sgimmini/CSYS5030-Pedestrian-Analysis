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

# function to calculate the autocorrelation of a 1D vector
def autocorrelation(x):
    data = x - np.mean(x)
    return np.correlate(data, data, mode='full')[len(data)-1:] / np.var(x) / len(data)
    

    # result = np.correlate(x, x, mode='full')
    # return result[result.size//2:]


# function to calculate the active informaiton storage of a sensor
def active_information_storage_calculation(file_root, outfile_name, verbose=False):

    # array with all files in file_root with os.path
    files = [f for f in os.listdir(file_root) if osp.isfile(osp.join(file_root, f))]

    # pandas df with columns year, sensor, value
    df = pd.DataFrame(columns=["Year", "Sensor_ID", "AIS"])


    for file in tqdm(files, position=0, desc="Processing files"):
        file_path = osp.join(file_root, file)
        tqdm.write("Processing file: \"" + file + "\"")
        # get year from filename 
        year = re.search(r'\d{4}', file).group(0)

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

        # done once in AutoAnalyser and set in code
        # calc.setProperty("AUTO_EMBED_METHOD", "MAX_CORR_AIS")
        # calc.setProperty("AUTO_EMBED_K_SEARCH_MAX", "5")
        # calc.setProperty("AUTO_EMBED_TAU_SEARCH_MAX", "1")

        for i in range(0, data.shape[1]):
            variable = JArray(JDouble, 1)(data[:,i].tolist())

            # 1. Construct the calculator:
            calcClass = JPackage("infodynamics.measures.continuous.kraskov").ActiveInfoStorageCalculatorKraskov
            calc = calcClass()
            # 2. Set any properties to non-default values:
            calc.setProperty("k_HISTORY", "5")

            # 3. Initialise the calculator for (re-)use:
            calc.initialise()
            # 4. Supply the sample data:
            calc.setObservations(variable)
            # 5. Compute the estimate:
            result = calc.computeAverageLocalOfObservations()
            # localAISValues = calc.computeLocalFromPreviousObservations(variable)
            # print result for each column with 4 decimal places using f-string
            if verbose:
                print(f"AIS_Kraskov (KSG) for sensor {column_names[i]} = {result:.4f} nats")

            # add values to df with concat
            df = pd.concat([df, pd.DataFrame([[year, column_names[i], result]], columns=["Year", "Sensor_ID", "AIS"])])


    # save df to csv
    df.to_csv(outfile_name, index=False)




def mutal_information_calculation(file_root, outfile_name, verbose=False, stat_signif=False, time_lag_max=10):

    # array with all files in file_root with os.path
    if ".csv" in file_root:
        files = [file_root]
    else:
        files = [f for f in os.listdir(file_root) if osp.isfile(osp.join(file_root, f))]

    # debug:
    #files = [os.listdir(file_root)[:2]]

    # pandas df with columns Year, Month, Sensor1, Sensor2, Time_lag, MI and Stat_sig
    df = pd.DataFrame(columns=["Year", "Month", "Sensor1", "Sensor2", "Time_lag", "MI", "Stat_sig"])

    # nested dictionary with year as key. for each time_lag, min and max are keys with a tuple of an index-pair and value as value
    # {year: {time_lag: {min: (index_pair, value), max: (index_pair, value)}}}
    min_max_dict = {}

    for file in tqdm(files, position=0, desc="Processing files"):

        file_path = osp.join(file_root, file)
        # print("----------------------------------")
        tqdm.write("Processing file: \"" + file + "\"")
        # get year from filename
        year = re.search(r'\d{4}', file).group(0)
        # get one or two digit month from filename between "_" and "-"
        month = re.search(r'_(\d{1,2})-', file).group(1)

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
            calc.setProperty("DYN_CORR_EXCL", "55")

            # calculate autocorrelation for one column
            # dyn_corr = np.corrcoef(data[time_lag:, 0], data[:-time_lag, 0])[0, 1]
            # dyn_corr = autocorrelation(data[:, 0])
            # # plot dyn_corr
            # plt.plot(dyn_corr)
            # plt.show()
            # plot_acf(data[:, 0], lags=100)
            # plt.show()
            # exit()


            # set min and max 
            result_max = 0
            result_max_index = None
            result_min = 10
            result_min_index = None

            # Compute for all pairs:
            for s in range(data.shape[1]):
                for d in range(data.shape[1]):
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
                    else: 
                        measDist = np.nan

                    # set max and min
                    if result > result_max:
                        result_max = result
                        result_max_index = (s, d)
                    if result < result_min:
                        result_min = result
                        result_min_index = (s, d)

                    # save results in df with pd.concat
                    df = pd.concat([df, pd.DataFrame([[year, month, column_names[s], column_names[d], time_lag, result, measDist]], columns=["Year", "Month", "Sensor1", "Sensor2", "Time_lag", "MI", "Stat_sig"])])
 
                    # print result for each sensor pair with 4 decimal places, null distribution, std, p-value and time lag using f-string
                    if verbose:
                        if stat_signif:
                            print(f"MI_Kraskov for sensor {column_names[s]} to sensor {column_names[d]} = {result:.4f} nats, null distribution: {measDist.getMeanOfDistribution()}, std: {measDist.getStdOfDistribution()}, p-value: {measDist.pValue}, time lag: {time_lag}")
                        else:
                            print(f"MI_Kraskov for sensor {column_names[s]} to sensor {column_names[d]} = {result:.4f} nats, time lag: {time_lag}")

                    
            # add min and max to dictionary
            min_max_dict.setdefault(year, {})[time_lag] = {"min": (result_min_index, result_min), "max": (result_max_index, result_max)}
                      
    # print dictionary
    pprint(min_max_dict)

    # save df to csv
    if stat_signif:
        outfile_name = outfile_name.split(".")[0] + "_stat_sig.csv"
        df.to_csv(outfile_name, index=False)
    else:
        df.to_csv(outfile_name, index=False)
    

def transfer_entropy_calculation(file_root, outfile_name, verbose=False, stat_signif=False, time_lag_max=10):

    # array with all files in file_root with os.path
    if ".csv" in file_root:
        files = [file_root]
    else:
        files = [f for f in os.listdir(file_root) if osp.isfile(osp.join(file_root, f))]

    # debug
    # files = files[:2]

    # pandas df with columns Year, Month, Sensor1, Sensor2, Time_lag, TE and Stat_sig
    df = pd.DataFrame(columns=["Year", "Month", "Sensor1", "Sensor2", "Time_lag", "TE", "Stat_sig"])

    for file in tqdm(files, position=0, desc="Processing files"):

        file_path = osp.join(file_root, file)
        tqdm.write("Processing file: \"" + file + "\"")
        # get year from filename
        year = re.search(r'\d{4}', file).group(0)
        # get one or two digit month from filename between "_" and "-"
        month = re.search(r'_(\d{1,2})-', file).group(1)

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
        calcClass = JPackage("infodynamics.measures.continuous.kraskov").TransferEntropyCalculatorKraskov
        calc = calcClass()
        # 2. Set any properties to non-default values:
        calc.setProperty("k_HISTORY", "4")
        calc.setProperty("l_HISTORY", "4")
        calc.setProperty("DYN_CORR_EXCL", "55")
        # calc.setProperty("AUTO_EMBED_METHOD", "MAX_CORR_AIS")
        # calc.setProperty("AUTO_EMBED_K_SEARCH_MAX", "10")
        # calc.setProperty("AUTO_EMBED_TAU_SEARCH_MAX", "10")

        for time_lag in tqdm(range(1, time_lag_max), position=1, leave=False, desc="Processing time lags"):
            calc.setProperty("DELAY", str(time_lag))
            # Compute for all pairs:
            for s in tqdm(range(data.shape[1]), position=2, leave=False, desc="Processing sensor pairs"):
                for d in range(data.shape[1]):
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
                    df = pd.concat([df, pd.DataFrame([[year, month, column_names[s], column_names[d], time_lag, result, p_value]], columns=["Year", "Month", "Sensor1", "Sensor2", "Time_lag", "TE", "Stat_sig"])])

                    # print result for each sensor pair with 4 decimal places, null distribution, std, p-value and time lag using f-string
                    if verbose:
                        if stat_signif:
                            print(f"TE_Kraskov for sensor {column_names[s]} to sensor {column_names[d]} = {result:.4f} nats, null distribution: {nulldist}, std: {std}, p-value: {p_value}, time lag: {time_lag}")
                        else:
                            print(f"TE_Kraskov for sensor {column_names[s]} to sensor {column_names[d]} = {result:.4f} nats, time lag: {time_lag}")

        # save df to csv every time a file is processed
        if stat_signif:
            outfile_name = outfile_name.split(".")[0] + "_stat_sig.csv"
            df.to_csv(outfile_name, index=False)
        else:
            df.to_csv(outfile_name, index=False)

    


# main function
def main():

    # Add JIDT jar library to the path
    jarLocation = "/Users/simongimmini/forks/jidt/infodynamics.jar"

    interval = "hourly"

    # file root
    # file_root = f"/Users/simongimmini/Documents/Studium/CSYS5030 Information Theory/assignments/3/pedestrians/data/refactored_years_{interval}/"
    #file_root = f"/Users/simongimmini/Documents/Studium/CSYS5030 Information Theory/assignments/3/pedestrians/data/datetime_sensor_id_week-in_11-2018.csv"
    file_root = f"/Users/simongimmini/Documents/Studium/CSYS5030 Information Theory/assignments/3/pedestrians/hourly_months"


    # Start the JVM (add the "-Xmx" option with say 1024M if you get crashes due to not enough memory space)
    startJVM(getDefaultJVMPath(), "-ea", "-Djava.class.path=" + jarLocation)

    # active_information_storage_calculation(file_root, outfile_name="daily_AIS.csv", verbose=False)

    # mutal_information_calculation(file_root, outfile_name="week_in_11_MI.csv", verbose=False, stat_signif=False, time_lag_max=5)

    transfer_entropy_calculation(file_root, outfile_name=f"sensor_monthly_hourly_TE_TL5.csv", verbose=False, stat_signif=False, time_lag_max=5)


if __name__ == '__main__':
    main()