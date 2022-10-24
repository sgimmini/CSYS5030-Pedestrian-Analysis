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


def mutal_information_calculation(file, outfile_name, verbose=False, stat_signif=False):

    # pandas df with columns Year1, Year2, MI and Stat_Sig
    df = pd.DataFrame(columns=["Year1", "Year2", "MI", "Stat_Sig"])

    # print("----------------------------------")
    tqdm.write("Processing file: \"" + file + "\"")

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

    # 2. Set any properties to non-default values:
    # calc.setProperty("DYN_CORR_EXCL", "55")

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
                nulldist = measDist.getMeanOfDistribution()
                std = measDist.getStdOfDistribution()
                p_value = measDist.pValue
            else: 
                measDist = np.nan

            # save results in df with pd.concat
            df = pd.concat([df, pd.DataFrame([[column_names[s], column_names[d], result, p_value]], columns=["Year1", "Year2", "MI", "Stat_Sig"])])
 
            # print result for each year pair with 4 decimal places, null distribution, std, p-value using f-string
            if verbose:
                if stat_signif:
                    print(f"MI({column_names[s]} -> {column_names[d]}): {result:.4f} nulldist: {nulldist:.4f} std: {std:.4f} p-value: {p_value:.4f}")
                else:
                    print(f"MI_{column_names[s]}_{column_names[d]} = {result:.4f} nats")


    # save df to csv
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
    file = f"/Users/simongimmini/Documents/Studium/CSYS5030 Information Theory/assignments/3/pedestrians/data/adjusted_years/hourly_adjusted_per_year_11-to-21.csv"

    # Start the JVM (add the "-Xmx" option with say 1024M if you get crashes due to not enough memory space)
    startJVM(getDefaultJVMPath(), "-ea", "-Djava.class.path=" + jarLocation)

    # active_information_storage_calculation(file_root, outfile_name="daily_AIS.csv", verbose=False)

    mutal_information_calculation(file, outfile_name="years_hourly_MI_19-21.csv", verbose=False, stat_signif=True)

    # transfer_entropy_calculation(file_root, outfile_name=f"{interval}_TE_TL5_dyncorr55.csv", verbose=False, stat_signif=False)


if __name__ == '__main__':
    main()