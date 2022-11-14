import re
import os
from os import path as osp


# function to add a "#" to the beginning of the first line of a file and save it
def add_comment_to_file(file_path):
    with open(file_path, 'r+') as f:
        lines = f.readlines()
        # check if any of the first characters of the first line is a "#"
        if not re.match(r'^#', lines[0]):
            # if not, add a "#" to the beginning of the first line
            lines[0] = "# " + lines[0]
            # set the cursor to the beginning of the file
            f.seek(0)
            # write the lines back to the file
            f.writelines(lines)
            return True
        else: 
            return False

# function to read a csv file and return a latex table
def csv_to_latex_table(file_path):
    with open(file_path, 'r') as f:
        lines = f.readlines()
        # create a string that contains the latex table
        table = ""
        for j, line in enumerate(lines):
            # remove the last character of the line (it is a newline character)
            line = line[:-1]
            # split the line into the columns
            columns = line.split(";")
            #print(columns)
            if not j == 0:
                # every column has a maximum length of 4 characters
                for i in range(len(columns)):
                    #pass
                    columns[i] = columns[i][:5]

            # add a row to the table
            table += f"{' & '.join(columns)} \\\\ \\hline \n"
        # print the table
        print(table)
        print(len(columns))
        return table



def main():

    # csv_to_latex_table("/Users/simongimmini/Documents/Studium/CSYS5030 Information Theory/assignments/3/pedestrians/TE_top_10.csv")

    # file path 
    file_path = "/Users/simongimmini/Documents/Studium/CSYS5030 Information Theory/assignments/3/pedestrians/data/"

    # get all files in the directory and its subfolders and add file name to file path if file is a .csv file
    files = [osp.join(dp, f) for dp, dn, fn in os.walk(osp.expanduser(file_path)) for f in fn if f.endswith(".csv")]

    counter = 0
    already_commented = 0

    # iterate over all files in the directory
    for file in files:
        # add a "#" to the beginning of the first line of the file
        if add_comment_to_file(file):
            counter += 1
        else:
            already_commented += 1

    print(f"Added a '#' to the beginning of the first line of {counter} files. {already_commented} files were already commented.")


if __name__ == '__main__':
    main()
