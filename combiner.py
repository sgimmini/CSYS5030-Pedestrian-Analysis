import os 


# combine all *.R and *.py files in the current directory in one .txt file 
def main():
    # get all files in the current directory with the extension .R and .py
    files = [f for f in os.listdir('.') if f.endswith('.R') or f.endswith('.py')]
    # open the output file
    with open('output.txt', 'w') as out:
        # loop over all files
        for file in files:
            # check if the file is a .R or .py file
            if file.endswith('.R') or file.endswith('.py'):
                # open the file
                with open(file, 'r') as f:
                    # write the file name
                    out.write('\n' + file + '\n')
                    # write the file contents
                    out.write(f.read())



if __name__ == '__main__':
    main()