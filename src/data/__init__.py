import os
import urllib.request


# Data sources
wpPoliceShootings = "https://raw.githubusercontent.com/washingtonpost/data-police-shootings/" +\
                    "master/fatal-police-shootings-data.csv"
pgPoliceShootings2015 = "https://s3.amazonaws.com/postgraphics/policeshootings/" +\
                        "policeshootings2015.json"
pgPoliceShootings2016 = "https://s3.amazonaws.com/postgraphics/policeshootings/" +\
                        "policeshootings2016.json"


def saveData(uri, out_fn):
    """
    Grab the datasets from the internet and save them to the data
    directory.
    """
    data = (urllib.request.urlopen(uri)
            .read()
            .decode('utf-8')
            .replace('\r', '')
            .split('\n'))
    with open(out_fn, 'w+') as fp:
        for d in data:
            fp.write(d + '\n')

if __name__ == '__main__':
    if (not os.path.exists('data')):
        os.mkdir('data')

    saveData(wpPoliceShootings, 'data/wp-police-shootings.csv')
    saveData(pgPoliceShootings2015, 'data/pg-police-shootings-2015.json')
    saveData(pgPoliceShootings2016, 'data/pg-police-shootings-2016.json')
