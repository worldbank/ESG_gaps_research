
"""
coverage.py produces quick density coverage stats for an indicator

Usage:
  coverage.py [--verbose] [--start YEAR] [--income INC] [--region RGN] INDICATOR...

Options:
  --verbose, -v       detailed output
  --start, -s YEAR    start year [default: 2010]
  --income INC        only this income group (~ to exclude)
  --region RGN        only this region (~ to exclude)

INDICATOR can be in the form CETS or SOURCE:CETS. If omitted, SOURCE
defaults to 2

"""

import requests
import datetime
import sys
import csv
import copy
from docopt import docopt

config = docopt(__doc__)
minYear = int(config['--start'])
maxYear = datetime.datetime.now().year
actualMaxYear = None

incomeFlag = regionFlag = True
if config['--income'] and config['--income'][0] == '~':
    config['--income'] = config['--income'][1:]
    incomeFlag = False

if config['--region'] and config['--region'][0] == '~':
    config['--region'] = config['--region'][1:]
    regionFlag = False

# sanity checks
if len(config['INDICATOR']) > 1:
    config['--verbose'] = False

# start by fetching the country list
url = 'https://api.worldbank.org/v2/en/country?format=json&per_page=20000'
response = requests.get(url)
data = response.json()
data = data[1]
_countries = {}
for elem in data:
    if config['--income'] and (elem['incomeLevel']['id'] == config['--income']) != incomeFlag:
        continue
        
    if config['--region'] and (elem['region']['id'] == config['--region']) != incomeFlag:
        continue
        
    if elem['region']['id'] != 'NA' and elem['id'] != 'TWN':
        _countries[elem['id']] = [0] * (maxYear-minYear+1)

writer = csv.writer(sys.stdout, quoting=csv.QUOTE_MINIMAL)
writer.writerow(['CETS', 'NAME', 'MAX_YEAR', 'COUNT', 'MIN', 'MAX', 'AVERAGE'])

for id in config['INDICATOR']:
    minYear = int(config['--start'])
    maxYear = datetime.datetime.now().year
    actualMaxYear = None
    countries = copy.deepcopy(_countries)

    parts = id.split(':')
    if len(parts) > 1:
        (src,cets) = (parts[0],parts[1])
    else:
        (src,cets) = (2, parts[0])

    url = 'https://api.worldbank.org/v2/en/country/all/indicator/{}?source={}&format=json&per_page=20000&date={}:{}'.format(cets, src, minYear, maxYear)

    response = requests.get(url)
    data = response.json()

    data = data[1]
    for elem in data:
        cets_name = elem['indicator']['value']
        date = int(elem['date'])
        iso3 = elem['countryiso3code']
        actualMaxYear = date if actualMaxYear is None else max(actualMaxYear, date)
        if countries.get(iso3) and date >= minYear and date <= maxYear and elem['value'] is not None:
            offset = date - minYear
            countries[iso3][offset] = 1

    allCoverage = []
    for k,elem in countries.iteritems():
        coverage = sum(elem) * 100 / (actualMaxYear-minYear+1)
        allCoverage.append(coverage)


    if len(allCoverage) > 0:
        writer.writerow([cets, cets_name, actualMaxYear, len(countries),
            int(round(min(allCoverage))),
            int(round(max(allCoverage))),
            int(round(sum(allCoverage)/len(allCoverage)))
            ])

    if config['--verbose']:
        for k,elem in countries.iteritems():
            coverage = sum(elem) * 100 / (actualMaxYear-minYear+1)
            writer.writerow([k, coverage])
