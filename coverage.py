#!/usr/bin/python

"""
coverage.py produces quick density coverage stats for an indicator

Usage:
  coverage.py [--verbose] [--start YEAR] [--income INC] [--region RGN] [--since YEARS] INDICATOR...

Options:
  --verbose, -v       detailed output
  --start, -s YEAR    start year [default: 2010]
  --since YEARS       comma-separated list of years for coverage analysis [default: 2010,2013,2015,2017]
  --income INC        only this income group (~ to exclude)
  --region RGN        only this region (~ to exclude)

INDICATOR can be in the form CETS or SOURCE:CETS. If omitted, SOURCE defaults to 2

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

yearKeys = [int(i) for i in config['--since'].split(',')]
_yearBreaks = {}
for i in yearKeys:
    _yearBreaks[i] = 0
    
  
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
output = ['CETS', 'NAME', 'MINMRV', 'MAXMRV', 'COUNTRIES', 'TOTAL_COUNTRIES', 'MIN', 'MAX', 'AVERAGE']
for i in yearKeys:
    output.append('SINCE{}'.format(i))

writer.writerow(output)

for id in config['INDICATOR']:
    minYear = int(config['--start'])
    maxYear = datetime.datetime.now().year
    actualMaxYear = None
    minMRV = None
    countries = copy.deepcopy(_countries)
    yearBreaks = copy.deepcopy(_yearBreaks)

    parts = id.split(':')
    if len(parts) > 1:
        (src,cets) = (parts[0],parts[1])
    else:
        (src,cets) = (2, parts[0])

    url = 'https://api.worldbank.org/v2/en/country/all/indicator/{}?source={}&format=json&per_page=20000&date={}:{}'.format(cets, src, minYear, maxYear)

    response = requests.get(url)
    data = response.json()

    if len(data) < 2:
        print ""
        continue

    data = data[1]
    for elem in data:
        cets_name = elem['indicator']['value']
        date = int(elem['date'])
        iso3 = elem['countryiso3code']
        if countries.get(iso3) and date >= minYear and date <= maxYear and elem['value'] is not None:
            actualMaxYear = date if actualMaxYear is None else max(actualMaxYear, date)
            offset = date - minYear
            countries[iso3][offset] = date

    allCoverage = []
    countriesWithData = 0
    if actualMaxYear:
        for k,elem in countries.iteritems():
            coverage = sum([1 if i else 0 for i in elem]) * 100 / (actualMaxYear-minYear+1)
            if sum(elem) > 0:
                countriesWithData += 1
                minMRV = min(minMRV,max(elem)) if minMRV else max(elem)
                for i in yearKeys:
                    if len([x for x in elem if x >= i]) > 0:
                        yearBreaks[i] += 1

            allCoverage.append(coverage)


    if len(allCoverage) > 0:
        output = [cets, cets_name, minMRV, actualMaxYear, countriesWithData, len(countries),
            int(round(min(allCoverage))),
            int(round(max(allCoverage))),
            int(round(sum(allCoverage)/len(allCoverage)))
            ]

        for i in yearKeys:
            output.append(yearBreaks[i])

        writer.writerow(output)
    else:
        writer.writerow([cets, cets_name])

    if config['--verbose']:
        for k,elem in countries.iteritems():
            coverage = sum([1 if i else 0 for i in elem]) * 100 / (actualMaxYear-minYear+1)
            if sum(elem) > 0:
                minMRV = min([i for i in elem if i > 0])
                maxMRV = max(elem)
            else:
                minMRV = maxMRV = ''

            writer.writerow([k, minMRV, maxMRV, coverage])
