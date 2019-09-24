
* **coverage.py**: computes data availability for a specified set of WBG indicators. This has evolved
  over time to provide different types of analysis based on what can be ascertained from the API.

* **indicators.txt**: list of indicators under study, i.e., input to the coverage script

### Examples ###

    # single indicator (assumes WDI)
    python coverage.py EN.ATM.CO2E.KT

    # single indicator from another database
    python coverage.py 3:GE.EST

    # analyze all indicators in the current study set
    python coverage.py `cat indicators.txt`

    # same but save to a file
    python coverage.py `cat indicators.txt` > indicators.csv

    # tee output to file and stdout
    python -u coverage.py `cat indicators.txt` | tee indicators.csv

