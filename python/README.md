
* **coverage.py**: computes data availability for a specified set of WBG indicators. This has evolved
  over time to provide different types of analysis based on what can be ascertained from the API.

* **indicators.txt**: list of indicators under study, i.e., input to the coverage script

### Examples ###

```python coverage.py \`cat indicators.txt\` ```

```python coverage.py \`cat indicators.txt\` > indicators.csv```

```python -u coverage.py `cat indicators.txt` | tee indicators.csv```
