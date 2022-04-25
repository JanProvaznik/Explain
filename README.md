# Explain
Group creation automation for [Explaining concepts activity](https://forum.effectivealtruism.org/posts/r8Qv7QHjJyafmiLnp/#Explaining_Concepts__9_30_10_40_). Used as an icebreaker and helping to establish a shared context in a group that wants to have a deep discussion. Intended for creating groups with a ratio of 1 explainer : 2 students in three rounds for 15-50 people and 8-15 concepts. 

### Workflow (not working yet, might change so that everything is done by prolog):
1. Create a google form in this template: https://docs.google.com/forms/d/e/1FAIpQLSft_PXbcs4GAv8Hp12ibl69OBMkb2hgdeBdxDKwaEFncDUdyw/viewform?usp=sf_link
2. If you use other names for options create your own `translate.json` that has keys as your options for each value: CE = Can explain, HE = Wants to have explained, MHE = Maybe wants to have explained, N = Does not want to explain nor have explained 
3. link it to google sheets
4. download the Gsheet as csv
5. use the explain python script to sanitize the input `python3 sanitize.py translate.json /path/to/exported.csv /path/to/sanitized.csv`
6. run `explain.pl`
7. Groups are created in a new csv file!

## Documentation
