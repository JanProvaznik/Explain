# Explain (probably in a development dead end)
Group creation automation for [Explaining concepts activity](https://forum.effectivealtruism.org/posts/r8Qv7QHjJyafmiLnp/#Explaining_Concepts__9_30_10_40_). Used as an icebreaker and helping to establish a shared context in a group that wants to have a deep discussion. Intended for creating groups with a ratio of 1 explainer : 2 students in three rounds for ~~15-50~~ 9 people and 8-15 concepts. 

## Documentation

The main predicate is `explain(+InPath,+OutPath,-OutputRows)` which loads csv from *InPath*, dynamically establishes relations based on its content, generates solutions using `rounds9`, and writes the current solution to *OutPath*.

The `rounds9(?Rounds)` predicate generates a list of 3 rounds for 9 people using  *clpfd* library integer constraints. It uses first-fail heuristic in the `labeling([ff],Vs]` predicate and global constraints: `global_cardinality(Vs,Pairs]` and `all_distinct(Vs)`.

Other code is mainly used for converting input into integers and then back to strings for the output.

## Development journal
- March
I was inspired to do this project by hearing a friend complain how long it took him to create reasonable groups manually for the activity described above.

- April
Ambitious project proposal.

- 2022-05-21
Accompanying python script. For transforming form output to desired file. Discovered the `=..` operator for converting functors to lists. Basic input handling into lists of relations.

- 2022-05-28
Generating groups and then selecting groups to form a solution is not viable.
9 People ~1000 groups, 18 People ~20k groups, 36 People ~400k groups, 54 People ~2M groups
I tried using a trick -> divide people into subgroups of 9 people and solve for them separately.
If we generate the rounds naively from groups in the case of 9 people, it's still doing having 1000 choose 3 ~ 166 million options, that's too much. 

- 2022-05-30
`dynamic` predicate is what I needed from the beginning! Checking for membership in relationship list is strange and having relationships be predicates is significantly faster. Now I can use integer constraints with ease.

- 2022-06-03
I tried reducing the search space by constraining in which order the students in one group, and teachers in one round can be. It helped, but still when a solution is impossible the program usually hangs instead of outputting `false`.

- 2022-06-04
The original plan was to have a variable number of students in each group, but that looks like a very bad idea now, that it barely computes any solutions with fixed size groups.

On artificial data (such as running `explain("example9.csv","output.csv",Output).`) it performs barely OK, but for the intended use with many edge cases (like a person that can only explain or only wants to have explained) it's far too slow. I expected that with all the global constraints and first-fail heuristic it would be usable, but now I'm not sure how to save this. In retrospect, I was way too optimistic and should have done more calculations, whether the project is feasible.



## ~~Workflow~~ (does not work):
1. Create a google form in this template: https://docs.google.com/forms/d/e/1FAIpQLSft_PXbcs4GAv8Hp12ibl69OBMkb2hgdeBdxDKwaEFncDUdyw/viewform?usp=sf_link
2. If you use other names for options create your own `translate.json` that has keys as your options for each value: CE = Can explain, HE = Wants to have explained, MHE = Maybe wants to have explained, N = Does not want to explain nor have explained 
3. link it to google sheets
4. download the Gsheet as csv
5. use the `sanitize.py` script to sanitize the input `python3 sanitize.py translate.json /path/to/exported.csv /path/to/sanitized.csv`
6. run `swipl explain.pl` and then inside `swipl` run `explain("input.csv","output.csv",Rows).`
7. Rounds are outputted to the csv file!