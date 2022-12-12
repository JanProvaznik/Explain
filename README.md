# Explain
Group creation automation for [Explaining concepts activity](https://forum.effectivealtruism.org/posts/r8Qv7QHjJyafmiLnp/#Explaining_Concepts__9_30_10_40_). Used as an icebreaker and helping to establish a shared context in a group that wants to have a deep discussion. Intended for creating groups with a ratio of 1 explainer : 2 students in N rounds (tested for 2-5) for K (multiple of 3, best between 9-27) people and 8-15 concepts.

## Note
If you are not familiar with Prolog or you need more flexibility in the group creation, you can use [**this script**](https://gist.github.com/spirali/4e8cba87dcbc61e9fe7b4f08b3d1e03e)  instead.

## Prerequisites:
- install Python
- install [SWI Prolog](https://www.swi-prolog.org/)

## Workflow:
1. Create a google form in this template: https://docs.google.com/forms/d/e/1FAIpQLSft_PXbcs4GAv8Hp12ibl69OBMkb2hgdeBdxDKwaEFncDUdyw/viewform?usp=sf_link
2. If you use other names for options create your own `translate.json` that has keys as your options for each value: CE = Can explain, HE = Wants to have explained, MHE = Maybe wants to have explained, N = Does not want to explain nor have explained 
3. link it to google sheets
4. download the Gsheet as csv
5. use the `sanitize.py` script to sanitize the input `python3 sanitize.py translate.json /path/to/exported.csv /path/to/sanitized.csv`
6. partition to subgroups whose counts of people are divisible by three and put them into their own .csv files, you can add fake people who want to have explained everything if the subgroups don't add up.
7. run `swipl explain.pl` and then inside `swipl` run `explain("input.csv","output.csv",NumberOfRounds,Rows).`
8. Rounds are outputted to the output.csv file! If you don't like the current output, you can explore more options with `;`.

## Documentation

The main predicates are `explain(+InPath,+OutPath,+NumberOfRounds,-Output)`  which loads csv from *InPath*, dynamically establishes relations based on its contents, generates solutions using `roundsN`, and writes the current solution to *OutPath*.

The `roundsN(+N,+K,+CE,+WE,?Rounds)` predicate generates a list of *N* rounds for *K* groups of three using the *clpfd* library integer constraints. It uses first-fail heuristic in the `labeling([ff],Vs]` predicate and global constraints: `all_distinct(Vs)` and `tuples_in`. The other roundsN work analogically.

Other code is mainly used for converting input into integers and then back to strings for the output.

The program supports all numbers of people, that are divisible by 3, but works best between 9-27.

## Development journal
- March
I was inspired to do this project by hearing a friend complain how long it took him to create reasonable groups manually for the activity described above.

- April
Ambitious project proposal.

- 2022-05-21
Accompanying python script. For transforming form output to desired file. Discovered the `=..` operator for converting functors to lists. Basic input handling into lists of relations.

- ~2022-05-28
Generating groups and then selecting groups to form a solution is not viable.
9 People ~1000 groups, 18 People ~20k groups, 36 People ~400k groups, 54 People ~2M groups
I tried using a trick -> divide people into subgroups of 9 people and solve for them separately.
If we generate the rounds naively from groups in the case of 9 people, it's still doing having 1000 choose 3 ~ 166 million options, that's too much. 

- 2022-05-30
`:-dynamic` meta-predicate is exaclty what I needed from the beginning! Checking for membership in relationship list is strange and having relationships represented as predicates is significantly faster. Now I can use integer constraints with ease.

- 2022-06-03
I tried reducing the search space by constraining in which order the students in one group, and teachers in one round can be. It helped, but still when a solution is impossible the program usually hangs instead of outputting `false`.

- 2022-06-04
The original plan was to have a variable number of students in each group, but that looks like a very bad idea now, that it barely computes any solutions with fixed size groups.

On artificial data (such as running `explain("example9.csv","output.csv",Output).`) it performs barely OK, but for the intended use with many edge cases (like a person that can only explain or only wants to have explained) it's far too slow. I expected that with all the global constraints and first-fail heuristic it would be usable, but now I'm not sure how to save this. In retrospect, I was way too optimistic and should have done more calculations, whether the project is feasible.

- 2022-06-05 AM
My professor recreated the main functionality in a small fraction of time that I've spent on this and it worked better, now I'll replicate what he'd done and try to squeeze more out of this. `tuples_in` is what is important.

- 2022-06-05 PM
The performance boost is actually quite impressive, and I was able to make it general enough that it's not that painful replicating making a 15/27 person version of the `rounds` predicate, still it's not DRY in the least.
Now it works even for 15 people, this is cool! 27 seems like too much, it sometimes works and sometimes not. But for the real data I was able to generate reasonable groups, so this is a successs.

- 2022-06-09
Added scoring how many people had explained the thing they really wanted to have explained.

- 2022-06-10 AM
Generalized roundsN so that we don't have to select which one to use.
- 2022-06-10 PM
Refactor to be more DRY, now the program can generate any number of rounds (tested betwen 2-5).
