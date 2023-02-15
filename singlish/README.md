# SinglishERG

A branch of the English Resource Grammar (ERG) that is used for Singlish. 
Created as a Master's Research project. It is published under the same license 
as the ERG, the MIT license.

The singlish subdirectory contains the files that have been added or in the
case of parse-nodes.tdl, cloned from the trunk and changed.
Refer to ace/config-singlish.tdl and singlish.tdl to see which files are in use.

To compile this grammar using LKB most conveniently:
* Under Lkb Top's Advanced menu, select 'Evaluate Lisp expression' and type in
  `(push :singlish \*features\*)`
* Load the ERG as usual, via the Load--Complete Grammar on the LKB Top menu.

To compile this grammar using ace:
* In the ERG directory
$ ace -G singlish.dat -g ace/config-singlish.tdl

To check the semantics:

```
$ echo "sentence" | ace -g [grammar].dat -Tfq
```

And to generate in another grammar:

```
echo "sentence" | ace -g [grammar1].dat -Tfq | ace -g [grammar2].dat -e
```
To generate from MRS:

```
cat [mrs] | ace -g [grammar2].dat -e
```
* to change the generation root, add `-r [root]` to the command
* and add `--disable-subsumption-test` for easier generation

### Data
Data was extracted from examples on Wiktionary pages with words that were marked
to be Singlish. It includes also the other non-Singlish definitions and usages
of the words. The example sentences include some that are offensive and racist 
but they not taken out as it reflects how this variety is used.

To parse the data using ace (parts in brackets are optional)
* To parse with only top tree:
$ cat singlish/data/wikidata/wikiexamples_first300.txt | ace -g singlish.dat -Tf1 > /tmp/output.txt

* To remove lines starting with "#": 
`grep -vP "^#" wikiexamples_first300.txt | ace...`

*lexicon_goldtrees.tdl* contains the words added to the standard English lexicon
 when parsing the 30 Singlish sentences from *skeletons/treebankset*. 
