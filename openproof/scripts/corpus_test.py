#!/env/usr/bin python

import sys

itemDelimiter = '---ITEM---'
fullItemDelimiter = '\n' + itemDelimiter + '\n'

evaluateByResultType = True
showLineItems = True
verbose = False

def lineEnder(tl, punct='.'):
	n = len(tl)
	if n == 0:
		return " no FOL sentences" + punct
	if n == 1:
		return " one FOL sentence" + punct
	else:
		return ' ' + str(n) + " FOL sentences" + punct

def main():
	# test arguments
	if len(sys.argv) not in (4, 5):
		print "Usage: ./corpus_test fols_file goals_file test_file [-v]"
		print "\t [fols_file] should have N desired FOL sentences, one per line"
		print "\t [goals_file] should have N desired English sentences, one per line (corresponding to the lines of the [fols_file]"
		print "\t [test_file] should have clusters of generated sentences (one per line of the [fols_file]), one per line, separated by '" + itemDelimiter + "'."
		print "\t -v is for 'verbose'"
		return

	folsFile = sys.argv[1]
	goalsFile = sys.argv[2]
	testFile = sys.argv[3]
	verbose = (len(sys.argv) > 4 and sys.argv[4] in ('v', '-v'))


	# load fol sentences
	fols = []
	for line in open(folsFile):
		fols.append(line.strip())

	# load goal sentences
	goals = []
	for line in open(goalsFile):
		goals.append(line.strip())

	# load test blocks
	testText = ''
	for line in open(testFile):
		testText += line
	testBlocks = testText.split(fullItemDelimiter)[:-1]

	# test lengths
	if len(fols) != len(goals) or len(goals) != len(testBlocks):
		print "Something's wrong: there should be one NL goal sentence and one generated block for every FOL sentence."
		print '\tFOLs count:  ' + str(len(fols))
		print '\tGoals count: ' + str(len(goals))
		print '\tTests count: ' + str(len(testBlocks))
		return


	# evaluate
	passed = []
	failures = []
	goalNotGenerated = []

	for i in range(len(goals)):
		folSentence = fols[i]
		goal = goals[i]
		results = testBlocks[i].split('\n')
		if len(results) == 0:
			failures.append(folSentence)
		found = goals[i] in results
		item = str(i+1) + ". " + folSentence + " > " + goal
		if found:
			print "  " + item
			passed.append((folSentence, goal))
		else:
			print "! " + item
			blob = (folSentence, goal, ("\t'" + "'\n\t'".join(results) + "'"))
			goalNotGenerated.append(blob)

	print ''
	print "==================="
	print "PUNCHLINE"
	print "==================="
	passedCount = len(passed)
	failedCount = len(failures)
	gngCount = len(goalNotGenerated)
	totalCount = passedCount + failedCount + gngCount
	print str(passedCount) + ' / ' + str(totalCount) + ' tests passed.'

	if verbose:
		print '' 
		print "==================="
		print "SUMMARY"
		print "==================="
		print "*** Successfully generated" + lineEnder(passed)
		if len(failures) > 0:
			print "!!! Failed to generate anything for" + lineEnder(failures)
		print "!!! Failed to generate the expected NL sentence for" + lineEnder(goalNotGenerated)
		print ''
		print "==================="
		print "LINE ITEMS"
		print "==================="
		print "*** Successfully generated" + lineEnder(passed, ':')
		print '  ' + '\n  '.join([pT[0] + " > '" + pT[1] + "'" for pT in passed])
		if len(failures) > 0:
			print '!!! Failed to generate anything for' + lineEnder(failures, ':')
			print '  ' + '\n  '.join(failures)
		print '!!! Did not generate the expected NL sentence for' + lineEnder(goalNotGenerated, ':')
		for gngTuple in goalNotGenerated:
			print '  expected: ' + gngTuple[0] + " > '" + gngTuple[1] + "'"
			print '  ...instead found:\n' + gngTuple[2]



if __name__ == '__main__':
	main()






