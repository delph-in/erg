from process import preprocess, postprocess
from infix_to_prefix import infix_to_prefix
from list2tuple import tuple_for_polish_expression
from fol2umrs import prettyUMRSForTuple
import sys


# takes an infix LPL string like 'dodec(d)|~(cube(a)&larger(f, a))'
# returns an underspecified MRS
def e2e(s):
	# make the input space-delimited in prefix notation
	ir = postprocess(infix_to_prefix(preprocess(s)))
	# split on space and turn into nested tuples
	tup = tuple_for_polish_expression(ir.split(' '))
	# convert to MRS and return
	return prettyUMRSForTuple(tup)

if __name__ == '__main__':
	if len(sys.argv) != 2:
		print "Usage: run with a single argument in the form of an LPL FOL string. Example:"
		print "(-cube(b)&large(c))-->((between(d, e, f)<->larger(b,c))|cube(a))"
	else:
		print e2e(sys.argv[1])