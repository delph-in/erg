"""
backof(f,a) | smaller(f,a) 


('or', ('backof', 'f', 'a'), ('smaller', 'f', 'a'))

 [ LTOP: h1
   INDEX: e3
   RELS: < [ _or_c_rel LBL: h2 ARG0: e3 L-INDEX: e4 R-INDEX: e5 ]
           [ named_rel LBL: h8 ARG0: x6 CARG: "F" ]
           [ named_rel LBL: h9 ARG0: x7 CARG: "A" ]
           [ "backof" LBL: h3 ARG0: e4 ARG1: x6 ARG2: x7 ]
           [ "smaller" LBL: h12 ARG0: e5 ARG1: x6 ARG2: x7 ] >
   ]

[ _or_c_rel LBL: h1 ARG0: e1 L-INDEX: e2 R-INDEX: e3 ]
[ named_rel LBL: h2 ARG0: x1 CARG: "F" ]
[ named_rel LBL: h3 ARG0: x2 CARG: "A" ]
[ "backof" LBL: h4 ARG0: e2 ARG1: x1 ARG2: x2 ]
[ "smaller" LBL: h5 ARG0: e3 ARG1: x1 ARG2: x2 ]

"""

connectives = {
	'|':'or',
	'&':'and',
	'%':'iff',
	'$':'if',
	'~':'not',
	'-':'not',

	'or':'or',
	'and':'and',
	'iff':'iff',
	'if':'if',
	'not':'not'
	# extend...
}

def isPred(x):
	if type(x) == tuple:
		if type(x[1]) == str:
			return True
	return False


def isConn(x):
	return not isPred(x) #hack
	# return False

def strForMPred(mPred):
	res = '[ "' + mPred['_rel'] + '" LBL: ' + mPred['LBL'] + ' ARG0: ' + mPred['ARG0'] + ' '
	if 'CARG' in mPred:
		# named_rel
		res += 'CARG: "' + mPred['CARG'] + '"'
	elif 'L-INDEX' in mPred:
		# connective
		res += 'L-INDEX: ' + mPred['L-INDEX'] + ' R-INDEX: ' + mPred['R-INDEX']
		
	elif mPred['_rel'] == 'not':
		# negation - the unary connective
		res += 'ARG1: ' + mPred['ARG1']
	else:
		# some other predicate
		for i in range(len(mPred) - 3):
			argName = 'ARG' + str(i + 1)
			res += argName + ': ' + mPred[argName] + ' '

	if res[-1] == ' ': #hack
		res = res[0:-1]
	res += ' ]'
	return res


mPreds = []
hCounter = 1
eCounter = 1
xCounter = 1

def reset():
	global mPreds
	global hCounter
	global eCounter
	global xCounter
	mPreds = []
	hCounter = 2
	eCounter = 1
	xCounter = 1

# def fTup2mPred(fTup, mPreds = [], hCounter = 1, eCounter = 1, xCounter = 1): # add mPreds to set and return reference to the current object
def fTup2mPred(fTup): # add mPreds to set and return reference to the current object
	global mPreds
	global hCounter
	global eCounter
	global xCounter

	if isConn(fTup): # needs to be defined
		if len(fTup) == 2: # unary (negation)
			"""
			Actual [ neg_rel LBL: h10 ARG1: h3 ]
			"""
			# but instead ;-), let's pretend we want 
			# [_neg_c_rel LBL: h1 ARG0: e1 INDEX: e2 ]
			ref = 'e' + str(eCounter)
			label = 'h' + str(hCounter)
			eCounter += 1
			hCounter += 1
			#  
			i_ref = fTup2mPred(fTup[1])
			#                                                                                        vvvvvv
			mPreds.append({'_rel':connectives[fTup[0]], 'LBL':label, 'ARG0':ref, 'ARG1':i_ref})
			#######
			# return i_ref #ref
			return ref
		elif len(fTup) == 3: # binary
			conn = fTup[0]
			# [ _or_c_rel LBL: h1 ARG0: e1 L-INDEX: e2 R-INDEX: e3 ]
			ref = 'e' + str(eCounter)
			label = 'h' + str(hCounter)
			eCounter += 1
			hCounter += 1
			l_ref = fTup2mPred(fTup[1])
			r_ref = fTup2mPred(fTup[2])
			#                                                                               vvvvv            vvvvv
			mPreds.append({'_rel':connectives[fTup[0]], 'LBL':label, 'ARG0':ref, 'L-INDEX':l_ref, 'R-INDEX':r_ref})
			"""
			if conn in ('if', '$'):
				return r_ref
			elif conn in ('iff', '%'):
				return l_ref
			else: # and, or
				print "and/or"
				return ref
			"""
			return ref
	elif isPred(fTup):
		pred = fTup[0]
		args = fTup[1:]
		ref = 'e' + str(eCounter)
		xArgs = []
		for arg in args:
			ARG = arg.upper()
			xArg = 'x' + str(xCounter)
			mPreds.append({'_rel':'name', 'LBL':('h' + str(hCounter)), 'ARG0':xArg, 'CARG':ARG})
			xArgs.append(xArg)
			hCounter += 1
			xCounter += 1
		
		mPred = {'_rel':pred, 'LBL':('h' + str(hCounter)), 'ARG0':ref}

		for i in range(len(args)):
			mPred['ARG' + str(i + 1)] = xArgs[i]

		mPreds.append(mPred)

		### decomp better:
		hCounter += 1
		
		eCounter += 1

		return ref

sample = ('|', ('dodec', 'd'), ('~', ('&', ('cube', 'a'), ('larger', 'f', 'a'))))


def printDict(d):
	for key in d:
		print key, ":", d[key]

def index(mpred):
	global indexedMPreds
	conn = mpred['_rel']
	if conn == "if":
		return index(indexedMPreds[mpred['R-INDEX']])
	elif conn == "iff":
		return index(indexedMPreds[mpred['L-INDEX']])
	elif conn == "not":
		return index(indexedMPreds[mpred['ARG1']])
	else: # and, or, $pred
		return mpred['ARG0']

indexedMPreds = {}

def prettyUMRSForTuple(t):
	global indexedMPreds
	reset()
	ref = fTup2mPred(t)
	indexedMPreds = {mp.get('ARG0'):mp for mp in mPreds}
	topIndex = index(mPreds[-1])

	fres = """[ LTOP: h1
  INDEX: """+topIndex+"""
  RELS: < """ + strForMPred(mPreds[0])
	for mPred in mPreds[1:]:
		fres += '\n          ' + strForMPred(mPred)

	fres += ' > ]'

	return fres

if __name__ == '__main__':
	print prettyUMRSForTuple(sample)


"""
 line 62:
		res += 'ARG1: ' + mPred['INDEX']
  line 112:
		mPreds.append({'_rel':connectives[fTup[0]], 'LBL':label, 'ARG0':ref, 'ARG1':i_ref})
  line 174:
		return top['ARG1']
"""

