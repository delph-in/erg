

ops = ['~', '-', '&', '|', '$', '%']

# takes a string of the form predicate(x,y) and returns a tuple of the form (predicate, x, y)
def pred2t(s):
	if s == "NULL":
		return s
	parts = s[:-1].split('(')
	l = []
	l.append(parts[0])
	l.extend(parts[1].split(','))
	return(tuple(l))

# takes a prefix expression as a list and returns a dictionary containing the tupleized form plus the remainder
def p2t(l):
	if l[0] not in ops:
		return {#'tuple':l[0],
				'tuple':pred2t(l[0]),
				'rest':l[1:]}
	elif l[0] in ('~', '-'):
		next = p2t(l[1:])
		# arg1 = next['tuple'] -- guaranteed to be 'NULL'
		nextnext = p2t(next['rest'])
		arg = nextnext['tuple']
		rest = nextnext['rest']
		return {'tuple':('-', arg), 'rest':rest}
	else:
		op = l[0]
		next = p2t(l[1:])
		arg1 = next['tuple']
		nextnext = p2t(next['rest'])
		arg2 = nextnext['tuple']
		rest = nextnext['rest']
		return {'tuple':(op, arg1, arg2), 'rest':rest}

def tuple_for_polish_expression(pe):
	return p2t(pe)['tuple']

#print p2t(['&', 'a', '|', 'b', 'c'])['tuple'];
#print p2t(['&', 'a', '~', 'NULL', '|', 'c', 'd'])['tuple'];
if __name__ == '__main__':
	print p2t(['|', 'dodec(d)', '~', 'NULL', '&', 'cube(a)', 'larger(f,a)'])['tuple'];
