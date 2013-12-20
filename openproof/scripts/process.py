

def stringAfterFullReplacement(s, a, b):
	while (s.find(a) >= 0):
		s = s.replace(a, b)
	return s

def rebracketed(s):
	# first replace predicate parens with {}
	res = ''
	i = 0
	while(i < len(s)):
		c = s[i]
		if c != '(':
			res += c
		else:
			if s[i+2] == ')':
				res += '{' + s[i+1] + '}'
				i += 2
			elif s[i+2] == ',' and s[i+4] == ')':
				res += '{' + s[i+1:i+4] + '}'
				i += 4
			elif s[i+2] == ',' and s[i+4] == ',' and s[i+6] == ')':
				res += '{' + s[i+1:i+6] + '}'
				i += 6
			else:
				res += c
		i += 1
	# then replace all remaining parens with []
	res = res.replace('(', '[')
	res = res.replace(')', ']')
	# finally switch {} back to ()
	res = res.replace('{', '(')
	res = res.replace('}', ')')
	return res

def translated(s):
	s = s.replace('--->', '-->')
	s = s.replace('<->', '%')
	s = s.replace('-->', '$')
	return s

def padded(s):
	s = ' ' + s + ' '
	for op in ['|', '&', '$', '%', '~', '-', '[', ']']:
		s = s.replace(op, '  ' + op + '  ')

	return s

def preprocess(s):
	s = translated(s.lower())
	"""
	Start with:
	dodec(d)|~(cube(a)&larger(f, a))

	1. Remove all spaces following commas:
	dodec(d)|~(cube(a)&larger(f,a))
	"""
	s = stringAfterFullReplacement(s, ', ', ',')
	"""
	2. Convert grouping parentheses (but not predicate parentheses) into brackets:
	dodec(d)|~[cube(a)&larger(f,a)]
	"""
	s = rebracketed(s)
	"""
	3. Prefix every negation with a filler NULL
	dodec(d)|NULL~[cube(a)&larger(f,a)]
	"""
	s = s.replace('~', 'NULL ~')
	s = s.replace('-', 'NULL -')
	"""
	4. Pad front, back, and all operators (including brackets) with a space:
	 dodec(d) | NULL ~ [ cube(a) & larger(f,a) ] 
	"""
	s = padded(s)

	return s

def postprocess(s):
	s = padded(s)
	"""
	6. Reduce extra whitespace
	 | dodec(d) ~ NULL & cube(a) larger(f,a)  
	"""
	s = stringAfterFullReplacement(s, '  ',' ')
	"""
	7. Strip padding
	| dodec(d) ~ NULL & cube(a) larger(f,a)
	"""
	s = s.strip()
	"""
	8. Split on space
	['|', 'dodec(d)', '~', 'NULL', '&', 'cube(a)', 'larger(f,a)']
	"""
	return s

if __name__ == '__main__':
	print preprocess('dodec(d)|~(cube(a)&larger(f, a))')