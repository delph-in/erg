for line in set(open('../irregs.tab')) - set(open('dropirregs.tab')):
    line = line.strip()
    print line

