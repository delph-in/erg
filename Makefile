flop:
	flop -t english
	flop -t speech

tar:
	tar zpScvf /tmp/erg.tgz \
	  --exclude "*/.svn*" --exclude "./tsdb*" --exclude "*.grm" \
	  --exclude "*.mem" --exclude "*~" --exclude "*.dat" .

titan:
	rsync -va *.tdl lkb pet tmr --exclude "*.svn*" --exclude "*~" \
	  login1.titan.uio.no:src/logon/lingo/terg;
