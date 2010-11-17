titan:
	rsync -va *.tdl lkb pet tmr --exclude "*.svn*" --exclude "*~" \
	  login1.titan.uio.no:src/logon/lingo/terg;
