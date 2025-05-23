flop:
	flop -t english
	flop -t speech

tar:
	tar zpScvf /tmp/erg.tgz \
	  --exclude "*/.svn*" --exclude "./tsdb*" --exclude "*.grm" \
	  --exclude "*/open*" --exclude "./www*" --exclude "./agree" \
	  --exclude "*/ut*" --exclude "./ldc*" --exclude "./dict*" \
	  --exclude "*/lexdb*" --exclude "./robot*" --exclude "./mrs*" \
	  --exclude "*/Notes*" --exclude "./Tmplex*" --exclude "./essay*" \
	  --exclude "*/Todo*" --exclude "./fontconfig" \
	  --exclude "./singlish*" \
	  --exclude "jhpstg.g.mem" --exclude "*~" --exclude "*.dat" .

titan:
	rsync -va *.tdl lkb pet tmr --exclude "*.svn*" --exclude "*~" \
	  login1.titan.uio.no:src/logon/lingo/terg;
