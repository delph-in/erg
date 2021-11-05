# English Resource Grammar

The English Resource Grammar is a broad-coverage HPSG grammar of English.

This project is currently only for [issue-tracking](https://github.com/delph-in/erg/issues).

Please see the [ERG wiki](https://github.com/delph-in/docs/wiki/ErgTop) for more information.

## Getting the grammar

The ERG grammar files can be obtained via SVN:

```bash
$ svn checkout http://svn.delph-in.net/erg/trunk
```

It is also available as part of the [LOGON distribution](http://moin.delph-in.net/LogonTop).

The quickest way to get started using the ERG is with the
pre-compiled binary 
[ACE](http://sweaglesw.org/linguistics/ace/) parser. There are also precompiled grammars available there. If you want more recent versions of the grammar to use with ACE, you can create a grammar file by checking out a specific ERG release. ERG releases are tagged in the [SVN repository](http://svn.delph-in.net/erg/tags/). Choose the version you want and check it out using the tag name like this:
```
svn checkout http://svn.delph-in.net/erg/tags/tag_name
cd trunk/ace
ace -G grammar.dat -g ./config.tdl
```
Then, the grammar.dat file can be used with the 

## References

* Dan Flickinger (2011) *Accuracy vs. Robustness in Grammar Engineering*.
  in E.M. Bender and J.E. Arnold, eds.
  Language from a Cognitive Perspective: Grammar, Usage, and Processing, pp. 31--50. CSLI Publications, Stanford.
* Ann Copestake and Dan Flickinger (2000)
  [*An open-source grammar development environment and broad-coverage English grammar using HPSG*](http://www.cl.cam.ac.uk/~aac10/papers/lrec2000.pdf)
  In Proceedings of the Second conference on Language Resources and Evaluation (LREC-2000), Athens, Greece.
