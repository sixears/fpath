1.0.6.0 2020-09-16
==================
- add AsDir, AsFile, AsFilePath', DirAs, FPathAs, IsDir classes

1.0.5.3 2020-09-16
==================
- use tasty-plus 1.4.0.0

1.0.5.2 2020-05-13
==================
- use ≟ from tasty-plus rather than more-unicode
- use Text for paths rather than strings, for avoidance of UTF8 issues
- update for GenValidity 0.5+

1.0.5.1 2020-02-15
==================
- use tasty-plus 1.3.0.0

1.0.5.0 2020-02-10
==================
- use (~~) from MonadError.IO.Error; clean up unimplemented pResolveDirLenient
  instances
- use quasiquoting library; improve quasiquoters to error at compile-time
- migrate most tests from FPath.T.FPath.AbsDir to FPath.AbsDir
- add `parents`
- add instance AsFilePath FilePath
- export FPath constructors (for pattern matching)
- pResolve{,Dir} now handles dirs with a resolvable, extant prefix (can cd
  into), and a parseable suffix.

1.0.4.0 2020-02-03
==================
- remove redundant patterns (thanks to COMPLETE markers in SeqNE) on AbsDir
- _inDir (in FPath.IO) now forcibly attaches the path in use
- add pResolve{,Dir}Lenient

1.0.3.1 2019-12-14
==================
- use non-empty-containers-1.2.0.0

1.0.3.0 2019-11-04
==================
- add ⊙ to PathComponent export

1.0.2.0 2019-10-14
==================
- export _FPIO_IO_ERROR, _FPIO_PATH_ERROR from FPathError

1.0.1.0 2019-10-05
==================
- add Printable,Textual,AsFilePath instances for File,Dir,Abs,Rel,FPath

1.0.0.1 2019-09-25
==================
- change cabal requirements to use >= to allow for build updates

1.0.0.0 2019-09-22
==================
- first release
