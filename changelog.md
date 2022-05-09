1.3.1.0 2022-05-09
=================
- add FileTypeC,ToFile instances of NonRootRelDir

1.3.0.0 2022-05-08
==================
- add AbsAs class
- factor out NonRootRelDir type
- remove ToAbsDir (use AbsDirAs instead)
- start using Base1T
- add Printable, AsFilePath constraints for convenience to some classes

1.2.2.0 2022-05-02
==================
- add FPath.ToDir

1.2.1.0 2022-04-27
==================
- export FPath.FPath.FPathAs

1.2.0.1 2022-04-06
==================
- upgrade dependencies

1.2.0.0 2021-08-11
==================
- change AppendableFPath to three-param typeclass; add Dir+(RelFile|RelDir)
  instances of AppendableFPath

1.1.5.0 2021-07-15
==================
- remove FPath.IO
- implement liftTyped where needed
- simpler show output for basic types using quasi-quotes
- add NFData instances of FPathError*

1.1.4.2 2021-06-11
==================
- use non-empty-containers 1.4.1.0

1.1.4.1 2021-05-20
==================
- make PathComponent Parseable, AsFilePath*; add HasCallStack to parse*

1.1.4.0 2021-05-15
==================
- add HasCallstack to Errors

1.1.3.0 2021-04-26
==================
- add FileTypeC, RelAs, FileAs instance of RelFile

1.1.2.0 2021-04-01
==================
- add ancestors{,'}, MonoFoldable, MonoFunctor, ToMonoSeq{,NonEmpty} throughout

1.1.1.0 2021-03-26
==================
- add dirname,basename throughout

1.1.0.0 2021-03-03
==================
- _Dir' -> _Dir_
- Begone, IsFile & IsDir classes (and DirLike.hs)

1.0.8.1 2021-02-11
==================
- deprecate `IsFile` in favour of `FileAs`

1.0.8.0 2021-02-10
==================
- add FileAs class

1.0.7.0 2021-02-06
==================
- add FPath.Error.FPathError.fpathIOErrorEither

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
