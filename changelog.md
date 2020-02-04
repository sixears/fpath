1.0.4.1 2020-02-04
==================
- use (~~) from MonadError.IO.Error; clean up unimplemented pResolveDirLenient
  instances

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
