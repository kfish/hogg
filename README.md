HOgg - Ogg encapsulation stuffz
===============================
[![Build Status](https://travis-ci.org/tmcgilchrist/hogg.svg)](https://travis-ci.org/tmcgilchrist/hogg)

**THIS PACKAGE IS ORPHNED, I MADE SOME FIXES TO COMPILE IT BUT IT WONT BE DEVELOPED FURTHER**

Requirements
------------

-	Cabal (tested with various version from 1.1.3 to 1.6.0)

-	Data.ByteString, included with GHC >= 6.6, and available separately for GHC 6.4 at: http://www.cse.unsw.edu.au/~dons/fps.html (version 0.7 or greater)

-	If building on Debian GNU/Linux

	```
	$ apt-get install ghc6 libghc6-mtl-dev libghc6-hunit-dev
	$ apt-get install libghc6-cabal-dev # not needed for Debian unstable
	```

### Optional

-	HTTP1 with lazy bytestring support, from: http://www.dtek.chalmers.se/~tox/site/http.php4 To configure with HTTP support, first install the above library, then go install through Cabal, but run configure like

	```
	$ runhaskell Setup configure --flags="http"
	```

Building
--------

This package is configured by default to build with GHC 6.8.

If you are building with GHC 6.6, you need to follow the usual Cabal setup but run configure like

```
    $ runhaskell Setup configure --flags="-splitBase"
```

If you are building with GHC 6.4, you will need to edit hogg.cabal by adding 'fps' to the Build-Depends line and probably remove the Cabal conditionals. You should consider upgrading to a more recent GHC.

Once configured, the following procedure will build hogg:

```
    $ chmod +x Setup.hs
    $ ./Setup.hs configure
    $ ./Setup.hs build
    $ ./Setup.hs install
```

Usage
-----

-	General help (lists subcommands)

	```
	hogg help
	```

-	Help on a subcommand

	```
	hogg help <subcommand>
	```

-	Info about the various tracks of one or more files

	```
	hogg info file1.ogg file2.ogg ...
	```

-	Dump packets of an Ogg file

	```
	hogg dump file1.ogg file2.ogg ...
	```

-	Dump pages of an Ogg file

	```
	hogg pagedump file1.ogg file2.ogg ...
	```

-	Dump vorbis (theora, speex) packets

	```
	hogg dump -c vorbis file.ogg ...
	hogg pagedump -c vorbis file.ogg ...
	hogg dumpraw -c vorbis file.ogg ...
	```

-	Chop out part of a file

	```
	hogg chop --start 01:10 --end 2:30 -o output.ogg file.ogg
	```

-	Merge pages from many input files and produce a single output file

	```
	hogg merge -o output.ogg file1.ogg file2.ogg file3.ogg ...
	```

Checking stuff works
--------------------

-	Rewrite a file (parse pages, rewrite page data)

	```
	hogg rip file.ogg > newfile.ogg
	diff file.ogg newfile.ogg
	```

-	Repacket a file (parse to packets, rewrite with original segmentation)

	```
	hogg reconstruct file.ogg > newfile.ogg
	diff file.ogg newfile.ogg
	```
