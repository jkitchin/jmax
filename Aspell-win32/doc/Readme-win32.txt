Readme-win32.txt for GNU ASpell-0.50 (currently 0.50.3)
Author: Thorsten Maerz <torte@netztorte.de>


Index
========
	About this package
	Installing GNU ASpell-0.50
	Differences to the *nix version
	Creating dictionaries
	Compiling GNU Aspell using MinGW GCC
	Links


About this package
--------------------------------------------------------------------------------
This is the first release of GNU ASpell as a native win32 application.
An installer will be created soon.


Installing GNU ASpell-0.50
--------------------------------------------------------------------------------
The easiest way to use this build of GNU ASpell-0.50 is to use the default
installation path C:\ASPELL. To use it in a different location you have to
define the new prefix by either passing it as an argument to aspell.exe
	aspell --prefix=g:/nu/aspell
or by setting the environment variable ASPELL_CONF
	set ASPELL_CONF="prefix g:/nu/aspell"
You can also modify and import the aspell.reg.

You will need dictionaries to use aspell. Unless you find ready compiled
ones (try the Link section below), you will have to download and compile
them yourself.


Creating dictionaries
--------------------------------------------------------------------------------
Required:
	GNU Aspell-0.50.2 [1]
	MSys-1.0.7 [3] (or other *nix tools like Cygwin, DJGPP, GnuWin32,...)

1. Download and install Aspell
  Be sure to put "C:\Aspell\bin" in your path. To use a different location
  than "C:\Aspell", you have to define the new prefix using the ASPELL_CONF
  environment variable, e.g. to use "G:\NU\Aspell", set
	ASPELL_CONF="prefix g:/nu/aspell" .
  You can also modify and import aspell.reg.
  If you have problems with these settings, just use the default "c:\aspell".
2. Download and install MinGW (tested with 2.0.3)
3. Download and install MSys (tested with 1.0.7)
4. Open the MSys shell
5. Unpack the languge pack and move into that directory
	tar -xvjf aspell-de-0-50-2.tar.bz2
	cd aspell-de-0-50-2
6. If aspell.exe is not included in $PATH, include it
	export PATH=/c/aspell/bin:$PATH
7. Build and install the dictionaries
	configure
	make
	make install


Differences to the *nix version
--------------------------------------------------------------------------------
pkgdatadir
	Data files (*.alias, *.multi, *.rws)
	*nix:	/usr/share/aspell
	win:	c:\aspell\data
pkglibdir
	Dictionary files (*.dat, *.kbd)
	*nix:	/usr/lib/aspell
	win:	c:\aspell\dict
confdir
	Location of aspell.conf
	*nix:	/etc
	win:	c:\aspell
homedir
	Location of personal files
	*nix:	~
	win:	c:\aspell

The automatic prefix detection from ASpell-0.33 is not included.


Compiling GNU Aspell using MinGW GCC
--------------------------------------------------------------------------------
Required:
	GNU Aspell-0.50.2 [1]
	MinGW-2.0.3 (GCC-3.2) [2]
	(or mingw-gcc-3.2 cross compiler for *nix)
Otional:
	MSys-1.0.7 [3] (or other *nix tools like Cygwin, DJGPP, GnuWin32,...)
	PDCurses [4]

Compiling:
    Download and extract the ASpell-MinGW package [5] into the ASpell source
    directory and apply the patch from there:
      patch -p1 < ..\win32\mingw-changes.diff
    Then chdir into the win32 subdirectory and edit the Makefile
    to customize your build, e.g.
    - include pdcurses
    - change default directories / win32-relocatable
    - build msvc import libs
    Afterwards call "make" to build the application.
    The plain "make" doesnt need any additional *nix utilities and can be
    invoked from the commandline if GCC is in your path. To make other targets
    (clean, install,...), typical *nix tools like rm, cp, sh will be required.


Links
--------------------------------------------------------------------------------
[1] GNU Aspell 0.50-2: http://aspell.net/
[2] MinGW 2.0.3: http://mingw.sourceforge.net/
[3] MSys 1.0.7: http://mingw.sourceforge.net/msys.shtml
[4] PDCurses 2.4: http://pdcurses.sourceforge.net/
[5] Aspell/MinGW downloads (unofficial):
    http://sourceforge.net/project/showfiles.php?group_id=53368&release_id=115907
