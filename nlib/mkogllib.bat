@echo off
rem ####################################################################
rem
rem   Routine: mkogllib
rem
rem   Purpose: Creates the 'libopnncl.lib' library which contains the
rem            OpenNCL Machining Library objects.  Also creates a source
rem            file directory containing all source and make files for
rem            escrow purposes.
rem
rem   Usage:   mkogllib src    > Creates source files
rem            mkogllib debug  > Creates debug library
rem            mkogllib        > Creates nodebug library
rem
rem ####################################################################
rem
rem...Set directory paths
rem
set UNIFILES=%NCLPATH%\ncluni
set CMD=%0
set typ=%1
set nlib=%NCLPATH%\nlib
if "%typ%"=="debug" set nlib=%NCLPATH%\lib
set libfile=openngl.lib
if "%typ%"=="debug" set libfile=openngld.lib
set ND="n"
if "%typ%"=="debug" set ND=
rem
rem...Source files
rem
rem
rem...Temporary file to delete
rem
set SYMLIB=temp.pch
if NOT "%typ%"=="src" @erase %libfile%
rem
rem...Work in a temp directory
rem
set dir=tmp
if "%typ%"=="src" set dir=oglsrc
if EXIST %dir% goto cd1
echo Making '%dir%' directory to work in.
mkdir %dir%
:cd1
	cd %dir%
	echo # > make.log
rem
rem...Remove any pre-existing objects
rem
if "%typ%"=="src" @erase *
if NOT "%typ%"=="src" @erase *.obj
rem
rem...Let the user know we are making the library
rem
echo _
if "%typ%"=="src" echo Making OGL Source Files ...
if "%typ%"=="" echo Making %libfile% ...
echo _
rem
rem...Create include files
rem
if NOT "%typ%"=="src" goto lb2
	echo Creating C Includes
	mkntfile src inc
	make oml  >> make.log
	erase makefile isysddl.ddl
rem
rem...Create Makefiles
rem
	echo Creating Makefiles
	mkntoglfile
	mv makefile makefile.nt
rem
rem...Create README file
rem
	echo Creating README file
	cp %UNIFILES%/README.ogl README
rem
rem...Source file lists
rem
:lb2
set classsrc= c1dba.c cuinit.c
 
set igessrc= tigcompar.c tigmatch.c
 
set interfacesrc= znuinit.c
 
set modelsrc= m1mcrv.c m2dasup4.c m2dba.c m2dba2.c m2eirel.c
set modelsrc=%modelsrc% m2elabel.c m2eval.c m2global.c m2ilayer.c m3ecirc1.c
set modelsrc=%modelsrc% m3ecirc6.c m3ecomp1.c m3eline2.c m3icirc1.c m4bndry2.c
set modelsrc=%modelsrc% m4evcrv.c m4ioslo1.c m4ioslo2.c m5ioslo.c m5ioslo1.c
set modelsrc=%modelsrc% m7geom.c m7math1.c m7math2.c m7math3.c m7math2d.c
set modelsrc=%modelsrc% m7mathco.c m7mathtf.c m7mathvc.c mmsmathf.c
 
set nclcsrc= nauth.c nebound.c necolors.c negeogn4.c neglobal.c
set nclcsrc=%nclcsrc% neinit.c nelabtbl.c nemsfun.c nerd.c nerd1.c
set nclcsrc=%nclcsrc% nerevsf1.c nesf2.c nesfprim.c nesfprim1.c netmp.c
set nclcsrc=%nclcsrc% neub2sup1.c neubtoub.c neuvcvass.c nevolve1.c nevolvegn.c
set nclcsrc=%nclcsrc% nevolvegn1.c nevx.c ngtimx.c
 
set oglsrc= ogmisc.c ogstubs.c
 
set omlsrc= omalloc.c
 
set opennclsrc= ymdlattrib.c ymdlput.c ymodel.c yunibase.c
 
set symbolssrc= bconinit.c bxinit.c
 
set umbsrc= lmisc.c lmodfile.c lmparse.c lsetdir.c
 
set utlsrc= ubits.c ucompb.c ulist.c ulists.c umoveb.c
set utlsrc=%utlsrc% unserve.c uqsort.c
 
set xiosrc= xe1paths.c xf0udos0.c xf1farea.c xf1files.c
if NOT "%typ%"=="src" goto makelib
rem
rem...Get all source files
rem
	echo Extracting source from class
	FOR %%i IN (%classsrc%) DO call :srcit class %%i
	echo Extracting source from iges
	FOR %%i IN (%igessrc%) DO call :srcit iges %%i
	echo Extracting source from interface
	FOR %%i IN (%interfacesrc%) DO call :srcit interface %%i
	echo Extracting source from model
	FOR %%i IN (%modelsrc%) DO call :srcit model %%i
	echo Extracting source from nclc
	FOR %%i IN (%nclcsrc%) DO call :srcit nclc %%i
	echo Extracting source from ogl
	FOR %%i IN (%oglsrc%) DO call :srcit ogl %%i
	echo Extracting source from oml
	FOR %%i IN (%omlsrc%) DO call :srcit oml %%i
	echo Extracting source from openncl
	FOR %%i IN (%opennclsrc%) DO call :srcit openncl %%i
	echo Extracting source from symbols
	FOR %%i IN (%symbolssrc%) DO call :srcit symbols %%i
	echo Extracting source from umb
	FOR %%i IN (%umbsrc%) DO call :srcit umb %%i
	echo Extracting source from utl
	FOR %%i IN (%utlsrc%) DO call :srcit utl %%i
	echo Extracting source from xio
	FOR %%i IN (%xiosrc%) DO call :srcit xio %%i
goto done
rem
rem...Make objects for library
rem
:makelib
	copy %nlib%\%ND%unibase.lib %libfile%
	echo Making objects from class
	FOR %%i IN (%classsrc%) DO call :makeit %ND%class %%i
	echo Making objects from iges
	FOR %%i IN (%igessrc%) DO call :makeit %ND%iges %%i
	echo Making objects from interface
	FOR %%i IN (%interfacesrc%) DO call :makeit %ND%interface %%i
	echo Making objects from model
	FOR %%i IN (%modelsrc%) DO call :makeit %ND%model %%i
	echo Making objects from nclc
	FOR %%i IN (%nclcsrc%) DO call :makeit %ND%nclc %%i
	echo Making objects from ogl
	FOR %%i IN (%oglsrc%) DO call :makeit %ND%ogl %%i
	echo Making objects from oml
	FOR %%i IN (%omlsrc%) DO call :makeit %ND%oml %%i
	echo Making objects from openncl
	FOR %%i IN (%opennclsrc%) DO call :makeit %ND%openncl %%i
	echo Making objects from symbols
	FOR %%i IN (%symbolssrc%) DO call :makeit %ND%symbols %%i
	echo Making objects from umb
	FOR %%i IN (%umbsrc%) DO call :makeit %ND%umb %%i
	echo Making objects from utl
	FOR %%i IN (%utlsrc%) DO call :makeit %ND%utl %%i
	echo Making objects from xio
	FOR %%i IN (%xiosrc%) DO call :makeit %ND%xio %%i
	echo Creating %libfile%
	lib /nologo /out:..\%libfile% %libfile% *.obj
	erase %libfile%
	erase *.obj
	goto done
rem
rem...Subroutine to get source file
rem
:srcit
	set d=%1
rem	if "%1"=="nclf1" set d=nclf
rem	if "%1"=="nclf2" set d=nclf
rem	if "%1"=="nclf3" set d=nclf
	echo get %d%/%2 >> make.log
	call %UNIFILES%\get %d%/%2 >> make.log
	goto :eof1
rem
rem...Subroutine to get object file
rem
:makeit
	set d=%1
rem	if "%1"=="nclf1" set d=nclf
rem	if "%1"=="nclf2" set d=nclf
rem	if "%1"=="nclf3" set d=nclf
	echo Extracting %~n2.obj >> make.log
	lib /nologo /extract:%~n2.obj %nlib%\\%d%.lib
	goto :eof1
:done
	cd ..
:eof1
