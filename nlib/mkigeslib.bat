@echo off
rem ####################################################################
rem
rem   Routine: mkigeslib
rem
rem   Purpose: Creates the 'ncliges.lib' library which contains the
rem            objects required for linking IGES.
rem
rem   Usage:   mkigeslib src    > Creates source files
rem            mkigeslib debug  > Creates debug library
rem            mkigeslib d32    > Creates 32-bit debug library
rem            mkigeslib n32    > Creates 32-bit nodebug library
rem            mkigeslib        > Creates nodebug library
rem
rem ####################################################################
rem
rem...Set directory paths
rem
set UNIFILES=%NCLPATH%\ncluni
set CMD=%0
set typ=%1

set nlib=%NCLPATH%\nlib
set libfile=nncliges.lib
set ND="n"
if "%typ%"=="n32" (
	set nlib=%NCLPATH%\nlib32
)
if "%typ%"=="debug" (
	set nlib=%NCLPATH%\lib
	set libfile=ncliges.lib
	set ND=
)
if "%typ%"=="d32" (
	set nlib=%NCLPATH%\lib32
	set libfile=ncliges.lib
	set ND=
)
rem
rem...Temporary file to delete
rem
set SYMLIB=temp.pch
if NOT "%typ%"=="src" @erase %libfile%
rem
rem...Work in a temp directory
rem
set dir=tmp
if "%typ%"=="src" set dir=igessrc
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
if "%typ%"=="src" echo Making IGES Source Files ...
if NOT "%typ%"=="src" echo Making %libfile% ...
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
	cp %UNIFILES%/README.iges README
rem
rem...Source file lists
rem
:lb2
set classsrc= c1dba.c cuinit.c
 
set draftingsrc= ainitent.c
 
set nclcsrc= nauth.c nclspc001.c nebound.c neclosed.c necv2.c
set nclcsrc=%nclcsrc% necvfix.c necvoffs.c negeogn.c negeogn1.c negeogn4.c
set nclcsrc=%nclcsrc% neglobal.c neinit.c nelabtbl.c nemsfun.c nerbsf.c
set nclcsrc=%nclcsrc% nerbsp2.c nerd.c nerd1.c nerevsf.c nerevsf1.c
set nclcsrc=%nclcsrc% nesfprim.c nesfprim1.c nesfsup1.c nesupt2.c netmp.c
set nclcsrc=%nclcsrc% neub2sup1.c neubtoub.c neuvcvass.c nevolve.c nevolve1.c
set nclcsrc=%nclcsrc% nevolvegn.c nevolvegn1.c nevx.c nutil.c nutil1.c
set nclfsrc= evlrbs.c getifl.c gtdesc.c nclspf001.c setifl.c
set nclfsrc=%nclfsrc% sfevl2.c
 
set umbsrc= ldir.c lmisc.c lmodfile.c lmparse.c lsetdir.c
set wssrc= wsntclrbtn.c wsntclrcell.c wsntclrdlg.c wsntdrawbutton.c wsnthls.c
set wssrc=%wssrc% wsnthuesatchooser.c wsntlumchooser.c wsntpalettectl.c wsntpalettewnd.c
if NOT "%typ%"=="src" goto makelib
rem
rem...Get all source files
rem
	echo Extracting source from class
	FOR %%i IN (%classsrc%) DO call :srcit class %%i
	echo Extracting source from drafting
	FOR %%i IN (%draftingsrc%) DO call :srcit drafting %%i
	echo Extracting source from nclc
	FOR %%i IN (%nclcsrc%) DO call :srcit nclc %%i
	echo Extracting source from nclf
	FOR %%i IN (%nclfsrc%) DO call :srcit nclf %%i
	echo Extracting source from umb
	FOR %%i IN (%umbsrc%) DO call :srcit umb %%i
	echo Extracting source from ws
	FOR %%i IN (%wssrc%) DO call :srcit ws %%i
goto done
rem
rem...Make objects for library
rem
:makelib
	copy %nlib%\%ND%iges.lib %libfile%
	echo Making objects from class
	FOR %%i IN (%classsrc%) DO call :makeit %ND%class %%i
	echo Making objects from drafting
	FOR %%i IN (%draftingsrc%) DO call :makeit %ND%drafting %%i
	echo Making objects from nclc
	FOR %%i IN (%nclcsrc%) DO call :makeit %ND%nclc %%i
	echo Making objects from nclf
	FOR %%i IN (%nclfsrc%) DO call :makeit %ND%nclf %%i
	echo Making objects from umb
	FOR %%i IN (%umbsrc%) DO call :makeit %ND%umb %%i
	echo Making objects from ws
	FOR %%i IN (%wssrc%) DO call :makeit %ND%ws %%i
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
	echo get %d%/%2 >> make.log
	call %UNIFILES%\get %d%/%2 >> make.log
	goto :eof1
rem
rem...Subroutine to get object file
rem
:makeit
	set d=%1
	echo Extracting %~n2.obj >> make.log
	lib /nologo /extract:%~n2.obj %nlib%\\%d%.lib
	goto :eof1
:done
	cd ..
:eof1
