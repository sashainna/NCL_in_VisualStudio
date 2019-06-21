@echo off
rem ####################################################################
rem
rem   Routine: mksteplib
rem
rem   Purpose: Creates the 'nclstep.lib' library which contains the
rem            objects required for linking STEP.
rem
rem   Usage:   mksteplib src    > Creates source files
rem            mksteplib debug  > Creates debug library
rem            mksteplib d32    > Creates 32-bit debug library
rem            mksteplib n32    > Creates 32-bit nodebug library
rem            mksteplib        > Creates nodebug library
rem
rem ####################################################################
rem
rem...Set directory paths
rem
set UNIFILES=%NCLPATH%\ncluni
set CMD=%0
set typ=%1

set nlib=%NCLPATH%\nlib
set libfile=nnclstep.lib
set ND="n"
if "%typ%"=="n32" (
	set nlib=%NCLPATH%\nlib32
)
if "%typ%"=="debug" (
	set nlib=%NCLPATH%\lib
	set libfile=nclstep.lib
	set ND=
)
if "%typ%"=="d32" (
	set nlib=%NCLPATH%\lib32
	set libfile=nclstep.lib
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
if "%typ%"=="src" set dir=stepsrc
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
if "%typ%"=="src" echo Making STEP Source Files ...
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
	cp %UNIFILES%/README.step README
rem
rem...Source file lists
rem
:lb2
set classsrc= c1dba.c cuinit.c
 
set draftingsrc= ainitent.c
 
set modelsrc= m1mcrv.c m2dasup4.c m2dba.c m2dba2.c m2egen.c
set modelsrc=%modelsrc% m2eirel.c m2elabel.c m2global.c m2iattr.c m2ilayer.c
set modelsrc=%modelsrc% m2lnwid.c m3ecirc1.c m3ecirc2.c m3ecirc6.c m3ecn1.c
set modelsrc=%modelsrc% m3ecn3.c m3ecirc3.c m3ecomp.c m3ecomp1.c m3eline2.c
set modelsrc=%modelsrc% m3eplin.c m3espan.c m3etrim1.c m3icirc1.c m3icn1.c
set modelsrc=%modelsrc% m3icomp.c m3iptbtw.c m4bndry.c m4bndry2.c m4ebsc.c
set modelsrc=%modelsrc% m4ecrv0.c m4ecrv2.c m4ecvsf.c m4edba.c m4erbsc.c
set modelsrc=%modelsrc% m4esavld.c m4evcrv.c m4ibsc.c m4icrv1.c m4ioslo.c
set modelsrc=%modelsrc% m4ioslo1.c m4ioslo2.c m4ipcvh.c m4isect.c m4isect1.c
set modelsrc=%modelsrc% m5econv.c m5edba.c m5ioslo.c m5ioslo1.c m7geom.c
set modelsrc=%modelsrc% m7geom1.c m7math1.c m7math2.c m7math2d.c m7math3.c
set modelsrc=%modelsrc% m7mathco.c m7mathmx.c m7mathtf.c m7mathvc.c m8stubs.c
set modelsrc=%modelsrc% m9edrw.c m9eplot.c mmsmathf.c
 
set nclcsrc= nauth.c nclspc001.c nebound.c neclosed.c necv2.c
set nclcsrc=%nclcsrc% necvfix.c necvoffs.c negeogn.c negeogn1.c negeogn4.c
set nclcsrc=%nclcsrc% neglobal.c neinit.c nelabtbl.c nemsfun.c nerbsf.c
set nclcsrc=%nclcsrc% nerbsp2.c nerd.c nerd1.c nerevsf.c nerevsf1.c
set nclcsrc=%nclcsrc% nesfprim.c nesfprim1.c nesfsup1.c nesupt2.c netmp.c
set nclcsrc=%nclcsrc% netrimsf3.c neub2sup1.c neubtoub.c neuvcvass.c nevolve.c
set nclcsrc=%nclcsrc% nevolve1.c nevolvegn.c nevolvegn1.c nevx.c nutil.c
set nclcsrc=%nclcsrc% nutil1.c
 
set nclfsrc= evlrbs.c getifl.c gtdesc.c nclspf001.c setifl.c
set nclfsrc=%nclfsrc% sfevl2.c
 
set opennclsrc= yunibase.c
 
set umbsrc= ldir.c lmisc.c lmodfile.c lmparse.c lsetdir.c
set wssrc= wsntclrbtn.c wsntclrcell.c wsntclrdlg.c wsntdrawbutton.c wsnthls.c
set wssrc=%wssrc% wsnthuesatchooser.c wsntlumchooser.c wsntpalettectl.c wsntpalettewnd.c
 
set igessrc= igsupt.c tigcompar.c tigcreat.c tigdbsp.c tigdrf1.c
set igessrc=%igessrc% tigdrwt.c tigintrf.c tigmain.c tigmatch.c tigmisc.c
set igessrc=%igessrc% tigmodsp.c tigsave.c tigstubs.c tigsupt.c tigsys.c
set igessrc=%igessrc% tigtrimsf.c tigview.c tioeval.c tioinit.c tiomain.c
set igessrc=%igessrc% tiomutil.c tiosgsec.c
if NOT "%typ%"=="src" goto makelib
rem
rem...Get all source files
rem
	echo Extracting source from class
	FOR %%i IN (%classsrc%) DO call :srcit class %%i
	echo Extracting source from drafting
	FOR %%i IN (%draftingsrc%) DO call :srcit drafting %%i
	echo Extracting source from model
	FOR %%i IN (%modelsrc%) DO call :srcit model %%i
	echo Extracting source from nclc
	FOR %%i IN (%nclcsrc%) DO call :srcit nclc %%i
	echo Extracting source from nclf
	FOR %%i IN (%nclfsrc%) DO call :srcit nclf %%i
	echo Extracting source from openncl
	FOR %%i IN (%opennclsrc%) DO call :srcit openncl %%i
	echo Extracting source from umb
	FOR %%i IN (%umbsrc%) DO call :srcit umb %%i
	echo Extracting source from ws
	FOR %%i IN (%wssrc%) DO call :srcit ws %%i
	echo Extracting source from iges
	FOR %%i IN (%igessrc%) DO call :srcit iges %%i
goto done
rem
rem...Make objects for library
rem
:makelib
	copy %nlib%\%ND%step.lib %libfile%
	echo Making objects from class
	FOR %%i IN (%classsrc%) DO call :makeit %ND%class %%i
	echo Making objects from drafting
	FOR %%i IN (%draftingsrc%) DO call :makeit %ND%drafting %%i
	echo Making objects from model
	FOR %%i IN (%modelsrc%) DO call :makeit %ND%model %%i
	echo Making objects from nclc
	FOR %%i IN (%nclcsrc%) DO call :makeit %ND%nclc %%i
	echo Making objects from nclf
	FOR %%i IN (%nclfsrc%) DO call :makeit %ND%nclf %%i
	echo Making objects from openncl
	FOR %%i IN (%opennclsrc%) DO call :makeit %ND%openncl %%i
	echo Making objects from umb
	FOR %%i IN (%umbsrc%) DO call :makeit %ND%umb %%i
	echo Making objects from ws
	FOR %%i IN (%wssrc%) DO call :makeit %ND%ws %%i
	echo Making objects from iges
	FOR %%i IN (%igessrc%) DO call :makeit %ND%iges %%i
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
