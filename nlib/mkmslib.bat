@echo off
rem ####################################################################
rem
rem   Routine: mkmslib
rem
rem   Purpose: Creates the 'mslib.lib' library which contains the
rem            objects required for linking MSLITE.
rem
rem   Usage:   mkipvlib src    > Creates source files
rem            mkipvlib debug  > Creates debug library
rem            mkipvlib d32    > Creates 32-bit debug library
rem            mkipvlib n32    > Creates 32-bit nodebug library
rem            mkipvlib        > Creates nodebug library
rem
rem ####################################################################
rem
rem...Set directory paths
rem
set UNIFILES=%NCLPATH%\ncluni
set CMD=%0
set typ=%1

set nlib=%NCLPATH%\nlib
set libfile=nmslib.lib
set ND="n"
if "%typ%"=="n32" (
   set nlib=%NCLPATH%\nlib32
)
if "%typ%"=="debug" (
   set nlib=%NCLPATH%\lib
   set libfile=mslib.lib
   set ND=
)
if "%typ%"=="d32" (
   set nlib=%NCLPATH%\lib32
   set libfile=mslib.lib
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
if "%typ%"=="src" set dir=ipvsrc
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
if "%typ%"=="src" echo Making MSLITE Source Files ...
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
	cp %UNIFILES%/README.ipv README
rem
rem...Source file lists
rem
:lb2
set calcsrc= qlangio2.c
 
set dassrc= d1dopngl.c d1gkssup.c d1initd.c d1rect.c d1rubber.c
set dassrc=%dassrc% d2timsp.c d3jump.c d3uims2.c d4actfm.c d4ckdat.c
set dassrc=%dassrc% d4datx.c d4form.c d4loadf.c d4meth.c d4meth2.c
set dassrc=%dassrc% d4prunit.c d5dasin.c d5lgio.c d5lgsub.c d5stack.c
set dassrc=%dassrc% d6layrn.c d7menu.c d7mlayout.c
 
set interfacesrc= iunihep.msg zuserkeys.c zwntcalls.c
 
set modelsrc= m3ecirc1.c m3ecirc6.c m3ecpln1.c m3icirc1.c m4evcrv.c
set modelsrc=%modelsrc% m7math1.c m7math2d.c m7math3.c m7mathtf.c m7mathvc.c
set modelsrc=%modelsrc% m7mathco.c mmsmathf.c
 
set nclcsrc= necattr.c necdraw.c nemsfun.c nescalar2.c nesolid1.c
set umbsrc= ldir.c linit.c lipv1.c lipvassem.c lipvaxis.c
set umbsrc=%umbsrc% lipvdiag.c lipvmach1.c lipvmach3.c lipvmsl.c lipvregion.c
set umbsrc=%umbsrc% lipvsess.c lipvstk.c lipvstk1.c lipvstk2.c lipvsup.c
set umbsrc=%umbsrc% lipvview2.c lmiconf.c lmisc.c lmodfile.c lmparse.c
set umbsrc=%umbsrc% lsetdir.c
 
set viewsrc= vuview3.c
 
set wssrc= wsglatt.c wsgldyn.c wsglfun.c wsglmsfun.c wsglmx.c
set wssrc=%wssrc% wsglmx2.c wsgltran.c wsgltrk.c wshdibapi.cpp wsncldockbar.cpp
set wssrc=%wssrc% wsncldockframe.cpp wsnclframe.cpp wsncltoolbar.cpp wsntapp.c wsntbitmap.cpp
set wssrc=%wssrc% wsntbmpbtn.cpp wsntclrbtn.cpp wsntclrcell.cpp wsntclrdlg.cpp wsntcmdbar.cpp
set wssrc=%wssrc% wsntcursor.cpp wsntdir.cpp wsntdlgbar.cpp wsntdlgitem.cpp wsntdoc.cpp
set wssrc=%wssrc% wsntdockcont.cpp wsntdrawbutton.cpp wsntdroptarget.cpp wsntdum.c wsntdynint.cpp
set wssrc=%wssrc% wsnteditctl.cpp wsntevent.cpp wsntform.cpp wsntform2.cpp wsntformbar.cpp
set wssrc=%wssrc% wsntfsview.cpp wsntgettxt2.cpp wsntgraphic.cpp wsnthls.cpp wsnthuesatchooser.cpp
set wssrc=%wssrc% wsntlstctl.cpp wsntlstctl2.cpp wsntlumchooser.cpp wsntmenu.cpp wsntnclbutton.cpp
set wssrc=%wssrc% wsntopt.cpp wsntpalettectl.cpp wsntpalettewnd.cpp wsntpicsel.cpp wsntpmenu.cpp
set wssrc=%wssrc% wsntsecbtn.cpp wsntsliderctrl.cpp wsntsmouse.c wsnttmenu.cpp wsnttooltip.cpp
set wssrc=%wssrc% wsnttxtwin.cpp
 
set xiosrc= xe1gtenv.c xe1paths.c xf0udos0.c xf1farea.c xf1files.c
if NOT "%typ%"=="src" goto makelib
rem
rem...Get all source files
rem
	echo Extracting source from calc
	FOR %%i IN (%calcsrc%) DO call :srcit calc %%i
	echo Extracting source from das
	FOR %%i IN (%dassrc%) DO call :srcit das %%i
	echo Extracting source from interface
	FOR %%i IN (%interfacesrc%) DO call :srcit interface %%i
	echo Extracting source from model
	FOR %%i IN (%modelsrc%) DO call :srcit model %%i
	echo Extracting source from nclc
	FOR %%i IN (%nclcsrc%) DO call :srcit nclc %%i
	echo Extracting source from umb
	FOR %%i IN (%umbsrc%) DO call :srcit umb %%i
	echo Extracting source from view
	FOR %%i IN (%viewsrc%) DO call :srcit view %%i
	echo Extracting source from ws
	FOR %%i IN (%wssrc%) DO call :srcit ws %%i
	echo Extracting source from xio
	FOR %%i IN (%xiosrc%) DO call :srcit xio %%i
goto done
rem
rem...Make objects for library
rem
:makelib
	copy %nlib%\%ND%mslite.lib %libfile%
	echo Making objects from calc
	FOR %%i IN (%calcsrc%) DO call :makeit %ND%calc %%i
	echo Making objects from das
	FOR %%i IN (%dassrc%) DO call :makeit %ND%das %%i
	echo Making objects from interface
	FOR %%i IN (%interfacesrc%) DO call :makeit %ND%interface %%i
	echo Making objects from model
	FOR %%i IN (%modelsrc%) DO call :makeit %ND%model %%i
	echo Making objects from nclc
	FOR %%i IN (%nclcsrc%) DO call :makeit %ND%nclc %%i
	echo Making objects from umb
	FOR %%i IN (%umbsrc%) DO call :makeit %ND%umb %%i
	echo Making objects from view
	FOR %%i IN (%viewsrc%) DO call :makeit %ND%view %%i
	echo Making objects from ws
	FOR %%i IN (%wssrc%) DO call :makeit %ND%ws %%i
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
