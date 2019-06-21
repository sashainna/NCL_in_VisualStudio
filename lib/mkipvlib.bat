@echo off
rem ####################################################################
rem
rem   Routine: mkipvlib
rem
rem   Purpose: Creates the 'nclipv.lib' library which contains the
rem            objects required for linking NCLIPV.
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
set libfile=nnclipv.lib
set ND="n"
if "%typ%"=="n32" (
	set nlib=%NCLPATH%\nlib32
)
if "%typ%"=="debug" (
	set nlib=%NCLPATH%\lib
	set libfile=nclipv.lib
	set ND=
)
if "%typ%"=="d32" (
	set nlib=%NCLPATH%\lib32
	set libfile=nclipv.lib
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
if "%typ%"=="src" echo Making IPV Source Files ...
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
	cp %UNIFILES%/README.ipv README
rem
rem...Source file lists
rem
:lb2
set calcsrc= qlangio2.c qsymtb2.c
 
set classsrc= c1class.c c1class1.c c1dba.c classtab.c classup.c
set classsrc=%classsrc% crootin.c csolin.c csupin.c cuinit.c cunibin.c
set dassrc= d1dopngl.c d1gkssup.c d1initd.c d1rect.c d1rubber.c
set dassrc=%dassrc% d2auxm.c d2timsp.c d3jump.c d3pipe1.c d3pipe2.c
set dassrc=%dassrc% d3pipe3.c d3read.c d3uims2.c d4actfm.c d4ckdat.c
set dassrc=%dassrc% d4datx.c d4form.c d4loadf.c d4meth.c d4meth2.c
set dassrc=%dassrc% d4prunit.c d5dasin.c d5lgio.c d5lgsub.c d5stack.c
set dassrc=%dassrc% d6layrn.c d7menu.c d7mlayout.c d4meth.c d4meth2.c
set dassrc=%dassrc% dprint.c
 
set generalsrc= jfmgt.c
 
set interfacesrc= imouse3.itr ipuck4.itr iunihep.msg nmodfy.pmu zmisc.c
set interfacesrc=%interfacesrc% zuserkeys.c zwntcalls.c
 
set modelsrc= m3ecirc1.c m3ecirc6.c m3ecpln1.c m3icirc1.c m3iptbtw.c
set modelsrc=%modelsrc% m4evcrv.c m4ipcvh.c m7geom1.c m7math1.c m7math2d.c
set modelsrc=%modelsrc% m7math3.c m7mathtf.c m7mathvc.c m7mathco.c mipv.c
set modelsrc=%modelsrc% mmsmathf.c
 
set nclcsrc= nclresize.c nclsig.c neauth.c necdraw.c necdraw1.c
set nclcsrc=%nclcsrc% necdraw2.c neclfile.c necmd.c necutprof.c necutsym.c
set nclcsrc=%nclcsrc% nemcdcnv.c necutprof.c necutsym.c nemcdcnv.c nemotattr.c
set nclcsrc=%nclcsrc% nemotcut.c nemotpb.c nemotpb1.c nemsfun.c nescalar2.c
set nclcsrc=%nclcsrc% nesequnc.c nesimulate.c nesmouse.c nesolid1.c nesrc.c
set nclcsrc=%nclcsrc% netmp.c nucutr1.c numot.c
 
set nclfsrc= barseg.f char.for clfile.for clword.for conent.f
set nclfsrc=%nclfsrc% cycman.f cylman.f fio.for getifl.f ipvfor.f
set nclfsrc=%nclfsrc% math.f nclxini.f rpchk.f rsttrm.f setifl.f
set nclfsrc=%nclfsrc% setsc.f upiter.f vector.f vocsch.f voccad.f
set viewsrc= vconv.c vdefvw.c vedynstep.c vedynvw.c vescreen.c
set viewsrc=%viewsrc% veview.c vevport.c viinit.c virel.c vudynvw.c
set viewsrc=%viewsrc% vuview3.c vwindow.c
 
set wssrc= wsgifsave.c wsglatt.c wsgldyn.c wsglfun.c wsimgcnv.c
set wssrc=%wssrc% wsglmsfun.c wsglmx.c wsglmx2.c wsglprint.c wsgltran.c
set wssrc=%wssrc% wsgltrk.c wshdibapi.cpp wsncldockbar.cpp wsncldockframe.cpp wsnclframe.cpp
set wssrc=%wssrc% wsncltoolbar.cpp wsntapp.cpp wsntbitmap.cpp wsntbmpbtn.cpp wsntclrbtn.cpp
set wssrc=%wssrc% wsntclrcell.cpp wsntclrdlg.cpp wsntcmdbar.cpp wsntcursor.cpp wsntDDSlider.cpp
set wssrc=%wssrc% wsntdir.cpp wsntdlgbar.cpp wsntdlgitem.cpp wsntdoc.cpp wsntdockcont.cpp
set wssrc=%wssrc% wsntdrawbutton.cpp wsntdroptarget.cpp wsntdum.c wsntdynint.cpp wsnteditctl.cpp
set wssrc=%wssrc% wsntevent.cpp wsntform.cpp wsntform2.cpp wsntformbar.cpp wsntfsview.cpp
set wssrc=%wssrc% wsntgettxt2.cpp wsntgraphic.cpp wsntheaderctrl.cpp wsnthls.cpp wsnthuesatchooser.cpp
set wssrc=%wssrc% wsntlstctl.cpp wsntlstctl2.cpp wsntlumchooser.cpp wsntmenu.cpp wsntmenudsndlg.cpp
set wssrc=%wssrc% wsntNCLButton.cpp wsntopt.cpp wsntpalettectl.cpp wsntpalettewnd.cpp wsntpicsel.cpp
set wssrc=%wssrc% wsntpmenu.cpp wsntsecbtn.cpp wsntSliderCtrl.cpp wsntsmouse.cpp wsnttblst.cpp
set wssrc=%wssrc% wsnttmenu.cpp wsnttooltip.cpp wsnttxtwin.cpp
if NOT "%typ%"=="src" goto makelib
rem
rem...Get all source files
rem
	echo Extracting source from calc
	FOR %%i IN (%calcsrc%) DO call :srcit calc %%i
	echo Extracting source from class
	FOR %%i IN (%classsrc%) DO call :srcit class %%i
	echo Extracting source from das
	FOR %%i IN (%dassrc%) DO call :srcit das %%i
	echo Extracting source from general
	FOR %%i IN (%generalsrc%) DO call :srcit general %%i
	echo Extracting source from interface
	FOR %%i IN (%interfacesrc%) DO call :srcit interface %%i
	echo Extracting source from model
	FOR %%i IN (%modelsrc%) DO call :srcit model %%i
	echo Extracting source from nclc
	FOR %%i IN (%nclcsrc%) DO call :srcit nclc %%i
	echo Extracting source from nclf
	FOR %%i IN (%nclfsrc%) DO call :srcit nclf %%i
	echo Extracting source from view
	FOR %%i IN (%viewsrc%) DO call :srcit view %%i
	echo Extracting source from ws
	FOR %%i IN (%wssrc%) DO call :srcit ws %%i
goto done
rem
rem...Make objects for library
rem
:makelib
	copy %nlib%\%ND%ipv.lib %libfile%
	echo Making objects from calc
	FOR %%i IN (%calcsrc%) DO call :makeit %ND%calc %%i
	echo Making objects from class
	FOR %%i IN (%classsrc%) DO call :makeit %ND%class %%i
	echo Making objects from das
	FOR %%i IN (%dassrc%) DO call :makeit %ND%das %%i
	echo Making objects from general
	FOR %%i IN (%generalsrc%) DO call :makeit %ND%general %%i
	echo Making objects from interface
	FOR %%i IN (%interfacesrc%) DO call :makeit %ND%interface %%i
	echo Making objects from model
	FOR %%i IN (%modelsrc%) DO call :makeit %ND%model %%i
	echo Making objects from nclc
	FOR %%i IN (%nclcsrc%) DO call :makeit %ND%nclc %%i
	echo Making objects from nclf
	FOR %%i IN (%nclfsrc%) DO call :makeit %ND%nclf %%i
	echo Making objects from view
	FOR %%i IN (%viewsrc%) DO call :makeit %ND%view %%i
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
