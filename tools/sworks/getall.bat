%echo off
rem
rem Set directory
rem
set %GET%=call \nclroot\ncluni\get.bat
cd SwUnibase
rem
rem %GET% include files
rem
%GET% inc/SwBitmapHandler.h
%GET% inc/SwDocView.h
%GET% inc/SwDocument.h
%GET% inc/SwNclUnibase.h
%GET% inc/SwStdAfx.h
%GET% inc/SwUniAttr.h
%GET% inc/SwUniOpt.h
%GET% inc/SwUniSketch.h
%GET% inc/SwUnibase.h
%GET% inc/SwUnibase_i.h
%GET% inc/SwUnibase.rc
rem
rem %GET% source files
rem
%GET% sworks/SwBitmapHandler.cpp
%GET% sworks/SwDocView.cpp
%GET% sworks/SwDocument.cpp
%GET% sworks/SwModals.cpp
%GET% sworks/SwNclUnibase.cpp
%GET% sworks/SwNclUnibase1.cpp
%GET% sworks/SwStdAfx.cpp
%GET% sworks/SwUniAttr.cpp
%GET% sworks/SwUniOpt.cpp
%GET% sworks/SwUniSketch.cpp
%GET% sworks/SwUniStat.cpp
%GET% sworks/SwUnibase.cpp
%GET% sworks/SwUnibase_module.cpp
rem
rem Reset current directory
rem
cd ..
