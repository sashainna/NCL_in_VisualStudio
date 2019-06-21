#
#--------------------------- make.header.wnt --------------------------
#
#
# Common symbols changed when creating a local makefile
#
PLACE_OBJ = 
PLACE_NOBJ = 
PLACE_OBJ32 = 
PLACE_NOBJ32 = 
#PLACE_OBJ = -erase ..\obj\$*.obj && $(MV) $*.obj ..\obj
#PLACE_NOBJ = -erase ..\nobj\$*.obj && $(MV) $*.obj ..\nobj
#PLACE_OBJ32 = -erase ..\obj32\$*.obj && $(MV) $*.obj ..\obj32
#PLACE_NOBJ32 = -erase ..\nobj32\$*.obj && $(MV) $*.obj ..\nobj32
#
LI_DIR=C:\mworks72
LI_ARCH=intel\win\msoft10
LI_ARCH32=intel\win\msoft10
LI_ARCH64=amd\winxp64\msoft10
#
VCD1=C:\Program Files (x86)\Microsoft Visual Studio 10.0
VCD2=C:\Program Files (x86)\Microsoft Visual Studio 10.0\VC
VCD3=C:\Program Files (x86)\Microsoft SDKS\Windows\V7.0A
DFD1=C:\Program Files (x86)\Intel\Composer XE 2011 SP1\bin\intel64
DFD2=C:\Program Files (x86)\Intel\Composer XE 2011 SP1\compiler
#
INC1=$(DFD2)\include;$(VCD2)\Include;$(VCD2)\ATLMFC\Include
INC2=$(VCD3)\Include;$(VCD1)\SDK\V3.5\Include
INCLUDE=$(INC1);$(INC2)
#
PATH1=$(VCD1)\VC\BIN\amd64;$(VCD3)\bin\x64;$(VCD3)\bin
PATH2=$(VCD1)\VC\VCPackages;$(VCD1)\COmmon7\IDE
PATH3=$(VCD1)\Common7\Tools;$(VCD1)\Common7\Tools\bin;$(VCD1)\SDK\v3.5\bin
PATH4=C:\WINDOWS\system32;C:\WINDOWS\System32\Wbem
PATH5=$(DFD1)
PATH64=$(PATH1);$(PATH2);$(PATH3);$(PATH4);$(PATH5)
#
VCD31=C:\Program Files (x86)\Microsoft Visual Studio 10.0
VCD32=C:\Program Files (x86)\Microsoft Visual Studio 10.0\VC
DFD31=C:\Program Files (x86)\Intel\Composer XE 2011 SP1\bin\ia32
DFD32=C:\Program Files (x86)\Intel\Composer XE 2011 SP1\compiler
#
#PATH31=$(VCD31)\VC\BIN;$(VCD31)\VC\PlatformSDK\bin
#PATH32=$(VCD31)\VC\PlatformSDK\bin;$(VCD31)\VC\VCPackages;$(VCD31)\COmmon7\IDE
PATH31=$(VCD31)\VC\BIN
PATH32=$(VCD31)\VC\VCPackages;$(VCD31)\COmmon7\IDE
PATH33=$(VCD31)\Common7\Tools;$(VCD31)\Common7\Tools\bin;$(VCD31)\SDK\v3.5\bin
PATH34=C:\WINDOWS\system32;C:\WINDOWS\System32\Wbem
PATH35=$(DFD31)
PATH32=$(PATH31);$(PATH32);$(PATH33);$(PATH34);$(PATH35)

SCCSREL=
#
BIN=bin
SBIN=sbin
LIB=lib
NLIB=nlib
LIB32=lib32
NLIB32=nlib32
#
# define sccs command - alias where to find the source
# if it's not here.
#
# These SCCS commands should be changed to use copysrc.
#
GET = $(NCLROOT)\ncluni\get
#
SYMLIB =
#
# device specific flags used for compiling and loading
#
NDEBUG = /D "NDEBUG" /MD /Od
DEBUG = /D "_DEBUG" /MDd /Zi /Fd"$(SRCDIR)" /Od
NDEB = /Od
DEB = /debug:full /Od

DEVCFLAGS=/nologo /EHsc /D "WIN32" /D "_WINDOWS" /D "WINVER=0x502" \
	/D "_CRT_SECURE_NO_DEPRECATE" \
	/D "NO_XMLRPC_FUNCTIONS" \
	/D "VOLUMILL_ENGINE_LINKED" /Fo".\\" /FD /c
DEVCPFLAGS=$(DEVCFLAGS) /D "_AFXDLL"
DEVFFLAGS=/integer_size:16 /compile_only /nologo /libs:dll /names:lowercase \
	/iface:nomixed_str_len_arg /fpscomp:general /iface:cref \
	/warn:nofileopt 
DEVRCFLAGS=/l 0x409 /d "_AFXDDL"

FINCLUDE=/include:$(NCLPATH)\src\incf
FSYS=k
PWCFLAGS=/nologo /EHsc /Od /D"WIN32" /D"_WINDOWS" /D"WNT" /D "WINVER=0x601" \
	/D "_CRT_SECURE_NO_DEPRECATE" /Fo".\\" /FD /c
PWCPFLAGS=$(PWCFLAGS) /D "_AFXDLL"
PWCPINCS=
PWFFLAGS=/compile_only /nologo /libs:dll /names:lowercase \
	/iface:nomixed_str_len_arg /fpscomp:general /iface:cref /warn:nofileopt
PWLDFLAGS=
PWSYS=WNT
#
# often changed symbols used for compiling
#
UU_COMP=/D UU_COMP=UU_WIN2K
UU_SEGDATA=/D UU_SEGDATA=DOUBLE
UU_PREC=/D UU_DOUBLE
UU_DEBUG=/D UU_DEBUGOFF
UU_NCLIPV=/D UU_IPV /D LI_FLOAT_IS_DOUBLE /D LI_MACROS
#
# flags to set if optimization or profiling are wanted
#
OPFLAGS=
#
#---------------------------EOF make.header.wnt --------------------------
#
#
#--------------------------- make.prefix.postworks.wnt ------------------------
#
#
# PostWorks includes, libraries and tools
#
PWORKS=postworks
PWINC=$(NCLPATH)\$(PWORKS)\pwinc
NCLFILES=$(NCLPATH)\nclfiles
#
CINCLUDE=/I .\ /I$(NCLPATH)\postworks\pwinc
FINCLUDE=/include:$(NCLPATH)\postworks\pwinc /include:"$(DFD)\include"
RCINCLUDE=/i "$(PWINC)" /i "$(NNCLLIB)\obj"
#
NCLLIB=$(NCLPATH)\pworkslib
NNCLLIB=$(NCLPATH)\pworksnlib
NCLLIB32=$(NCLPATH)\pworkslib32
NNCLLIB32=$(NCLPATH)\pworksnlib32
#
NCLBIN=$(NCLPATH)\$(BIN)
NCLSBIN=$(NCLPATH)\$(SBIN)
#
FORFILTER=- $(NCLBIN)\port
SCCSTOOL=$(NCLSBIN)\formtool
#
CFLAGS=$(CINCLUDE) $(PWCFLAGS) 
CPFLAGS=$(CINCLUDE) $(PWCPINCS) $(PWCPFLAGS)
FFLAGS=$(FINCLUDE) $(PWFFLAGS)
RCFLAGS=$(RCINCLUDE) $(DEVRCFLAGS)
#
# Compile variations
#
CC=cl $(DEBUG)
CPP=cl $(DEBUG)
F77=ifort $(DEB)
NCC=cl $(NDEBUG)
NCPP=cl $(NDEBUG)
NF77=ifort $(NDEB)
RSC=rc
#
# define some general things
#
AR = lib /nologo
CP=copy 
RM=erase /f
MV=move
TOUCH=date /t >
#
# debug library
#
RMLIB=$(RM) $(NCLLIB)\$(NCLLIBRARY)
CRELIB1=$(AR) /OUT:$(NCLLIB)\$(NCLLIBRARY) $(NCLLIB)\$(NCLLIBRARY) *.obj
CRELIB2=$(AR) /OUT:$(NCLLIB)\$(NCLLIBRARY) *.obj
#
# nodebug library
#
NRMLIB=$(RM) $(NNCLLIB)\$(NCLLIBRARY)
NCRELIB1=$(AR) /OUT:$(NNCLLIB)\$(NNCLLIBRARY) $(NNCLLIB)\$(NNCLLIBRARY) *.obj
NCRELIB2=$(AR) /OUT:$(NNCLLIB)\$(NNCLLIBRARY) *.obj
#
# debug 32-bit library
#
RMLIB32=$(RM) $(NCLLIB)\$(NCLLIBRARY)
CRELIB321=$(AR) /OUT:$(NCLLIB32)\$(NCLLIBRARY) $(NCLLIB32)\$(NCLLIBRARY) *.obj
CRELIB322=$(AR) /OUT:$(NCLLIB32)\$(NCLLIBRARY) *.obj
#
# nodebug 32-bit library
#
NRMLIB32=$(RM) $(NNCLLIB32)\$(NCLLIBRARY)
NCRELIB321=$(AR) /OUT:$(NNCLLIB32)\$(NNCLLIBRARY) $(NNCLLIB32)\$(NNCLLIBRARY) *.obj
NCRELIB322=$(AR) /OUT:$(NNCLLIB32)\$(NNCLLIBRARY) *.obj
#
#
# Here come some rules on how to deal with the OBJS lists
#
#
# debug rules
#
.SUFFIXES: .c .cpp .f .for .ts .nts .t32 .nt32
.c.ts:
	@set PATH=$(PATH64)
	$(RM) $@
	@echo "*****  MAKING DEBUG VERSION OF $<  *****"
	$(CC) $(CFLAGS) $<
	$(PLACE_OBJ)
	$(TOUCH) $@
	@echo "***** FINISHED BUILDING $< IN $(SRCDIR) *****"
	@echo ""
.cpp.ts:
	@set PATH=$(PATH64)
	$(RM) $@
	@echo "*****  MAKING DEBUG VERSION OF $<  *****"
	$(CPP) $(CPFLAGS) $<
	$(PLACE_OBJ)
	$(TOUCH) $@
	@echo "***** FINISHED BUILDING $< IN $(SRCDIR) *****"
	@echo ""
.f.ts:
	@set PATH=$(PATH64)
	$(RM) $@
	@echo "*****  MAKING DEBUG VERSION OF $*.f  *****"
 	echo $$ FOR $* > tmpf.com
 	$(FORFILTER) tmpf.com $(PWSYS) 
 	$(RM) tmpf.com
	$(F77) $(FFLAGS) $<
	$(PLACE_OBJ)
	$(TOUCH) $@
	@echo "***** FINISHED BUILDING $< IN $(SRCDIR) *****"
	@echo ""
#
#
# nodebug rules
#
.c.nts:
	@set PATH=$(PATH64)
	$(RM) $@
	@echo "*****  MAKING DEBUG VERSION OF $<  *****"
	$(NCC) $(CFLAGS) $<
	$(PLACE_NOBJ)
	$(TOUCH) $@
	@echo "***** FINISHED BUILDING $< IN $(SRCDIR) *****"
	@echo ""
.cpp.nts:
	@set PATH=$(PATH64)
	$(RM) $@
	@echo "*****  MAKING DEBUG VERSION OF $<  *****"
	$(NCPP) $(CPFLAGS) $<
	$(PLACE_NOBJ)
	$(TOUCH) $@
	@echo "***** FINISHED BUILDING $< IN $(SRCDIR) *****"
	@echo ""
.f.nts:
	@set PATH=$(PATH64)
	$(RM) $@
	@echo "*****  MAKING DEBUG VERSION OF $*.f  *****"
#	echo $$ FOR $* > tmpf.com
#	chmod u+rw $<
#	$(FORFILTER) tmpf.com $(PWSYS) 
#	chmod u-w $<
#	$(RM) tmpf.com
	$(NF77) $(FFLAGS) $<
	$(PLACE_NOBJ)
	$(TOUCH) $@
	@echo "***** FINISHED BUILDING $< IN $(SRCDIR) *****"
	@echo ""
#
# 32-bit debug rules
#
.c.t32:
	@set PATH=$(PATH32)
	$(RM) $@
	@echo "*****  MAKING 32-BIT DEBUG VERSION OF $<  *****"
	$(CC) $(CFLAGS) $<
	$(PLACE_OBJ32)
	$(TOUCH) $@
	@echo "***** FINISHED BUILDING $< IN $(SRCDIR) *****"
	@echo ""
.cpp.t32:
	@set PATH=$(PATH32)
	$(RM) $@
	@echo "*****  MAKING 32-BIT DEBUG VERSION OF $<  *****"
	$(CPP) $(CPFLAGS) $<
	$(PLACE_OBJ32)
	$(TOUCH) $@
	@echo "***** FINISHED BUILDING $< IN $(SRCDIR) *****"
	@echo ""
.f.t32:
	@set PATH=$(PATH32)
	$(RM) $@
	@echo "*****  MAKING 32-BIT DEBUG VERSION OF $*.f  *****"
# 	echo $$ FOR $* > tmpf.com
# 	$(FORFILTER) tmpf.com $(PWSYS) 
# 	$(RM) tmpf.com
	$(F77) $(FFLAGS) $<
	$(PLACE_OBJ32)
	$(TOUCH) $@
	@echo "***** FINISHED BUILDING $< IN $(SRCDIR) *****"
	@echo ""
#
# 32-bit nodebug rules
#
.c.nt32:
	@set PATH=$(PATH32)
	$(RM) $@
	@echo "*****  MAKING 32-BIT NODEBUG VERSION OF $<  *****"
	$(NCC) $(CFLAGS) $<
	$(PLACE_NOBJ32)
	$(TOUCH) $@
	@echo "***** FINISHED BUILDING $< IN $(SRCDIR) *****"
	@echo ""
.cpp.nt32:
	@set PATH=$(PATH32)
	$(RM) $@
	@echo "*****  MAKING 32-BIT NODEBUG VERSION OF $<  *****"
	$(NCPP) $(CPFLAGS) $<
	$(PLACE_NOBJ32)
	$(TOUCH) $@
	@echo "***** FINISHED BUILDING $< IN $(SRCDIR) *****"
	@echo ""
.f.nt32:
	@set PATH=$(PATH32)
	$(RM) $@
	@echo "*****  MAKING 32-BIT NODEBUG VERSION OF $*.f  *****"
#	echo $$ FOR $* > tmpf.com
#	chmod u+rw $<
#	$(FORFILTER) tmpf.com $(PWSYS) 
#	chmod u-w $<
#	$(RM) tmpf.com
	$(NF77) $(FFLAGS) $<
	$(PLACE_NOBJ32)
	$(TOUCH) $@
	@echo "***** FINISHED BUILDING $< IN $(SRCDIR) *****"
	@echo ""
#
#-------------------------EOF make.prefix.postworks.wnt -----------------------
#
#
#--------------------------- make.list --------------------------
#
SRCS1= \
GenHelp.rc GenPrompt.rc GenWord.rc GenXfer.rc Pted.rc Pworks.rc gencrypt.rc \
mpost.rc nccs_auth.rc nccs_license.rc nccs_mid.rc pmacro.rc postcomp.rc pwconv.rc \
clnrd.inc compile.inc docum.inc dos.inc license_nt.inc lintol.inc menu.inc \
menuinc.inc ncicom.inc passwd.inc post.inc postworks_nt.inc pregen.inc pted.inc \
ptedpost.inc ptedtool.inc CreateLicDlg.h DialogButton.h DialogForm.h DialogHelp.h DialogItem.h \
DialogPrompt.h DialogTemplate.h GenHelp.h GenPrompt.h GenWord.h GenXfer.h LicenseLstDlg.h \
NpwHeaders.h PCompOptDlg.h PMacroOptDlg.h PWBrowser.h PWMainFrm.h PWMessageBox.h PWScrollView.h \
PWconvOptDlg.h PWorksOptDlg.h PWorksView.h PasswdDlg.h Pcfunc.h Pmfunc.h PostworksDoc.h \
PtdApplication.h PtdBase.h PtdBatch.h PtdButton.h PtdChildTemplate.h PtdChildView.h PtdCommand.h \
PtdCommandArray.h PtdCommandBase.h PtdCommandIfArray.h PtdCommandInterface.h PtdCommandList.h PtdDialogCbData.h PtdDialogManager.h \
PtdDocViewTemplate.h PtdErrorDialogManager.h PtdFileDialogManager.h PtdFileOpenCommand.h PtdFindDialogManager.h PtdFrameWindow.h PtdFunc.h \
PtdGlobal.h PtdIncFileDialogManager.h PtdInfoDialogManager.h PtdList.h PtdMainTemplate.h PtdMainView.h PtdMainWindow.h \
PtdMainWindowArray.h PtdMenuBar.h PtdMenuEntries.h PtdMenuSeparator.h PtdMessageDialogManager.h PtdMotifHelp.h PtdNoUndoCommand.h \
PtdNode.h PtdPArray.h PtdPWoptDialogManager.h PtdProcessDlg.h PtdPromptDialogManager.h PtdProtocol.h PtdQuestionDialogManager.h \
PtdRangeDialogManager.h PtdReseqDialogManager.h PtdString.h PtdTemplate.h PtdUndoCommand.h PtdView.h PtdViewList.h \
PtdWindowStatus.h Ptdmenu.h Pted.h PtedBatch.h PtedChildWindow.h
OBJS1= \
GenHelp.ts GenPrompt.ts GenWord.ts GenXfer.ts Pted.ts Pworks.ts gencrypt.ts \
mpost.ts nccs_auth.ts nccs_license.ts nccs_mid.ts pmacro.ts postcomp.ts pwconv.ts \
clnrd.ts compile.ts docum.ts dos.ts license_nt.ts lintol.ts menu.ts \
menuinc.ts ncicom.ts passwd.ts post.ts postworks_nt.ts pregen.ts pted.ts \
ptedpost.ts ptedtool.ts CreateLicDlg.ts DialogButton.ts DialogForm.ts DialogHelp.ts DialogItem.ts \
DialogPrompt.ts DialogTemplate.ts GenHelp.ts GenPrompt.ts GenWord.ts GenXfer.ts LicenseLstDlg.ts \
NpwHeaders.ts PCompOptDlg.ts PMacroOptDlg.ts PWBrowser.ts PWMainFrm.ts PWMessageBox.ts PWScrollView.ts \
PWconvOptDlg.ts PWorksOptDlg.ts PWorksView.ts PasswdDlg.ts Pcfunc.ts Pmfunc.ts PostworksDoc.ts \
PtdApplication.ts PtdBase.ts PtdBatch.ts PtdButton.ts PtdChildTemplate.ts PtdChildView.ts PtdCommand.ts \
PtdCommandArray.ts PtdCommandBase.ts PtdCommandIfArray.ts PtdCommandInterface.ts PtdCommandList.ts PtdDialogCbData.ts PtdDialogManager.ts \
PtdDocViewTemplate.ts PtdErrorDialogManager.ts PtdFileDialogManager.ts PtdFileOpenCommand.ts PtdFindDialogManager.ts PtdFrameWindow.ts PtdFunc.ts \
PtdGlobal.ts PtdIncFileDialogManager.ts PtdInfoDialogManager.ts PtdList.ts PtdMainTemplate.ts PtdMainView.ts PtdMainWindow.ts \
PtdMainWindowArray.ts PtdMenuBar.ts PtdMenuEntries.ts PtdMenuSeparator.ts PtdMessageDialogManager.ts PtdMotifHelp.ts PtdNoUndoCommand.ts \
PtdNode.ts PtdPArray.ts PtdPWoptDialogManager.ts PtdProcessDlg.ts PtdPromptDialogManager.ts PtdProtocol.ts PtdQuestionDialogManager.ts \
PtdRangeDialogManager.ts PtdReseqDialogManager.ts PtdString.ts PtdTemplate.ts PtdUndoCommand.ts PtdView.ts PtdViewList.ts \
PtdWindowStatus.ts Ptdmenu.ts Pted.ts PtedBatch.ts PtedChildWindow.ts
NOBJS1= \
GenHelp.nts GenPrompt.nts GenWord.nts GenXfer.nts Pted.nts Pworks.nts gencrypt.nts \
mpost.nts nccs_auth.nts nccs_license.nts nccs_mid.nts pmacro.nts postcomp.nts pwconv.nts \
clnrd.nts compile.nts docum.nts dos.nts license_nt.nts lintol.nts menu.nts \
menuinc.nts ncicom.nts passwd.nts post.nts postworks_nt.nts pregen.nts pted.nts \
ptedpost.nts ptedtool.nts CreateLicDlg.h DialogButton.h DialogForm.h DialogHelp.h DialogItem.h \
DialogPrompt.h DialogTemplate.h GenHelp.h GenPrompt.h GenWord.h GenXfer.h LicenseLstDlg.h \
NpwHeaders.h PCompOptDlg.h PMacroOptDlg.h PWBrowser.h PWMainFrm.h PWMessageBox.h PWScrollView.h \
PWconvOptDlg.h PWorksOptDlg.h PWorksView.h PasswdDlg.h Pcfunc.h Pmfunc.h PostworksDoc.h \
PtdApplication.h PtdBase.h PtdBatch.h PtdButton.h PtdChildTemplate.h PtdChildView.h PtdCommand.h \
PtdCommandArray.h PtdCommandBase.h PtdCommandIfArray.h PtdCommandInterface.h PtdCommandList.h PtdDialogCbData.h PtdDialogManager.h \
PtdDocViewTemplate.h PtdErrorDialogManager.h PtdFileDialogManager.h PtdFileOpenCommand.h PtdFindDialogManager.h PtdFrameWindow.h PtdFunc.h \
PtdGlobal.h PtdIncFileDialogManager.h PtdInfoDialogManager.h PtdList.h PtdMainTemplate.h PtdMainView.h PtdMainWindow.h \
PtdMainWindowArray.h PtdMenuBar.h PtdMenuEntries.h PtdMenuSeparator.h PtdMessageDialogManager.h PtdMotifHelp.h PtdNoUndoCommand.h \
PtdNode.h PtdPArray.h PtdPWoptDialogManager.h PtdProcessDlg.h PtdPromptDialogManager.h PtdProtocol.h PtdQuestionDialogManager.h \
PtdRangeDialogManager.h PtdReseqDialogManager.h PtdString.h PtdTemplate.h PtdUndoCommand.h PtdView.h PtdViewList.h \
PtdWindowStatus.h Ptdmenu.h Pted.h PtedBatch.h PtedChildWindow.h
OBJS321= \
GenHelp.t32 GenPrompt.t32 GenWord.t32 GenXfer.t32 Pted.t32 Pworks.t32 gencrypt.t32 \
mpost.t32 nccs_auth.t32 nccs_license.t32 nccs_mid.t32 pmacro.t32 postcomp.t32 pwconv.t32 \
clnrd.t32 compile.t32 docum.t32 dos.t32 license_nt.t32 lintol.t32 menu.t32 \
menuinc.t32 ncicom.t32 passwd.t32 post.t32 postworks_nt.t32 pregen.t32 pted.t32 \
ptedpost.t32 ptedtool.t32 CreateLicDlg.t32 DialogButton.t32 DialogForm.t32 DialogHelp.t32 DialogItem.t32 \
DialogPrompt.t32 DialogTemplate.t32 GenHelp.t32 GenPrompt.t32 GenWord.t32 GenXfer.t32 LicenseLstDlg.t32 \
NpwHeaders.t32 PCompOptDlg.t32 PMacroOptDlg.t32 PWBrowser.t32 PWMainFrm.t32 PWMessageBox.t32 PWScrollView.t32 \
PWconvOptDlg.t32 PWorksOptDlg.t32 PWorksView.t32 PasswdDlg.t32 Pcfunc.t32 Pmfunc.t32 PostworksDoc.t32 \
PtdApplication.t32 PtdBase.t32 PtdBatch.t32 PtdButton.t32 PtdChildTemplate.t32 PtdChildView.t32 PtdCommand.t32 \
PtdCommandArray.t32 PtdCommandBase.t32 PtdCommandIfArray.t32 PtdCommandInterface.t32 PtdCommandList.t32 PtdDialogCbData.t32 PtdDialogManager.t32 \
PtdDocViewTemplate.t32 PtdErrorDialogManager.t32 PtdFileDialogManager.t32 PtdFileOpenCommand.t32 PtdFindDialogManager.t32 PtdFrameWindow.t32 PtdFunc.t32 \
PtdGlobal.t32 PtdIncFileDialogManager.t32 PtdInfoDialogManager.t32 PtdList.t32 PtdMainTemplate.t32 PtdMainView.t32 PtdMainWindow.t32 \
PtdMainWindowArray.t32 PtdMenuBar.t32 PtdMenuEntries.t32 PtdMenuSeparator.t32 PtdMessageDialogManager.t32 PtdMotifHelp.t32 PtdNoUndoCommand.t32 \
PtdNode.t32 PtdPArray.t32 PtdPWoptDialogManager.t32 PtdProcessDlg.t32 PtdPromptDialogManager.t32 PtdProtocol.t32 PtdQuestionDialogManager.t32 \
PtdRangeDialogManager.t32 PtdReseqDialogManager.t32 PtdString.t32 PtdTemplate.t32 PtdUndoCommand.t32 PtdView.t32 PtdViewList.t32 \
PtdWindowStatus.t32 Ptdmenu.t32 Pted.t32 PtedBatch.t32 PtedChildWindow.t32
NOBJS321= \
GenHelp.nt32 GenPrompt.nt32 GenWord.nt32 GenXfer.nt32 Pted.nt32 Pworks.nt32 gencrypt.nt32 \
mpost.nt32 nccs_auth.nt32 nccs_license.nt32 nccs_mid.nt32 pmacro.nt32 postcomp.nt32 pwconv.nt32 \
clnrd.nt32 compile.nt32 docum.nt32 dos.nt32 license_nt.nt32 lintol.nt32 menu.nt32 \
menuinc.nt32 ncicom.nt32 passwd.nt32 post.nt32 postworks_nt.nt32 pregen.nt32 pted.nt32 \
ptedpost.nt32 ptedtool.nt32 CreateLicDlg.h DialogButton.h DialogForm.h DialogHelp.h DialogItem.h \
DialogPrompt.h DialogTemplate.h GenHelp.h GenPrompt.h GenWord.h GenXfer.h LicenseLstDlg.h \
NpwHeaders.h PCompOptDlg.h PMacroOptDlg.h PWBrowser.h PWMainFrm.h PWMessageBox.h PWScrollView.h \
PWconvOptDlg.h PWorksOptDlg.h PWorksView.h PasswdDlg.h Pcfunc.h Pmfunc.h PostworksDoc.h \
PtdApplication.h PtdBase.h PtdBatch.h PtdButton.h PtdChildTemplate.h PtdChildView.h PtdCommand.h \
PtdCommandArray.h PtdCommandBase.h PtdCommandIfArray.h PtdCommandInterface.h PtdCommandList.h PtdDialogCbData.h PtdDialogManager.h \
PtdDocViewTemplate.h PtdErrorDialogManager.h PtdFileDialogManager.h PtdFileOpenCommand.h PtdFindDialogManager.h PtdFrameWindow.h PtdFunc.h \
PtdGlobal.h PtdIncFileDialogManager.h PtdInfoDialogManager.h PtdList.h PtdMainTemplate.h PtdMainView.h PtdMainWindow.h \
PtdMainWindowArray.h PtdMenuBar.h PtdMenuEntries.h PtdMenuSeparator.h PtdMessageDialogManager.h PtdMotifHelp.h PtdNoUndoCommand.h \
PtdNode.h PtdPArray.h PtdPWoptDialogManager.h PtdProcessDlg.h PtdPromptDialogManager.h PtdProtocol.h PtdQuestionDialogManager.h \
PtdRangeDialogManager.h PtdReseqDialogManager.h PtdString.h PtdTemplate.h PtdUndoCommand.h PtdView.h PtdViewList.h \
PtdWindowStatus.h Ptdmenu.h Pted.h PtedBatch.h PtedChildWindow.h
SRCS2= \
PtedEditCmd.h PtedFileDialog.h PtedFindReplaceDialog.h PtedIncFileDialog.h PtedMainWindow.h PtedPWOptDialog.h PtedProcessDlg.h \
PtedRangeBox.h PtedReseqDialog.h PtedSetDialog.h PtedSetupPageDialog.h PtedStatusDlg.h PtedStdAfx.h PtedTextBuffer.h \
PtedTextView.h PtedWindow.h Pted_nt.h Ptedres.h Pvfunc.h PwAboutDlg.h PwApplication.h \
PwCommand.h PwDynFormDlg.h PwDynPromptDlg.h PwDynPushDlg.h PwFDialogevent.h PwHelpDlg.h PwMainDocument.h \
PwMainFrame.h PwMainTemplate.h PwMainView.h PwMenuEntries.h PwNTAboutDlg.h PwStdAfx.h PwTextDlg.h \
PwconvView.h Pwfunc.h Pworks.h Pworksres.h Pwwindef.h SUiDynDlg.h authres.h \
compres.h cryptres.h gencrypt.h gencryptDlg.h genhelpView.h genpromptView.h genwordView.h \
genxferView.h helpres.h licfc.h licres.h mpost.h mpostres.h nccs_auth.h \
nccs_authDlg.h nccs_license.h nccs_licenseDlg.h nccs_mid.h pmacro.h pmacroView.h pmacrores.h \
postcomp.h postcompView.h promptres.h propsht.h pwconv.h pwconvres.h pwenv.h \
resource_nt.h scribe_nt.h shtemp.h spromeps.h stdafx.h wordres.h xferres.h
OBJS2= \
PtedEditCmd.ts PtedFileDialog.ts PtedFindReplaceDialog.ts PtedIncFileDialog.ts PtedMainWindow.ts PtedPWOptDialog.ts PtedProcessDlg.ts \
PtedRangeBox.ts PtedReseqDialog.ts PtedSetDialog.ts PtedSetupPageDialog.ts PtedStatusDlg.ts PtedStdAfx.ts PtedTextBuffer.ts \
PtedTextView.ts PtedWindow.ts Pted_nt.ts Ptedres.ts Pvfunc.ts PwAboutDlg.ts PwApplication.ts \
PwCommand.ts PwDynFormDlg.ts PwDynPromptDlg.ts PwDynPushDlg.ts PwFDialogevent.ts PwHelpDlg.ts PwMainDocument.ts \
PwMainFrame.ts PwMainTemplate.ts PwMainView.ts PwMenuEntries.ts PwNTAboutDlg.ts PwStdAfx.ts PwTextDlg.ts \
PwconvView.ts Pwfunc.ts Pworks.ts Pworksres.ts Pwwindef.ts SUiDynDlg.ts authres.ts \
compres.ts cryptres.ts gencrypt.ts gencryptDlg.ts genhelpView.ts genpromptView.ts genwordView.ts \
genxferView.ts helpres.ts licfc.ts licres.ts mpost.ts mpostres.ts nccs_auth.ts \
nccs_authDlg.ts nccs_license.ts nccs_licenseDlg.ts nccs_mid.ts pmacro.ts pmacroView.ts pmacrores.ts \
postcomp.ts postcompView.ts promptres.ts propsht.ts pwconv.ts pwconvres.ts pwenv.ts \
resource_nt.ts scribe_nt.ts shtemp.ts spromeps.ts stdafx.ts wordres.ts xferres.ts
NOBJS2= \
PtedEditCmd.h PtedFileDialog.h PtedFindReplaceDialog.h PtedIncFileDialog.h PtedMainWindow.h PtedPWOptDialog.h PtedProcessDlg.h \
PtedRangeBox.h PtedReseqDialog.h PtedSetDialog.h PtedSetupPageDialog.h PtedStatusDlg.h PtedStdAfx.h PtedTextBuffer.h \
PtedTextView.h PtedWindow.h Pted_nt.h Ptedres.h Pvfunc.h PwAboutDlg.h PwApplication.h \
PwCommand.h PwDynFormDlg.h PwDynPromptDlg.h PwDynPushDlg.h PwFDialogevent.h PwHelpDlg.h PwMainDocument.h \
PwMainFrame.h PwMainTemplate.h PwMainView.h PwMenuEntries.h PwNTAboutDlg.h PwStdAfx.h PwTextDlg.h \
PwconvView.h Pwfunc.h Pworks.h Pworksres.h Pwwindef.h SUiDynDlg.h authres.h \
compres.h cryptres.h gencrypt.h gencryptDlg.h genhelpView.h genpromptView.h genwordView.h \
genxferView.h helpres.h licfc.h licres.h mpost.h mpostres.h nccs_auth.h \
nccs_authDlg.h nccs_license.h nccs_licenseDlg.h nccs_mid.h pmacro.h pmacroView.h pmacrores.h \
postcomp.h postcompView.h promptres.h propsht.h pwconv.h pwconvres.h pwenv.h \
resource_nt.h scribe_nt.h shtemp.h spromeps.h stdafx.h wordres.h xferres.h
OBJS322= \
PtedEditCmd.t32 PtedFileDialog.t32 PtedFindReplaceDialog.t32 PtedIncFileDialog.t32 PtedMainWindow.t32 PtedPWOptDialog.t32 PtedProcessDlg.t32 \
PtedRangeBox.t32 PtedReseqDialog.t32 PtedSetDialog.t32 PtedSetupPageDialog.t32 PtedStatusDlg.t32 PtedStdAfx.t32 PtedTextBuffer.t32 \
PtedTextView.t32 PtedWindow.t32 Pted_nt.t32 Ptedres.t32 Pvfunc.t32 PwAboutDlg.t32 PwApplication.t32 \
PwCommand.t32 PwDynFormDlg.t32 PwDynPromptDlg.t32 PwDynPushDlg.t32 PwFDialogevent.t32 PwHelpDlg.t32 PwMainDocument.t32 \
PwMainFrame.t32 PwMainTemplate.t32 PwMainView.t32 PwMenuEntries.t32 PwNTAboutDlg.t32 PwStdAfx.t32 PwTextDlg.t32 \
PwconvView.t32 Pwfunc.t32 Pworks.t32 Pworksres.t32 Pwwindef.t32 SUiDynDlg.t32 authres.t32 \
compres.t32 cryptres.t32 gencrypt.t32 gencryptDlg.t32 genhelpView.t32 genpromptView.t32 genwordView.t32 \
genxferView.t32 helpres.t32 licfc.t32 licres.t32 mpost.t32 mpostres.t32 nccs_auth.t32 \
nccs_authDlg.t32 nccs_license.t32 nccs_licenseDlg.t32 nccs_mid.t32 pmacro.t32 pmacroView.t32 pmacrores.t32 \
postcomp.t32 postcompView.t32 promptres.t32 propsht.t32 pwconv.t32 pwconvres.t32 pwenv.t32 \
resource_nt.t32 scribe_nt.t32 shtemp.t32 spromeps.t32 stdafx.t32 wordres.t32 xferres.t32
NOBJS322= \
PtedEditCmd.h PtedFileDialog.h PtedFindReplaceDialog.h PtedIncFileDialog.h PtedMainWindow.h PtedPWOptDialog.h PtedProcessDlg.h \
PtedRangeBox.h PtedReseqDialog.h PtedSetDialog.h PtedSetupPageDialog.h PtedStatusDlg.h PtedStdAfx.h PtedTextBuffer.h \
PtedTextView.h PtedWindow.h Pted_nt.h Ptedres.h Pvfunc.h PwAboutDlg.h PwApplication.h \
PwCommand.h PwDynFormDlg.h PwDynPromptDlg.h PwDynPushDlg.h PwFDialogevent.h PwHelpDlg.h PwMainDocument.h \
PwMainFrame.h PwMainTemplate.h PwMainView.h PwMenuEntries.h PwNTAboutDlg.h PwStdAfx.h PwTextDlg.h \
PwconvView.h Pwfunc.h Pworks.h Pworksres.h Pwwindef.h SUiDynDlg.h authres.h \
compres.h cryptres.h gencrypt.h gencryptDlg.h genhelpView.h genpromptView.h genwordView.h \
genxferView.h helpres.h licfc.h licres.h mpost.h mpostres.h nccs_auth.h \
nccs_authDlg.h nccs_license.h nccs_licenseDlg.h nccs_mid.h pmacro.h pmacroView.h pmacrores.h \
postcomp.h postcompView.h promptres.h propsht.h pwconv.h pwconvres.h pwenv.h \
resource_nt.h scribe_nt.h shtemp.h spromeps.h stdafx.h wordres.h xferres.h
SRCS3= \

OBJS3= \

NOBJS3= \

OBJS323= \

NOBJS323= \

SRCS4= \

OBJS4= \

NOBJS4= \

OBJS324= \

NOBJS324= \

SRCS5= \

OBJS5= \

NOBJS5= \

OBJS325= \

NOBJS325= \

SRCS6= \

OBJS6= \

NOBJS6= \

OBJS326= \

NOBJS326= \

SRCS7= \

OBJS7= \

NOBJS7= \

OBJS327= \

NOBJS327= \

SRCS8= \

OBJS8= \

NOBJS8= \

OBJS328= \

NOBJS328= \

SRCS9= \

OBJS9= \

NOBJS9= \

OBJS329= \

NOBJS329= \

SRCS=$(SRCS1) $(SRCS2) $(SRCS3) $(SRCS4) $(SRCS5) $(SRCS6) \
     $(SRCS7) $(SRCS8) $(SRCS9)
OBJS=$(OBJS1) $(OBJS2) $(OBJS3) $(OBJS4) $(OBJS5) $(OBJS6) \
     $(OBJS7) $(OBJS8) $(OBJS9)
NOBJS=$(NOBJS1) $(NOBJS2) $(NOBJS3) $(NOBJS4) $(NOBJS5) $(NOBJS6) \
     $(NOBJS7) $(NOBJS8) $(NOBJS9)
OBJS32=$(OBJS321) $(OBJS322) $(OBJS323) $(OBJS324) $(OBJS325) \
     $(OBJS326) $(OBJS327) $(OBJS328) $(OBJS329)
NOBJS32=$(NOBJS321) $(NOBJS322) $(NOBJS323) $(NOBJS324) $(NOBJS325) \
     $(NOBJS326) $(NOBJS327) $(NOBJS328) $(NOBJS329)
#
# Define the NCL directory this makefile should run in.
#
DIR=postworks
SRCDIR=pwinc
NCLLIBRARY=pwinc.lib
NNCLLIBRARY=npwinc.lib
NCLLIBRARY32=pwinc32.lib
NNCLLIBRARY32=npwinc32.lib

#
#---------------------------EOF make.list --------------------------
#
#
#--------------------------- pwinc.rules.wnt --------------------------
#
PLACE_OBJS= @echo
PLACE_NOBJS= @echo
#
# make debug stuff
#
debugrel: makeall
nodebugrel: makeall
nmakeall: makeall

makeall: cleanup $(SRCS) GenHelp.res GenPrompt.res GenWord.res GenXfer.res \
	Pted.res Pworks.res gencrypt.res mpost.res nccs_auth.res nccs_license.res \
	nccs_mid.res pmacro.res postcomp.res

.rc.res:
	-$(RM) $(NNCLLIB)\$*.res
	$(RSC) $(RCFLAGS) /fo $*.res $<
	$(MV) $*.res $(NNCLLIB)

#
#---------------------------EOF pwinc.rules.wnt --------------------------
#
#
#--------------------------- make.suffix.wnt --------------------------
#
cleanup: 
!IF "$(SRCS1)" != ""
	-@$(RM) $(SRCS1) 
	-@$(RM) $(OBJS1) 
	-@$(RM) $(NOBJS1) 
	-@$(RM) $(OBJS321) 
	-@$(RM) $(NOBJS321) 
!ENDIF
!IF "$(SRCS2)" != ""
	-@$(RM) $(SRCS2) 
	-@$(RM) $(OBJS2) 
	-@$(RM) $(NOBJS2) 
	-@$(RM) $(OBJS322) 
	-@$(RM) $(NOBJS322) 
!ENDIF
!IF "$(SRCS3)" != ""
	-@$(RM) $(SRCS3) 
	-@$(RM) $(OBJS3) 
	-@$(RM) $(NOBJS3) 
	-@$(RM) $(OBJS323) 
	-@$(RM) $(NOBJS323) 
!ENDIF
!IF "$(SRCS4)" != ""
	-@$(RM) $(SRCS4) 
	-@$(RM) $(OBJS4) 
	-@$(RM) $(NOBJS4) 
	-@$(RM) $(OBJS324) 
	-@$(RM) $(NOBJS324) 
!ENDIF
!IF "$(SRCS5)" != ""
	-@$(RM) $(SRCS5) 
	-@$(RM) $(OBJS5) 
	-@$(RM) $(NOBJS5) 
	-@$(RM) $(OBJS325) 
	-@$(RM) $(NOBJS325) 
!ENDIF
!IF "$(SRCS6)" != ""
	-@$(RM) $(SRCS6) 
	-@$(RM) $(OBJS6) 
	-@$(RM) $(NOBJS6) 
	-@$(RM) $(OBJS326) 
	-@$(RM) $(NOBJS326) 
!ENDIF
!IF "$(SRCS7)" != ""
	-@$(RM) $(SRCS7) 
	-@$(RM) $(OBJS7) 
	-@$(RM) $(NOBJS7) 
	-@$(RM) $(OBJS327) 
	-@$(RM) $(NOBJS327) 
!ENDIF
!IF "$(SRCS8)" != ""
	-@$(RM) $(SRCS8) 
	-@$(RM) $(OBJS8) 
	-@$(RM) $(NOBJS8) 
	-@$(RM) $(OBJS328) 
	-@$(RM) $(NOBJS328) 
!ENDIF
!IF "$(SRCS9)" != ""
	-@$(RM) $(SRCS9) 
	-@$(RM) $(OBJS9) 
	-@$(RM) $(NOBJS9) 
	-@$(RM) $(OBJS329) 
	-@$(RM) $(NOBJS397) 
!ENDIF
	-@$(RM) *.obj
!IF "$(SYMLIB)" != ""
	-@$(RM) $(SYMLIB)
!ENDIF
#
wait:
#	@$(NCLPATH)\ncluni\sleep 10
#
sources: $(SRCS)
$(SRCS):
	$(GET) $(SRCDIR)/$@
#
#---------------------------EOF make.suffix.wnt --------------------------
#
