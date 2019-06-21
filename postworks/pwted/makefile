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
badstat.f cirted.f clatocl.f clfile.f cltocla.f cycted.f fmtcod.f \
mottap.f motted.f pstcmd.f ptdffunc.f ptdstubs.f regfmt.f settool.f \
spcblk.f tapblk.f tedapt.f tedmch.f PtdGlobal.c PtdcFunc.c clload.c \
PtedBatch.cpp PtedChildWindow.cpp PtedCom.cpp PtedFileDialog.cpp PtedFindReplaceDialog.cpp PtedIncFileDialog.cpp PtedMainWindow.cpp \
PtedPWOptDialog.cpp PtedProcessDlg.cpp PtedRangeBox.cpp PtedReseqDialog.cpp PtedSetDialog.cpp PtedSetupPageDialog.cpp PtedStatusDlg.cpp \
PtedTextBuffer.cpp PtedTextBuffer2.cpp PtedTextBuffer3.cpp PtedTextView.cpp PtedTextView2.cpp PtedTextView3.cpp PtedTextView4.cpp \
PtedTextView5.cpp PtedWindow.cpp PtedWindow2.cpp PtedWindow3.cpp PtedWindow4.cpp PtedWindow5.cpp Ptedctl.cpp \
StdAfx.cpp
OBJS1= \
badstat.ts cirted.ts clatocl.ts clfile.ts cltocla.ts cycted.ts fmtcod.ts \
mottap.ts motted.ts pstcmd.ts ptdffunc.ts ptdstubs.ts regfmt.ts settool.ts \
spcblk.ts tapblk.ts tedapt.ts tedmch.ts PtdGlobal.ts PtdcFunc.ts clload.ts \
PtedBatch.ts PtedChildWindow.ts PtedCom.ts PtedFileDialog.ts PtedFindReplaceDialog.ts PtedIncFileDialog.ts PtedMainWindow.ts \
PtedPWOptDialog.ts PtedProcessDlg.ts PtedRangeBox.ts PtedReseqDialog.ts PtedSetDialog.ts PtedSetupPageDialog.ts PtedStatusDlg.ts \
PtedTextBuffer.ts PtedTextBuffer2.ts PtedTextBuffer3.ts PtedTextView.ts PtedTextView2.ts PtedTextView3.ts PtedTextView4.ts \
PtedTextView5.ts PtedWindow.ts PtedWindow2.ts PtedWindow3.ts PtedWindow4.ts PtedWindow5.ts Ptedctl.ts \
StdAfx.ts
NOBJS1= \
badstat.nts cirted.nts clatocl.nts clfile.nts cltocla.nts cycted.nts fmtcod.nts \
mottap.nts motted.nts pstcmd.nts ptdffunc.nts ptdstubs.nts regfmt.nts settool.nts \
spcblk.nts tapblk.nts tedapt.nts tedmch.nts PtdGlobal.nts PtdcFunc.nts clload.nts \
PtedBatch.nts PtedChildWindow.nts PtedCom.nts PtedFileDialog.nts PtedFindReplaceDialog.nts PtedIncFileDialog.nts PtedMainWindow.nts \
PtedPWOptDialog.nts PtedProcessDlg.nts PtedRangeBox.nts PtedReseqDialog.nts PtedSetDialog.nts PtedSetupPageDialog.nts PtedStatusDlg.nts \
PtedTextBuffer.nts PtedTextBuffer2.nts PtedTextBuffer3.nts PtedTextView.nts PtedTextView2.nts PtedTextView3.nts PtedTextView4.nts \
PtedTextView5.nts PtedWindow.nts PtedWindow2.nts PtedWindow3.nts PtedWindow4.nts PtedWindow5.nts Ptedctl.nts \
StdAfx.nts
OBJS321= \
badstat.t32 cirted.t32 clatocl.t32 clfile.t32 cltocla.t32 cycted.t32 fmtcod.t32 \
mottap.t32 motted.t32 pstcmd.t32 ptdffunc.t32 ptdstubs.t32 regfmt.t32 settool.t32 \
spcblk.t32 tapblk.t32 tedapt.t32 tedmch.t32 PtdGlobal.t32 PtdcFunc.t32 clload.t32 \
PtedBatch.t32 PtedChildWindow.t32 PtedCom.t32 PtedFileDialog.t32 PtedFindReplaceDialog.t32 PtedIncFileDialog.t32 PtedMainWindow.t32 \
PtedPWOptDialog.t32 PtedProcessDlg.t32 PtedRangeBox.t32 PtedReseqDialog.t32 PtedSetDialog.t32 PtedSetupPageDialog.t32 PtedStatusDlg.t32 \
PtedTextBuffer.t32 PtedTextBuffer2.t32 PtedTextBuffer3.t32 PtedTextView.t32 PtedTextView2.t32 PtedTextView3.t32 PtedTextView4.t32 \
PtedTextView5.t32 PtedWindow.t32 PtedWindow2.t32 PtedWindow3.t32 PtedWindow4.t32 PtedWindow5.t32 Ptedctl.t32 \
StdAfx.t32
NOBJS321= \
badstat.nt32 cirted.nt32 clatocl.nt32 clfile.nt32 cltocla.nt32 cycted.nt32 fmtcod.nt32 \
mottap.nt32 motted.nt32 pstcmd.nt32 ptdffunc.nt32 ptdstubs.nt32 regfmt.nt32 settool.nt32 \
spcblk.nt32 tapblk.nt32 tedapt.nt32 tedmch.nt32 PtdGlobal.nt32 PtdcFunc.nt32 clload.nt32 \
PtedBatch.nt32 PtedChildWindow.nt32 PtedCom.nt32 PtedFileDialog.nt32 PtedFindReplaceDialog.nt32 PtedIncFileDialog.nt32 PtedMainWindow.nt32 \
PtedPWOptDialog.nt32 PtedProcessDlg.nt32 PtedRangeBox.nt32 PtedReseqDialog.nt32 PtedSetDialog.nt32 PtedSetupPageDialog.nt32 PtedStatusDlg.nt32 \
PtedTextBuffer.nt32 PtedTextBuffer2.nt32 PtedTextBuffer3.nt32 PtedTextView.nt32 PtedTextView2.nt32 PtedTextView3.nt32 PtedTextView4.nt32 \
PtedTextView5.nt32 PtedWindow.nt32 PtedWindow2.nt32 PtedWindow3.nt32 PtedWindow4.nt32 PtedWindow5.nt32 Ptedctl.nt32 \
StdAfx.nt32
SRCS2= \

OBJS2= \

NOBJS2= \

OBJS322= \

NOBJS322= \

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
SRCDIR=pwted
NCLLIBRARY=pwted.lib
NNCLLIBRARY=npwted.lib
NCLLIBRARY32=pwted32.lib
NNCLLIBRARY32=npwted32.lib

#
#---------------------------EOF make.list --------------------------
#
#
#--------------------------- make.middle.postworks.wnt -----------------------
#
#
# release labels
#
debugrel: makeall
nodebugrel: nmakeall
makeall: $(OBJS) mklib
nmakeall: cleanup $(SRCS) wait $(NOBJS) nmklib makeall makeall32 nmakeall32
makeall32: $(OBJS32) mklib32
nmakeall32: $(NOBJS32) nmklib32
updatelib: $(NCLLIBRARY)
updatenlib: $(NNCLLIBRARY) updatelib updatelib32 updatenlib32
updatelib32: $(NCLLIBRARY32)
updatenlib32: $(NNCLLIBRARY32)
#
# Library labels
#
mklib: $(OBJS)
	$(CRELIB2)
	-$(RM) *.obj
!IF "$(SYMLIB)" != ""
	$(RM) $(SYMLIB)
!ENDIF

nmklib: $(NOBJS)
	$(NCRELIB2)
	-$(RM) *.obj
!IF "$(SYMLIB)" != ""
	$(RM) $(SYMLIB)
!ENDIF

mklib32: $(OBJS)
	$(CRELIB322)
	-$(RM) *.obj
!IF "$(SYMLIB)" != ""
	$(RM) $(SYMLIB)
!ENDIF

nmklib32: $(NOBJS)
	$(NCRELIB322)
	-$(RM) *.obj
!IF "$(SYMLIB)" != ""
	$(RM) $(SYMLIB)
!ENDIF

$(NCLLIBRARY): $(OBJS)
	$(CRELIB1)
	-$(RM) *.obj
!IF "$(SYMLIB)" != ""
	$(RM) $(SYMLIB)
!ENDIF

$(NNCLLIBRARY): $(NOBJS)
	$(NCRELIB1)
	-$(RM) *.obj
!IF "$(SYMLIB)" != ""
	$(RM) $(SYMLIB)
!ENDIF

$(NCLLIBRARY32): $(OBJS32)
	$(CRELIB321)
	-$(RM) *.obj
!IF "$(SYMLIB)" != ""
	$(RM) $(SYMLIB)
!ENDIF

$(NNCLLIBRARY32): $(NOBJS32)
	$(NCRELIB321)
	-$(RM) *.obj
!IF "$(SYMLIB)" != ""
	$(RM) $(SYMLIB)
!ENDIF

arx:
	$(RM) *.o
!IF "$(SYMLIB)" != ""
	$(RM) $(SYMLIB)
!ENDIF
	$(ARX) $(NCLLIB)/$(NCLLIBRARY)
#
# do nothing with .h files
#
.inc.ts:
	touch $*.ts
	@echo "***** FINISHED BUILDING $*.ts IN $(SRCDIR) *****"
.inc.nts:
	touch $*.nts
	@echo "***** FINISHED BUILDING $*.nts IN $(SRCDIR) *****"

.h.ts:
	touch $*.ts
	@echo "***** FINISHED BUILDING $*.ts IN $(SRCDIR) *****"
.h.nts:
	touch $*.nts
	@echo "***** FINISHED BUILDING $*.nts IN $(SRCDIR) *****"

.rc.res:
	-$(RM) $(NNCLLIB)\$*.res
	$(RSC) $(RCFLAGS) /fo $*.res $<
	$(MV) $*.res $(NNCLLIB)

#
#-------------------------EOF make.middle.postworks.wnt -----------------------
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
