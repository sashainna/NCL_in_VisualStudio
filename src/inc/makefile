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
LI_DIR=C:\mworks73
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

DEVCFLAGS=/nologo /EHsc /D "WIN32" /D "_WINDOWS" /D "WINVER=0x601" \
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
#--------------------------- make.prefix.src.wnt --------------------------
#
NCLLIB=$(NCLPATH)\$(LIB)
NNCLLIB=$(NCLPATH)\$(NLIB)
NCLLIB32=$(NCLPATH)\$(LIB32)
NNCLLIB32=$(NCLPATH)\$(NLIB32)
NCLBIN=$(NCLPATH)\$(BIN)
NCLSBIN=$(NCLPATH)\$(SBIN)
#
MUTOOL=$(NCLBIN)\mu
ITRTOOL=$(NCLBIN)\itr
MSGTOOL=$(NCLBIN)\msg -u
DDLTOOL=$(NCLBIN)\ddltool
DDLTOOL32=$(NCLBIN)\ddltool32
#
# NCL includes, libraries and tools
#
NCLINC=$(NCLPATH)\src\inc
NCLFILES=$(NCLPATH)\nclfiles
VMDIR=C:\VoluMillSDK_70
VMSDKINC=$(VMDIR)\Code\Common  /I $(VMDIR)\Code\VoluMillResources \
   /I $(VMDIR)\ThirdParty\boost \
   /I $(VMDIR)\ThirdParty /I $(VMDIR)\ThirdParty\Loki\include \
   /I $(VMDIR)\ThirdParty\threadpool\include
SMINC=C:\Program Files (x86)\3Dconnexion\3DxWare SDK\inc
#
FORFILTER=$(NCLBIN)\filter
#
CINCLUDE=/I $(NCLINC) /I $(LI_DIR)\include /I $(LI_DIR)\include\$(LI_ARCH) \
	/I $(VMSDKINC)
CPINCLUDE=$(CINCLUDE) /I "$(VCD3)\include" /I "$(VCD2)\Include" \
   /I "$(VCD1)/MFC/Include" /I "$(VCD1)/ATL/Include" /I "$(SMINC)"
RCINCLUDE=/i "$(NCLINC)" /i "$(NNCLLIB)\obj" /i "$(VCD1)\MFC\Include"

CFLAGS=$(CINCLUDE) $(DEVCFLAGS) $(UU_COMP) $(UU_PREC) $(UU_SEGDATA) $(UU_DEBUG) $(UU_NCLIPV)
CPFLAGS=$(CPINCLUDE) $(DEVCPFLAGS) $(UU_COMP) $(UU_PREC) $(UU_SEGDATA) $(UU_DEBUG) $(UU_NCLIPV)
FFLAGS=$(FINCLUDE) $(DEVFFLAGS)
RCFLAGS=$(RCINCLUDE) $(DEVRCFLAGS)
#
# Compile variations
#
CC=cl $(DEBUG)
CPP=cl $(DEBUG)
F77=ifort $(DEB) $(FFLAGS) -c 
NCC=cl $(NDEBUG)
NCPP=cl $(NDEBUG)
NF77=ifort $(NDEB) $(FFLAGS) -c 
RSC=rc
#
# define some general things
#
AR = lib
CP=copy
RM=erase
MV=move
TOUCH=date /t >
#
# debug library
#
RMLIB=$(RM) $(NCLLIB)\$@
CRELIB1=$(AR) /OUT:$(NCLLIB)\$(NCLLIBRARY) $(NCLLIB)\$(NCLLIBRARY) *.obj
CRELIB2=$(AR) /OUT:$(NCLLIB)\$(NCLLIBRARY) *.obj
#
# nodebug library
#
NRMLIB=$(RM) $(NNCLLIB)\$@ 
NCRELIB1=$(AR) /OUT:$(NNCLLIB)\$(NNCLLIBRARY) $(NNCLLIB)\$(NNCLLIBRARY) *.obj
NCRELIB2=$(AR) /OUT:$(NNCLLIB)\$(NNCLLIBRARY) *.obj
#
# debug 32-bit library
#
RMLIB32=$(RM) $(NCLLIB32)\$@
CRELIB321=$(AR) /OUT:$(NCLLIB32)\$(NCLLIBRARY) $(NCLLIB32)\$(NCLLIBRARY) *.obj
CRELIB322=$(AR) /OUT:$(NCLLIB32)\$(NCLLIBRARY) *.obj
#
# nodebug 32-bit library
#
NRMLIB32=$(RM) $(NNCLLIB32)\$@ 
NCRELIB321=$(AR) /OUT:$(NNCLLIB32)\$(NNCLLIBRARY) $(NNCLLIB32)\$(NNCLLIBRARY) *.obj
NCRELIB322=$(AR) /OUT:$(NNCLLIB32)\$(NNCLLIBRARY) *.obj
#
#
# Here come some rules on how to deal with the OBJS lists
#
#
# debug rules
#
.c.ts:
	@set PATH=$(PATH64)
	$(RM) $@
	@echo "*****  MAKING DEBUG VERSION OF $*.c  *****"
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
	@echo "*****  MAKING DEBUG VERSION OF $<  *****"
	$(F77) $(FFLAGS) $<
	$(PLACE_OBJ)
	$(TOUCH) $@
	@echo "***** FINISHED BUILDING $< IN $(SRCDIR) *****"
	@echo ""

.for.ts: 
	@set PATH=$(PATH64)
	$(RM) $@
	@echo "*****  MAKING DEBUG VERSION OF $<  *****"
	$(FORFILTER) $<,$(FSYS)
	$(F77) $(FFLAGS) $*.f
	$(RM) $*.f
	$(PLACE_OBJ)
	$(TOUCH) $@
	@echo "***** FINISHED BUILDING $< IN $(SRCDIR) *****"
	@echo ""

.itr.ts: 
	@set PATH=$(PATH64)
	$(RM) $@
	@echo "*****  MAKING DEBUG VERSION OF $<  *****"
	$(ITRTOOL) $<
	$(CC) $(CFLAGS) $*.c
	$(RM) *.tmp $*.c
	$(PLACE_OBJ)
	$(TOUCH) $@
	@echo "***** FINISHED BUILDING $< IN $(SRCDIR) *****"
	@echo ""
	
.msg.ts: 
	@set PATH=$(PATH64)
	$(RM) $@
	@echo "*****  MAKING DEBUG VERSION OF $<  *****"
	$(MSGTOOL) $<
	$(RM) *.tmp
	$(RM) $*.hlp
#	$(MV) $*.hlp $(NCLFILES)
	$(CC) $(CFLAGS) $*.c
	$(RM) *.tmp $*.c
	$(PLACE_OBJ)
	$(TOUCH) $@
	@echo "***** FINISHED BUILDING $< IN $(SRCDIR) *****"
	@echo ""
	
.pmu.ts: 
	@set PATH=$(PATH64)
	$(RM) $@
	@echo "*****  MAKING DEBUG VERSION OF $<  *****"
	$(MUTOOL) $<
	$(CC) $(CFLAGS) $*.c
	$(RM) *.tmp $*.c
	$(PLACE_OBJ)
	$(TOUCH) $@
	@echo "***** FINISHED BUILDING $< IN $(SRCDIR) *****"
	@echo ""
#
# nodebug rules
#
.c.nts: 
	@set PATH=$(PATH64)
	$(RM) $@
	@echo "*****  MAKING NODEBUG VERSION OF $<  *****"
	$(NCC) $(CFLAGS) $<
	$(PLACE_NOBJ)
	$(TOUCH) $@
	@echo "***** FINISHED BUILDING $< IN $(SRCDIR) *****"
	@echo ""

.cpp.nts: 
	@set PATH=$(PATH64)
	$(RM) $@
	@echo "*****  MAKING NODEBUG VERSION OF $<  *****"
	$(NCPP) $(CPFLAGS) $<
	$(PLACE_NOBJ)
	$(TOUCH) $@
	@echo "***** FINISHED BUILDING $< IN $(SRCDIR) *****"
	@echo ""

.f.nts: 
	@set PATH=$(PATH64)
	$(RM) $@
	@echo "*****  MAKING DEBUG VERSION OF $*.c  *****"
	$(NF77) $(FFLAGS) $<
	$(PLACE_NOBJ)
	$(TOUCH) $@
	@echo "***** FINISHED BUILDING $< IN $(SRCDIR) *****"
	@echo ""

.for.nts: 
	@set PATH=$(PATH64)
	$(RM) $@
	@echo "*****  MAKING DEBUG VERSION OF $*.c  *****"
	$(FORFILTER) $<,$(FSYS)
	$(NF77) $(FFLAGS) $*.f
	$(RM) $*.f
	$(PLACE_NOBJ)
	$(TOUCH) $@
	@echo "***** FINISHED BUILDING $< IN $(SRCDIR) *****"
	@echo ""

.itr.nts: 
	@set PATH=$(PATH64)
	$(RM) $@
	@echo "*****  MAKING NODEBUG VERSION OF $<  *****"
	$(ITRTOOL) $<
	$(NCC) $(CFLAGS) $*.c
	$(RM) *.tmp $*.c
	$(PLACE_NOBJ)
	$(TOUCH) $@
	@echo "***** FINISHED BUILDING $< IN $(SRCDIR) *****"
	@echo ""
	
.msg.nts: 
	@set PATH=$(PATH64)
	$(RM) $@
	@echo "*****  MAKING NODEBUG VERSION OF $<  *****"
	$(MSGTOOL) $<
	$(RM) *.tmp
	$(RM) $(NCLFILES)\$*.hlp
	$(MV) $*.hlp $(NCLFILES)
	$(NCC) $(CFLAGS) $*.c
	$(RM) *.tmp $*.c
	$(PLACE_NOBJ)
	$(TOUCH) $@
	@echo "***** FINISHED BUILDING $< IN $(SRCDIR) *****"
	@echo ""
	
.pmu.nts: 
	@set PATH=$(PATH64)
	$(RM) $@
	@echo "*****  MAKING NODEBUG VERSION OF $<  *****"
	$(MUTOOL) $<
	$(NCC) $(CFLAGS) $*.c
	$(RM) *.tmp $*.c
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
	@echo "*****  MAKING 32-BIT DEBUG VERSION OF $*.c  *****"
	$(CC) $(CFLAGS) /D "BUILD_32" $<
	$(PLACE_OBJ32)
	$(TOUCH) $@
	@echo "***** FINISHED BUILDING $< IN $(SRCDIR) *****"
	@echo ""

.cpp.t32: 
	@set PATH=$(PATH32)
	$(RM) $@
	@echo "*****  MAKING 32-BIT DEBUG VERSION OF $<  *****"
	$(CPP) $(CPFLAGS) /D "BUILD_32" $<
	$(PLACE_OBJ32)
	$(TOUCH) $@
	@echo "***** FINISHED BUILDING $< IN $(SRCDIR) *****"
	@echo ""

.f.t32: 
	@set PATH=$(PATH32)
	$(RM) $@
	@echo "*****  MAKING 32-BIT DEBUG VERSION OF $<  *****"
	$(F77) $(FFLAGS) $<
	$(PLACE_OBJ32)
	$(TOUCH) $@
	@echo "***** FINISHED BUILDING $< IN $(SRCDIR) *****"
	@echo ""

.for.t32: 
	@set PATH=$(PATH32)
	$(RM) $@
	@echo "*****  MAKING 32-BIT DEBUG VERSION OF $<  *****"
	$(FORFILTER) $<,$(FSYS)
	$(F77) $(FFLAGS) $*.f
	$(RM) $*.f
	$(PLACE_OBJ32)
	$(TOUCH) $@
	@echo "***** FINISHED BUILDING $< IN $(SRCDIR) *****"
	@echo ""

.itr.t32: 
	@set PATH=$(PATH32)
	$(RM) $@
	@echo "*****  MAKING 32-BIT DEBUG VERSION OF $<  *****"
	$(ITRTOOL) $<
	$(CC) $(CFLAGS) /D "BUILD_32" $*.c
	$(RM) *.tmp $*.c
	$(PLACE_OBJ32)
	$(TOUCH) $@
	@echo "***** FINISHED BUILDING $< IN $(SRCDIR) *****"
	@echo ""
	
.msg.t32: 
	@set PATH=$(PATH32)
	$(RM) $@
	@echo "*****  MAKING 32-BIT DEBUG VERSION OF $<  *****"
	$(MSGTOOL) $<
	$(RM) *.tmp
	$(RM) $*.hlp
#	$(MV) $*.hlp $(NCLFILES)
	$(CC) $(CFLAGS) /D "BUILD_32" $*.c
	$(RM) *.tmp $*.c
	$(PLACE_OBJ32)
	$(TOUCH) $@
	@echo "***** FINISHED BUILDING $< IN $(SRCDIR) *****"
	@echo ""
	
.pmu.t32: 
	@set PATH=$(PATH32)
	$(RM) $@
	@echo "*****  MAKING 32-BIT DEBUG VERSION OF $<  *****"
	$(MUTOOL) $<
	$(CC) $(CFLAGS) /D "BUILD_32" $*.c
	$(RM) *.tmp $*.c
	$(PLACE_OBJ32)
	$(TOUCH) $@
	@echo "***** FINISHED BUILDING $< IN $(SRCDIR) *****"
	@echo ""
#
# nodebug rules
#
.c.nt32: 
	@set PATH=$(PATH32)
	$(RM) $@
	@echo "*****  MAKING 32-BIT NODEBUG VERSION OF $<  *****"
	$(NCC) $(CFLAGS) /D "BUILD_32" $<
	$(PLACE_NOBJ)
	$(TOUCH) $@
	@echo "***** FINISHED BUILDING $< IN $(SRCDIR) *****"
	@echo ""

.cpp.nt32: 
	@set PATH=$(PATH32)
	$(RM) $@
	@echo "*****  MAKING 32-BIT NODEBUG VERSION OF $<  *****"
	$(NCPP) $(CPFLAGS) /D "BUILD_32" $<
	$(PLACE_NOBJ)
	$(TOUCH) $@
	@echo "***** FINISHED BUILDING $< IN $(SRCDIR) *****"
	@echo ""

.f.nt32: 
	@set PATH=$(PATH32)
	$(RM) $@
	@echo "*****  MAKING 32-BIT NODEBUG VERSION OF $*.c  *****"
	$(NF77) $(FFLAGS) /D "BUILD_32" $<
	$(PLACE_NOBJ)
	$(TOUCH) $@
	@echo "***** FINISHED BUILDING $< IN $(SRCDIR) *****"
	@echo ""

.for.nt32: 
	@set PATH=$(PATH32)
	$(RM) $@
	@echo "*****  MAKING 32-BIT NODEBUG VERSION OF $*.c  *****"
	$(FORFILTER) $<,$(FSYS)
	$(NF77) $(FFLAGS) $*.f
	$(RM) $*.f
	$(PLACE_NOBJ)
	$(TOUCH) $@
	@echo "***** FINISHED BUILDING $< IN $(SRCDIR) *****"
	@echo ""

.itr.nt32: 
	@set PATH=$(PATH32)
	$(RM) $@
	@echo "*****  MAKING 32-BIT NODEBUG VERSION OF $<  *****"
	$(ITRTOOL) $<
	$(NCC) $(CFLAGS) /D "BUILD_32" $*.c
	$(RM) *.tmp $*.c
	$(PLACE_NOBJ)
	$(TOUCH) $@
	@echo "***** FINISHED BUILDING $< IN $(SRCDIR) *****"
	@echo ""
	
.msg.nt32: 
	@set PATH=$(PATH32)
	$(RM) $@
	@echo "*****  MAKING 32-BIT NODEBUG VERSION OF $<  *****"
	$(MSGTOOL) $<
	$(RM) *.tmp
	$(RM) $(NCLFILES)\$*.hlp
	$(MV) $*.hlp $(NCLFILES)
	$(NCC) $(CFLAGS) /D "BUILD_32" $*.c
	$(RM) *.tmp $*.c
	$(PLACE_NOBJ)
	$(TOUCH) $@
	@echo "***** FINISHED BUILDING $< IN $(SRCDIR) *****"
	@echo ""
	
.pmu.nt32: 
	@set PATH=$(PATH32)
	$(RM) $@
	@echo "*****  MAKING 32-BIT NODEBUG VERSION OF $<  *****"
	$(MUTOOL) $<
	$(NCC) $(CFLAGS) /D "BUILD_32" $*.c
	$(RM) *.tmp $*.c
	$(PLACE_NOBJ)
	$(TOUCH) $@
	@echo "***** FINISHED BUILDING $< IN $(SRCDIR) *****"
	@echo ""
	
.SUFFIXES: .c .itr .msg .pmu .ts .nts .t32 .nt32 .obj .o
#
#---------------------------EOF make.prefix.src.wnt --------------------------
#
#
#--------------------------- make.list --------------------------
#
SRCS1= \
GLtest.rc OG.rc SwUnibase.rc iges.rc mslite.rc nclipv.rc nclrec.rc \
ncq.rc plot.rc step.rc toolib.rc wsntncl.rc isysddl.ddl ComDlg.h \
CutDlg.h DefineDlg.h GLtest.h GLtestDoc.h GLtestView.h IgesAttrDlg.h IgesFiltDlg.h \
IgesListDlg.h IgesModalDlg.h IgesOptDlg.h IgesPromptDlg.h Igesres.h MachineWorks.h MainFrm.h \
ModListDlg.h Ncqres.h OG.h OGDoc.h OGView.h OgModals.h OperateDlg.h \
ParmsDlg.h StdAfx.h StepAttrDlg.h StepFiltDlg.h StepListDlg.h StepModalDlg.h StepOptDlg.h \
StepOutFiltDlg.h StepPromptDlg.h Stepres.h SwBitmapHandler.h SwDocView.h SwDocument.h SwModals.h \
SwNclUnibase.h SwStdAfx.h SwUniAttr.h SwUniOpt.h SwUniSketch.h SwUniStat.h SwUnibase.h \
SwUnibase_i.h Swresource.h ToolChildVw.h ToolDispDlg.h ToolFindDlg.h ToolPromptDlg.h adraft.h \
adrf.h adrfcom.h adrfdefs.h ag_def.h ag_globinit.h agddl.h alloc.h \
asalc.h atext.h axhatch.h bsym.h calcom.h canbe.h cdef.h \
class.h cstblist.h cvmatch.h d1cmdsem.h dascom.h dasg.h dasnog.h \
dassem.h dcapture.h ddef.h derror.h dextern.h diconm.h dinput.h \
dmark.h dmenucom.h
OBJS1= \
GLtest.ts OG.ts SwUnibase.ts iges.ts mslite.ts nclipv.ts nclrec.ts \
ncq.ts plot.ts step.ts toolib.ts wsntncl.ts isysddl.sql ComDlg.ts \
CutDlg.ts DefineDlg.ts GLtest.ts GLtestDoc.ts GLtestView.ts IgesAttrDlg.ts IgesFiltDlg.ts \
IgesListDlg.ts IgesModalDlg.ts IgesOptDlg.ts IgesPromptDlg.ts Igesres.ts MachineWorks.ts MainFrm.ts \
ModListDlg.ts Ncqres.ts OG.ts OGDoc.ts OGView.ts OgModals.ts OperateDlg.ts \
ParmsDlg.ts StdAfx.ts StepAttrDlg.ts StepFiltDlg.ts StepListDlg.ts StepModalDlg.ts StepOptDlg.ts \
StepOutFiltDlg.ts StepPromptDlg.ts Stepres.ts SwBitmapHandler.ts SwDocView.ts SwDocument.ts SwModals.ts \
SwNclUnibase.ts SwStdAfx.ts SwUniAttr.ts SwUniOpt.ts SwUniSketch.ts SwUniStat.ts SwUnibase.ts \
SwUnibase_i.ts Swresource.ts ToolChildVw.ts ToolDispDlg.ts ToolFindDlg.ts ToolPromptDlg.ts adraft.ts \
adrf.ts adrfcom.ts adrfdefs.ts ag_def.ts ag_globinit.ts agddl.ts alloc.ts \
asalc.ts atext.ts axhatch.ts bsym.ts calcom.ts canbe.ts cdef.ts \
class.ts cstblist.ts cvmatch.ts d1cmdsem.ts dascom.ts dasg.ts dasnog.ts \
dassem.ts dcapture.ts ddef.ts derror.ts dextern.ts diconm.ts dinput.ts \
dmark.ts dmenucom.ts
NOBJS1= \
GLtest.nts OG.nts SwUnibase.nts iges.nts mslite.nts nclipv.nts nclrec.nts \
ncq.nts plot.nts step.nts toolib.nts wsntncl.nts isysddl.sql ComDlg.h \
CutDlg.h DefineDlg.h GLtest.h GLtestDoc.h GLtestView.h IgesAttrDlg.h IgesFiltDlg.h \
IgesListDlg.h IgesModalDlg.h IgesOptDlg.h IgesPromptDlg.h Igesres.h MachineWorks.h MainFrm.h \
ModListDlg.h Ncqres.h OG.h OGDoc.h OGView.h OgModals.h OperateDlg.h \
ParmsDlg.h StdAfx.h StepAttrDlg.h StepFiltDlg.h StepListDlg.h StepModalDlg.h StepOptDlg.h \
StepOutFiltDlg.h StepPromptDlg.h Stepres.h SwBitmapHandler.h SwDocView.h SwDocument.h SwModals.h \
SwNclUnibase.h SwStdAfx.h SwUniAttr.h SwUniOpt.h SwUniSketch.h SwUniStat.h SwUnibase.h \
SwUnibase_i.h Swresource.h ToolChildVw.h ToolDispDlg.h ToolFindDlg.h ToolPromptDlg.h adraft.h \
adrf.h adrfcom.h adrfdefs.h ag_def.h ag_globinit.h agddl.h alloc.h \
asalc.h atext.h axhatch.h bsym.h calcom.h canbe.h cdef.h \
class.h cstblist.h cvmatch.h d1cmdsem.h dascom.h dasg.h dasnog.h \
dassem.h dcapture.h ddef.h derror.h dextern.h diconm.h dinput.h \
dmark.h dmenucom.h
OBJS321= \
GLtest.t32 OG.t32 SwUnibase.t32 iges.t32 mslite.t32 nclipv.t32 nclrec.t32 \
ncq.t32 plot.t32 step.t32 toolib.t32 wsntncl.t32 isysddl.sql ComDlg.t32 \
CutDlg.t32 DefineDlg.t32 GLtest.t32 GLtestDoc.t32 GLtestView.t32 IgesAttrDlg.t32 IgesFiltDlg.t32 \
IgesListDlg.t32 IgesModalDlg.t32 IgesOptDlg.t32 IgesPromptDlg.t32 Igesres.t32 MachineWorks.t32 MainFrm.t32 \
ModListDlg.t32 Ncqres.t32 OG.t32 OGDoc.t32 OGView.t32 OgModals.t32 OperateDlg.t32 \
ParmsDlg.t32 StdAfx.t32 StepAttrDlg.t32 StepFiltDlg.t32 StepListDlg.t32 StepModalDlg.t32 StepOptDlg.t32 \
StepOutFiltDlg.t32 StepPromptDlg.t32 Stepres.t32 SwBitmapHandler.t32 SwDocView.t32 SwDocument.t32 SwModals.t32 \
SwNclUnibase.t32 SwStdAfx.t32 SwUniAttr.t32 SwUniOpt.t32 SwUniSketch.t32 SwUniStat.t32 SwUnibase.t32 \
SwUnibase_i.t32 Swresource.t32 ToolChildVw.t32 ToolDispDlg.t32 ToolFindDlg.t32 ToolPromptDlg.t32 adraft.t32 \
adrf.t32 adrfcom.t32 adrfdefs.t32 ag_def.t32 ag_globinit.t32 agddl.t32 alloc.t32 \
asalc.t32 atext.t32 axhatch.t32 bsym.t32 calcom.t32 canbe.t32 cdef.t32 \
class.t32 cstblist.t32 cvmatch.t32 d1cmdsem.t32 dascom.t32 dasg.t32 dasnog.t32 \
dassem.t32 dcapture.t32 ddef.t32 derror.t32 dextern.t32 diconm.t32 dinput.t32 \
dmark.t32 dmenucom.t32
NOBJS321= \
GLtest.nt32 OG.nt32 SwUnibase.nt32 iges.nt32 mslite.nt32 nclipv.nt32 nclrec.nt32 \
ncq.nt32 plot.nt32 step.nt32 toolib.nt32 wsntncl.nt32 isysddl.sql ComDlg.h \
CutDlg.h DefineDlg.h GLtest.h GLtestDoc.h GLtestView.h IgesAttrDlg.h IgesFiltDlg.h \
IgesListDlg.h IgesModalDlg.h IgesOptDlg.h IgesPromptDlg.h Igesres.h MachineWorks.h MainFrm.h \
ModListDlg.h Ncqres.h OG.h OGDoc.h OGView.h OgModals.h OperateDlg.h \
ParmsDlg.h StdAfx.h StepAttrDlg.h StepFiltDlg.h StepListDlg.h StepModalDlg.h StepOptDlg.h \
StepOutFiltDlg.h StepPromptDlg.h Stepres.h SwBitmapHandler.h SwDocView.h SwDocument.h SwModals.h \
SwNclUnibase.h SwStdAfx.h SwUniAttr.h SwUniOpt.h SwUniSketch.h SwUniStat.h SwUnibase.h \
SwUnibase_i.h Swresource.h ToolChildVw.h ToolDispDlg.h ToolFindDlg.h ToolPromptDlg.h adraft.h \
adrf.h adrfcom.h adrfdefs.h ag_def.h ag_globinit.h agddl.h alloc.h \
asalc.h atext.h axhatch.h bsym.h calcom.h canbe.h cdef.h \
class.h cstblist.h cvmatch.h d1cmdsem.h dascom.h dasg.h dasnog.h \
dassem.h dcapture.h ddef.h derror.h dextern.h diconm.h dinput.h \
dmark.h dmenucom.h
SRCS2= \
dmodul.h dmotif.h dpipe.h driver.h drubber.h dselect.h dselmask.h \
dsubcom.h dtuto.h dtypes.h dvmatch.h dwindow.h fastimmed.h files.h \
fmill.h g.h garch.h gcolors.h gconvert.h gcurses.h gdidd.h \
gdidd2.h gentry.h gentryctl.h gentryout.h gerror.h gerroratt.h gerrorid.h \
gerrorinp.h gerrormet.h gerrorout.h gerrorst.h gerrorws.h gerrorxf.h gfont.h \
gi.h gi1.h gichoice.h giloc.h gimisc.h ginq.h ginqatt.h \
ginqatt2.h ginqatt3.h ginqatti.h ginqdsiz.h ginqinp.h ginqst.h ginqxf.h \
gipick.h gistr.h gistroke.h gival.h glStdAfx.h glists.h gloc3.h \
glresource.h glsi2.h gmac.h gmat4.h go.h go1.h go2.h \
go3.h goatt.h gobas.h gobndl.h gofac.h gomisc.h goren.h \
goseg.h gows.h grender.h gsegac.h gsegdet.h gsegop.h gstack.h \
gtbl.h gtbldef.h gtbldesc.h
OBJS2= \
dmodul.ts dmotif.ts dpipe.ts driver.ts drubber.ts dselect.ts dselmask.ts \
dsubcom.ts dtuto.ts dtypes.ts dvmatch.ts dwindow.ts fastimmed.ts files.ts \
fmill.ts g.ts garch.ts gcolors.ts gconvert.ts gcurses.ts gdidd.ts \
gdidd2.ts gentry.ts gentryctl.ts gentryout.ts gerror.ts gerroratt.ts gerrorid.ts \
gerrorinp.ts gerrormet.ts gerrorout.ts gerrorst.ts gerrorws.ts gerrorxf.ts gfont.ts \
gi.ts gi1.ts gichoice.ts giloc.ts gimisc.ts ginq.ts ginqatt.ts \
ginqatt2.ts ginqatt3.ts ginqatti.ts ginqdsiz.ts ginqinp.ts ginqst.ts ginqxf.ts \
gipick.ts gistr.ts gistroke.ts gival.ts glStdAfx.ts glists.ts gloc3.ts \
glresource.ts glsi2.ts gmac.ts gmat4.ts go.ts go1.ts go2.ts \
go3.ts goatt.ts gobas.ts gobndl.ts gofac.ts gomisc.ts goren.ts \
goseg.ts gows.ts grender.ts gsegac.ts gsegdet.ts gsegop.ts gstack.ts \
gtbl.ts gtbldef.ts gtbldesc.ts
NOBJS2= \
dmodul.h dmotif.h dpipe.h driver.h drubber.h dselect.h dselmask.h \
dsubcom.h dtuto.h dtypes.h dvmatch.h dwindow.h fastimmed.h files.h \
fmill.h g.h garch.h gcolors.h gconvert.h gcurses.h gdidd.h \
gdidd2.h gentry.h gentryctl.h gentryout.h gerror.h gerroratt.h gerrorid.h \
gerrorinp.h gerrormet.h gerrorout.h gerrorst.h gerrorws.h gerrorxf.h gfont.h \
gi.h gi1.h gichoice.h giloc.h gimisc.h ginq.h ginqatt.h \
ginqatt2.h ginqatt3.h ginqatti.h ginqdsiz.h ginqinp.h ginqst.h ginqxf.h \
gipick.h gistr.h gistroke.h gival.h glStdAfx.h glists.h gloc3.h \
glresource.h glsi2.h gmac.h gmat4.h go.h go1.h go2.h \
go3.h goatt.h gobas.h gobndl.h gofac.h gomisc.h goren.h \
goseg.h gows.h grender.h gsegac.h gsegdet.h gsegop.h gstack.h \
gtbl.h gtbldef.h gtbldesc.h
OBJS322= \
dmodul.t32 dmotif.t32 dpipe.t32 driver.t32 drubber.t32 dselect.t32 dselmask.t32 \
dsubcom.t32 dtuto.t32 dtypes.t32 dvmatch.t32 dwindow.t32 fastimmed.t32 files.t32 \
fmill.t32 g.t32 garch.t32 gcolors.t32 gconvert.t32 gcurses.t32 gdidd.t32 \
gdidd2.t32 gentry.t32 gentryctl.t32 gentryout.t32 gerror.t32 gerroratt.t32 gerrorid.t32 \
gerrorinp.t32 gerrormet.t32 gerrorout.t32 gerrorst.t32 gerrorws.t32 gerrorxf.t32 gfont.t32 \
gi.t32 gi1.t32 gichoice.t32 giloc.t32 gimisc.t32 ginq.t32 ginqatt.t32 \
ginqatt2.t32 ginqatt3.t32 ginqatti.t32 ginqdsiz.t32 ginqinp.t32 ginqst.t32 ginqxf.t32 \
gipick.t32 gistr.t32 gistroke.t32 gival.t32 glStdAfx.t32 glists.t32 gloc3.t32 \
glresource.t32 glsi2.t32 gmac.t32 gmat4.t32 go.t32 go1.t32 go2.t32 \
go3.t32 goatt.t32 gobas.t32 gobndl.t32 gofac.t32 gomisc.t32 goren.t32 \
goseg.t32 gows.t32 grender.t32 gsegac.t32 gsegdet.t32 gsegop.t32 gstack.t32 \
gtbl.t32 gtbldef.t32 gtbldesc.t32
NOBJS322= \
dmodul.h dmotif.h dpipe.h driver.h drubber.h dselect.h dselmask.h \
dsubcom.h dtuto.h dtypes.h dvmatch.h dwindow.h fastimmed.h files.h \
fmill.h g.h garch.h gcolors.h gconvert.h gcurses.h gdidd.h \
gdidd2.h gentry.h gentryctl.h gentryout.h gerror.h gerroratt.h gerrorid.h \
gerrorinp.h gerrormet.h gerrorout.h gerrorst.h gerrorws.h gerrorxf.h gfont.h \
gi.h gi1.h gichoice.h giloc.h gimisc.h ginq.h ginqatt.h \
ginqatt2.h ginqatt3.h ginqatti.h ginqdsiz.h ginqinp.h ginqst.h ginqxf.h \
gipick.h gistr.h gistroke.h gival.h glStdAfx.h glists.h gloc3.h \
glresource.h glsi2.h gmac.h gmat4.h go.h go1.h go2.h \
go3.h goatt.h gobas.h gobndl.h gofac.h gomisc.h goren.h \
goseg.h gows.h grender.h gsegac.h gsegdet.h gsegop.h gstack.h \
gtbl.h gtbldef.h gtbldesc.h
SRCS3= \
gtblopst.h gtblseg.h gtblst.h gtbluni.h gtblvar1.h gtblvar2.h gtblvar3.h \
gtblvar4.h gtblvar5.h gtblvar6.h gtblvar7.h gtblvar8.h gtblws.h gviw.h \
gvlib.h icalcsem.h icon.h iges.h igesDlg.h igesDlg2.h igesfc.h \
imattrib.h imdraw.h ipipe.h iriscurses.h jplot.h jquery.h layout.h \
lcom.h lipv.h lipvmach.h lipvmplay.h lipvstack.h lipvstubs.h lumb.h \
m2dattr.h mattr.h mconst.h mcrv.h mcvh.h mdattr.h mdclass.h \
mdcoord.h mdcpln.h mdebug.h mderror.h mdeval.h mdgenent.h mdmatrix.h \
mdpick.h mdrel.h mdrwsize.h mdunits.h mfcifdef.h mfeatcom.h mfort.h \
mgeom.h minitrel.h misect.h mlab.h mlists.h mmath.h mnns.h \
modef.h mplot.h mpocket.h mpopmenu.h mrender.h mromcom.h msave.h \
mslMainFrm.h mslite.h msliteView.h msol.h msrf.h mstor.h msurf.h \
mtextcom.h mxxx.h nccs.h
OBJS3= \
gtblopst.ts gtblseg.ts gtblst.ts gtbluni.ts gtblvar1.ts gtblvar2.ts gtblvar3.ts \
gtblvar4.ts gtblvar5.ts gtblvar6.ts gtblvar7.ts gtblvar8.ts gtblws.ts gviw.ts \
gvlib.ts icalcsem.ts icon.ts iges.ts igesDlg.ts igesDlg2.ts igesfc.ts \
imattrib.ts imdraw.ts ipipe.ts iriscurses.ts jplot.ts jquery.ts layout.ts \
lcom.ts lipv.ts lipvmach.ts lipvmplay.ts lipvstack.ts lipvstubs.ts lumb.ts \
m2dattr.ts mattr.ts mconst.ts mcrv.ts mcvh.ts mdattr.ts mdclass.ts \
mdcoord.ts mdcpln.ts mdebug.ts mderror.ts mdeval.ts mdgenent.ts mdmatrix.ts \
mdpick.ts mdrel.ts mdrwsize.ts mdunits.ts mfcifdef.ts mfeatcom.ts mfort.ts \
mgeom.ts minitrel.ts misect.ts mlab.ts mlists.ts mmath.ts mnns.ts \
modef.ts mplot.ts mpocket.ts mpopmenu.ts mrender.ts mromcom.ts msave.ts \
mslMainFrm.ts mslite.ts msliteView.ts msol.ts msrf.ts mstor.ts msurf.ts \
mtextcom.ts mxxx.ts nccs.ts
NOBJS3= \
gtblopst.h gtblseg.h gtblst.h gtbluni.h gtblvar1.h gtblvar2.h gtblvar3.h \
gtblvar4.h gtblvar5.h gtblvar6.h gtblvar7.h gtblvar8.h gtblws.h gviw.h \
gvlib.h icalcsem.h icon.h iges.h igesDlg.h igesDlg2.h igesfc.h \
imattrib.h imdraw.h ipipe.h iriscurses.h jplot.h jquery.h layout.h \
lcom.h lipv.h lipvmach.h lipvmplay.h lipvstack.h lipvstubs.h lumb.h \
m2dattr.h mattr.h mconst.h mcrv.h mcvh.h mdattr.h mdclass.h \
mdcoord.h mdcpln.h mdebug.h mderror.h mdeval.h mdgenent.h mdmatrix.h \
mdpick.h mdrel.h mdrwsize.h mdunits.h mfcifdef.h mfeatcom.h mfort.h \
mgeom.h minitrel.h misect.h mlab.h mlists.h mmath.h mnns.h \
modef.h mplot.h mpocket.h mpopmenu.h mrender.h mromcom.h msave.h \
mslMainFrm.h mslite.h msliteView.h msol.h msrf.h mstor.h msurf.h \
mtextcom.h mxxx.h nccs.h
OBJS323= \
gtblopst.t32 gtblseg.t32 gtblst.t32 gtbluni.t32 gtblvar1.t32 gtblvar2.t32 gtblvar3.t32 \
gtblvar4.t32 gtblvar5.t32 gtblvar6.t32 gtblvar7.t32 gtblvar8.t32 gtblws.t32 gviw.t32 \
gvlib.t32 icalcsem.t32 icon.t32 iges.t32 igesDlg.t32 igesDlg2.t32 igesfc.t32 \
imattrib.t32 imdraw.t32 ipipe.t32 iriscurses.t32 jplot.t32 jquery.t32 layout.t32 \
lcom.t32 lipv.t32 lipvmach.t32 lipvmplay.t32 lipvstack.t32 lipvstubs.t32 lumb.t32 \
m2dattr.t32 mattr.t32 mconst.t32 mcrv.t32 mcvh.t32 mdattr.t32 mdclass.t32 \
mdcoord.t32 mdcpln.t32 mdebug.t32 mderror.t32 mdeval.t32 mdgenent.t32 mdmatrix.t32 \
mdpick.t32 mdrel.t32 mdrwsize.t32 mdunits.t32 mfcifdef.t32 mfeatcom.t32 mfort.t32 \
mgeom.t32 minitrel.t32 misect.t32 mlab.t32 mlists.t32 mmath.t32 mnns.t32 \
modef.t32 mplot.t32 mpocket.t32 mpopmenu.t32 mrender.t32 mromcom.t32 msave.t32 \
mslMainFrm.t32 mslite.t32 msliteView.t32 msol.t32 msrf.t32 mstor.t32 msurf.t32 \
mtextcom.t32 mxxx.t32 nccs.t32
NOBJS323= \
gtblopst.h gtblseg.h gtblst.h gtbluni.h gtblvar1.h gtblvar2.h gtblvar3.h \
gtblvar4.h gtblvar5.h gtblvar6.h gtblvar7.h gtblvar8.h gtblws.h gviw.h \
gvlib.h icalcsem.h icon.h iges.h igesDlg.h igesDlg2.h igesfc.h \
imattrib.h imdraw.h ipipe.h iriscurses.h jplot.h jquery.h layout.h \
lcom.h lipv.h lipvmach.h lipvmplay.h lipvstack.h lipvstubs.h lumb.h \
m2dattr.h mattr.h mconst.h mcrv.h mcvh.h mdattr.h mdclass.h \
mdcoord.h mdcpln.h mdebug.h mderror.h mdeval.h mdgenent.h mdmatrix.h \
mdpick.h mdrel.h mdrwsize.h mdunits.h mfcifdef.h mfeatcom.h mfort.h \
mgeom.h minitrel.h misect.h mlab.h mlists.h mmath.h mnns.h \
modef.h mplot.h mpocket.h mpopmenu.h mrender.h mromcom.h msave.h \
mslMainFrm.h mslite.h msliteView.h msol.h msrf.h mstor.h msurf.h \
mtextcom.h mxxx.h nccs.h
SRCS4= \
ncl.h nclclip.h nclcmd.h ncldef.h nclfc.h nclfile.h nclicons.h \
nclinp.h nclipv.h nclipvFrm.h nclipvView.h nclmodals.h nclmplay.h nclpockofs.h \
nclpsmult.h nclrec.h nclrecDlg.h nclrecres.h nclstack.h nclupok.h ncluvmp.h \
nclver.h nclvmptool.h nclvx.h nclwaterln.h nclx.h nclxmdl.h nclxmot.h \
nclxunib.h nclxuser.h nconst.h ncq.h ncqcom.h ncqdoc.h ncqmainfrm.h \
ncqmonitor.h ncqoptdlg.h ncqview.h nesrc.h ngeom.h nkeywd.h nproj.h \
nsimulate.h ogMainFrm.h ogStdAfx.h ogcom.h ogresource.h openvx_mdl.h openvx_tim.h \
openvx_uif.h plot.h plotDlg.h plotfrm.h plotres.h qlang.h qsymtb.h \
r1emacro.h r1emsgpk.h r1esched.h r1isched.h rbase.h rebus.h recStdAfx.h \
reddlerr.h rerrdef.h ribase.h ridctdef.h ridctver.h ridctver1.h ridctver10.h \
ridctver11.h ridctver12.h ridctver13.h ridctver14.h ridctver15.h ridctver2.h ridctver3.h \
ridctver4.h ridctver5.h ridctver6.h
OBJS4= \
ncl.ts nclclip.ts nclcmd.ts ncldef.ts nclfc.ts nclfile.ts nclicons.ts \
nclinp.ts nclipv.ts nclipvFrm.ts nclipvView.ts nclmodals.ts nclmplay.ts nclpockofs.ts \
nclpsmult.ts nclrec.ts nclrecDlg.ts nclrecres.ts nclstack.ts nclupok.ts ncluvmp.ts \
nclver.ts nclvmptool.ts nclvx.ts nclwaterln.ts nclx.ts nclxmdl.ts nclxmot.ts \
nclxunib.ts nclxuser.ts nconst.ts ncq.ts ncqcom.ts ncqdoc.ts ncqmainfrm.ts \
ncqmonitor.ts ncqoptdlg.ts ncqview.ts nesrc.ts ngeom.ts nkeywd.ts nproj.ts \
nsimulate.ts ogMainFrm.ts ogStdAfx.ts ogcom.ts ogresource.ts openvx_mdl.ts openvx_tim.ts \
openvx_uif.ts plot.ts plotDlg.ts plotfrm.ts plotres.ts qlang.ts qsymtb.ts \
r1emacro.ts r1emsgpk.ts r1esched.ts r1isched.ts rbase.ts rebus.ts recStdAfx.ts \
reddlerr.ts rerrdef.ts ribase.ts ridctdef.ts ridctver.ts ridctver1.ts ridctver10.ts \
ridctver11.ts ridctver12.ts ridctver13.ts ridctver14.ts ridctver15.ts ridctver2.ts ridctver3.ts \
ridctver4.ts ridctver5.ts ridctver6.ts
NOBJS4= \
ncl.h nclclip.h nclcmd.h ncldef.h nclfc.h nclfile.h nclicons.h \
nclinp.h nclipv.h nclipvFrm.h nclipvView.h nclmodals.h nclmplay.h nclpockofs.h \
nclpsmult.h nclrec.h nclrecDlg.h nclrecres.h nclstack.h nclupok.h ncluvmp.h \
nclver.h nclvmptool.h nclvx.h nclwaterln.h nclx.h nclxmdl.h nclxmot.h \
nclxunib.h nclxuser.h nconst.h ncq.h ncqcom.h ncqdoc.h ncqmainfrm.h \
ncqmonitor.h ncqoptdlg.h ncqview.h nesrc.h ngeom.h nkeywd.h nproj.h \
nsimulate.h ogMainFrm.h ogStdAfx.h ogcom.h ogresource.h openvx_mdl.h openvx_tim.h \
openvx_uif.h plot.h plotDlg.h plotfrm.h plotres.h qlang.h qsymtb.h \
r1emacro.h r1emsgpk.h r1esched.h r1isched.h rbase.h rebus.h recStdAfx.h \
reddlerr.h rerrdef.h ribase.h ridctdef.h ridctver.h ridctver1.h ridctver10.h \
ridctver11.h ridctver12.h ridctver13.h ridctver14.h ridctver15.h ridctver2.h ridctver3.h \
ridctver4.h ridctver5.h ridctver6.h
OBJS324= \
ncl.t32 nclclip.t32 nclcmd.t32 ncldef.t32 nclfc.t32 nclfile.t32 nclicons.t32 \
nclinp.t32 nclipv.t32 nclipvFrm.t32 nclipvView.t32 nclmodals.t32 nclmplay.t32 nclpockofs.t32 \
nclpsmult.t32 nclrec.t32 nclrecDlg.t32 nclrecres.t32 nclstack.t32 nclupok.t32 ncluvmp.t32 \
nclver.t32 nclvmptool.t32 nclvx.t32 nclwaterln.t32 nclx.t32 nclxmdl.t32 nclxmot.t32 \
nclxunib.t32 nclxuser.t32 nconst.t32 ncq.t32 ncqcom.t32 ncqdoc.t32 ncqmainfrm.t32 \
ncqmonitor.t32 ncqoptdlg.t32 ncqview.t32 nesrc.t32 ngeom.t32 nkeywd.t32 nproj.t32 \
nsimulate.t32 ogMainFrm.t32 ogStdAfx.t32 ogcom.t32 ogresource.t32 openvx_mdl.t32 openvx_tim.t32 \
openvx_uif.t32 plot.t32 plotDlg.t32 plotfrm.t32 plotres.t32 qlang.t32 qsymtb.t32 \
r1emacro.t32 r1emsgpk.t32 r1esched.t32 r1isched.t32 rbase.t32 rebus.t32 recStdAfx.t32 \
reddlerr.t32 rerrdef.t32 ribase.t32 ridctdef.t32 ridctver.t32 ridctver1.t32 ridctver10.t32 \
ridctver11.t32 ridctver12.t32 ridctver13.t32 ridctver14.t32 ridctver15.t32 ridctver2.t32 ridctver3.t32 \
ridctver4.t32 ridctver5.t32 ridctver6.t32
NOBJS324= \
ncl.h nclclip.h nclcmd.h ncldef.h nclfc.h nclfile.h nclicons.h \
nclinp.h nclipv.h nclipvFrm.h nclipvView.h nclmodals.h nclmplay.h nclpockofs.h \
nclpsmult.h nclrec.h nclrecDlg.h nclrecres.h nclstack.h nclupok.h ncluvmp.h \
nclver.h nclvmptool.h nclvx.h nclwaterln.h nclx.h nclxmdl.h nclxmot.h \
nclxunib.h nclxuser.h nconst.h ncq.h ncqcom.h ncqdoc.h ncqmainfrm.h \
ncqmonitor.h ncqoptdlg.h ncqview.h nesrc.h ngeom.h nkeywd.h nproj.h \
nsimulate.h ogMainFrm.h ogStdAfx.h ogcom.h ogresource.h openvx_mdl.h openvx_tim.h \
openvx_uif.h plot.h plotDlg.h plotfrm.h plotres.h qlang.h qsymtb.h \
r1emacro.h r1emsgpk.h r1esched.h r1isched.h rbase.h rebus.h recStdAfx.h \
reddlerr.h rerrdef.h ribase.h ridctdef.h ridctver.h ridctver1.h ridctver10.h \
ridctver11.h ridctver12.h ridctver13.h ridctver14.h ridctver15.h ridctver2.h ridctver3.h \
ridctver4.h ridctver5.h ridctver6.h
SRCS5= \
ridctver7.h ridctver8.h ridctver9.h riddldef.h riddle.h rienvtab.h rimsgdef.h \
rireldef.h ritokdef.h ritrnerr.h rmtuple.h rstack.h rver10000.h rver10100.h \
rver8103.h rver8105.h rver8200.h rver8400.h rver8500.h rver9100.h rver9200.h \
rver9300.h rver9400.h rver9600.h rver9700.h rver9900.h sallist.h salloc.h \
scgen.h scomm.h sddl.h sdebug.h sdecl.h sdoprint.h serror.h \
sexec.h sexrun.h sfuncs.h sinput.h slist.h snode.h spmouse.h \
spublic.h sreswd.h srun.h sstate.h sstore.h ssym.h step.h \
stepDlg.h stepDlg2.h sxreswd.h tabledef.h taool.h tbdef.h tcvert.h \
testMainFrm.h tigconst.h tigdefs.h tigeldct.h tiges.h tigesddl.h tigglobal.h \
tigmf.h tigspdef.h tigsupp.h tigtrdct.h tigtypes.h tioconv.h tioglb.h \
tlangtool.h tmedcut.h tmsg.h tmudef.h tmugm.h tmusem.h tmutab.h \
toolStdAfx.h toolgettxt2.h toolib.h
OBJS5= \
ridctver7.ts ridctver8.ts ridctver9.ts riddldef.ts riddle.ts rienvtab.ts rimsgdef.ts \
rireldef.ts ritokdef.ts ritrnerr.ts rmtuple.ts rstack.ts rver10000.ts rver10100.ts \
rver8103.ts rver8105.ts rver8200.ts rver8400.ts rver8500.ts rver9100.ts rver9200.ts \
rver9300.ts rver9400.ts rver9600.ts rver9700.ts rver9900.ts sallist.ts salloc.ts \
scgen.ts scomm.ts sddl.ts sdebug.ts sdecl.ts sdoprint.ts serror.ts \
sexec.ts sexrun.ts sfuncs.ts sinput.ts slist.ts snode.ts spmouse.ts \
spublic.ts sreswd.ts srun.ts sstate.ts sstore.ts ssym.ts step.ts \
stepDlg.ts stepDlg2.ts sxreswd.ts tabledef.ts taool.ts tbdef.ts tcvert.ts \
testMainFrm.ts tigconst.ts tigdefs.ts tigeldct.ts tiges.ts tigesddl.ts tigglobal.ts \
tigmf.ts tigspdef.ts tigsupp.ts tigtrdct.ts tigtypes.ts tioconv.ts tioglb.ts \
tlangtool.ts tmedcut.ts tmsg.ts tmudef.ts tmugm.ts tmusem.ts tmutab.ts \
toolStdAfx.ts toolgettxt2.ts toolib.ts
NOBJS5= \
ridctver7.h ridctver8.h ridctver9.h riddldef.h riddle.h rienvtab.h rimsgdef.h \
rireldef.h ritokdef.h ritrnerr.h rmtuple.h rstack.h rver10000.h rver10100.h \
rver8103.h rver8105.h rver8200.h rver8400.h rver8500.h rver9100.h rver9200.h \
rver9300.h rver9400.h rver9600.h rver9700.h rver9900.h sallist.h salloc.h \
scgen.h scomm.h sddl.h sdebug.h sdecl.h sdoprint.h serror.h \
sexec.h sexrun.h sfuncs.h sinput.h slist.h snode.h spmouse.h \
spublic.h sreswd.h srun.h sstate.h sstore.h ssym.h step.h \
stepDlg.h stepDlg2.h sxreswd.h tabledef.h taool.h tbdef.h tcvert.h \
testMainFrm.h tigconst.h tigdefs.h tigeldct.h tiges.h tigesddl.h tigglobal.h \
tigmf.h tigspdef.h tigsupp.h tigtrdct.h tigtypes.h tioconv.h tioglb.h \
tlangtool.h tmedcut.h tmsg.h tmudef.h tmugm.h tmusem.h tmutab.h \
toolStdAfx.h toolgettxt2.h toolib.h
OBJS325= \
ridctver7.t32 ridctver8.t32 ridctver9.t32 riddldef.t32 riddle.t32 rienvtab.t32 rimsgdef.t32 \
rireldef.t32 ritokdef.t32 ritrnerr.t32 rmtuple.t32 rstack.t32 rver10000.t32 rver10100.t32 \
rver8103.t32 rver8105.t32 rver8200.t32 rver8400.t32 rver8500.t32 rver9100.t32 rver9200.t32 \
rver9300.t32 rver9400.t32 rver9600.t32 rver9700.t32 rver9900.t32 sallist.t32 salloc.t32 \
scgen.t32 scomm.t32 sddl.t32 sdebug.t32 sdecl.t32 sdoprint.t32 serror.t32 \
sexec.t32 sexrun.t32 sfuncs.t32 sinput.t32 slist.t32 snode.t32 spmouse.t32 \
spublic.t32 sreswd.t32 srun.t32 sstate.t32 sstore.t32 ssym.t32 step.t32 \
stepDlg.t32 stepDlg2.t32 sxreswd.t32 tabledef.t32 taool.t32 tbdef.t32 tcvert.t32 \
testMainFrm.t32 tigconst.t32 tigdefs.t32 tigeldct.t32 tiges.t32 tigesddl.t32 tigglobal.t32 \
tigmf.t32 tigspdef.t32 tigsupp.t32 tigtrdct.t32 tigtypes.t32 tioconv.t32 tioglb.t32 \
tlangtool.t32 tmedcut.t32 tmsg.t32 tmudef.t32 tmugm.t32 tmusem.t32 tmutab.t32 \
toolStdAfx.t32 toolgettxt2.t32 toolib.t32
NOBJS325= \
ridctver7.h ridctver8.h ridctver9.h riddldef.h riddle.h rienvtab.h rimsgdef.h \
rireldef.h ritokdef.h ritrnerr.h rmtuple.h rstack.h rver10000.h rver10100.h \
rver8103.h rver8105.h rver8200.h rver8400.h rver8500.h rver9100.h rver9200.h \
rver9300.h rver9400.h rver9600.h rver9700.h rver9900.h sallist.h salloc.h \
scgen.h scomm.h sddl.h sdebug.h sdecl.h sdoprint.h serror.h \
sexec.h sexrun.h sfuncs.h sinput.h slist.h snode.h spmouse.h \
spublic.h sreswd.h srun.h sstate.h sstore.h ssym.h step.h \
stepDlg.h stepDlg2.h sxreswd.h tabledef.h taool.h tbdef.h tcvert.h \
testMainFrm.h tigconst.h tigdefs.h tigeldct.h tiges.h tigesddl.h tigglobal.h \
tigmf.h tigspdef.h tigsupp.h tigtrdct.h tigtypes.h tioconv.h tioglb.h \
tlangtool.h tmedcut.h tmsg.h tmudef.h tmugm.h tmusem.h tmutab.h \
toolStdAfx.h toolgettxt2.h toolib.h
SRCS6= \
toolibDoc.h toolibView.h toolibcc.h toolibdata.h toolibmf.h toolibres.h toolibstdafx.h \
toollstctl.h tplot.h tptdef.h tptfcifdef.h tptmfort.h tptran.h tptsupp.h \
trender.h tsemcmp.h tsemcmp1.h tstep.h tstepcmd.h ttut.h twmakews.h \
tzadrf.h tzallmod.h tzattrdd.h tzbase.h tzbsym.h tzclass.h tzcrvddl.h \
tzdcoord.h tzddldef.h tzdrfddl.h tzenvtab.h tzlabddl.h tzmattr.h tzmcrv.h \
tzmdrel.h tzmlab.h tzmsol.h tzmsrf.h tzmtuple.h tzmxxx.h tznccs.h \
tznccsddl.h tznitrel.h tzrbase.h tzrstack.h tzsolddl.h tzspland.h tzsrfddl.h \
tzsymddl.h tzxxxddl.h tzysplan.h ualloc.h ubuffio.h uctype.h udeboff.h \
udebon.h udebug.h udfconst.h udfdata.h udfmracs.h udforms.h udfrmdec.h \
udfrmdef.h uerror.h uhash.h uhep.h uiarray.h uims.h uio.h \
ulist.h umasks.h umath.h umessages.h uminmax.h umoveb.h unserve.h \
usdbldef.h usetjmp.h usfltdef.h
OBJS6= \
toolibDoc.ts toolibView.ts toolibcc.ts toolibdata.ts toolibmf.ts toolibres.ts toolibstdafx.ts \
toollstctl.ts tplot.ts tptdef.ts tptfcifdef.ts tptmfort.ts tptran.ts tptsupp.ts \
trender.ts tsemcmp.ts tsemcmp1.ts tstep.ts tstepcmd.ts ttut.ts twmakews.ts \
tzadrf.ts tzallmod.ts tzattrdd.ts tzbase.ts tzbsym.ts tzclass.ts tzcrvddl.ts \
tzdcoord.ts tzddldef.ts tzdrfddl.ts tzenvtab.ts tzlabddl.ts tzmattr.ts tzmcrv.ts \
tzmdrel.ts tzmlab.ts tzmsol.ts tzmsrf.ts tzmtuple.ts tzmxxx.ts tznccs.ts \
tznccsddl.ts tznitrel.ts tzrbase.ts tzrstack.ts tzsolddl.ts tzspland.ts tzsrfddl.ts \
tzsymddl.ts tzxxxddl.ts tzysplan.ts ualloc.ts ubuffio.ts uctype.ts udeboff.ts \
udebon.ts udebug.ts udfconst.ts udfdata.ts udfmracs.ts udforms.ts udfrmdec.ts \
udfrmdef.ts uerror.ts uhash.ts uhep.ts uiarray.ts uims.ts uio.ts \
ulist.ts umasks.ts umath.ts umessages.ts uminmax.ts umoveb.ts unserve.ts \
usdbldef.ts usetjmp.ts usfltdef.ts
NOBJS6= \
toolibDoc.h toolibView.h toolibcc.h toolibdata.h toolibmf.h toolibres.h toolibstdafx.h \
toollstctl.h tplot.h tptdef.h tptfcifdef.h tptmfort.h tptran.h tptsupp.h \
trender.h tsemcmp.h tsemcmp1.h tstep.h tstepcmd.h ttut.h twmakews.h \
tzadrf.h tzallmod.h tzattrdd.h tzbase.h tzbsym.h tzclass.h tzcrvddl.h \
tzdcoord.h tzddldef.h tzdrfddl.h tzenvtab.h tzlabddl.h tzmattr.h tzmcrv.h \
tzmdrel.h tzmlab.h tzmsol.h tzmsrf.h tzmtuple.h tzmxxx.h tznccs.h \
tznccsddl.h tznitrel.h tzrbase.h tzrstack.h tzsolddl.h tzspland.h tzsrfddl.h \
tzsymddl.h tzxxxddl.h tzysplan.h ualloc.h ubuffio.h uctype.h udeboff.h \
udebon.h udebug.h udfconst.h udfdata.h udfmracs.h udforms.h udfrmdec.h \
udfrmdef.h uerror.h uhash.h uhep.h uiarray.h uims.h uio.h \
ulist.h umasks.h umath.h umessages.h uminmax.h umoveb.h unserve.h \
usdbldef.h usetjmp.h usfltdef.h
OBJS326= \
toolibDoc.t32 toolibView.t32 toolibcc.t32 toolibdata.t32 toolibmf.t32 toolibres.t32 toolibstdafx.t32 \
toollstctl.t32 tplot.t32 tptdef.t32 tptfcifdef.t32 tptmfort.t32 tptran.t32 tptsupp.t32 \
trender.t32 tsemcmp.t32 tsemcmp1.t32 tstep.t32 tstepcmd.t32 ttut.t32 twmakews.t32 \
tzadrf.t32 tzallmod.t32 tzattrdd.t32 tzbase.t32 tzbsym.t32 tzclass.t32 tzcrvddl.t32 \
tzdcoord.t32 tzddldef.t32 tzdrfddl.t32 tzenvtab.t32 tzlabddl.t32 tzmattr.t32 tzmcrv.t32 \
tzmdrel.t32 tzmlab.t32 tzmsol.t32 tzmsrf.t32 tzmtuple.t32 tzmxxx.t32 tznccs.t32 \
tznccsddl.t32 tznitrel.t32 tzrbase.t32 tzrstack.t32 tzsolddl.t32 tzspland.t32 tzsrfddl.t32 \
tzsymddl.t32 tzxxxddl.t32 tzysplan.t32 ualloc.t32 ubuffio.t32 uctype.t32 udeboff.t32 \
udebon.t32 udebug.t32 udfconst.t32 udfdata.t32 udfmracs.t32 udforms.t32 udfrmdec.t32 \
udfrmdef.t32 uerror.t32 uhash.t32 uhep.t32 uiarray.t32 uims.t32 uio.t32 \
ulist.t32 umasks.t32 umath.t32 umessages.t32 uminmax.t32 umoveb.t32 unserve.t32 \
usdbldef.t32 usetjmp.t32 usfltdef.t32
NOBJS326= \
toolibDoc.h toolibView.h toolibcc.h toolibdata.h toolibmf.h toolibres.h toolibstdafx.h \
toollstctl.h tplot.h tptdef.h tptfcifdef.h tptmfort.h tptran.h tptsupp.h \
trender.h tsemcmp.h tsemcmp1.h tstep.h tstepcmd.h ttut.h twmakews.h \
tzadrf.h tzallmod.h tzattrdd.h tzbase.h tzbsym.h tzclass.h tzcrvddl.h \
tzdcoord.h tzddldef.h tzdrfddl.h tzenvtab.h tzlabddl.h tzmattr.h tzmcrv.h \
tzmdrel.h tzmlab.h tzmsol.h tzmsrf.h tzmtuple.h tzmxxx.h tznccs.h \
tznccsddl.h tznitrel.h tzrbase.h tzrstack.h tzsolddl.h tzspland.h tzsrfddl.h \
tzsymddl.h tzxxxddl.h tzysplan.h ualloc.h ubuffio.h uctype.h udeboff.h \
udebon.h udebug.h udfconst.h udfdata.h udfmracs.h udforms.h udfrmdec.h \
udfrmdef.h uerror.h uhash.h uhep.h uiarray.h uims.h uio.h \
ulist.h umasks.h umath.h umessages.h uminmax.h umoveb.h unserve.h \
usdbldef.h usetjmp.h usfltdef.h
SRCS7= \
usignal.h ustdio.h ustrings.h usysdef.h usysg.h utime.h uvarargs.h \
uversion.h vconst.h vda.h version.h view.h view0.h view1.h \
vsegbf.h ws.h ws104.h ws4014em.h ws410x.h ws410xc.h ws411x.h \
ws411xc.h ws433x.h ws7475.h ws7580.h ws7580e.h ws7580i.h wsapo.h \
wscim.h wscimdefs.h wsconvert.h wsgc.h wsgcdev.h wsgcfo.h wsgctrm.h \
wsgcur.h wsgl.h wsglfun.h wsi24.h wsi24im.h wsi24trm.h wsi4d.h \
wsi4dim.h wsi4dtrm.h wsimath.h wsjpgimg.h wsmc.h wsmf.h wsmfmdesgn.h \
wsncldockbar.h wsncldockframe.h wsnclframe.h wsncltoolbar.h wsnews.h wsntDDButton.h wsntDDCombo.h \
wsntDDEdit.h wsntDDListBox.h wsntDDProcess.h wsntDDSlider.h wsntDDclrbtn.h wsntDDform.h wsntDDgroup.h \
wsntDDlstctl.h wsntDDlstctl2.h wsntDDpic.h wsntDDstatic.h wsntFdsnFrm.h wsntFdsnView.h wsntFormProp.h \
wsntIconListBox.h wsntModalFrame.h wsntNCLButton.h wsntSliderCtrl.h wsntStatusBar.h wsntarea.h wsntbitmap.h \
wsntbmpbtn.h wsntcfunc.h wsntchild1.h
OBJS7= \
usignal.ts ustdio.ts ustrings.ts usysdef.ts usysg.ts utime.ts uvarargs.ts \
uversion.ts vconst.ts vda.ts version.ts view.ts view0.ts view1.ts \
vsegbf.ts ws.ts ws104.ts ws4014em.ts ws410x.ts ws410xc.ts ws411x.ts \
ws411xc.ts ws433x.ts ws7475.ts ws7580.ts ws7580e.ts ws7580i.ts wsapo.ts \
wscim.ts wscimdefs.ts wsconvert.ts wsgc.ts wsgcdev.ts wsgcfo.ts wsgctrm.ts \
wsgcur.ts wsgl.ts wsglfun.ts wsi24.ts wsi24im.ts wsi24trm.ts wsi4d.ts \
wsi4dim.ts wsi4dtrm.ts wsimath.ts wsjpgimg.ts wsmc.ts wsmf.ts wsmfmdesgn.ts \
wsncldockbar.ts wsncldockframe.ts wsnclframe.ts wsncltoolbar.ts wsnews.ts wsntDDButton.ts wsntDDCombo.ts \
wsntDDEdit.ts wsntDDListBox.ts wsntDDProcess.ts wsntDDSlider.ts wsntDDclrbtn.ts wsntDDform.ts wsntDDgroup.ts \
wsntDDlstctl.ts wsntDDlstctl2.ts wsntDDpic.ts wsntDDstatic.ts wsntFdsnFrm.ts wsntFdsnView.ts wsntFormProp.ts \
wsntIconListBox.ts wsntModalFrame.ts wsntNCLButton.ts wsntSliderCtrl.ts wsntStatusBar.ts wsntarea.ts wsntbitmap.ts \
wsntbmpbtn.ts wsntcfunc.ts wsntchild1.ts
NOBJS7= \
usignal.h ustdio.h ustrings.h usysdef.h usysg.h utime.h uvarargs.h \
uversion.h vconst.h vda.h version.h view.h view0.h view1.h \
vsegbf.h ws.h ws104.h ws4014em.h ws410x.h ws410xc.h ws411x.h \
ws411xc.h ws433x.h ws7475.h ws7580.h ws7580e.h ws7580i.h wsapo.h \
wscim.h wscimdefs.h wsconvert.h wsgc.h wsgcdev.h wsgcfo.h wsgctrm.h \
wsgcur.h wsgl.h wsglfun.h wsi24.h wsi24im.h wsi24trm.h wsi4d.h \
wsi4dim.h wsi4dtrm.h wsimath.h wsjpgimg.h wsmc.h wsmf.h wsmfmdesgn.h \
wsncldockbar.h wsncldockframe.h wsnclframe.h wsncltoolbar.h wsnews.h wsntDDButton.h wsntDDCombo.h \
wsntDDEdit.h wsntDDListBox.h wsntDDProcess.h wsntDDSlider.h wsntDDclrbtn.h wsntDDform.h wsntDDgroup.h \
wsntDDlstctl.h wsntDDlstctl2.h wsntDDpic.h wsntDDstatic.h wsntFdsnFrm.h wsntFdsnView.h wsntFormProp.h \
wsntIconListBox.h wsntModalFrame.h wsntNCLButton.h wsntSliderCtrl.h wsntStatusBar.h wsntarea.h wsntbitmap.h \
wsntbmpbtn.h wsntcfunc.h wsntchild1.h
OBJS327= \
usignal.t32 ustdio.t32 ustrings.t32 usysdef.t32 usysg.t32 utime.t32 uvarargs.t32 \
uversion.t32 vconst.t32 vda.t32 version.t32 view.t32 view0.t32 view1.t32 \
vsegbf.t32 ws.t32 ws104.t32 ws4014em.t32 ws410x.t32 ws410xc.t32 ws411x.t32 \
ws411xc.t32 ws433x.t32 ws7475.t32 ws7580.t32 ws7580e.t32 ws7580i.t32 wsapo.t32 \
wscim.t32 wscimdefs.t32 wsconvert.t32 wsgc.t32 wsgcdev.t32 wsgcfo.t32 wsgctrm.t32 \
wsgcur.t32 wsgl.t32 wsglfun.t32 wsi24.t32 wsi24im.t32 wsi24trm.t32 wsi4d.t32 \
wsi4dim.t32 wsi4dtrm.t32 wsimath.t32 wsjpgimg.t32 wsmc.t32 wsmf.t32 wsmfmdesgn.t32 \
wsncldockbar.t32 wsncldockframe.t32 wsnclframe.t32 wsncltoolbar.t32 wsnews.t32 wsntDDButton.t32 wsntDDCombo.t32 \
wsntDDEdit.t32 wsntDDListBox.t32 wsntDDProcess.t32 wsntDDSlider.t32 wsntDDclrbtn.t32 wsntDDform.t32 wsntDDgroup.t32 \
wsntDDlstctl.t32 wsntDDlstctl2.t32 wsntDDpic.t32 wsntDDstatic.t32 wsntFdsnFrm.t32 wsntFdsnView.t32 wsntFormProp.t32 \
wsntIconListBox.t32 wsntModalFrame.t32 wsntNCLButton.t32 wsntSliderCtrl.t32 wsntStatusBar.t32 wsntarea.t32 wsntbitmap.t32 \
wsntbmpbtn.t32 wsntcfunc.t32 wsntchild1.t32
NOBJS327= \
usignal.h ustdio.h ustrings.h usysdef.h usysg.h utime.h uvarargs.h \
uversion.h vconst.h vda.h version.h view.h view0.h view1.h \
vsegbf.h ws.h ws104.h ws4014em.h ws410x.h ws410xc.h ws411x.h \
ws411xc.h ws433x.h ws7475.h ws7580.h ws7580e.h ws7580i.h wsapo.h \
wscim.h wscimdefs.h wsconvert.h wsgc.h wsgcdev.h wsgcfo.h wsgctrm.h \
wsgcur.h wsgl.h wsglfun.h wsi24.h wsi24im.h wsi24trm.h wsi4d.h \
wsi4dim.h wsi4dtrm.h wsimath.h wsjpgimg.h wsmc.h wsmf.h wsmfmdesgn.h \
wsncldockbar.h wsncldockframe.h wsnclframe.h wsncltoolbar.h wsnews.h wsntDDButton.h wsntDDCombo.h \
wsntDDEdit.h wsntDDListBox.h wsntDDProcess.h wsntDDSlider.h wsntDDclrbtn.h wsntDDform.h wsntDDgroup.h \
wsntDDlstctl.h wsntDDlstctl2.h wsntDDpic.h wsntDDstatic.h wsntFdsnFrm.h wsntFdsnView.h wsntFormProp.h \
wsntIconListBox.h wsntModalFrame.h wsntNCLButton.h wsntSliderCtrl.h wsntStatusBar.h wsntarea.h wsntbitmap.h \
wsntbmpbtn.h wsntcfunc.h wsntchild1.h
SRCS8= \
wsntchild2.h wsntchild3.h wsntchild4.h wsntclrbtn.h wsntclrcell.h wsntclrdlg.h wsntcmdbar.h \
wsntctl.h wsntctlsetting.h wsntdir.h wsntdlgbar.h wsntdlgitem.h wsntdoc.h wsntdockcont.h \
wsntdrawbutton.h wsntdropsource.h wsntdroptarget.h wsnteditctl.h wsntfile.h wsntform.h wsntformbar.h \
wsntformdoc.h wsntformfrm.h wsntframe.h wsntfrmFview.h wsntfrmMview.h wsntfrmPview.h wsntfrmview.h \
wsntfsview.h wsntfuncid.h wsntgettxt.h wsntgettxt2.h wsntglfunc.h wsntgphwin.h wsntgraphic.h \
wsntheaderctrl.h wsnthls.h wsnthuesatchooser.h wsntlstctl.h wsntlstctl2.h wsntlumchooser.h wsntmdroptarget.h \
wsntmdroptarget2.h wsntmenudsndlg.h wsntmmap.h wsntopt.h wsntpalettectl.h wsntpalettewnd.h wsntpicsel.h \
wsntpktwin.h wsntpmenu.h wsntres.h wsntsecbtn.h wsntsecprop.h wsntsmouse.h wsntstatbtn.h \
wsntstdafx.h wsnttblst.h wsnttblst2.h wsnttmenu.h wsnttoolbar.h wsnttooltip.h wsnttxtwin.h \
wsntview.h wsntwininfo.h wsps.h wssgp.h wssgpD.h wssgpDI.h wsskel.h \
wstty.h wsxw.h xenv1.h xfsys0.h xfsys1.h xfsys2.h ycom.h \
ydebug.h z2frmed.h z2pipe.h
OBJS8= \
wsntchild2.ts wsntchild3.ts wsntchild4.ts wsntclrbtn.ts wsntclrcell.ts wsntclrdlg.ts wsntcmdbar.ts \
wsntctl.ts wsntctlsetting.ts wsntdir.ts wsntdlgbar.ts wsntdlgitem.ts wsntdoc.ts wsntdockcont.ts \
wsntdrawbutton.ts wsntdropsource.ts wsntdroptarget.ts wsnteditctl.ts wsntfile.ts wsntform.ts wsntformbar.ts \
wsntformdoc.ts wsntformfrm.ts wsntframe.ts wsntfrmFview.ts wsntfrmMview.ts wsntfrmPview.ts wsntfrmview.ts \
wsntfsview.ts wsntfuncid.ts wsntgettxt.ts wsntgettxt2.ts wsntglfunc.ts wsntgphwin.ts wsntgraphic.ts \
wsntheaderctrl.ts wsnthls.ts wsnthuesatchooser.ts wsntlstctl.ts wsntlstctl2.ts wsntlumchooser.ts wsntmdroptarget.ts \
wsntmdroptarget2.ts wsntmenudsndlg.ts wsntmmap.ts wsntopt.ts wsntpalettectl.ts wsntpalettewnd.ts wsntpicsel.ts \
wsntpktwin.ts wsntpmenu.ts wsntres.ts wsntsecbtn.ts wsntsecprop.ts wsntsmouse.ts wsntstatbtn.ts \
wsntstdafx.ts wsnttblst.ts wsnttblst2.ts wsnttmenu.ts wsnttoolbar.ts wsnttooltip.ts wsnttxtwin.ts \
wsntview.ts wsntwininfo.ts wsps.ts wssgp.ts wssgpD.ts wssgpDI.ts wsskel.ts \
wstty.ts wsxw.ts xenv1.ts xfsys0.ts xfsys1.ts xfsys2.ts ycom.ts \
ydebug.ts z2frmed.ts z2pipe.ts
NOBJS8= \
wsntchild2.h wsntchild3.h wsntchild4.h wsntclrbtn.h wsntclrcell.h wsntclrdlg.h wsntcmdbar.h \
wsntctl.h wsntctlsetting.h wsntdir.h wsntdlgbar.h wsntdlgitem.h wsntdoc.h wsntdockcont.h \
wsntdrawbutton.h wsntdropsource.h wsntdroptarget.h wsnteditctl.h wsntfile.h wsntform.h wsntformbar.h \
wsntformdoc.h wsntformfrm.h wsntframe.h wsntfrmFview.h wsntfrmMview.h wsntfrmPview.h wsntfrmview.h \
wsntfsview.h wsntfuncid.h wsntgettxt.h wsntgettxt2.h wsntglfunc.h wsntgphwin.h wsntgraphic.h \
wsntheaderctrl.h wsnthls.h wsnthuesatchooser.h wsntlstctl.h wsntlstctl2.h wsntlumchooser.h wsntmdroptarget.h \
wsntmdroptarget2.h wsntmenudsndlg.h wsntmmap.h wsntopt.h wsntpalettectl.h wsntpalettewnd.h wsntpicsel.h \
wsntpktwin.h wsntpmenu.h wsntres.h wsntsecbtn.h wsntsecprop.h wsntsmouse.h wsntstatbtn.h \
wsntstdafx.h wsnttblst.h wsnttblst2.h wsnttmenu.h wsnttoolbar.h wsnttooltip.h wsnttxtwin.h \
wsntview.h wsntwininfo.h wsps.h wssgp.h wssgpD.h wssgpDI.h wsskel.h \
wstty.h wsxw.h xenv1.h xfsys0.h xfsys1.h xfsys2.h ycom.h \
ydebug.h z2frmed.h z2pipe.h
OBJS328= \
wsntchild2.t32 wsntchild3.t32 wsntchild4.t32 wsntclrbtn.t32 wsntclrcell.t32 wsntclrdlg.t32 wsntcmdbar.t32 \
wsntctl.t32 wsntctlsetting.t32 wsntdir.t32 wsntdlgbar.t32 wsntdlgitem.t32 wsntdoc.t32 wsntdockcont.t32 \
wsntdrawbutton.t32 wsntdropsource.t32 wsntdroptarget.t32 wsnteditctl.t32 wsntfile.t32 wsntform.t32 wsntformbar.t32 \
wsntformdoc.t32 wsntformfrm.t32 wsntframe.t32 wsntfrmFview.t32 wsntfrmMview.t32 wsntfrmPview.t32 wsntfrmview.t32 \
wsntfsview.t32 wsntfuncid.t32 wsntgettxt.t32 wsntgettxt2.t32 wsntglfunc.t32 wsntgphwin.t32 wsntgraphic.t32 \
wsntheaderctrl.t32 wsnthls.t32 wsnthuesatchooser.t32 wsntlstctl.t32 wsntlstctl2.t32 wsntlumchooser.t32 wsntmdroptarget.t32 \
wsntmdroptarget2.t32 wsntmenudsndlg.t32 wsntmmap.t32 wsntopt.t32 wsntpalettectl.t32 wsntpalettewnd.t32 wsntpicsel.t32 \
wsntpktwin.t32 wsntpmenu.t32 wsntres.t32 wsntsecbtn.t32 wsntsecprop.t32 wsntsmouse.t32 wsntstatbtn.t32 \
wsntstdafx.t32 wsnttblst.t32 wsnttblst2.t32 wsnttmenu.t32 wsnttoolbar.t32 wsnttooltip.t32 wsnttxtwin.t32 \
wsntview.t32 wsntwininfo.t32 wsps.t32 wssgp.t32 wssgpD.t32 wssgpDI.t32 wsskel.t32 \
wstty.t32 wsxw.t32 xenv1.t32 xfsys0.t32 xfsys1.t32 xfsys2.t32 ycom.t32 \
ydebug.t32 z2frmed.t32 z2pipe.t32
NOBJS328= \
wsntchild2.h wsntchild3.h wsntchild4.h wsntclrbtn.h wsntclrcell.h wsntclrdlg.h wsntcmdbar.h \
wsntctl.h wsntctlsetting.h wsntdir.h wsntdlgbar.h wsntdlgitem.h wsntdoc.h wsntdockcont.h \
wsntdrawbutton.h wsntdropsource.h wsntdroptarget.h wsnteditctl.h wsntfile.h wsntform.h wsntformbar.h \
wsntformdoc.h wsntformfrm.h wsntframe.h wsntfrmFview.h wsntfrmMview.h wsntfrmPview.h wsntfrmview.h \
wsntfsview.h wsntfuncid.h wsntgettxt.h wsntgettxt2.h wsntglfunc.h wsntgphwin.h wsntgraphic.h \
wsntheaderctrl.h wsnthls.h wsnthuesatchooser.h wsntlstctl.h wsntlstctl2.h wsntlumchooser.h wsntmdroptarget.h \
wsntmdroptarget2.h wsntmenudsndlg.h wsntmmap.h wsntopt.h wsntpalettectl.h wsntpalettewnd.h wsntpicsel.h \
wsntpktwin.h wsntpmenu.h wsntres.h wsntsecbtn.h wsntsecprop.h wsntsmouse.h wsntstatbtn.h \
wsntstdafx.h wsnttblst.h wsnttblst2.h wsnttmenu.h wsnttoolbar.h wsnttooltip.h wsnttxtwin.h \
wsntview.h wsntwininfo.h wsps.h wssgp.h wssgpD.h wssgpDI.h wsskel.h \
wstty.h wsxw.h xenv1.h xfsys0.h xfsys1.h xfsys2.h ycom.h \
ydebug.h z2frmed.h z2pipe.h
SRCS9= \
z4pipe.h zaxestut.h zcalctut.h zdrfttut.h zedittut.h zenvtut.h zfeatut.h \
zfiletut.h zgeomtut.h zkeysym.h zpipe.h zplottut.h zpmgttut.h zseltut.h \
zsysdep.h zunitut.h zviewtut.h
OBJS9= \
z4pipe.ts zaxestut.ts zcalctut.ts zdrfttut.ts zedittut.ts zenvtut.ts zfeatut.ts \
zfiletut.ts zgeomtut.ts zkeysym.ts zpipe.ts zplottut.ts zpmgttut.ts zseltut.ts \
zsysdep.ts zunitut.ts zviewtut.ts
NOBJS9= \
z4pipe.h zaxestut.h zcalctut.h zdrfttut.h zedittut.h zenvtut.h zfeatut.h \
zfiletut.h zgeomtut.h zkeysym.h zpipe.h zplottut.h zpmgttut.h zseltut.h \
zsysdep.h zunitut.h zviewtut.h
OBJS329= \
z4pipe.t32 zaxestut.t32 zcalctut.t32 zdrfttut.t32 zedittut.t32 zenvtut.t32 zfeatut.t32 \
zfiletut.t32 zgeomtut.t32 zkeysym.t32 zpipe.t32 zplottut.t32 zpmgttut.t32 zseltut.t32 \
zsysdep.t32 zunitut.t32 zviewtut.t32
NOBJS329= \
z4pipe.h zaxestut.h zcalctut.h zdrfttut.h zedittut.h zenvtut.h zfeatut.h \
zfiletut.h zgeomtut.h zkeysym.h zpipe.h zplottut.h zpmgttut.h zseltut.h \
zsysdep.h zunitut.h zviewtut.h
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
DIR=src
SRCDIR=inc
NCLLIBRARY=inc.lib
NNCLLIBRARY=ninc.lib
NCLLIBRARY32=inc32.lib
NNCLLIBRARY32=ninc32.lib

#
#---------------------------EOF make.list --------------------------
#
#
#--------------------------- inc.rules.wnt --------------------------
#
#
# make debug stuff
#
debugrel: makeall
nodebugrel: makeall
nmakeall: makeall

oml: $(SRCS) nccsddl.h

makeall: cleanup $(SRCS) isysddl.h wsntncl.res iges.res plot.res toolib.res

.rc.res:
	-$(RM) $(NNCLLIB)\$*.res
	$(RSC) $(RCFLAGS) /fo $*.res $<
	$(MV) $*.res $(NNCLLIB)

isysddl.h: isysddl.ddl
	-$(RM) r*.c
	$(DDLTOOL32) isysddl.ddl
	$(MV) riatrdct.h riatrdct32.h
	-$(RM) r*.c
	$(DDLTOOL) isysddl.ddl
	$(MV) r*.c $(NCLPATH)\src\unibase 

nccsddl.h: isysddl.ddl
	-$(DDLTOOL) isysddl.ddl
	-$(RM) r*.c
#
#---------------------------EOF inc.rules.wnt --------------------------
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
