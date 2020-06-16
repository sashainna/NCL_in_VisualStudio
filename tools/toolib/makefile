#
#--------------------------- make.toolib.wnt --------------------------
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
	/D "_UNICODE" /D "UNICODE" /Fo".\\" /FD /c
DEVCPFLAGS=$(DEVCFLAGS) /D "_AFXDLL"
DEVFFLAGS=/integer_size:16 /compile_only /nologo /libs:dll /names:lowercase \
	/iface:nomixed_str_len_arg /fpscomp:general /iface:cref \
	/warn:nofileopt 
DEVRCFLAGS=/l 0x409 /d "_AFXDDL"

FINCLUDE=/include:$(NCLPATH)\src\incf
FSYS=k
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
#---------------------------EOF make.toolib.wnt --------------------------
#
#
#--------------------------- make.prefix.tools.wnt --------------------------
#
NCLBIN=$(NCLPATH)\$(BIN)
NCLSBIN=$(NCLPATH)\$(SBIN)
#
MUTOOL=$(NCLBIN)\mu
ITRTOOL=$(NCLBIN)\itr
MSGTOOL=$(NCLBIN)\msg -u
NCLMSGTOOL=$(NCLPATH)\$(SBIN)\nclmsgtool
DDLTOOL=$(NCLBIN)\ddltool
DDLTOOL32=$(NCLBIN)\ddltool32
#
# NCL includes, libraries and tools
#
NCLINC=$(NCLPATH)\src\inc
NCLFILES=$(NCLPATH)\nclfiles
NCLLIB=$(NCLPATH)\$(LIB)
NNCLLIB=$(NCLPATH)\$(NLIB)
NCLLIB32=$(NCLPATH)\$(LIB32)
NNCLLIB32=$(NCLPATH)\$(NLIB32)
NCLBIN=$(NCLPATH)\$(BIN)
#
SMINC=C:\Program Files (x86)\3Dconnexion\3DxWare SDK\inc
#
FORFILTER=$(NCLPATH)\$(BIN)\filter
IGESDIR=$(NCLPATH)\tools\iges
SCCSTOOL=$(NCLPATH)\$(SBIN)\formtool
#
CINCLUDE=/I $(NCLINC) /I $(LI_DIR)\include /I $(LI_DIR)\include\$(LI_ARCH)
CPINCLUDE=$(CINCLUDE) /I "$(VCD3)\include" /I "$(VCD2)\Include" \
	/I "$(VCD1)\MFC\include" /I "$(VCD1)\ATL/nclude" "$(SMINC)"
RCINCLUDE=/i "$(NCLINC)" /i "$(NNCLLIB)\obj" /i "$(VCD1)\MFC\Include"

CFLAGS=$(CINCLUDE) $(DEVCFLAGS) $(UU_COMP) $(UU_PREC) $(UU_SEGDATA) $(UU_DEBUG) $(UU_NCLIPV)
CPFLAGS=$(CPINCLUDE) $(DEVCPFLAGS) $(UU_COMP) $(UU_PREC) $(UU_SEGDATA) $(UU_DEBUG)
FFLAGS=$(FINCLUDE) $(DEVFFLAGS)
RCFLAGS=$(RCINCLUDE) $(DEVRCFLAGS)
#
NOERROR=-DUU_NOERROR
LDFLAGS=$(OPFLAGS) -lm -o
LOADFLG=$(DEVLDFLAGS) $(LDFLAGS)
UTLFLAGS=$(CFLAGS) -DUU_VMAIN -c
#
# Compile variations
#
CC=cl $(DEBUG)
CPP=cl $(DEBUG)
F77=ifort $(DEB) -c
NCC=cl $(NDEBUG)
NCPP=cl $(NDEBUG)
NF77=ifort $(NDEB) -c
RSC=rc
#
# define some general things
#
AR=lib
CP=cp 
RM=erase
MV=move
TOUCH=date /t >
#
# debug library
#
RMLIB=$(RM) $(NCLLIB)\$@
CRELIB1=$(AR) /out:$(NCLLIB)\$(NCLLIBRARY) $(NCLLIB)\$(NCLLIBRARY) *.obj
CRELIB2=$(AR) /out:$(NCLLIB)\$(NCLLIBRARY) *.obj
#
# nodebug library
#
NRMLIB=$(RM) $(NNCLLIB)\$@
NCRELIB1=$(AR) /out:$(NNCLLIB)\$(NNCLLIBRARY) $(NNCLLIB)\$(NNCLLIBRARY) *.obj
NCRELIB2=$(AR) /out:$(NNCLLIB)\$(NNCLLIBRARY) *.obj
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
.SUFFIXES: .c .cpp .f .for .ts .nts .t32 .nt32 .obj .o
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
	@echo "*****  MAKING DEBUG VERSION OF $<  *****"
	$(F77) $(FFLAGS) $*.f
	$(PLACE_OBJ)
	$(TOUCH) $@
	@echo "***** FINISHED COMPILING $< IN $(SRCDIR) *****"
	@echo ""
.for.ts:
	@set PATH=$(PATH64)
	$(RM) $*.ts
	@echo "*****  MAKING DEBUG VERSION OF $<  *****"
	$(FORFILTER) $<,$(FSYS)
	$(F77) $(FFLAGS) $*.f
	$(RM) $*.f
	$(PLACE_OBJ)
	$(TOUCH) $@
	@echo "***** FINISHED COMPILING FILES IN $(SRCDIR) *****"
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
	@echo "*****  MAKING NODEBUG VERSION OF $<  *****"
	$(NF77) $(FFLAGS) $*.f
	$(PLACE_NOBJ)
	$(TOUCH) $@
	@echo "***** FINISHED COMPILING $< IN $(SRCDIR) *****"
	@echo ""
.for.nts:
	@set PATH=$(PATH64)
	$(RM) $*.ts
	@echo "*****  MAKING NODEBUG VERSION OF $<  *****"
	$(FORFILTER) $<,$(FSYS)
	$(NF77) $(FFLAGS) $*.f
	$(RM) $*.f
	$(PLACE_NOBJ)
	$(TOUCH) $@
	@echo "***** FINISHED COMPILING FILES IN $(SRCDIR) *****"
	@echo ""
#
# 32-bit debug rules
#
.c.t32:
	@set PATH=$(PATH32)
	$(RM) $@
	@echo "*****  MAKING DEBUG VERSION OF $<  *****"
	$(CC) $(CFLAGS) /D "BUILD_32" $<
	$(PLACE_OBJ32)
	$(TOUCH) $@
	@echo "***** FINISHED BUILDING $< IN $(SRCDIR) *****"
	@echo ""
.cpp.t32:
	@set PATH=$(PATH32)
	$(RM) $@
	@echo "*****  MAKING DEBUG VERSION OF $<  *****"
	$(CPP) $(CPFLAGS) /D "BUILD_32" $<
	$(PLACE_OBJ32)
	$(TOUCH) $@
	@echo "***** FINISHED BUILDING $< IN $(SRCDIR) *****"
	@echo ""
.f.t32:
	@set PATH=$(PATH32)
	$(RM) $@
	@echo "*****  MAKING DEBUG VERSION OF $<  *****"
	$(F77) $(FFLAGS) $*.f
	$(PLACE_OBJ32)
	$(TOUCH) $@
	@echo "***** FINISHED COMPILING $< IN $(SRCDIR) *****"
	@echo ""
.for.t32:
	@set PATH=$(PATH32)
	$(RM) $*.t32
	@echo "*****  MAKING DEBUG VERSION OF $<  *****"
	$(FORFILTER) $<,$(FSYS)
	$(F77) $(FFLAGS) $*.f
	$(RM) $*.f
	$(PLACE_OBJ32)
	$(TOUCH) $@
	@echo "***** FINISHED COMPILING FILES IN $(SRCDIR) *****"
	@echo ""
#
# 32-bit nodebug rules
#
.c.nt32:
	@set PATH=$(PATH32)
	$(RM) $@
	@echo "*****  MAKING NODEBUG VERSION OF $<  *****"
	$(NCC) $(CFLAGS) /D "BUILD_32" $<
	$(PLACE_NOBJ32)
	$(TOUCH) $@
	@echo "***** FINISHED BUILDING $< IN $(SRCDIR) *****"
	@echo ""
.cpp.nt32:
	@set PATH=$(PATH32)
	$(RM) $@
	@echo "*****  MAKING NODEBUG VERSION OF $<  *****"
	$(NCPP) $(CPFLAGS) /D "BUILD_32" $<
	$(PLACE_NOBJ32)
	$(TOUCH) $@
	@echo "***** FINISHED BUILDING $< IN $(SRCDIR) *****"
	@echo ""
.f.nt32:
	@set PATH=$(PATH32)
	$(RM) $@
	@echo "*****  MAKING NODEBUG VERSION OF $<  *****"
	$(NF77) $(FFLAGS) $*.f
	$(PLACE_NOBJ32)
	$(TOUCH) $@
	@echo "***** FINISHED COMPILING $< IN $(SRCDIR) *****"
	@echo ""
.for.nt32:
	@set PATH=$(PATH32)
	$(RM) $*.nt32
	@echo "*****  MAKING NODEBUG VERSION OF $<  *****"
	$(FORFILTER) $<,$(FSYS)
	$(NF77) $(FFLAGS) $*.f
	$(RM) $*.f
	$(PLACE_NOBJ32)
	$(TOUCH) $@
	@echo "***** FINISHED COMPILING FILES IN $(SRCDIR) *****"
	@echo ""
#
#---------------------------EOF make.prefix.tools.wnt --------------------------
#
#
#--------------------------- make.list --------------------------
#
SRCS1= \
char1.for opnfil.for support.for supportc.c toolenv.c toolioc.c toolmfc.c \
toolsupt.c toolumb.c ComDlg.cpp CutDlg.cpp DefineDlg.cpp MainFrm.cpp ModListDlg.cpp \
OperateDlg.cpp ParmsDlg.cpp ToolChildVw.cpp ToolDispDlg.cpp ToolFindDlg.cpp ToolPromptDlg.cpp toolgettxt2.cpp \
toolglobal.cpp toolibDoc.cpp toolibView.cpp toolibapp.cpp toolibstdafx.cpp toollstctl.cpp toolopt.cpp
OBJS1= \
char1.ts opnfil.ts support.ts supportc.ts toolenv.ts toolioc.ts toolmfc.ts \
toolsupt.ts toolumb.ts ComDlg.ts CutDlg.ts DefineDlg.ts MainFrm.ts ModListDlg.ts \
OperateDlg.ts ParmsDlg.ts ToolChildVw.ts ToolDispDlg.ts ToolFindDlg.ts ToolPromptDlg.ts toolgettxt2.ts \
toolglobal.ts toolibDoc.ts toolibView.ts toolibapp.ts toolibstdafx.ts toollstctl.ts toolopt.ts
NOBJS1= \
char1.nts opnfil.nts support.nts supportc.nts toolenv.nts toolioc.nts toolmfc.nts \
toolsupt.nts toolumb.nts ComDlg.nts CutDlg.nts DefineDlg.nts MainFrm.nts ModListDlg.nts \
OperateDlg.nts ParmsDlg.nts ToolChildVw.nts ToolDispDlg.nts ToolFindDlg.nts ToolPromptDlg.nts toolgettxt2.nts \
toolglobal.nts toolibDoc.nts toolibView.nts toolibapp.nts toolibstdafx.nts toollstctl.nts toolopt.nts
OBJS321= \
char1.t32 opnfil.t32 support.t32 supportc.t32 toolenv.t32 toolioc.t32 toolmfc.t32 \
toolsupt.t32 toolumb.t32 ComDlg.t32 CutDlg.t32 DefineDlg.t32 MainFrm.t32 ModListDlg.t32 \
OperateDlg.t32 ParmsDlg.t32 ToolChildVw.t32 ToolDispDlg.t32 ToolFindDlg.t32 ToolPromptDlg.t32 toolgettxt2.t32 \
toolglobal.t32 toolibDoc.t32 toolibView.t32 toolibapp.t32 toolibstdafx.t32 toollstctl.t32 toolopt.t32
NOBJS321= \
char1.nt32 opnfil.nt32 support.nt32 supportc.nt32 toolenv.nt32 toolioc.nt32 toolmfc.nt32 \
toolsupt.nt32 toolumb.nt32 ComDlg.nt32 CutDlg.nt32 DefineDlg.nt32 MainFrm.nt32 ModListDlg.nt32 \
OperateDlg.nt32 ParmsDlg.nt32 ToolChildVw.nt32 ToolDispDlg.nt32 ToolFindDlg.nt32 ToolPromptDlg.nt32 toolgettxt2.nt32 \
toolglobal.nt32 toolibDoc.nt32 toolibView.nt32 toolibapp.nt32 toolibstdafx.nt32 toollstctl.nt32 toolopt.nt32
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
DIR=tools
SRCDIR=toolib
NCLLIBRARY=toolib.lib
NNCLLIBRARY=ntoolib.lib
NCLLIBRARY32=toolib32.lib
NNCLLIBRARY32=ntoolib32.lib

#
#---------------------------EOF make.list --------------------------
#
#
#--------------------------- make.middle.tools.wnt --------------------------
#
#
# release labels
#
makeall: $(OBJS) mklib
nmakeall: cleanup $(SRCS) wait $(NOBJS) nmklib makeall makeall32 nmakeall32
makeall32: $(OBJS32) mklib32
nmakeall32: $(NOBJS32) nmklib32
updatelib: $(NCLLIBRARY)
updatenlib: $(NNCLLIBRARY) updatelib updatelib32 updatenlib32
updatelib32: $(NCLLIBRARY32)
updatenlib32: $(NNCLLIBRARY32)
#
# Suffixes
#
.SUFFIXES: .ddl .sql .ts .nts .t32 .nt32 .obj .o

$(NCLLIBRARY): $(OBJS)
	$(CRELIB1)
	-$(RM) *.obj
!IF "$(SYMLIB)" != ""
	-$(RM) $(SYMLIB)
!ENDIF

$(NCLLIBRARY32): $(OBJS32)
	$(CRELIB321)
	-$(RM) *.obj
!IF "$(SYMLIB)" != ""
	-$(RM) $(SYMLIB)
!ENDIF

mklib:
	$(CRELIB2)
	-$(RM) *.obj
!IF "$(SYMLIB)" != ""
	-$(RM) $(SYMLIB)
!ENDIF

mklib32:
	$(CRELIB322)
	-$(RM) *.obj
!IF "$(SYMLIB)" != ""
	-$(RM) $(SYMLIB)
!ENDIF
#
# make nodebug stuff
#
$(NNCLLIBRARY): $(NOBJS)
	$(NCRELIB1)
	-$(RM) *.obj
!IF "$(SYMLIB)" != ""
	-$(RM) $(SYMLIB)
!ENDIF

$(NNCLLIBRARY32): $(NOBJS32)
	$(NCRELIB321)
	-$(RM) *.obj
!IF "$(SYMLIB)" != ""
	-$(RM) $(SYMLIB)
!ENDIF

nmklib:
	$(NCRELIB2)
	-$(RM) *.obj
!IF "$(SYMLIB)" != ""
	-$(RM) $(SYMLIB)
!ENDIF

nmklib32:
	$(NCRELIB322)
	-$(RM) *.obj
!IF "$(SYMLIB)" != ""
	-$(RM) $(SYMLIB)
!ENDIF
#
# do nothing with .ddl and .h files ... (for conv directory)
#
.ddl.sql:
	touch $*.sql
	@echo "***** FINISHED BUILDING $*.sql IN $(SRCDIR) *****"
.h.ts:
	touch $*.ts
	@echo "***** FINISHED BUILDING $*.ts IN $(SRCDIR) *****"
.h.nts:
	touch $*.nts
	@echo "***** FINISHED BUILDING $*.nts IN $(SRCDIR) *****"
#
# get and set current version number for the following tools:
#	plot, pcnv, conv, dcnv
#
nclver:
	rmts ncqcom
	rmts supportc

#
#--------------------------- EOF toolib.rules.wnt --------------------------
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
