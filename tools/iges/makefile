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
tiges.ddl igspln.f igsupt.f igesmesh.c igesmfmain.c igspline.c supportc.c \
tigclas.c tigcompar.c tigconsp.c tigconv.c tigcreat.c tigdbsp.c tigdisp.c \
tigdrf1.c tigdrf2.c tigdrwt.c tigdtdct.c tigindrf.c tigingom.c tiginput.c \
tiginsym.c tigintrf.c tiginvie.c tigmain.c tigmapd.c tigmapg.c tigmaps.c \
tigmapv.c tigmatch.c tigmfapp.c tigmfmatchattr.c tigmisc.c tigmodals.c tigmodsp.c \
tigmsgs.c tigmsrf.c tigsave.c tigstubs.c tigsupt.c tigsys.c tigtrimsf.c \
tigview.c tioconv.c tiodpsec.c tioeval.c tioinit.c tioloc.c tiomain.c \
tiomapd1.c tiomapd2.c tiomapg.c tiomaps.c tiomsrf.c tiomutil.c tiosgsec.c \
tiotrf.c vdaconv.c vdagetdata.c vdaintrf.c vdamain.c IgesAttrDlg.cpp IgesFiltDlg.cpp \
IgesListDlg.cpp IgesModalDlg.cpp IgesOptDlg.cpp IgesPromptDlg.cpp iges.cpp igesDlg.cpp igesDlg2.cpp \
igesglobal.cpp
OBJS1= \
tiges.sql igspln.ts igsupt.ts igesmesh.ts igesmfmain.ts igspline.ts supportc.ts \
tigclas.ts tigcompar.ts tigconsp.ts tigconv.ts tigcreat.ts tigdbsp.ts tigdisp.ts \
tigdrf1.ts tigdrf2.ts tigdrwt.ts tigdtdct.ts tigindrf.ts tigingom.ts tiginput.ts \
tiginsym.ts tigintrf.ts tiginvie.ts tigmain.ts tigmapd.ts tigmapg.ts tigmaps.ts \
tigmapv.ts tigmatch.ts tigmfapp.ts tigmfmatchattr.ts tigmisc.ts tigmodals.ts tigmodsp.ts \
tigmsgs.ts tigmsrf.ts tigsave.ts tigstubs.ts tigsupt.ts tigsys.ts tigtrimsf.ts \
tigview.ts tioconv.ts tiodpsec.ts tioeval.ts tioinit.ts tioloc.ts tiomain.ts \
tiomapd1.ts tiomapd2.ts tiomapg.ts tiomaps.ts tiomsrf.ts tiomutil.ts tiosgsec.ts \
tiotrf.ts vdaconv.ts vdagetdata.ts vdaintrf.ts vdamain.ts IgesAttrDlg.ts IgesFiltDlg.ts \
IgesListDlg.ts IgesModalDlg.ts IgesOptDlg.ts IgesPromptDlg.ts iges.ts igesDlg.ts igesDlg2.ts \
igesglobal.ts
NOBJS1= \
tiges.sql igspln.nts igsupt.nts igesmesh.nts igesmfmain.nts igspline.nts supportc.nts \
tigclas.nts tigcompar.nts tigconsp.nts tigconv.nts tigcreat.nts tigdbsp.nts tigdisp.nts \
tigdrf1.nts tigdrf2.nts tigdrwt.nts tigdtdct.nts tigindrf.nts tigingom.nts tiginput.nts \
tiginsym.nts tigintrf.nts tiginvie.nts tigmain.nts tigmapd.nts tigmapg.nts tigmaps.nts \
tigmapv.nts tigmatch.nts tigmfapp.nts tigmfmatchattr.nts tigmisc.nts tigmodals.nts tigmodsp.nts \
tigmsgs.nts tigmsrf.nts tigsave.nts tigstubs.nts tigsupt.nts tigsys.nts tigtrimsf.nts \
tigview.nts tioconv.nts tiodpsec.nts tioeval.nts tioinit.nts tioloc.nts tiomain.nts \
tiomapd1.nts tiomapd2.nts tiomapg.nts tiomaps.nts tiomsrf.nts tiomutil.nts tiosgsec.nts \
tiotrf.nts vdaconv.nts vdagetdata.nts vdaintrf.nts vdamain.nts IgesAttrDlg.nts IgesFiltDlg.nts \
IgesListDlg.nts IgesModalDlg.nts IgesOptDlg.nts IgesPromptDlg.nts iges.nts igesDlg.nts igesDlg2.nts \
igesglobal.nts
OBJS321= \
tiges.sql igspln.t32 igsupt.t32 igesmesh.t32 igesmfmain.t32 igspline.t32 supportc.t32 \
tigclas.t32 tigcompar.t32 tigconsp.t32 tigconv.t32 tigcreat.t32 tigdbsp.t32 tigdisp.t32 \
tigdrf1.t32 tigdrf2.t32 tigdrwt.t32 tigdtdct.t32 tigindrf.t32 tigingom.t32 tiginput.t32 \
tiginsym.t32 tigintrf.t32 tiginvie.t32 tigmain.t32 tigmapd.t32 tigmapg.t32 tigmaps.t32 \
tigmapv.t32 tigmatch.t32 tigmfapp.t32 tigmfmatchattr.t32 tigmisc.t32 tigmodals.t32 tigmodsp.t32 \
tigmsgs.t32 tigmsrf.t32 tigsave.t32 tigstubs.t32 tigsupt.t32 tigsys.t32 tigtrimsf.t32 \
tigview.t32 tioconv.t32 tiodpsec.t32 tioeval.t32 tioinit.t32 tioloc.t32 tiomain.t32 \
tiomapd1.t32 tiomapd2.t32 tiomapg.t32 tiomaps.t32 tiomsrf.t32 tiomutil.t32 tiosgsec.t32 \
tiotrf.t32 vdaconv.t32 vdagetdata.t32 vdaintrf.t32 vdamain.t32 IgesAttrDlg.t32 IgesFiltDlg.t32 \
IgesListDlg.t32 IgesModalDlg.t32 IgesOptDlg.t32 IgesPromptDlg.t32 iges.t32 igesDlg.t32 igesDlg2.t32 \
igesglobal.t32
NOBJS321= \
tiges.sql igspln.nt32 igsupt.nt32 igesmesh.nt32 igesmfmain.nt32 igspline.nt32 supportc.nt32 \
tigclas.nt32 tigcompar.nt32 tigconsp.nt32 tigconv.nt32 tigcreat.nt32 tigdbsp.nt32 tigdisp.nt32 \
tigdrf1.nt32 tigdrf2.nt32 tigdrwt.nt32 tigdtdct.nt32 tigindrf.nt32 tigingom.nt32 tiginput.nt32 \
tiginsym.nt32 tigintrf.nt32 tiginvie.nt32 tigmain.nt32 tigmapd.nt32 tigmapg.nt32 tigmaps.nt32 \
tigmapv.nt32 tigmatch.nt32 tigmfapp.nt32 tigmfmatchattr.nt32 tigmisc.nt32 tigmodals.nt32 tigmodsp.nt32 \
tigmsgs.nt32 tigmsrf.nt32 tigsave.nt32 tigstubs.nt32 tigsupt.nt32 tigsys.nt32 tigtrimsf.nt32 \
tigview.nt32 tioconv.nt32 tiodpsec.nt32 tioeval.nt32 tioinit.nt32 tioloc.nt32 tiomain.nt32 \
tiomapd1.nt32 tiomapd2.nt32 tiomapg.nt32 tiomaps.nt32 tiomsrf.nt32 tiomutil.nt32 tiosgsec.nt32 \
tiotrf.nt32 vdaconv.nt32 vdagetdata.nt32 vdaintrf.nt32 vdamain.nt32 IgesAttrDlg.nt32 IgesFiltDlg.nt32 \
IgesListDlg.nt32 IgesModalDlg.nt32 IgesOptDlg.nt32 IgesPromptDlg.nt32 iges.nt32 igesDlg.nt32 igesDlg2.nt32 \
igesglobal.nt32
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
SRCDIR=iges
NCLLIBRARY=iges.lib
NNCLLIBRARY=niges.lib
NCLLIBRARY32=iges32.lib
NNCLLIBRARY32=niges32.lib

#
#---------------------------EOF make.list --------------------------
#
#
#--------------------------- iges.rules.wnt --------------------------
#
CTAGS = ctags -wt `pwd`/*.[c,f,h]
VAXFORFILTER=(cd ../; $(INCFILTER) $(NCLDIR) $@; cd $(NCLDIR); $(MV) vax_$@ $@);
VAXFORRESET=(rm -f $@; $(GET) $(SCCSREL) $(HOME)$(DIR)/$(NCLDIR)/$@ $(WHERE));
#
#
#
.SUFFIXES: .ddl .sql 

.ddl.sql:
	@echo "*****  MAKING $<  *****"
	-$(RM) tiges.sql tiges.h
	-$(RM) $(NCLPATH)\src\inc\tigesddl.h
	-$(RM) $(NCLPATH)\src\inc\tigeldct.h
	-$(RM) $(NCLPATH)\src\inc\tigtrdct.h
	$(DDLTOOL32) $<
	$(MV) riatrdct.h $(NCLPATH)\src\inc\tigtrdct32.h
	-$(RM) tiges.sql tiges.h tigesddl.h r*.c r*.h
	$(DDLTOOL) $<
	$(MV) tigesddl.h $(NCLPATH)\src\inc
	$(MV) rireldct.h $(NCLPATH)\src\inc\tigeldct.h
	$(MV) riatrdct.h $(NCLPATH)\src\inc\tigtrdct.h
	$(RM) r*.c r*.h tiges.h
	touch $*.ts
	@echo "***** FINISHED BUILDING $< IN $(SRCDIR) *****"
#
# release labels
#
debugrel: makeall
nodebugrel: nmakeall
makeall: $(OBJS) mklib
nmakeall: cleanup $(SRCS) $(NOBJS) nmklib makeall makeall32 nmakeall32
makeall32: $(OBJS32) mklib32
nmakeall32: $(NOBJS32) nmklib32
updatelib: $(NCLLIBRARY)
updatenlib: $(NNCLLIBRARY) updatelib updatelib32 updatenlib32
updatelib32: $(NCLLIBRARY32)
updatenlib32: $(NNCLLIBRARY32)
#
# make debug stuff
#
$(NCLLIBRARY): $(OBJS) 
	$(CRELIB1)
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

mklib:
	$(CRELIB2)
	-$(RM) *.obj
!IF "$(SYMLIB)" != ""
	$(RM) $(SYMLIB)
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
	$(RM) $(SYMLIB)
!ENDIF

$(NNCLLIBRARY32): $(NOBJS32) 
	$(NCRELIB321)
	-$(RM) *.obj
!IF "$(SYMLIB)" != ""
	$(RM) $(SYMLIB)
!ENDIF

nmklib:
	$(NCRELIB2)
	-$(RM) *.obj
!IF "$(SYMLIB)" != ""
	$(RM) $(SYMLIB)
!ENDIF

nmklib32:
	$(NCRELIB322)
	-$(RM) *.obj
!IF "$(SYMLIB)" != ""
	$(RM) $(SYMLIB)
!ENDIF

#
#---------------------------EOF iges.rules.wnt --------------------------
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
