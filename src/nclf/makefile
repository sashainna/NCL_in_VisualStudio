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
aptsrc.for asnlun.for batch.for char.for clfile.for clword.for declsf.for \
declsh.for fio.for getkey.for gvtlin.for mosyn1.for motpn.for mshpre.for \
nclfin.for nclini.for ncltim.for nwait.for postp.for ppword.for quipre.for \
revers.for rsttrm.for runvt.for spwn.for statln.for unsol.for urest.for \
voyagr.for acibci.f acibln.f alnbci.f altcom.f angcal.f aptcl.f \
ascrub.f assgn.f atan2f.f barseg.f basesf.f beginp.f begpas.f \
bentst.f berr.f bscrub.f bsfpre.f buildn.f callit.f calrst.f \
canf.f cctrn.f chkent.f chkinp.f chklab.f chkpts.f \
chkvoc.f chkzsf.f chr2int.f chr2real.f cidist.f cintck.f cirapt.f \
cirbld.f circpt.f circpv.f cirdef.f cirpnt.f cirpre.f cirprg.f \
cirprh.f cirpri.f cirprj.f cirrec.f clcept.f clnssf.f clone.f \
clput.f condif.f conent.f conpre.f contck.f convert.f convid.f \
convpt.f convtk.f copy1.f copy2.f crvbld.f crvcls.f crvevl.f \
crvfit.f crvgen.f crvgn1.f crvlen.f crvpnt.f crvpre.f csdist.f \
csrel.f csrelx.f cssurf.f curnam.f curvpv.f cutget.f cutool.f \
cutool2.f cutseg.f cvepts.f cveval.f
OBJS1= \
aptsrc.ts asnlun.ts batch.ts char.ts clfile.ts clword.ts declsf.ts \
declsh.ts fio.ts getkey.ts gvtlin.ts mosyn1.ts motpn.ts mshpre.ts \
nclfin.ts nclini.ts ncltim.ts nwait.ts postp.ts ppword.ts quipre.ts \
revers.ts rsttrm.ts runvt.ts spwn.ts statln.ts unsol.ts urest.ts \
voyagr.ts acibci.ts acibln.ts alnbci.ts altcom.ts angcal.ts aptcl.ts \
ascrub.ts assgn.ts atan2f.ts barseg.ts basesf.ts beginp.ts begpas.ts \
bentst.ts berr.ts bscrub.ts bsfpre.ts buildn.ts callit.ts calrst.ts \
canf.ts cctrn.ts chkent.ts chkinp.ts chklab.ts chkpts.ts \
chkvoc.ts chkzsf.ts chr2int.ts chr2real.ts cidist.ts cintck.ts cirapt.ts \
cirbld.ts circpt.ts circpv.ts cirdef.ts cirpnt.ts cirpre.ts cirprg.ts \
cirprh.ts cirpri.ts cirprj.ts cirrec.ts clcept.ts clnssf.ts clone.ts \
clput.ts condif.ts conent.ts conpre.ts contck.ts convert.ts convid.ts \
convpt.ts convtk.ts copy1.ts copy2.ts crvbld.ts crvcls.ts crvevl.ts \
crvfit.ts crvgen.ts crvgn1.ts crvlen.ts crvpnt.ts crvpre.ts csdist.ts \
csrel.ts csrelx.ts cssurf.ts curnam.ts curvpv.ts cutget.ts cutool.ts \
cutool2.ts cutseg.ts cvepts.ts cveval.ts
NOBJS1= \
aptsrc.nts asnlun.nts batch.nts char.nts clfile.nts clword.nts declsf.nts \
declsh.nts fio.nts getkey.nts gvtlin.nts mosyn1.nts motpn.nts mshpre.nts \
nclfin.nts nclini.nts ncltim.nts nwait.nts postp.nts ppword.nts quipre.nts \
revers.nts rsttrm.nts runvt.nts spwn.nts statln.nts unsol.nts urest.nts \
voyagr.nts acibci.nts acibln.nts alnbci.nts altcom.nts angcal.nts aptcl.nts \
ascrub.nts assgn.nts atan2f.nts barseg.nts basesf.nts beginp.nts begpas.nts \
bentst.nts berr.nts bscrub.nts bsfpre.nts buildn.nts callit.nts calrst.nts \
canf.nts cctrn.nts chkent.nts chkinp.nts chklab.nts chkpts.nts \
chkvoc.nts chkzsf.nts chr2int.nts chr2real.nts cidist.nts cintck.nts cirapt.nts \
cirbld.nts circpt.nts circpv.nts cirdef.nts cirpnt.nts cirpre.nts cirprg.nts \
cirprh.nts cirpri.nts cirprj.nts cirrec.nts clcept.nts clnssf.nts clone.nts \
clput.nts condif.nts conent.nts conpre.nts contck.nts convert.nts convid.nts \
convpt.nts convtk.nts copy1.nts copy2.nts crvbld.nts crvcls.nts crvevl.nts \
crvfit.nts crvgen.nts crvgn1.nts crvlen.nts crvpnt.nts crvpre.nts csdist.nts \
csrel.nts csrelx.nts cssurf.nts curnam.nts curvpv.nts cutget.nts cutool.nts \
cutool2.nts cutseg.nts cvepts.nts cveval.nts
OBJS321= \
aptsrc.t32 asnlun.t32 batch.t32 char.t32 clfile.t32 clword.t32 declsf.t32 \
declsh.t32 fio.t32 getkey.t32 gvtlin.t32 mosyn1.t32 motpn.t32 mshpre.t32 \
nclfin.t32 nclini.t32 ncltim.t32 nwait.t32 postp.t32 ppword.t32 quipre.t32 \
revers.t32 rsttrm.t32 runvt.t32 spwn.t32 statln.t32 unsol.t32 urest.t32 \
voyagr.t32 acibci.t32 acibln.t32 alnbci.t32 altcom.t32 angcal.t32 aptcl.t32 \
ascrub.t32 assgn.t32 atan2f.t32 barseg.t32 basesf.t32 beginp.t32 begpas.t32 \
bentst.t32 berr.t32 bscrub.t32 bsfpre.t32 buildn.t32 callit.t32 calrst.t32 \
canf.t32 cctrn.t32 chkent.t32 chkinp.t32 chklab.t32 chkpts.t32 \
chkvoc.t32 chkzsf.t32 chr2int.t32 chr2real.t32 cidist.t32 cintck.t32 cirapt.t32 \
cirbld.t32 circpt.t32 circpv.t32 cirdef.t32 cirpnt.t32 cirpre.t32 cirprg.t32 \
cirprh.t32 cirpri.t32 cirprj.t32 cirrec.t32 clcept.t32 clnssf.t32 clone.t32 \
clput.t32 condif.t32 conent.t32 conpre.t32 contck.t32 convert.t32 convid.t32 \
convpt.t32 convtk.t32 copy1.t32 copy2.t32 crvbld.t32 crvcls.t32 crvevl.t32 \
crvfit.t32 crvgen.t32 crvgn1.t32 crvlen.t32 crvpnt.t32 crvpre.t32 csdist.t32 \
csrel.t32 csrelx.t32 cssurf.t32 curnam.t32 curvpv.t32 cutget.t32 cutool.t32 \
cutool2.t32 cutseg.t32 cvepts.t32 cveval.t32
NOBJS321= \
aptsrc.nt32 asnlun.nt32 batch.nt32 char.nt32 clfile.nt32 clword.nt32 declsf.nt32 \
declsh.nt32 fio.nt32 getkey.nt32 gvtlin.nt32 mosyn1.nt32 motpn.nt32 mshpre.nt32 \
nclfin.nt32 nclini.nt32 ncltim.nt32 nwait.nt32 postp.nt32 ppword.nt32 quipre.nt32 \
revers.nt32 rsttrm.nt32 runvt.nt32 spwn.nt32 statln.nt32 unsol.nt32 urest.nt32 \
voyagr.nt32 acibci.nt32 acibln.nt32 alnbci.nt32 altcom.nt32 angcal.nt32 aptcl.nt32 \
ascrub.nt32 assgn.nt32 atan2f.nt32 barseg.nt32 basesf.nt32 beginp.nt32 begpas.nt32 \
bentst.nt32 berr.nt32 bscrub.nt32 bsfpre.nt32 buildn.nt32 callit.nt32 calrst.nt32 \
canf.nt32 cctrn.nt32 chkent.nt32 chkinp.nt32 chklab.nt32 chkpts.nt32 \
chkvoc.nt32 chkzsf.nt32 chr2int.nt32 chr2real.nt32 cidist.nt32 cintck.nt32 cirapt.nt32 \
cirbld.nt32 circpt.nt32 circpv.nt32 cirdef.nt32 cirpnt.nt32 cirpre.nt32 cirprg.nt32 \
cirprh.nt32 cirpri.nt32 cirprj.nt32 cirrec.nt32 clcept.nt32 clnssf.nt32 clone.nt32 \
clput.nt32 condif.nt32 conent.nt32 conpre.nt32 contck.nt32 convert.nt32 convid.nt32 \
convpt.nt32 convtk.nt32 copy1.nt32 copy2.nt32 crvbld.nt32 crvcls.nt32 crvevl.nt32 \
crvfit.nt32 crvgen.nt32 crvgn1.nt32 crvlen.nt32 crvpnt.nt32 crvpre.nt32 csdist.nt32 \
csrel.nt32 csrelx.nt32 cssurf.nt32 curnam.nt32 curvpv.nt32 cutget.nt32 cutool.nt32 \
cutool2.nt32 cutseg.nt32 cvepts.nt32 cveval.nt32
SRCS2= \
cvgide.f cvintx.f cvio.f cvofst.f cvofwf.f cvp.f cvsfsd.f \
cycman.f cylman.f cylpre.f dbcall.f dbchk.f dbclos.f dbfind.f \
dbfn.f dbgent.f dbgmsh.f dbgnsf.f dbgqlt.f dbgssf.f dbmain.f \
dbpent.f dbpmsh.f dbpnsf.f dbpqlt.f dbpssf.f dbshow.f dbstor.f \
dcrvpt.f debuger.f debugi.f debugm.f decclr.f declan.f declbl.f \
declci.f declcv.f declln.f declmx.f declnf.f declpl.f declpn.f \
declpt.f declpv.f declso.f declss.f declsy.f declve.f decmac.f \
defnam.f delgeo.f dellns.f dispcv.f displq.f disply.f dispmx.f \
disppl.f dispsf.f dist.f dlen.f dnarrw.f dodata.f domode.f \
domove.f dotext.f dotf.f draft.f draft1.f draft2.f draft3.f \
driver.f drvwir.f drwent.f dsbarl.f dspcut.f dsplcc.f dsplge.f \
dsplpn.f dsplsf.f dsplsh.f
OBJS2= \
cvgide.ts cvintx.ts cvio.ts cvofst.ts cvofwf.ts cvp.ts cvsfsd.ts \
cycman.ts cylman.ts cylpre.ts dbcall.ts dbchk.ts dbclos.ts dbfind.ts \
dbfn.ts dbgent.ts dbgmsh.ts dbgnsf.ts dbgqlt.ts dbgssf.ts dbmain.ts \
dbpent.ts dbpmsh.ts dbpnsf.ts dbpqlt.ts dbpssf.ts dbshow.ts dbstor.ts \
dcrvpt.ts debuger.ts debugi.ts debugm.ts decclr.ts declan.ts declbl.ts \
declci.ts declcv.ts declln.ts declmx.ts declnf.ts declpl.ts declpn.ts \
declpt.ts declpv.ts declso.ts declss.ts declsy.ts declve.ts decmac.ts \
defnam.ts delgeo.ts dellns.ts dispcv.ts displq.ts disply.ts dispmx.ts \
disppl.ts dispsf.ts dist.ts dlen.ts dnarrw.ts dodata.ts domode.ts \
domove.ts dotext.ts dotf.ts draft.ts draft1.ts draft2.ts draft3.ts \
driver.ts drvwir.ts drwent.ts dsbarl.ts dspcut.ts dsplcc.ts dsplge.ts \
dsplpn.ts dsplsf.ts dsplsh.ts
NOBJS2= \
cvgide.nts cvintx.nts cvio.nts cvofst.nts cvofwf.nts cvp.nts cvsfsd.nts \
cycman.nts cylman.nts cylpre.nts dbcall.nts dbchk.nts dbclos.nts dbfind.nts \
dbfn.nts dbgent.nts dbgmsh.nts dbgnsf.nts dbgqlt.nts dbgssf.nts dbmain.nts \
dbpent.nts dbpmsh.nts dbpnsf.nts dbpqlt.nts dbpssf.nts dbshow.nts dbstor.nts \
dcrvpt.nts debuger.nts debugi.nts debugm.nts decclr.nts declan.nts declbl.nts \
declci.nts declcv.nts declln.nts declmx.nts declnf.nts declpl.nts declpn.nts \
declpt.nts declpv.nts declso.nts declss.nts declsy.nts declve.nts decmac.nts \
defnam.nts delgeo.nts dellns.nts dispcv.nts displq.nts disply.nts dispmx.nts \
disppl.nts dispsf.nts dist.nts dlen.nts dnarrw.nts dodata.nts domode.nts \
domove.nts dotext.nts dotf.nts draft.nts draft1.nts draft2.nts draft3.nts \
driver.nts drvwir.nts drwent.nts dsbarl.nts dspcut.nts dsplcc.nts dsplge.nts \
dsplpn.nts dsplsf.nts dsplsh.nts
OBJS322= \
cvgide.t32 cvintx.t32 cvio.t32 cvofst.t32 cvofwf.t32 cvp.t32 cvsfsd.t32 \
cycman.t32 cylman.t32 cylpre.t32 dbcall.t32 dbchk.t32 dbclos.t32 dbfind.t32 \
dbfn.t32 dbgent.t32 dbgmsh.t32 dbgnsf.t32 dbgqlt.t32 dbgssf.t32 dbmain.t32 \
dbpent.t32 dbpmsh.t32 dbpnsf.t32 dbpqlt.t32 dbpssf.t32 dbshow.t32 dbstor.t32 \
dcrvpt.t32 debuger.t32 debugi.t32 debugm.t32 decclr.t32 declan.t32 declbl.t32 \
declci.t32 declcv.t32 declln.t32 declmx.t32 declnf.t32 declpl.t32 declpn.t32 \
declpt.t32 declpv.t32 declso.t32 declss.t32 declsy.t32 declve.t32 decmac.t32 \
defnam.t32 delgeo.t32 dellns.t32 dispcv.t32 displq.t32 disply.t32 dispmx.t32 \
disppl.t32 dispsf.t32 dist.t32 dlen.t32 dnarrw.t32 dodata.t32 domode.t32 \
domove.t32 dotext.t32 dotf.t32 draft.t32 draft1.t32 draft2.t32 draft3.t32 \
driver.t32 drvwir.t32 drwent.t32 dsbarl.t32 dspcut.t32 dsplcc.t32 dsplge.t32 \
dsplpn.t32 dsplsf.t32 dsplsh.t32
NOBJS322= \
cvgide.nt32 cvintx.nt32 cvio.nt32 cvofst.nt32 cvofwf.nt32 cvp.nt32 cvsfsd.nt32 \
cycman.nt32 cylman.nt32 cylpre.nt32 dbcall.nt32 dbchk.nt32 dbclos.nt32 dbfind.nt32 \
dbfn.nt32 dbgent.nt32 dbgmsh.nt32 dbgnsf.nt32 dbgqlt.nt32 dbgssf.nt32 dbmain.nt32 \
dbpent.nt32 dbpmsh.nt32 dbpnsf.nt32 dbpqlt.nt32 dbpssf.nt32 dbshow.nt32 dbstor.nt32 \
dcrvpt.nt32 debuger.nt32 debugi.nt32 debugm.nt32 decclr.nt32 declan.nt32 declbl.nt32 \
declci.nt32 declcv.nt32 declln.nt32 declmx.nt32 declnf.nt32 declpl.nt32 declpn.nt32 \
declpt.nt32 declpv.nt32 declso.nt32 declss.nt32 declsy.nt32 declve.nt32 decmac.nt32 \
defnam.nt32 delgeo.nt32 dellns.nt32 dispcv.nt32 displq.nt32 disply.nt32 dispmx.nt32 \
disppl.nt32 dispsf.nt32 dist.nt32 dlen.nt32 dnarrw.nt32 dodata.nt32 domode.nt32 \
domove.nt32 dotext.nt32 dotf.nt32 draft.nt32 draft1.nt32 draft2.nt32 draft3.nt32 \
driver.nt32 drvwir.nt32 drwent.nt32 dsbarl.nt32 dspcut.nt32 dsplcc.nt32 dsplge.nt32 \
dsplpn.nt32 dsplsf.nt32 dsplsh.nt32
SRCS3= \
dspmev.f dspmsh.f dspssf.f dsrel.f dsrelx.f dstrcc.f dstrge.f \
echk.f endpas.f entcpy.f errcal.f error.f errpol.f ersall.f \
eval.f evlrbs.f expnam.f expres.f exprs2.f farslp.f filchk.f \
fillet.f fillin.f filmod.f filmsc.f filpre.f fincut.f find.f \
finfix.f fininv.f finpre.f fixpp.f fixrld.f flmath.f flshbf.f \
flunit.f fmill.f fmtsrc.f genpts.f geogn1.f geogn2.f geogna.f \
geognb.f geognc.f geognd.f geogne.f geomf.f getas.f getclf.f \
getdb.f getent.f getifl.f getmsg.f getnln.f getnst.f getnxt.f \
getpod.f getsrc.f getsuv.f gettol.f gettrm.f getxln.f gocntl.f \
gosrf1.f gosrf2.f gotopn.f gpgmnm.f gpnptt.f graphy.f gtcntt.f \
gtcseg.f gtdesc.f gtentt.f gtgeom.f gtmptt.f gtmx.f gtpptt.f \
gtprimt.f gtpsrc.f gtranw.f
OBJS3= \
dspmev.ts dspmsh.ts dspssf.ts dsrel.ts dsrelx.ts dstrcc.ts dstrge.ts \
echk.ts endpas.ts entcpy.ts errcal.ts error.ts errpol.ts ersall.ts \
eval.ts evlrbs.ts expnam.ts expres.ts exprs2.ts farslp.ts filchk.ts \
fillet.ts fillin.ts filmod.ts filmsc.ts filpre.ts fincut.ts find.ts \
finfix.ts fininv.ts finpre.ts fixpp.ts fixrld.ts flmath.ts flshbf.ts \
flunit.ts fmill.ts fmtsrc.ts genpts.ts geogn1.ts geogn2.ts geogna.ts \
geognb.ts geognc.ts geognd.ts geogne.ts geomf.ts getas.ts getclf.ts \
getdb.ts getent.ts getifl.ts getmsg.ts getnln.ts getnst.ts getnxt.ts \
getpod.ts getsrc.ts getsuv.ts gettol.ts gettrm.ts getxln.ts gocntl.ts \
gosrf1.ts gosrf2.ts gotopn.ts gpgmnm.ts gpnptt.ts graphy.ts gtcntt.ts \
gtcseg.ts gtdesc.ts gtentt.ts gtgeom.ts gtmptt.ts gtmx.ts gtpptt.ts \
gtprimt.ts gtpsrc.ts gtranw.ts
NOBJS3= \
dspmev.nts dspmsh.nts dspssf.nts dsrel.nts dsrelx.nts dstrcc.nts dstrge.nts \
echk.nts endpas.nts entcpy.nts errcal.nts error.nts errpol.nts ersall.nts \
eval.nts evlrbs.nts expnam.nts expres.nts exprs2.nts farslp.nts filchk.nts \
fillet.nts fillin.nts filmod.nts filmsc.nts filpre.nts fincut.nts find.nts \
finfix.nts fininv.nts finpre.nts fixpp.nts fixrld.nts flmath.nts flshbf.nts \
flunit.nts fmill.nts fmtsrc.nts genpts.nts geogn1.nts geogn2.nts geogna.nts \
geognb.nts geognc.nts geognd.nts geogne.nts geomf.nts getas.nts getclf.nts \
getdb.nts getent.nts getifl.nts getmsg.nts getnln.nts getnst.nts getnxt.nts \
getpod.nts getsrc.nts getsuv.nts gettol.nts gettrm.nts getxln.nts gocntl.nts \
gosrf1.nts gosrf2.nts gotopn.nts gpgmnm.nts gpnptt.nts graphy.nts gtcntt.nts \
gtcseg.nts gtdesc.nts gtentt.nts gtgeom.nts gtmptt.nts gtmx.nts gtpptt.nts \
gtprimt.nts gtpsrc.nts gtranw.nts
OBJS323= \
dspmev.t32 dspmsh.t32 dspssf.t32 dsrel.t32 dsrelx.t32 dstrcc.t32 dstrge.t32 \
echk.t32 endpas.t32 entcpy.t32 errcal.t32 error.t32 errpol.t32 ersall.t32 \
eval.t32 evlrbs.t32 expnam.t32 expres.t32 exprs2.t32 farslp.t32 filchk.t32 \
fillet.t32 fillin.t32 filmod.t32 filmsc.t32 filpre.t32 fincut.t32 find.t32 \
finfix.t32 fininv.t32 finpre.t32 fixpp.t32 fixrld.t32 flmath.t32 flshbf.t32 \
flunit.t32 fmill.t32 fmtsrc.t32 genpts.t32 geogn1.t32 geogn2.t32 geogna.t32 \
geognb.t32 geognc.t32 geognd.t32 geogne.t32 geomf.t32 getas.t32 getclf.t32 \
getdb.t32 getent.t32 getifl.t32 getmsg.t32 getnln.t32 getnst.t32 getnxt.t32 \
getpod.t32 getsrc.t32 getsuv.t32 gettol.t32 gettrm.t32 getxln.t32 gocntl.t32 \
gosrf1.t32 gosrf2.t32 gotopn.t32 gpgmnm.t32 gpnptt.t32 graphy.t32 gtcntt.t32 \
gtcseg.t32 gtdesc.t32 gtentt.t32 gtgeom.t32 gtmptt.t32 gtmx.t32 gtpptt.t32 \
gtprimt.t32 gtpsrc.t32 gtranw.t32
NOBJS323= \
dspmev.nt32 dspmsh.nt32 dspssf.nt32 dsrel.nt32 dsrelx.nt32 dstrcc.nt32 dstrge.nt32 \
echk.nt32 endpas.nt32 entcpy.nt32 errcal.nt32 error.nt32 errpol.nt32 ersall.nt32 \
eval.nt32 evlrbs.nt32 expnam.nt32 expres.nt32 exprs2.nt32 farslp.nt32 filchk.nt32 \
fillet.nt32 fillin.nt32 filmod.nt32 filmsc.nt32 filpre.nt32 fincut.nt32 find.nt32 \
finfix.nt32 fininv.nt32 finpre.nt32 fixpp.nt32 fixrld.nt32 flmath.nt32 flshbf.nt32 \
flunit.nt32 fmill.nt32 fmtsrc.nt32 genpts.nt32 geogn1.nt32 geogn2.nt32 geogna.nt32 \
geognb.nt32 geognc.nt32 geognd.nt32 geogne.nt32 geomf.nt32 getas.nt32 getclf.nt32 \
getdb.nt32 getent.nt32 getifl.nt32 getmsg.nt32 getnln.nt32 getnst.nt32 getnxt.nt32 \
getpod.nt32 getsrc.nt32 getsuv.nt32 gettol.nt32 gettrm.nt32 getxln.nt32 gocntl.nt32 \
gosrf1.nt32 gosrf2.nt32 gotopn.nt32 gpgmnm.nt32 gpnptt.nt32 graphy.nt32 gtcntt.nt32 \
gtcseg.nt32 gtdesc.nt32 gtentt.nt32 gtgeom.nt32 gtmptt.nt32 gtmx.nt32 gtpptt.nt32 \
gtprimt.nt32 gtpsrc.nt32 gtranw.nt32
SRCS4= \
gtref.f gtrtnm.f gtshpt.f gtsptt.f gtview.f help.f icbent.f \
ifelse.f ilbent.f includ.f index.f ipatch.f iptch2.f ipvfor.f \
jmpchk.f jumpto.f kolaps.f labcam.f labchk.f labflg.f labset.f \
labwf.f lathes.f lathex.f lblchk.f lnpv.f lnrdef.f loadpp.f \
loopnd.f loopst.f lrdata.f lrfind.f lrpop.f lrpush.f lrupdt.f \
lthctl.f lthfol.f lthmot.f math.f math4.f mdinit.f meshpn.f \
metool.f mocntl.f mocomb.f modfy.f moinfo.f mosyn.f mosyn2.f \
mosyn3.f motgxx.f motimm.f motout.f mover.f mshpn2.f mshsel.f \
mxmath.f ncl.f nclspf001.f nclver.f nclvxf.f nclxini.f ncvclf.f \
ncvevl.f nestpt.f nfuncs.f nsrfpn.f nstub2.f numf.f obtain.f \
off08.f off09.f off10.f off11.f off12.f offcv.f offpkt.f \
offpre.f onerr.f outmot.f
OBJS4= \
gtref.ts gtrtnm.ts gtshpt.ts gtsptt.ts gtview.ts help.ts icbent.ts \
ifelse.ts ilbent.ts includ.ts index.ts ipatch.ts iptch2.ts ipvfor.ts \
jmpchk.ts jumpto.ts kolaps.ts labcam.ts labchk.ts labflg.ts labset.ts \
labwf.ts lathes.ts lathex.ts lblchk.ts lnpv.ts lnrdef.ts loadpp.ts \
loopnd.ts loopst.ts lrdata.ts lrfind.ts lrpop.ts lrpush.ts lrupdt.ts \
lthctl.ts lthfol.ts lthmot.ts math.ts math4.ts mdinit.ts meshpn.ts \
metool.ts mocntl.ts mocomb.ts modfy.ts moinfo.ts mosyn.ts mosyn2.ts \
mosyn3.ts motgxx.ts motimm.ts motout.ts mover.ts mshpn2.ts mshsel.ts \
mxmath.ts ncl.ts nclspf001.ts nclver.ts nclvxf.ts nclxini.ts ncvclf.ts \
ncvevl.ts nestpt.ts nfuncs.ts nsrfpn.ts nstub2.ts numf.ts obtain.ts \
off08.ts off09.ts off10.ts off11.ts off12.ts offcv.ts offpkt.ts \
offpre.ts onerr.ts outmot.ts
NOBJS4= \
gtref.nts gtrtnm.nts gtshpt.nts gtsptt.nts gtview.nts help.nts icbent.nts \
ifelse.nts ilbent.nts includ.nts index.nts ipatch.nts iptch2.nts ipvfor.nts \
jmpchk.nts jumpto.nts kolaps.nts labcam.nts labchk.nts labflg.nts labset.nts \
labwf.nts lathes.nts lathex.nts lblchk.nts lnpv.nts lnrdef.nts loadpp.nts \
loopnd.nts loopst.nts lrdata.nts lrfind.nts lrpop.nts lrpush.nts lrupdt.nts \
lthctl.nts lthfol.nts lthmot.nts math.nts math4.nts mdinit.nts meshpn.nts \
metool.nts mocntl.nts mocomb.nts modfy.nts moinfo.nts mosyn.nts mosyn2.nts \
mosyn3.nts motgxx.nts motimm.nts motout.nts mover.nts mshpn2.nts mshsel.nts \
mxmath.nts ncl.nts nclspf001.nts nclver.nts nclvxf.nts nclxini.nts ncvclf.nts \
ncvevl.nts nestpt.nts nfuncs.nts nsrfpn.nts nstub2.nts numf.nts obtain.nts \
off08.nts off09.nts off10.nts off11.nts off12.nts offcv.nts offpkt.nts \
offpre.nts onerr.nts outmot.nts
OBJS324= \
gtref.t32 gtrtnm.t32 gtshpt.t32 gtsptt.t32 gtview.t32 help.t32 icbent.t32 \
ifelse.t32 ilbent.t32 includ.t32 index.t32 ipatch.t32 iptch2.t32 ipvfor.t32 \
jmpchk.t32 jumpto.t32 kolaps.t32 labcam.t32 labchk.t32 labflg.t32 labset.t32 \
labwf.t32 lathes.t32 lathex.t32 lblchk.t32 lnpv.t32 lnrdef.t32 loadpp.t32 \
loopnd.t32 loopst.t32 lrdata.t32 lrfind.t32 lrpop.t32 lrpush.t32 lrupdt.t32 \
lthctl.t32 lthfol.t32 lthmot.t32 math.t32 math4.t32 mdinit.t32 meshpn.t32 \
metool.t32 mocntl.t32 mocomb.t32 modfy.t32 moinfo.t32 mosyn.t32 mosyn2.t32 \
mosyn3.t32 motgxx.t32 motimm.t32 motout.t32 mover.t32 mshpn2.t32 mshsel.t32 \
mxmath.t32 ncl.t32 nclspf001.t32 nclver.t32 nclvxf.t32 nclxini.t32 ncvclf.t32 \
ncvevl.t32 nestpt.t32 nfuncs.t32 nsrfpn.t32 nstub2.t32 numf.t32 obtain.t32 \
off08.t32 off09.t32 off10.t32 off11.t32 off12.t32 offcv.t32 offpkt.t32 \
offpre.t32 onerr.t32 outmot.t32
NOBJS324= \
gtref.nt32 gtrtnm.nt32 gtshpt.nt32 gtsptt.nt32 gtview.nt32 help.nt32 icbent.nt32 \
ifelse.nt32 ilbent.nt32 includ.nt32 index.nt32 ipatch.nt32 iptch2.nt32 ipvfor.nt32 \
jmpchk.nt32 jumpto.nt32 kolaps.nt32 labcam.nt32 labchk.nt32 labflg.nt32 labset.nt32 \
labwf.nt32 lathes.nt32 lathex.nt32 lblchk.nt32 lnpv.nt32 lnrdef.nt32 loadpp.nt32 \
loopnd.nt32 loopst.nt32 lrdata.nt32 lrfind.nt32 lrpop.nt32 lrpush.nt32 lrupdt.nt32 \
lthctl.nt32 lthfol.nt32 lthmot.nt32 math.nt32 math4.nt32 mdinit.nt32 meshpn.nt32 \
metool.nt32 mocntl.nt32 mocomb.nt32 modfy.nt32 moinfo.nt32 mosyn.nt32 mosyn2.nt32 \
mosyn3.nt32 motgxx.nt32 motimm.nt32 motout.nt32 mover.nt32 mshpn2.nt32 mshsel.nt32 \
mxmath.nt32 ncl.nt32 nclspf001.nt32 nclver.nt32 nclvxf.nt32 nclxini.nt32 ncvclf.nt32 \
ncvevl.nt32 nestpt.nt32 nfuncs.nt32 nsrfpn.nt32 nstub2.nt32 numf.nt32 obtain.nt32 \
off08.nt32 off09.nt32 off10.nt32 off11.nt32 off12.nt32 offcv.nt32 offpkt.nt32 \
offpre.nt32 onerr.nt32 outmot.nt32
SRCS5= \
parser.f parsgn.f parsit.f parsuv.f patgen.f patpre.f pfillet.f \
pleval.f plotm.f pncalc.f pocket.f pockmo.f poddef.f podpts.f \
pokabc.f pokmod.f pokobt.f pokpar.f poksup.f polbld.f polfin.f \
ppnptt.f ppwrt.f premov.f profil.f profsy.f projcv.f prompt.f \
prsgeo.f prtgeo.f prtput.f psmod3.f psmult.f psproj.f psrel.f \
psrela.f ptdesc.f ptdfnm.f ptentt.f ptgeom.f pthru.f ptmhdt.f \
ptmptt.f ptonln.f ptppnm.f ptpsrc.f ptranw.f ptshed.f ptshpt.f \
ptsmth.f ptspnt.f putapt.f putcl.f putdb.f putent.f putinpp.f \
putmsg.f putmsg_w2.f putprt.f putsrc.f putw2.f pwild.f qsrfpn.f \
ran.f randel.f randlk.f ranini.f ranio.f ranstr.f razpk1.f \
razpok.f rectfy.f redef.f regdir.f regdis.f regfrm.f regmil.f \
regpas.f regprf.f regpts.f
OBJS5= \
parser.ts parsgn.ts parsit.ts parsuv.ts patgen.ts patpre.ts pfillet.ts \
pleval.ts plotm.ts pncalc.ts pocket.ts pockmo.ts poddef.ts podpts.ts \
pokabc.ts pokmod.ts pokobt.ts pokpar.ts poksup.ts polbld.ts polfin.ts \
ppnptt.ts ppwrt.ts premov.ts profil.ts profsy.ts projcv.ts prompt.ts \
prsgeo.ts prtgeo.ts prtput.ts psmod3.ts psmult.ts psproj.ts psrel.ts \
psrela.ts ptdesc.ts ptdfnm.ts ptentt.ts ptgeom.ts pthru.ts ptmhdt.ts \
ptmptt.ts ptonln.ts ptppnm.ts ptpsrc.ts ptranw.ts ptshed.ts ptshpt.ts \
ptsmth.ts ptspnt.ts putapt.ts putcl.ts putdb.ts putent.ts putinpp.ts \
putmsg.ts putmsg_w2.ts putprt.ts putsrc.ts putw2.ts pwild.ts qsrfpn.ts \
ran.ts randel.ts randlk.ts ranini.ts ranio.ts ranstr.ts razpk1.ts \
razpok.ts rectfy.ts redef.ts regdir.ts regdis.ts regfrm.ts regmil.ts \
regpas.ts regprf.ts regpts.ts
NOBJS5= \
parser.nts parsgn.nts parsit.nts parsuv.nts patgen.nts patpre.nts pfillet.nts \
pleval.nts plotm.nts pncalc.nts pocket.nts pockmo.nts poddef.nts podpts.nts \
pokabc.nts pokmod.nts pokobt.nts pokpar.nts poksup.nts polbld.nts polfin.nts \
ppnptt.nts ppwrt.nts premov.nts profil.nts profsy.nts projcv.nts prompt.nts \
prsgeo.nts prtgeo.nts prtput.nts psmod3.nts psmult.nts psproj.nts psrel.nts \
psrela.nts ptdesc.nts ptdfnm.nts ptentt.nts ptgeom.nts pthru.nts ptmhdt.nts \
ptmptt.nts ptonln.nts ptppnm.nts ptpsrc.nts ptranw.nts ptshed.nts ptshpt.nts \
ptsmth.nts ptspnt.nts putapt.nts putcl.nts putdb.nts putent.nts putinpp.nts \
putmsg.nts putmsg_w2.nts putprt.nts putsrc.nts putw2.nts pwild.nts qsrfpn.nts \
ran.nts randel.nts randlk.nts ranini.nts ranio.nts ranstr.nts razpk1.nts \
razpok.nts rectfy.nts redef.nts regdir.nts regdis.nts regfrm.nts regmil.nts \
regpas.nts regprf.nts regpts.nts
OBJS325= \
parser.t32 parsgn.t32 parsit.t32 parsuv.t32 patgen.t32 patpre.t32 pfillet.t32 \
pleval.t32 plotm.t32 pncalc.t32 pocket.t32 pockmo.t32 poddef.t32 podpts.t32 \
pokabc.t32 pokmod.t32 pokobt.t32 pokpar.t32 poksup.t32 polbld.t32 polfin.t32 \
ppnptt.t32 ppwrt.t32 premov.t32 profil.t32 profsy.t32 projcv.t32 prompt.t32 \
prsgeo.t32 prtgeo.t32 prtput.t32 psmod3.t32 psmult.t32 psproj.t32 psrel.t32 \
psrela.t32 ptdesc.t32 ptdfnm.t32 ptentt.t32 ptgeom.t32 pthru.t32 ptmhdt.t32 \
ptmptt.t32 ptonln.t32 ptppnm.t32 ptpsrc.t32 ptranw.t32 ptshed.t32 ptshpt.t32 \
ptsmth.t32 ptspnt.t32 putapt.t32 putcl.t32 putdb.t32 putent.t32 putinpp.t32 \
putmsg.t32 putmsg_w2.t32 putprt.t32 putsrc.t32 putw2.t32 pwild.t32 qsrfpn.t32 \
ran.t32 randel.t32 randlk.t32 ranini.t32 ranio.t32 ranstr.t32 razpk1.t32 \
razpok.t32 rectfy.t32 redef.t32 regdir.t32 regdis.t32 regfrm.t32 regmil.t32 \
regpas.t32 regprf.t32 regpts.t32
NOBJS325= \
parser.nt32 parsgn.nt32 parsit.nt32 parsuv.nt32 patgen.nt32 patpre.nt32 pfillet.nt32 \
pleval.nt32 plotm.nt32 pncalc.nt32 pocket.nt32 pockmo.nt32 poddef.nt32 podpts.nt32 \
pokabc.nt32 pokmod.nt32 pokobt.nt32 pokpar.nt32 poksup.nt32 polbld.nt32 polfin.nt32 \
ppnptt.nt32 ppwrt.nt32 premov.nt32 profil.nt32 profsy.nt32 projcv.nt32 prompt.nt32 \
prsgeo.nt32 prtgeo.nt32 prtput.nt32 psmod3.nt32 psmult.nt32 psproj.nt32 psrel.nt32 \
psrela.nt32 ptdesc.nt32 ptdfnm.nt32 ptentt.nt32 ptgeom.nt32 pthru.nt32 ptmhdt.nt32 \
ptmptt.nt32 ptonln.nt32 ptppnm.nt32 ptpsrc.nt32 ptranw.nt32 ptshed.nt32 ptshpt.nt32 \
ptsmth.nt32 ptspnt.nt32 putapt.nt32 putcl.nt32 putdb.nt32 putent.nt32 putinpp.nt32 \
putmsg.nt32 putmsg_w2.nt32 putprt.nt32 putsrc.nt32 putw2.nt32 pwild.nt32 qsrfpn.nt32 \
ran.nt32 randel.nt32 randlk.nt32 ranini.nt32 ranio.nt32 ranstr.nt32 razpk1.nt32 \
razpok.nt32 rectfy.nt32 redef.nt32 regdir.nt32 regdis.nt32 regfrm.nt32 regmil.nt32 \
regpas.nt32 regprf.nt32 regpts.nt32
SRCS6= \
regscl.f regstp.f remove.f renam.f retlbl.f revpre.f rldgen.f \
rldpat.f rldpre.f rmill.f rotpts.f rpchk.f rufcut.f rufpre.f \
savecm.f savepp.f savmot.f sbsolv.f scrfil.f scrub.f scurve.f \
sdepre.f segchk.f sequnc.f session.f setifl.f setins.f setsc.f \
setslp.f setstp.f setver.f sfeval.f sfevl2.f sffit1.f sffit2.f \
sfinit.f sfpkgy.f sfprim.f sfpt.f sfptdo.f sgeo.f sgeo1.f \
shafix.f shapre.f showpn.f sifl.f simsurf.f slpfar.f slpset.f \
smill.f smpre.f srfcls.f srfdef.f srfevl.f srffit.f srfpn2.f \
srfpre.f ssc.f ssdist.f ssrc.f ssrfpn.f ssurf.f star.f \
stock.f strcmm.f strmot.f strtup.f strwf1.f strwf2.f stunlb.f \
surfpn.f syscal.f tancir.f tanorm.f tanto.f tdsply.f termac.f \
testcs.f tlaxis.f tllock.f
OBJS6= \
regscl.ts regstp.ts remove.ts renam.ts retlbl.ts revpre.ts rldgen.ts \
rldpat.ts rldpre.ts rmill.ts rotpts.ts rpchk.ts rufcut.ts rufpre.ts \
savecm.ts savepp.ts savmot.ts sbsolv.ts scrfil.ts scrub.ts scurve.ts \
sdepre.ts segchk.ts sequnc.ts session.ts setifl.ts setins.ts setsc.ts \
setslp.ts setstp.ts setver.ts sfeval.ts sfevl2.ts sffit1.ts sffit2.ts \
sfinit.ts sfpkgy.ts sfprim.ts sfpt.ts sfptdo.ts sgeo.ts sgeo1.ts \
shafix.ts shapre.ts showpn.ts sifl.ts simsurf.ts slpfar.ts slpset.ts \
smill.ts smpre.ts srfcls.ts srfdef.ts srfevl.ts srffit.ts srfpn2.ts \
srfpre.ts ssc.ts ssdist.ts ssrc.ts ssrfpn.ts ssurf.ts star.ts \
stock.ts strcmm.ts strmot.ts strtup.ts strwf1.ts strwf2.ts stunlb.ts \
surfpn.ts syscal.ts tancir.ts tanorm.ts tanto.ts tdsply.ts termac.ts \
testcs.ts tlaxis.ts tllock.ts
NOBJS6= \
regscl.nts regstp.nts remove.nts renam.nts retlbl.nts revpre.nts rldgen.nts \
rldpat.nts rldpre.nts rmill.nts rotpts.nts rpchk.nts rufcut.nts rufpre.nts \
savecm.nts savepp.nts savmot.nts sbsolv.nts scrfil.nts scrub.nts scurve.nts \
sdepre.nts segchk.nts sequnc.nts session.nts setifl.nts setins.nts setsc.nts \
setslp.nts setstp.nts setver.nts sfeval.nts sfevl2.nts sffit1.nts sffit2.nts \
sfinit.nts sfpkgy.nts sfprim.nts sfpt.nts sfptdo.nts sgeo.nts sgeo1.nts \
shafix.nts shapre.nts showpn.nts sifl.nts simsurf.nts slpfar.nts slpset.nts \
smill.nts smpre.nts srfcls.nts srfdef.nts srfevl.nts srffit.nts srfpn2.nts \
srfpre.nts ssc.nts ssdist.nts ssrc.nts ssrfpn.nts ssurf.nts star.nts \
stock.nts strcmm.nts strmot.nts strtup.nts strwf1.nts strwf2.nts stunlb.nts \
surfpn.nts syscal.nts tancir.nts tanorm.nts tanto.nts tdsply.nts termac.nts \
testcs.nts tlaxis.nts tllock.nts
OBJS326= \
regscl.t32 regstp.t32 remove.t32 renam.t32 retlbl.t32 revpre.t32 rldgen.t32 \
rldpat.t32 rldpre.t32 rmill.t32 rotpts.t32 rpchk.t32 rufcut.t32 rufpre.t32 \
savecm.t32 savepp.t32 savmot.t32 sbsolv.t32 scrfil.t32 scrub.t32 scurve.t32 \
sdepre.t32 segchk.t32 sequnc.t32 session.t32 setifl.t32 setins.t32 setsc.t32 \
setslp.t32 setstp.t32 setver.t32 sfeval.t32 sfevl2.t32 sffit1.t32 sffit2.t32 \
sfinit.t32 sfpkgy.t32 sfprim.t32 sfpt.t32 sfptdo.t32 sgeo.t32 sgeo1.t32 \
shafix.t32 shapre.t32 showpn.t32 sifl.t32 simsurf.t32 slpfar.t32 slpset.t32 \
smill.t32 smpre.t32 srfcls.t32 srfdef.t32 srfevl.t32 srffit.t32 srfpn2.t32 \
srfpre.t32 ssc.t32 ssdist.t32 ssrc.t32 ssrfpn.t32 ssurf.t32 star.t32 \
stock.t32 strcmm.t32 strmot.t32 strtup.t32 strwf1.t32 strwf2.t32 stunlb.t32 \
surfpn.t32 syscal.t32 tancir.t32 tanorm.t32 tanto.t32 tdsply.t32 termac.t32 \
testcs.t32 tlaxis.t32 tllock.t32
NOBJS326= \
regscl.nt32 regstp.nt32 remove.nt32 renam.nt32 retlbl.nt32 revpre.nt32 rldgen.nt32 \
rldpat.nt32 rldpre.nt32 rmill.nt32 rotpts.nt32 rpchk.nt32 rufcut.nt32 rufpre.nt32 \
savecm.nt32 savepp.nt32 savmot.nt32 sbsolv.nt32 scrfil.nt32 scrub.nt32 scurve.nt32 \
sdepre.nt32 segchk.nt32 sequnc.nt32 session.nt32 setifl.nt32 setins.nt32 setsc.nt32 \
setslp.nt32 setstp.nt32 setver.nt32 sfeval.nt32 sfevl2.nt32 sffit1.nt32 sffit2.nt32 \
sfinit.nt32 sfpkgy.nt32 sfprim.nt32 sfpt.nt32 sfptdo.nt32 sgeo.nt32 sgeo1.nt32 \
shafix.nt32 shapre.nt32 showpn.nt32 sifl.nt32 simsurf.nt32 slpfar.nt32 slpset.nt32 \
smill.nt32 smpre.nt32 srfcls.nt32 srfdef.nt32 srfevl.nt32 srffit.nt32 srfpn2.nt32 \
srfpre.nt32 ssc.nt32 ssdist.nt32 ssrc.nt32 ssrfpn.nt32 ssurf.nt32 star.nt32 \
stock.nt32 strcmm.nt32 strmot.nt32 strtup.nt32 strwf1.nt32 strwf2.nt32 stunlb.nt32 \
surfpn.nt32 syscal.nt32 tancir.nt32 tanorm.nt32 tanto.nt32 tdsply.nt32 termac.nt32 \
testcs.nt32 tlaxis.nt32 tllock.nt32
SRCS7= \
tlpara.f tltang.f transf.f trimci.f trimsf.f trnmsh.f trnqhd.f \
trnqlt.f trnshd.f trnshp.f tsfsup.f ttcrv.f ubfn.f ubio.f \
ubshow.f ubstor.f ucrvpv.f uerr.f uevcvt.f uevsft.f unitcv.f \
uparrw.f upiter.f upncal.f uset.f ushow.f usrfpn.f utilty.f \
uvend.f uvend1.f uvend2.f uvmove.f uvprmv.f varfmt.f vcvpnt.f \
vector.f vmill.f vmpksy.f vocab2.f voccad.f vocsch.f vocsup.f \
vstchk.f vstore.f vvcvpt.f watrln.f wlproj.f wrtpp.f
OBJS7= \
tlpara.ts tltang.ts transf.ts trimci.ts trimsf.ts trnmsh.ts trnqhd.ts \
trnqlt.ts trnshd.ts trnshp.ts tsfsup.ts ttcrv.ts ubfn.ts ubio.ts \
ubshow.ts ubstor.ts ucrvpv.ts uerr.ts uevcvt.ts uevsft.ts unitcv.ts \
uparrw.ts upiter.ts upncal.ts uset.ts ushow.ts usrfpn.ts utilty.ts \
uvend.ts uvend1.ts uvend2.ts uvmove.ts uvprmv.ts varfmt.ts vcvpnt.ts \
vector.ts vmill.ts vmpksy.ts vocab2.ts voccad.ts vocsch.ts vocsup.ts \
vstchk.ts vstore.ts vvcvpt.ts watrln.ts wlproj.ts wrtpp.ts
NOBJS7= \
tlpara.nts tltang.nts transf.nts trimci.nts trimsf.nts trnmsh.nts trnqhd.nts \
trnqlt.nts trnshd.nts trnshp.nts tsfsup.nts ttcrv.nts ubfn.nts ubio.nts \
ubshow.nts ubstor.nts ucrvpv.nts uerr.nts uevcvt.nts uevsft.nts unitcv.nts \
uparrw.nts upiter.nts upncal.nts uset.nts ushow.nts usrfpn.nts utilty.nts \
uvend.nts uvend1.nts uvend2.nts uvmove.nts uvprmv.nts varfmt.nts vcvpnt.nts \
vector.nts vmill.nts vmpksy.nts vocab2.nts voccad.nts vocsch.nts vocsup.nts \
vstchk.nts vstore.nts vvcvpt.nts watrln.nts wlproj.nts wrtpp.nts
OBJS327= \
tlpara.t32 tltang.t32 transf.t32 trimci.t32 trimsf.t32 trnmsh.t32 trnqhd.t32 \
trnqlt.t32 trnshd.t32 trnshp.t32 tsfsup.t32 ttcrv.t32 ubfn.t32 ubio.t32 \
ubshow.t32 ubstor.t32 ucrvpv.t32 uerr.t32 uevcvt.t32 uevsft.t32 unitcv.t32 \
uparrw.t32 upiter.t32 upncal.t32 uset.t32 ushow.t32 usrfpn.t32 utilty.t32 \
uvend.t32 uvend1.t32 uvend2.t32 uvmove.t32 uvprmv.t32 varfmt.t32 vcvpnt.t32 \
vector.t32 vmill.t32 vmpksy.t32 vocab2.t32 voccad.t32 vocsch.t32 vocsup.t32 \
vstchk.t32 vstore.t32 vvcvpt.t32 watrln.t32 wlproj.t32 wrtpp.t32
NOBJS327= \
tlpara.nt32 tltang.nt32 transf.nt32 trimci.nt32 trimsf.nt32 trnmsh.nt32 trnqhd.nt32 \
trnqlt.nt32 trnshd.nt32 trnshp.nt32 tsfsup.nt32 ttcrv.nt32 ubfn.nt32 ubio.nt32 \
ubshow.nt32 ubstor.nt32 ucrvpv.nt32 uerr.nt32 uevcvt.nt32 uevsft.nt32 unitcv.nt32 \
uparrw.nt32 upiter.nt32 upncal.nt32 uset.nt32 ushow.nt32 usrfpn.nt32 utilty.nt32 \
uvend.nt32 uvend1.nt32 uvend2.nt32 uvmove.nt32 uvprmv.nt32 varfmt.nt32 vcvpnt.nt32 \
vector.nt32 vmill.nt32 vmpksy.nt32 vocab2.nt32 voccad.nt32 vocsch.nt32 vocsup.nt32 \
vstchk.nt32 vstore.nt32 vvcvpt.nt32 watrln.nt32 wlproj.nt32 wrtpp.nt32
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
DIR=src
SRCDIR=nclf
NCLLIBRARY=nclf.lib
NNCLLIBRARY=nnclf.lib
NCLLIBRARY32=nclf32.lib
NNCLLIBRARY32=nnclf32.lib

#
#---------------------------EOF make.list --------------------------
#
#
#--------------------------- make.middle.src.wnt --------------------------
#
#
# release labels
#
debugrel: makeall
nodebugrel: nmakeall
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
# Library labels
#
mklib:
	$(CRELIB2)
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

mklib32:
	$(CRELIB322)
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

#
#---------------------------EOF make.middle.src.wnt --------------------------
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
