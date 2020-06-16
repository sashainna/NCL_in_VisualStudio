#
#--------------------------- make.header.wnt --------------------------
#
#
# Common symbols changed when creating a local makefile
#
PLACE_OBJ = 
PLACE_NOBJ = 
#PLACE_OBJ = -erase ..\obj\$*.obj && $(MV) $*.obj ..\obj
#PLACE_NOBJ = -erase ..\nobj\$*.obj && $(MV) $*.obj ..\nobj
#
SCCSREL=
#
BIN=bin
SBIN=sbin
LIB=lib
NLIB=nlib
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
DEBUG = /D "_DEBUG" /MDd /ZI /Fd"$(SRCDIR)" /Od
NDEB = /Od
DEB = /debug:full /Od

DEVCFLAGS=/nologo /EHsc /D "WIN32" /D "_WINDOWS" /D "WINVER=0x502" \
	/D "_CRT_SECURE_NO_DEPRECATE" /Fo".\\" /FD /c
DEVCPFLAGS=$(DEVCFLAGS) /D "_AFXDLL"
DEVFFLAGS=/integer_size:16 /compile_only /nologo /libs:dll /names:lowercase \
	/iface:nomixed_str_len_arg /fpscomp:general /iface:cref \
	/warn:nofileopt 
DEVRCFLAGS=/l 0x409 /d "_AFXDDL"

FINCLUDE=/include:$(NCLPATH)\src\incf
FSYS=k
PWCFLAGS=/nologo /EHsc /Od /D"WIN32" /D"_WINDOWS" /D"WNT" /D "WINVER=0x502" \
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
#
# NCL includes, libraries and tools
#
NCLINC=$(NCLPATH)\src\inc
NCLFILES=$(NCLPATH)\nclfiles
NCLLIB=$(NCLPATH)\$(LIB)
NNCLLIB=$(NCLPATH)\$(NLIB)
NCLBIN=$(NCLPATH)\$(BIN)
#
FORFILTER=$(NCLPATH)\$(BIN)\filter
IGESDIR=$(NCLPATH)\tools\iges
SCCSTOOL=$(NCLPATH)\$(SBIN)\formtool
#
CINCLUDE=/I $(NCLINC) /I $(LI_DIR)\include /I $(LI_DIR)\include\$(LI_ARCH)
CPINCLUDE=/I $(NCLINC) /I "$(VCD)\mfc\src" /I "$(VCD)\mfc\include" /I "$(VCD)\atl\include" /I "$(VCD1)\MFC\Include" /I "$(VCD1)\ATL\Include"
RCINCLUDE=/i "$(NCLINC)" /i "$(NNCLLIB)\obj"

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
#
# nodebug library
#
NRMLIB=$(RM) $(NNCLLIB)\$@
NCRELIB1=$(AR) /out:$(NNCLLIB)\$(NNCLLIBRARY) $(NNCLLIB)\$(NNCLLIBRARY) *.obj
#
#
# Here come some rules on how to deal with the OBJS lists
#
#
# debug rules
#
.SUFFIXES: .c .cpp .f .for .ts .nts .obj .o
.c.ts:
	$(RM) $@
	@echo "*****  MAKING DEBUG VERSION OF $<  *****"
	$(CC) $(CFLAGS) $<
	$(PLACE_OBJ)
	$(TOUCH) $@
	@echo "***** FINISHED BUILDING $< IN $(SRCDIR) *****"
	@echo ""
.cpp.ts:
	$(RM) $@
	@echo "*****  MAKING DEBUG VERSION OF $<  *****"
	$(CPP) $(CPFLAGS) $<
	$(PLACE_OBJ)
	$(TOUCH) $@
	@echo "***** FINISHED BUILDING $< IN $(SRCDIR) *****"
	@echo ""
.f.ts:
	$(RM) $@
	@echo "*****  MAKING DEBUG VERSION OF $<  *****"
	$(F77) $(FFLAGS) $*.f
	$(PLACE_OBJ)
	$(TOUCH) $@
	@echo "***** FINISHED COMPILING $< IN $(SRCDIR) *****"
	@echo ""
.for.ts:
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
	$(RM) $@
	@echo "*****  MAKING NODEBUG VERSION OF $<  *****"
	$(NCC) $(CFLAGS) $<
	$(PLACE_NOBJ)
	$(TOUCH) $@
	@echo "***** FINISHED BUILDING $< IN $(SRCDIR) *****"
	@echo ""
.cpp.nts:
	$(RM) $@
	@echo "*****  MAKING NODEBUG VERSION OF $<  *****"
	$(NCPP) $(CPFLAGS) $<
	$(PLACE_NOBJ)
	$(TOUCH) $@
	@echo "***** FINISHED BUILDING $< IN $(SRCDIR) *****"
	@echo ""
.f.nts:
	$(RM) $@
	@echo "*****  MAKING NODEBUG VERSION OF $<  *****"
	$(NF77) $(FFLAGS) $*.f
	$(PLACE_NOBJ)
	$(TOUCH) $@
	@echo "***** FINISHED COMPILING $< IN $(SRCDIR) *****"
	@echo ""
.for.nts:
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
#---------------------------EOF make.prefix.tools.wnt --------------------------
#
#
#--------------------------- make.list --------------------------
#
SRCS1= \
testglfun.c GLtest.cpp GLtestDoc.cpp GLtestView.cpp StdAfx.cpp testMainFrm.cpp
OBJS1= \
testglfun.ts GLtest.ts GLtestDoc.ts GLtestView.ts StdAfx.ts testMainFrm.ts
NOBJS1= \
testglfun.nts GLtest.nts GLtestDoc.nts GLtestView.nts StdAfx.nts testMainFrm.nts
SRCS2= \

OBJS2= \

NOBJS2= \

SRCS3= \

OBJS3= \

NOBJS3= \

SRCS4= \

OBJS4= \

NOBJS4= \

SRCS5= \

OBJS5= \

NOBJS5= \

SRCS6= \

OBJS6= \

NOBJS6= \

SRCS7= \

OBJS7= \

NOBJS7= \

SRCS8= \

OBJS8= \

NOBJS8= \

SRCS9= \

OBJS9= \

NOBJS9= \

SRCS=$(SRCS1) $(SRCS2) $(SRCS3) $(SRCS4) $(SRCS5) $(SRCS6) \
     $(SRCS7) $(SRCS8) $(SRCS9)
OBJS=$(OBJS1) $(OBJS2) $(OBJS3) $(OBJS4) $(OBJS5) $(OBJS6) \
     $(OBJS7) $(OBJS8) $(OBJS9)
NOBJS=$(NOBJS1) $(NOBJS2) $(NOBJS3) $(NOBJS4) $(NOBJS5) $(NOBJS6) \
     $(NOBJS7) $(NOBJS8) $(NOBJS9)
#
# Define the NCL directory this makefile should run in.
#
DIR=tools
SRCDIR=gltest
NCLLIBRARY=gltest.lib
NNCLLIBRARY=ngltest.lib

#
#---------------------------EOF make.list --------------------------
#
#
#--------------------------- make.middle.tools.wnt --------------------------
#
#
# release labels
#
debugrel: makeall
nodebugrel: nmakeall
makeall: $(OBJS) library
nmakeall: cleanup $(SRCS) nlibrary makeall
library: $(NCLLIBRARY)
nlibrary: $(NNCLLIBRARY)
updatelib: $(NCLLIBRARY)
updatenlib: $(NNCLLIBRARY) updatelib
#
# make debug stuff
#
.SUFFIXES: .ddl .sql .ts .nts .obj .o

$(NCLLIBRARY): nclver $(OBJS)
	$(CRELIB1)
	-$(RM) *.obj
!IF "$(SYMLIB)" != ""
	-$(RM) $(SYMLIB)
!ENDIF
#
# make nodebug stuff
#
$(NNCLLIBRARY): nclver $(NOBJS)
	$(NCRELIB1)
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
	get inc/nclver.h
	-$(RM) temain.nts temain.ts \
	te26main.nts te26main.ts \
	tzmain.ts tzmain.nts \
	tpmain.ts tpmain.nts

#
#---------------------------EOF make.middle.tools.wnt --------------------------
#
#
#--------------------------- make.suffix.wnt --------------------------
#
cleanup: 
!IF "$(SRCS1)" != ""
	-@$(RM) $(SRCS1) 
	-@$(RM) $(OBJS1) 
	-@$(RM) $(NOBJS1) 
!ENDIF
!IF "$(SRCS2)" != ""
	-@$(RM) $(SRCS2) 
	-@$(RM) $(OBJS2) 
	-@$(RM) $(NOBJS2) 
!ENDIF
!IF "$(SRCS3)" != ""
	-@$(RM) $(SRCS3) 
	-@$(RM) $(OBJS3) 
	-@$(RM) $(NOBJS3) 
!ENDIF
!IF "$(SRCS4)" != ""
	-@$(RM) $(SRCS4) 
	-@$(RM) $(OBJS4) 
	-@$(RM) $(NOBJS4) 
!ENDIF
!IF "$(SRCS5)" != ""
	-@$(RM) $(SRCS5) 
	-@$(RM) $(OBJS5) 
	-@$(RM) $(NOBJS5) 
!ENDIF
!IF "$(SRCS6)" != ""
	-@$(RM) $(SRCS6) 
	-@$(RM) $(OBJS6) 
	-@$(RM) $(NOBJS6) 
!ENDIF
!IF "$(SRCS7)" != ""
	-@$(RM) $(SRCS7) 
	-@$(RM) $(OBJS7) 
	-@$(RM) $(NOBJS7) 
!ENDIF
!IF "$(SRCS8)" != ""
	-@$(RM) $(SRCS8) 
	-@$(RM) $(OBJS8) 
	-@$(RM) $(NOBJS8) 
!ENDIF
!IF "$(SRCS9)" != ""
	-@$(RM) $(SRCS9) 
	-@$(RM) $(OBJS9) 
	-@$(RM) $(NOBJS9) 
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
