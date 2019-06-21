#
#--------------------------- make.header.wnt --------------------------
#
#
# Common symbols changed when creating a local makefile
#
PLACE_OBJ = 
PLACE_NOBJ = 

NCLPATH=\nclnt
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
GET = $(NCLPATH)\ncluni\get
#
SYMLIB = vc50.idb vc50.pdb temp.pch
#
# device specific flags used for compiling and loading
#
NDEBUG = /D "NDEBUG" /MD /W3
DEBUG = /D "_DEBUG" /MLd /Zi
DEVCFLAGS=/nologo /GX /Od /D "WIN32" /D "_WINDOWS" /Fp"temp.pch" /YX \
 /Fo".\\" /Fd".\\" /FD /c
DEVFFLAGS=/integer_size:32 /compile_only /nologo /libs:dll /names:lowercase \
	/optimize:0 /iface:nomixed_str_len_arg /fpscomp:general /iface:cref \
	/warn:nofileopt 
FINCLUDE=/include:$(NCLPATH)\src\incf
FSYS=w
PWCFLAGS=-DSUN -ltermcap -I/usr/dt/share/include -I/usr/openwin/include
PWCPPFLAGS=
PWCPPINCS=-I$(SYRLIBS)/SUiApp/SUiAppFrameWork  -I$(SYRLIBS)/base
PWFFLAGS=-Nq600 -Nl130 -c -u
PWLDFLAGS=
PWSYS=SUN
#
# often changed symbols used for compiling
#
UU_COMP=/D UU_COMP=UU_WIN2K
UU_SEGDATA=/D UU_SEGDATA=DOUBLE
UU_PREC=/D UU_DOUBLE
UU_DEBUG=/D UU_DEBUGOFF
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
NCLBIN=$(NCLPATH)\$(BIN)
NCLSBIN=$(NCLPATH)\$(SBIN)
#
MUTOOL=$(NCLBIN)\mu
ITRTOOL=$(NCLBIN)\itr
MSGTOOL=$(NCLBIN)\msg -u
DDLTOOL=$(NCLBIN)\ddltool
#
# NCL includes, libraries and tools
#
NCLINC=$(NCLPATH)\src\inc
NCLFILES=$(NCLPATH)\nclfiles
#
FORFILTER=$(NCLBIN)\filter
#
CINCLUDE=/I $(NCLINC)
CFLAGS=$(CINCLUDE) $(DEVCFLAGS) $(UU_COMP) $(UU_PREC) $(UU_SEGDATA) $(UU_DEBUG)
FFLAGS=$(FINCLUDE) $(DEVFFLAGS)
#
# Compile variations
#
CC=cl $(DEBUG)
F77=df $(DEB) $(FFLAGS) -c 
NCC=cl $(NDEBUG)
NF77=df $(FFLAGS) -c 
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
CRELIB1=$(AR) /OUT:$(NCLLIB)\$(NCLLIBRARY) *.obj
#
# nodebug library
#
NRMLIB=$(RM) $(NNCLLIB)\$@ 
NCRELIB1=$(AR) /OUT:$(NNCLLIB)\$(NNCLLIBRARY) *.obj
#
#
# Here come some rules on how to deal with the OBJS lists
#
#
# debug rules
#
.c.ts:
	$(RM) $@
	@echo "*****  MAKING DEBUG VERSION OF $*.c  *****"
	$(CC) $(CFLAGS) $<
	$(PLACE_OBJ)
	$(TOUCH) $@
	@echo "***** FINISHED BUILDING $< IN $(SRCDIR) *****"
	@echo ""

.f.ts:
	$(RM) $@
	@echo "*****  MAKING DEBUG VERSION OF $*.c  *****"
	$(F77) $(FFLAGS) $<
	$(PLACE_OBJ)
	$(TOUCH) $@
	@echo "***** FINISHED BUILDING $< IN $(SRCDIR) *****"
	@echo ""

.for.ts:
	$(RM) $@
	@echo "*****  MAKING DEBUG VERSION OF $*.c  *****"
	$(FORFILTER) $<,$(FSYS)
	$(F77) $(FFLAGS) $*.f
	$(RM) $*.f
	$(PLACE_OBJ)
	$(TOUCH) $@
	@echo "***** FINISHED BUILDING $< IN $(SRCDIR) *****"
	@echo ""

.itr.ts:
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
	$(RM) $@
	@echo "*****  MAKING DEBUG VERSION OF $<  *****"
	$(MSGTOOL) $<
	$(RM) *.tmp
	$(RM) $*.hlp $(NCLFILES)\$*.hlp
	$(MV) $*.hlp $(NCLFILES)
	$(CC) $(CFLAGS) $*.c
	$(RM) *.tmp $*.c
	$(PLACE_OBJ)
	$(TOUCH) $@
	@echo "***** FINISHED BUILDING $< IN $(SRCDIR) *****"
	@echo ""
	
.pmu.ts:
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
	$(RM) $@
	@echo "*****  MAKING NODEBUG VERSION OF $<.c  *****"
	$(NCC) $(CFLAGS) $<
	$(PLACE_OBJ)
	$(TOUCH) $@
	@echo "***** FINISHED BUILDING $< IN $(SRCDIR) *****"
	@echo ""

.f.nts:
	$(RM) $@
	@echo "*****  MAKING DEBUG VERSION OF $*.c  *****"
	$(NF77) $(FFLAGS) $<
	$(PLACE_OBJ)
	$(TOUCH) $@
	@echo "***** FINISHED BUILDING $< IN $(SRCDIR) *****"
	@echo ""

.for.nts:
	$(RM) $@
	@echo "*****  MAKING DEBUG VERSION OF $*.c  *****"
	$(FORFILTER) $<,$(FSYS)
	$(NF77) $(FFLAGS) $*.f
	$(RM) $*.f
	$(PLACE_OBJ)
	$(TOUCH) $@
	@echo "***** FINISHED BUILDING $< IN $(SRCDIR) *****"
	@echo ""

.itr.nts:
	$(RM) $@
	@echo "*****  MAKING NODEBUG VERSION OF $<  *****"
	$(ITRTOOL) $<
	$(NCC) $(CFLAGS) $*.c
	$(RM) *.tmp $*.c
	$(PLACE_OBJ)
	$(TOUCH) $@
	@echo "***** FINISHED BUILDING $< IN $(SRCDIR) *****"
	@echo ""
	
.msg.nts:
	$(RM) $@
	@echo "*****  MAKING NODEBUG VERSION OF $<  *****"
	$(MSGTOOL) $<
	$(RM) *.tmp
	$(RM) $(NCLFILES)\$*.hlp
	$(MV) $*.hlp $(NCLFILES)
	$(NCC) $(CFLAGS) $*.c
	$(RM) *.tmp $*.c
	$(PLACE_OBJ)
	$(TOUCH) $@
	@echo "***** FINISHED BUILDING $< IN $(SRCDIR) *****"
	@echo ""
	
.pmu.nts:
	$(RM) $@
	@echo "*****  MAKING NODEBUG VERSION OF $<  *****"
	$(MUTOOL) $<
	$(NCC) $(CFLAGS) $<
	$(RM) *.tmp $*.c
	$(PLACE_OBJ)
	$(TOUCH) $@
	@echo "***** FINISHED BUILDING $< IN $(SRCDIR) *****"
	@echo ""
	
.SUFFIXES: .c .itr .msg .pmu .ts .nts .obj .o
#
#---------------------------EOF make.prefix.src.wnt --------------------------
#
#
#--------------------------- make.list --------------------------
#
SRCS1= \
ofmisc.f ofstubs.f ondist.f omain.c omalloc.c omisc.c ostubs.c
OBJS1= \
ofmisc.ts ofstubs.ts ondist.ts omain.ts omalloc.ts omisc.ts ostubs.ts
NOBJS1= \
ofmisc.nts ofstubs.nts ondist.nts omain.nts omalloc.nts omisc.nts ostubs.nts
LIBOBJS1= \
ofmisc.obj ofstubs.obj ondist.obj omain.obj omalloc.obj omisc.obj ostubs.obj
NLIBOBJS1= \
ofmisc.o ofstubs.o ondist.o omain.o omalloc.o omisc.o ostubs.o
SRCS2= \

OBJS2= \

NOBJS2= \

LIBOBJS2= \

NLIBOBJS2= \

SRCS3= \

OBJS3= \

NOBJS3= \

LIBOBJS3= \

NLIBOBJS3= \

SRCS4= \

OBJS4= \

NOBJS4= \

LIBOBJS4= \

NLIBOBJS4= \

SRCS5= \

OBJS5= \

NOBJS5= \

LIBOBJS5= \

NLIBOBJS5= \

SRCS6= \

OBJS6= \

NOBJS6= \

LIBOBJS6= \

NLIBOBJS6= \

SRCS7= \

OBJS7= \

NOBJS7= \

LIBOBJS7= \

NLIBOBJS7= \

SRCS8= \

OBJS8= \

NOBJS8= \

LIBOBJS8= \

NLIBOBJS8= \

SRCS9= \

OBJS9= \

NOBJS9= \

LIBOBJS9= \

NLIBOBJS9= \

SRCS=$(SRCS1) $(SRCS2) $(SRCS3) $(SRCS4) $(SRCS5) $(SRCS6) \
     $(SRCS7) $(SRCS8) $(SRCS9)
OBJS=$(OBJS1) $(OBJS2) $(OBJS3) $(OBJS4) $(OBJS5) $(OBJS6) \
     $(OBJS7) $(OBJS8) $(OBJS9)
NOBJS=$(NOBJS1) $(NOBJS2) $(NOBJS3) $(NOBJS4) $(NOBJS5) $(NOBJS6) \
     $(NOBJS7) $(NOBJS8) $(NOBJS9)
LIBOBJS=$(LIBOBJS1) $(LIBOBJS2) $(LIBOBJS3) $(LIBOBJS4) $(LIBOBJS5) \
        $(LIBOBJS6) $(LIBOBJS7) $(LIBOBJS8) $(LIBOBJS9)
NLIBOBJS=$(NLIBOBJS1) $(NLIBOBJS2) $(NLIBOBJS3) $(NLIBOBJS4) $(NLIBOBJS5) \
        $(NLIBOBJS6) $(NLIBOBJS7) $(NLIBOBJS8) $(NLIBOBJS9)
#
# Define the NCL directory this makefile should run in.
#
DIR=src
SRCDIR=oml
NCLLIBRARY=oml.lib
NNCLLIBRARY=noml.lib

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
makeall: $(OBJS) $(NCLLIBRARY) 
nmakeall: cleanup $(SRCS) wait $(NOBJS) $(NNCLLIBRARY) makeall
updatelib: $(LIBOBJS) $(NCLLIBRARY)
updatenlib: $(NLIBOBJS) $(NNCLLIBRARY) updatelib
#
# Library labels
#
$(NCLLIBRARY): $(OBJS)
	$(RMLIB)
	$(CRELIB1)
	$(RM) *.obj
	$(RM) $(SYMLIB)

$(NNCLLIBRARY): $(NOBJS)
	$(NRMLIB)
	$(NCRELIB1)
	$(RM) *.o
	$(RM) $(SYMLIB)

$(LIBOBJS):
	lib $(NCLLIB)\$(NCLLIBRARY) /extract:$*.obj

$(NLIBOBJS):
	lib $(NNCLLIB)\$(NNCLLIBRARY) /extract:$*.obj

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
	-@$(RM) $(SYMLIB)
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
