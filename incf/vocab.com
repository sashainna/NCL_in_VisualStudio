c*********************************************************************
c**
c**    NAME         :  vocab.com
c**
c**    CONTAINS:  
c**      The vocabulary words for Fortran parsing
c**
c**    MODULE NAME AND RELEASE LEVEL
c**        vocab.com , 25.1
c**    DATE AND TIME OF LAST MODIFICATION
c**        04/29/15 , 15:07:33
c*********************************************************************

      integer*2 OUT,ON,IN,VOCEND,VPOCK,SCRUB,FMILL,HEIGHT,PASS,FEDRAT
      integer*2 VTOLER,VSTEP,VOCTO,PAST,RETRCT,RAPTO,RAPID,START,FWD
      integer*2 AVOID,THRU,UP,DOWN,OFF,BOTH,SMILL,COMBIN

      integer*2 LAYER,WCOMP,WCURVE,STOCK,DEPTH,OFFSET,OMIT,PART,FIT,
     x          AT,WERROR,RANDOM,NEGX,NEGY,NEGZ,POSX,POSY,POSZ,
     x          SMALL,LARGE,NEARPT,CONTCT,ALL,WRAP,BOUND,VOPEN

      integer*2 FINISH,THICK,MAIN,ZONE,LAST,VSTOP,DEEP,LEVEL,NORMAL,
     x          SAME,VDS,VPS,VSHORT,CLW,CCLW,VOCREV,VOCPLN

      integer*2 ramp, plunge, helix
      integer*2 sharp, arc, INCR
      integer*2 ATANGL,warn,nwarn,CUTCOM
      integer*2 left,right,xyplan,yzplan,zxplan
      integer*2 NOMORE,TANTOV,PERPTO,TRANSV,ISLAND,COLAPS,LACE
      integer*2 LONG,COUPLE,VCYCLE

      parameter (ATANGL = 1, ALL = 816, AT = 189, AVOID = 327)
      parameter (BOTH = 83, BOUND = 624)
      parameter (CLW = 60, CCLW = 59, CONTCT = 856, DEEP = 153)
      parameter (DEPTH = 510)
      parameter (FEDRAT = 1009, FINISH = 323, FIT = 834, FMILL = 748)

      parameter (FWD = 651, HEIGHT = 754, IN = 652, LARGE = 662)
      parameter (LAYER = 902, LAST = 52, LEVEL = 759, MAIN = 93)
      parameter (NEARPT = 866, NORMAL = 820)

      parameter (NEGX = 657, NEGY = 658, NEGZ = 659) 
      parameter (POSX = 654, POSY = 655, POSZ = 656)

      parameter (OFF = 72, ON = 71, OFFSET = 666)
      parameter (OMIT = 172, OUT = 653, PART = 260, PASS = 755)
      parameter (PAST = 715, VPOCK = 738)

      parameter (RANDOM = 326, RAPID = 5, RAPTO = 280)
      parameter (RETRCT = 7, SAME = 730, SCRUB = 739, SMALL = 663)
      parameter (START = 57, STOCK = 321, THICK = 717, THRU = 152)

      parameter (VOCEND = 499, VOCTO = 714, VDS = 729, VPS = 728)
      parameter (VOPEN = 50, VIN = 652)
      parameter (VSTEP = 92, VSTOP = 2, VTOLER = 731, WCOMP = 612)
      parameter (WCURVE = 608, WERROR = 937, ZONE = 760)
      parameter (WRAP = 889)

      parameter (LEFT = 8, RIGHT = 24)
      parameter (XYPLAN = 33, YZPLAN = 37, ZXPLAN = 41)
      parameter (NOMORE = 53)
      parameter (TANTOV = 646, PERPTO = 630)
      parameter (ramp = 854, plunge = 1001, helix = 855)
      parameter (COUPLE = 1049, VCYCLE = 1054)
      parameter (COLAPS = 343)
      parameter (LACE   = 342)
      parameter (LONG = 850, VSHORT = 851)
      parameter (UP = 112, DOWN = 113)
      parameter (sharp = 183, arc = 182)
      parameter (INCR = 66)
      parameter (ISLAND = 757, TRANSV = 1037)
      parameter (nwarn = 853, warn = 852)
      parameter (CUTCOM = 1007)
      parameter (VOCREV = 861, VOCPLN = 606)
      parameter (SMILL = 756)
      parameter (COMBIN = 1071)
