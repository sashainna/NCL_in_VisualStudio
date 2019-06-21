c
c*********************************************************************
c*    NAME         :  cutter.com
c*     MODULE NAME AND RELEASE LEVEL 
c*       cutter.com , 25.2
c*    DATE AND TIME OF LAST MODIFICATION
c*       12/01/15 , 08:17:36
c********************************************************************/
c
      integer*2 N_ICTCOM,N_CCTCOM
c
      parameter (N_ICTCOM = 20)
      parameter (N_KCTCOM = 400)
      parameter (N_GCTCOM = 250)
      parameter (N_CCTCOM = 5500)
c
      common /ictcom/ ICTCOM
      integer*2 ICTCOM(N_ICTCOM)
c
      equivalence (ICIPRM,ICTCOM(001)), (FITYP,ICTCOM(011))
      equivalence (CYCRET,ICTCOM(012))
c
      integer*2 ICIPRM(10),FITYP,CYCRET
c
      common /kctcom/ KCTCOM
      integer*4 KCTCOM(N_KCTCOM)
c
      equivalence (CCOOL ,KCTCOM(001)), (ICCMOD,KCTCOM(002))
      equivalence (ICCDIR,KCTCOM(003)), (CTLNO ,KCTCOM(004))
      equivalence (ICTSAV,KCTCOM(005)), (ITRAFL,KCTCOM(006))
      equivalence (NCTOOL,KCTCOM(007)), (ICTINC,KCTCOM(008))
      equivalence (ICTENT,KCTCOM(009)), (ICTNC ,KCTCOM(010))
      equivalence (ICTLIN,KCTCOM(011)), (TOOLPT,KCTCOM(012))
      equivalence (TOOLIX,KCTCOM(013)), (FROMCT,KCTCOM(141))
      equivalence (ICOMNC,KCTCOM(142)), (NCOM  ,KCTCOM(342))
      equivalence (LTHBIT,KCTCOM(343)), (I2NDMX,KCTCOM(344))
      equivalence (CSPDIR,KCTCOM(345)), (TVERSION,KCTCOM(346))
      equivalence (ICUTFL,KCTCOM(347)), (CUTKEY,KCTCOM(357))
c
      integer*4 ICUTFL(10),ITRAFL,NCTOOL,ICTINC,ICTENT,ICTNC,ICTLIN,
     1          TOOLPT,TOOLIX(128),FROMCT,ICOMNC(200),NCOM,LTHBIT,
     2          I2NDMX,CSPDIR,TVERSION,CUTKEY(3),CCOOL,ICCMOD,ICCDIR,
     3          CTLNO,ICTSAV
c
      common /gctcom/ GCTCOM
      real*8 GCTCOM(N_GCTCOM)
c
      equivalence (DCUTR ,GCTCOM(001)), (FGREC ,GCTCOM(021))
      equivalence (FGBUF ,GCTCOM(085)), (RCIPRM,GCTCOM(213))
      equivalence (CFEED ,GCTCOM(223)), (CRPM  ,GCTCOM(225))
      equivalence (PSTNUM,GCTCOM(226)), (CTLEN ,GCTCOM(227))
c
      real*8 DCUTR(20),FGREC(64),FGBUF(128),RCIPRM(10),CFEED(2),CRPM,
     1       PSTNUM,CTLEN
c
      common /cctcom/ CCTCOM
c
      character*1 CCTCOM(N_CCTCOM)
c
      equivalence (LSC   ,CCTCOM(0001))
c      equivalence (CUTLIB,CCTCOM(2001)), (LCTOOL,CCTCOM(2065))
      equivalence (LCTOOL,CCTCOM(2065))
      equivalence (TOOLIB,CCTCOM(3345)), (NCDATA,CCTCOM(4369))
      equivalence (CUTSYM,CCTCOM(4433)), (CPSTNM,CCTCOM(4673))
	  equivalence (CUTLIB,CCTCOM(5001)) 
c
      character*40 CPSTNM
c      character*64 CUTLIB,LCTOOL(20),NCDATA
      character*64 LCTOOL(20),NCDATA
	  character*256 CUTLIB
      character*80 CUTSYM(3),LSC(25)
      character*(MAX_PATH) TOOLIB
c
      integer*2 firec(256),fibuf(512)
      integer*4 frec(128),fbuf(256)
      character*256 sylib
      character*512 fcrec
      character*1024 fcbuf
c
      equivalence (fgrec,firec,frec,fcrec)
      equivalence (fgbuf,fibuf,fbuf,fcbuf)
      equivalence (sylib ,firec(38))
c
      common /ctcomc/ comtl
      character*80 comtl(200)
