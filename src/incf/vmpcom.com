c*********************************************************************
c**
c**    NAME         :  vmpcom.com 
c**
c**    CONTAINS:
c**     Common block of data used by the VMPOCK routines.
c**
c**    MODULE NAME AND RELEASE LEVEL 
c**       vmpcom.com , 25.1
c**    DATE AND TIME OF LAST MODIFICATION
c**       04/29/15 , 15:07:33
c*********************************************************************
c
      integer*2 N_IVMP,N_KVMP,N_RVMP,N_GVMP,N_CVMP
c
      parameter (N_IVMP = 100)
      parameter (N_KVMP = 10)
      parameter (N_RVMP = 10)
      parameter (N_GVMP = 150)
      parameter (N_CVMP = 400)
c
      common /ivmpcm/ IVMP(N_IVMP)
c
      equivalence (IVMPFL,IVMP(001)), (IVMPFD,IVMP(026))
      equivalence (IVMPSP,IVMP(032)), (IVMPMX,IVMP(038))
      equivalence (IVMSFL,IVMP(039)), (IVMSFD,IVMP(064))
      equivalence (IVMSSP,IVMP(070)), (IVMADJ,IVMP(076))
	  equivalence (IVMSDJ,IVMP(077)), (IVMCON,IVMP(078))
	  equivalence (IVMSCN,IVMP(079)), (IVMMIN,IVMP(080))
	  equivalence (IVMSMN,IVMP(081))
c
      integer*2 IVMPFL(25),IVMPFD(6),IVMPSP(6),IVMSFL(25),IVMSFD(6),
     1          IVMSSP(6),IVMADJ,IVMSDJ,IVMCON,IVMSCN,IVMMIN,IVMSMN
      logical IVMPMX
c
      common /kvmpcm/ KVMP(N_KVMP)
c
      equivalence (KVMPDM,KVMP(001))
c
      integer*4 KVMPDM
c
      common /rvmpcm/ RVMP
c
      equivalence (RVMPDM,RVMP(001))
c
      real*4 RVMP(N_RVMP)
c
      common /gvmpcm/ GVMP
c
      real*8 GVMP(N_GVMP)
c
      equivalence (VMPPAS,GVMP(001)), (VMPENT,GVMP(011))
      equivalence (VMPFED,GVMP(016)), (VMPSPD,GVMP(022))
      equivalence (VMPCLF,GVMP(028)), (VMPRPT,GVMP(032))
      equivalence (VMPPOS,GVMP(036)), (VMPBPL,GVMP(040))
      equivalence (VMPTPL,GVMP(044)), (VMPTHK,GVMP(048))
      equivalence (VMPMX ,GVMP(051)), (VMPMXI,GVMP(063))
      equivalence (VMSPAS,GVMP(075)), (VMSENT,GVMP(085))
      equivalence (VMSFED,GVMP(090)), (VMSSPD,GVMP(096))
      equivalence (VMSCLF,GVMP(102)), (VMSRPT,GVMP(106))
      equivalence (VMSPOS,GVMP(110)), (VMSBPL,GVMP(114))
      equivalence (VMSTPL,GVMP(118)), (VMPFLN,GVMP(122))
      equivalence (VMSFLN,GVMP(123)), (VMPTLN,GVMP(124))
      equivalence (VMSTLN,GVMP(125)), (VMPRAP,GVMP(126))
      equivalence (VMSRAP,GVMP(128)), (VMPMIN,GVMP(130))
      equivalence (VMSMIN,GVMP(131)), (VMPDRL,GVMP(132))
      equivalence (VMSDRL,GVMP(136))
c
c
      real*8 VMPPAS(10),VMPENT(5),VMPFED(6),VMPSPD(6),VMPCLF(4),
     1       VMPRPT(4),VMPPOS(4),VMPBPL(4),VMPTPL(4),VMPTHK(3),
     2       VMPMX(12),VMPMXI(12),VMSPAS(10),VMSENT(5),VMSFED(6),
     3       VMSSPD(6),VMSCLF(4),VMSRPT(4),VMSPOS(4),VMSBPL(4),
     4       VMSTPL(4),VMPFLN,VMSFLN,VMPTLN,VMSTLN,VMPRAP(2),VMSRAP(2),
     5       VMPMIN,VMSMIN,VMPDRL(4),VMSDRL(4)
c
      common /cvmpcm/ CVMP
c
      character*1 CVMP(N_CVMP)
c
      equivalence (VMPLBB,CVMP(001)), (VMPLBT,CVMP(097))
      equivalence (VMPLBC,CVMP(0193))
c
      character*96 VMPLBB,VMPLBT,VMPLBC
c
