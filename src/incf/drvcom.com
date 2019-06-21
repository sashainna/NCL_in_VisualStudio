c*********************************************************************
c**
c**    NAME         :  drvcom.com 
c**
c**    CONTAINS:
c**     NCL surface and net surface evaluation common.
c**
c**    MODULE NAME AND RELEASE LEVEL 
c**       drvcom.com , 25.1
c**    DATE AND TIME OF LAST MODIFICATION
c**       04/29/15 , 15:07:30
c*********************************************************************
C
      integer*2 N_GSFCOM
c
      parameter (N_GSFCOM = 520)
c
      parameter (maxpt=50)
      parameter (maxpn=20)
      parameter (mxsfhd=(2+(maxpn+1)/2))
      parameter (mxpnhd=(2+(maxpt+1)/2))
      parameter (mxsdhd=82)
c
      common /gsfcom/ GSFCOM
c
      real*8 GSFCOM(N_GSFCOM)
c
      equivalence (SRFHED,GSFCOM(001)), (PANHED,GSFCOM(049))
      equivalence (SD    ,GSFCOM(157))
c
      real*8 SRFHED(4*mxsfhd),PANHED(4*mxpnhd),SD(4*mxsdhd)
      real*4 apnhed(mxpnhd*8)
      integer*2 ksfhed(mxsfhd*16)
      equivalence (panhed,apnhed),(srfhed,ksfhed)
c
      real*4 asd(4*mxsdhd*2)
      integer*4 jsd(4*mxsdhd*2)
      integer*2 ksd(4*mxsdhd*4)
      equivalence (sd,asd,jsd,ksd)
