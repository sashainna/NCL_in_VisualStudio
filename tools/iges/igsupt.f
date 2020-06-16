C *********************************************************************
C **    MODULE NAME AND RELEASE LEVEL                                **
C **       igsupt.f , 25.1                                                 **
C **    DATE AND TIME OF LAST  MODIFICATION                          **
C **       04/29/15 , 15:12:43                                                 **
C **                                                                 **
C **  PURPOSE OF MODULE:                                             **
C **                                                                 ** 
C **    Fortan support routines.                                     **
C **       gtgeom                                                    **
C **       gtmptt                                                    **
C **       gtpptt                                                    **
C **                                                                 **
C *********************************************************************
c **********************************************************************
c **  subroutine name: gtgeom
c **
c **  purpose of subroutine:
c **
c **********************************************************************
c **********************************************************************
 
      subroutine gtgeom (asw, buf, nclkey, nwds, ietype)

      include 'com4a.com'

	  real*8 asw, buf(12)
      integer*4 nclkey
      integer*2 nwds, ietype

	  call gtdesc (asw, nclkey, nwds, ietype)
      call gtgeo (nclkey, buf)

99999 return
      end
c **********************************************************************
c **********************************************************************
c **  subroutine name: gtmptt
c **
c **  purpose of subroutine:
c **
c **********************************************************************
c **********************************************************************
 
      subroutine gtmptt (nclkey, ipat, buf)

      include 'com4a.com'

      integer*4 nclkey
      integer*2 ipat
	  real*8 buf(12)

      call gtmpat (nclkey, ipat, buf)

99999 return
      end
c
c **********************************************************************
c **********************************************************************
c **  subroutine name: gtpptt (pankey,ipatnm,dbuf,nwds)
c **
c **  last revision:
c **  purpose of subroutine: this routine  is the link to NCL 'gtppat' 
c **                         routine to get patch of surface. 
c **
c **********************************************************************
c **********************************************************************
 
		subroutine gtpptt (pankey,ipatnm,dbuf,nwds)

		include 'com8a.com'

		real*8 dbuf
		integer*2 nwds
		integer*4 pankey

		call gtppat (pankey,ipatnm,dbuf)

		return
		end
