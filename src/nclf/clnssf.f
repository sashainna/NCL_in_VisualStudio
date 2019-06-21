c*****************************************************
c**
c**     module name and release level
c**       clnssf.f , 25.1 
c**    date and time of last modification
c**       04/29/15 , 15:09:42
c**
c*****************************************************
c**
c** copyright (c) 1987 mills data systems corporation
c**
c **********************************************************************
c **********************************************************************
c **  program name: clnssf (nclkey,rmx,isfkey)
c **
c **  purpose of program: clone a net surface.
c **
c **    input arguments
c **           nclkey - key of surface to clone
c **           rmx    - matrix to clone surface with
c **    output arguments
c **           isfkey - key of cloned surface.
c **
c **********************************************************************
c **********************************************************************

      subroutine clnssf (nclkey, rmx, isfkey)

      include 'com4a.com'

      common/pblok/p(400)

      integer*4 nclkey, isfkey
      real*8 rmx(12)

      real*8 buf(300), asn, pbuf(200)
      real*4 abuf(600)
      integer*4 i4sshd(200), ikey, jkey
      integer*2 ksn(4), ietype, ibuf(1200), i2sshd(4)
      equivalence (asn, ksn), (p, buf, abuf, ibuf)
      equivalence (pbuf, ssfhed, i4sshd, i2sshd)
      logical trflg

      trflg=.false.
      call gtgeo(nclkey,ssfhed)
      call ssfcre(ssfhed,isfkey)
      do 200 isf=1,i2sshd(2)
        ikey=i4sshd(isf+2)
        call sftype(ikey,isftyp)
        call ptdesc (ikey,9,asn)
        call gtentt (asn, trflg, ikey, ietype, buf)
        if (isftyp.eq.26) then
c                                       mesh surface
              npat=ibuf(2)
              call ptmhdt(buf,jkey)
              call nevdsp(jkey)
              do 210 ipat=1,npat
                  call gtmptt(ikey,ipat,buf)
                  call trnmsh(buf,rmx)
                  if (ifl(2) .gt. 0) goto 9999
210               call ptmptt(jkey,ipat,buf)
        else if (isftyp .eq. 25) then
c                                        quilt surface
              npat=ibuf(2)
              call trnqhd(buf,rmx)
              if (ifl(2) .gt. 0) goto 9999
              call ptqhed(buf,jkey)
              call nevdsp(jkey)
              do 310 ipat=1,npat
                  call gtqpat(ikey,ipat,buf)
                  call trnqlt(buf,rmx)
                  if (ifl(2) .gt. 0) goto 9999
310               call ptqpat(jkey,ipat,buf)
        else
c                                      standard surface
              npan=ibuf(2)
              call ptshed (buf, jkey)
              call nevdsp(jkey)
              do 130 ipan=1,npan
c                                      get panel ipan header
                  call gtspa1(ikey,ipan,buf,pankey)
                  npat=ibuf(2)+1
                  ix=npat/2+3
                  nwds=14
                  if (ibuf(1).eq.1) nwds=8
c                                      get patches of panel ipan
                  do 120 ipat=1,npat
                      call gtpptt(pankey,ipat,buf(ix),nwds)
120                   ix=ix+nwds
c                                      transform panel ipan & store
                  nwds=ix-1
                  call transf(buf,rmx,nwds,ietype)
                  if (ifl(2) .gt. 0) goto 9999
130               call ptspnt(jkey,ipan,buf)
        endif
        call ssfupd(isfkey, isf, jkey)
200   continue

      goto 99999

9999  call error (ifl(2))

99999 return
      end
