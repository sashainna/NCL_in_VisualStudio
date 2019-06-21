C*********************************************************************
C*    NAME         :  calrst.f
C*       CONTAINS:
C*    COPYRIGHT 1991 (c) Numerical Control Computer Sciences
C*                       All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       calrst.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:09:39
C********************************************************************/
C
C **********************************************************************
C **  PROGRAM NAME: calrst (knum)
C **
C **  PURPOSE OF PROGRAM: Processes the *RESET/CALL[,n] command, backing
c **                      out of the current call/loop stack.
c **
c **  Input:  knum    I*2   D1   Contains the number of Macro calls to
c **                             back out of.
c **
c **  Output: none.
C **
C **********************************************************************
C **********************************************************************
 
      subroutine calrst (knum)
c
      include 'com8a.com'
c
      integer*2 knum
c
      integer*2 icnt,iwhich,declar,exdo,exloop,exmac,undo,lopend
c
      integer*4 iel,idum,nc,tmp,curlin
c
      logical isav
c
      character*64 csc155
      character*64 lbuf
c
      equivalence (sc155,csc155)
c
      data exmac /2/
      data declar /1/, exloop /2/, exdo /3/
      data lopend /1/, undo /2/
c
c...Declaring loop
c...Ignore this command
c
      isav   = echo
      echo   = .false.
      if (ifl(45) .eq. declar) go to 8000
c
c...Are we done yet
c
      icnt   = knum
  100 if (icnt .le. 0) go to 8000
c
c...DO Loop is currently active
c
      if (ifl(45) .eq. exdo .and. csc155 .ne. ' ') then
          iwhich = undo
          call loopnd (iwhich)
c
c...MACRO call is currently active
c
      else if (ifl(38) .eq. exmac) then
          call termac
          icnt   = icnt   - 1
c
c...LOOPST is currently active
c
      else if (ifl(45) .eq. exloop) then
          call lrdata (lbuf,tmp,lbuf,tmp,idum,idum,iel,idum,idum,lbuf,
     1                 tmp,1)
          call nclf_src_rec_to_line (iel,nline)
          call getsrc (w2,ncw2,nline-1,0)
          call nclf_putw2(ifl(123),w2,ncw2,irctyp)
          iwhich = lopend
          call loopnd (iwhich)
          call nclf_src_rec_to_line (iel,curlin)
          ifl4(10) = iel + 1
c
c...Nothing is active
c
      else
          go to 8000
      endif
c
c...Loop again
c
      lskip  = .true.
      if (ifl4(10) .ne. 0) nline = ifl4(10)
      ifl4(10) = 0
      go to 100
c
c...End of routine
c
 8000 echo   = isav
      return
      end
