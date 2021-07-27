C*********************************************************************
C*    NAME         :  pwild.f
C*       CONTAINS:
C*    COPYRIGHT 2007 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       pwild.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:33
C*********************************************************************
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine pwild
C*      This routine handles parser wildcard string.  It is called by
C*      the PARSIT and it returns the list of geoms in geolst
C*      with ITYP equal to ietype.  
C*    PARAMETERS
C*       INPUT  :
C*          none
C*       OUTPUT :
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/

      subroutine pwild
c
      include 'com8a.com'

      common/subwild/ nextnum,wname, nxtptr, fstlst
      integer*4 nextnum, nxtptr, fstlst, ssub, nlen
      character*64 wname, stok, cmpstr, numstr

      integer*2 ietype
      integer*4 nclkey, isub,kwds,wlen,len1, strlen1, ipg, iel
      integer*4 match
c
C...Initialization
c
      wildnum = 0
      nextnum = 0
      fstlst = 1
      stok = token2
      ssub = IVXSUB
c
c... the token have to be a 'wildcard string' 
c
      if (ityp.ne.10) then
          err=.true.
          goto 99999
      endif
      
	wname = token2
      wlen = strlen1 (token2)
      call vxlfst
620   continue
      call vxlnxt (token2, isub, nclkey, kwds, ietype, ipg, iel)
      if (ietype.eq.1) goto 630
c
c...wildcard string could be '*', '*str', 'str1*str2', '*str1*str2*...*str'
c...just call 'chkwstr' to see if token2 match the wild card string
c
c......we compile wild card include subscript sunch as ABC(*)
c
      len1 = strlen1 (token2)
      cmpstr = token2
      if (isub.gt.0) then
         len1 = len1 + 1
         cmpstr(len1:len1) = '('
         len1 = len1 + 1
         write (numstr, 101) isub
         nlen = strlen1 (numstr)
         cmpstr(len1:len1+nlen) = numstr(1:nlen)
         len1 = len1+nlen
         cmpstr(len1:len1) = ')'    
      endif        
 101  format (i4)
      call chkwstr(cmpstr, len1, wname, wlen, match)
c...match=0: not match, -1, abort, 1: matched
      if (match.ne.1)  go to 620
c
c...we don't return certain type of the label
c
      if (((ietype.ge.11).and.(ietype.le.17))
     x     .or. (ietype.eq.19)) go to 620
      wildnum = wildnum + 1
      goto 620
  630 continue

99999 if (ifl(2) .ne. 0) then
          err=.true.
      endif
C 
C...save nextyp in ifl(122)
C 
      ifl(122)=nextyp
      token2 = stok
      IVXSUB = ssub
      wildwd = .true.
      return
      end

C*********************************************************************
C*    E_SUBROUTINE     : subroutine getwildlst()
C*       return wildcard next label/index into current token2 and ivxsub.
C*    PARAMETERS
C*       INPUT  : none
C*       OUTPUT : none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/

      subroutine getwildlst

      include 'com.com'

      common/subcom/ idelim
      integer*2 idelim
      common/subwild/ nextnum,wname,nxtptr,fstlst
      integer*4 nextnum,nxtptr,fstlst,wlen,len1, strlen1, ipg, iel
      character*64 wname, cmpstr, numstr
      integer*4 match

      integer*2 ietype
      integer*4 nclkey,n, nlen
c
c...   Return the next entity
c
      if (ifl(44).eq.0) then
c
c...   If caller is not skipping delimeters, return comma every second call.
c
        idelim = 1-idelim
        if (idelim.eq.1) then
          ityp = 5
          ist = nextyp
          return
        endif
      endif
      if (wildnum.eq.0) then
           token2 = ' '
           ivxsub = 0
      endif
      wlen = strlen1 (wname)
      n = 0
      if (fstlst .eq. 1) then
          call vxlfst
          fstlst = 0
      else
          call setvxidx(nextnum,nxtptr)
      endif
620   continue
      call vxlnxt (token2, ivxsub, nclkey, kwds, ietype, ipg, iel)
      if (ietype.eq.1) goto 630
c
c...wildcard string could be '*', '*str', 'str1*str2', '*str1*str2*...*str'
c...just call 'chkwstr' to see if token2 match the wild card string
c
c
c......we compile wild card include subscript sunch as ABC(*)
c
      len1 = strlen1 (token2)
      cmpstr = token2
      if (ivxsub.gt.0) then
         len1 = len1 + 1
         cmpstr(len1:len1) = '('
         len1 = len1 + 1
         write (numstr, 101) ivxsub
         nlen = strlen1 (numstr)
         cmpstr(len1:len1+nlen) = numstr(1:nlen)
         len1 = len1+nlen
         cmpstr(len1:len1) = ')'    
      endif        
 101  format (i4)
      call chkwstr(cmpstr, len1, wname, wlen, match)
      if (match.ne.1)  go to 620
c
c...we don't return certain type of the label
c
      if (((ietype.ge.11).and.(ietype.le.17))
     x     .or. (ietype.eq.19)) go to 620
c...      n = n + 1
c...      if (nextnum.ne.(n-1)) goto 620
c
c...save the NCL_name_list_idx into a common value to reset next time call vxlnxt
c
      call getvxinx(nextnum,nxtptr)     
  630 continue
c...      nextnum = nextnum + 1
      wildnum = wildnum - 1
      ityp = 2
      ist = ietype
c
c...call this function to assign some value (such tv)
c
      call vstchk
C
C...restore nextyp
C
      if (wildnum.eq.0) then
         nextyp=ifl(122)
         nextnum = 0
      else
C
C...set nextyp to ','
C
         nextyp=9
      endif
      wildwd = .true.
      return
      end

C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine setwildmin()
C*      This routine set parser wildcard string count number to minus 1.  
C*    PARAMETERS
C*       INPUT  :
C*          none
C*       OUTPUT :
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/

      subroutine setwildmin
c
      common/subwild/ nextnum,wname,nxtptr,fstlst
      integer*4 nextnum,nxtptr,fstlst
      character*64 wname

c...temp      if (nextnum.gt.0)
c...     x    nextnum = nextnum - 1
      return
      end
