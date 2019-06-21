C*********************************************************************
C*    NAME         :  ppword.f
C*
C*       CONTAINS:    ppword
C*                    fndpwd
C*                    nshent
C*                    pwddel
C*
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL
C*       ppword.for , 25.1
C*    DATE AND TIME OF LAST MODIFICATION
C*       04/29/15 , 15:09:34
C********************************************************************/
C
 
C********************************************************
C**
C**        FUNCTION: ppword(shpkey,cmmnd,pickid)
C**
C**        PURPOSE:  Places postprocessor commands into
C**                  the p array.
C**
C*********************************************************
 
      subroutine ppword(shpkey,cmmnd,pickid)
 
      include 'com8a.com'
 
      common/pblok/p(400)
      integer*2 kp(1600),isflag,is1,is4
      integer*4 shpkey, pickid,ifind
      integer*4 savncl
      integer*2  np,itemp(4),itemp1(4)
      character*1 cmmnd(80)
      integer*2 ksn(4),numwds
      real*8 buf(3),asn,pang,pi2
      real*8 temp,temp1,pbuf(80),psav(400)
      real*4 ang(2)
      equivalence(p,kp),(asn,ksn),(pang,ang)
      equivalence(temp1,itemp1),(temp,itemp)
c
      data is1 /4/, is4 /1/
      pi2=6.283185307d0
      ifind=0
      istrt=0
      inx=1
      icnt=1
      icnt2=0
      isflag=0
      npts = 0
C
C...Clear out ain so that no extra characters are lurking about.
C...Load ain with the uppercase postprocessor command cmmnd.
C
      do 1,i=1,51
1        ain(i)=' '
      do 2,i=1,80
2        call convrt(cmmnd(i),ain(i),1)
C
C...Call parser to get the asn numbers for the command.
C...and put them in pbuf for insertion into the p array.
C
3     call parser
C
C...If ityp.is equal to 7 and istrt is equal to 0 then there are spaces
C...at the begining of the command and need to call parser until the
C...command begins.
C
      if(ityp.eq.7.and.istrt.eq.0) then
        inx=inx+1
        goto 3
      endif
      if (ityp.eq.1.and.icnt.ne.1) then
C
C...Minor Word
C
         itemp1(is1)=0
         itemp1(is4)=ist
         pbuf(icnt)=temp1
         icnt=icnt+1
         icnt2=icnt2+1
      else if (ityp.eq.1) then
C
C...Major Word
C
         itemp(1)=ist
         itemp(4)=2000
         pbuf(icnt)=temp
         icnt=icnt+1
         icnt2=icnt2+1
         istrt=1
      else if(ityp.eq.2.or.ityp.eq.3.or.ityp.eq.4) then
C
C...Number
C
         pbuf(icnt)=tv
         icnt=icnt+1
         icnt2=icnt2+1
      end if
C
C...if the next token is a / ignore it and continue, if it is the end
C...of the command, continue with the routine, otherwise, go back
C...and get the next token.
C
      if(nextyp.eq.5) inx=inx+1
      if(nextyp.ne.0.and.nextyp.ne.11) then
         goto 3
      else
         pbuf(icnt) = 0
      endif
C
C...Clear out P. Get the shape information into p.
C
      do 5, i=1,400
5        p(i)=0
      call gtshap(p, shpkey)
C
C...Want to save p so that we can insert the command without losing any
C...valuable information
C
      do 10 i=1,400
10       psav(i)=p(i)
C
C...Save the key number of the shape.
C
      savncl=shpkey
      np=kp(3)
      buf(3)=0.
      tol=sc(27)*4.
C
C...Find stpt (on dummy lim1) .
C
      do 15 i=8,np
      j=i
      k=4*i
15    if(kp(k).eq.5)goto 20
 
20    continue
C
C...Run thru p-list.
C
      i=j+2
30    i=i+1
      k=4*i
C
C...If line or circle.
C
      if (i.ge.np) goto 35
      if(kp(k).eq.5)goto 40
      if(kp(k).eq.7)goto 50
      if(kp(k).eq.2000) i=i+kp(k-1)
      goto 30
C
C...All done.
C
35    goto 99
C
C...Line.
C
40    ifind=ifind+1
C
C...The new command should be placed right after the pickid
C...entity, in otherwords when ifind equals pickid +1, we have come
C...to the appropriate place in the p array. So load the new info
C...into p and then move all the rest down the array.
C
      if(ifind.eq.(pickid+1)) then
         do 41, j=1,icnt
41          p(i+(j-1))=pbuf(j)
         do 42, j=i,397
42          p(j+icnt)=psav(j)
C
C...For some reason not all of kp is changed when p is changes, so
C...assign the number of words in the command to kp.
C
         kp((i*4)-1)=icnt2-1
         isflag=1
      endif
      i=i+2
      goto 30
C
C...Circle.
C
50    ifind=ifind+1
C
C...The new command should be placed right after the pickid
C...entity, in otherwords when ifind equals pickid +1, we have come
C...to the appropriate place in the p array. So load the new info
C...into p and then move all the rest down the array.
C
      if(ifind.eq.(pickid+1)) then
         do 51, j=1,icnt
51          p(i+(j-1))=pbuf(j)
         do 52, j=i,397
52          p(j+icnt)=psav(j)
C
C...For some reason not all of kp is changed when p is changes, so
C...assign the number of words in the command to kp.
C
         kp((i*4)-1)=icnt2-1
         isflag=1
      endif
55    n=n+1
      if(n.lt.npts)goto 55
      i=i+6
      goto 30
 
99    continue
C
C...If isflag is equal to zero the pp command is going at the end of the shape
C...and need to make sure that pbuf is placed appropriately into p.
C
      if(isflag.eq.0) then
         do 61, j=1,icnt
61          p(np+j)=pbuf(j)
         do 62, j=i,397
62          p(j+icnt)=psav(j)
      endif
 
C
C...Update kp(3) with the new element count.
C
      kp(3)=kp(3)+icnt
      numwds=kp(3)
C
C...Save the p array with the new information in it.
C
      call ptshap(p,shpkey,numwds)
      return
      end
C**************************************************************
C*
C*      FUNCTION:  fndpwd(shpkey,pickid,commnd)
C*
C*      PURPOSE:  To find all the postprocessor commands in
C*                a shape array.
C*
C**************************************************************
 
      subroutine fndpwd(shpkey,pickid,commnd)
 
      include 'com8a.com'
 
 
      common/pblok/p(400)
c
      integer*4 shpkey, ifind,pickid
c
c...ist already declared in com.com
c...it will has syntex error in WNT
c
c...      integer*2  np,itemp(4),itemp1(4),ist
      integer*2  np,itemp(4),itemp1(4),is1,is4
      integer*2 ksn(4),kp(1600),q,ccnt,aflag
      integer*4 nc,strlen1
      character*7 commnd(10,20)
      character*6 pvoc
      character*80 hope
      real*8 asn,pang,pi2
      real*8 temp,temp1,lstcmd
      real*4 ang(2)
      equivalence(p,kp),(asn,ksn),(pang,ang)
      equivalence(temp1,itemp1),(temp,itemp)
      pi2=6.283185307d0
c
      data is1 /4/, is4 /1/
c
      ifind=0
      q=3
      imaj=1
      ccnt=0
      aflag=0
      npts = 0
 
 
C
C...Clear out P and commnd. Get the shape information into p.
C
      do 5, i=1,400
5        p(i)=0
      do 6, j=1,20
         do 7, i=1,10
7           commnd(i,j)=' '
C7           commnd(i,j)='\0'
6     continue
      call gtshap(p, shpkey)
C
C...np is the number of elements in the p array.
C
      np=kp(3)
C
C...Find stpt (on dummy lim1).
C
      do 15 i=8,np
      j=i
      k=4*i
15    if(kp(k).eq.5)goto 20
 
20    continue
C
C...Run thru p-list.
C
      i=j+2
30    i=i+1
      k=4*i
C
C...If line or circle. ifind is used to determine when the
C...correct location in the p array has been reached.  This
C...happens when ifind is equal to the pickid number.
C
      if (i.ge.np) goto 35
      if (kp(k).eq.5) then
         ifind=ifind+1
         goto 40
      endif
      if (kp(k).eq.7) then
         ifind=ifind+1
         goto 50
      endif
C
C...aflag is used to determine if it is the start of a new
C...pp command or if still building on the current command.
C...If it is a new command then aflag is equal to 0.
C
      if (ifind.ne.pickid) goto 30
      if (aflag.eq.0) then
C
C...If ccnt is not equal to zero then we want to clear out
C...the remaining portion of that row in the array so that no
C...garbage is left in there.
C
         if (ccnt.ne.0) then
            if (q .eq. 4) q = q - 1
            do 31, ii=(q-1),10
C
C...Using \0 as a NULL character caused problems with the HP.
C...By removing the \0 and not replacing it with anything, fixes
C...the problem.  It will cause the rs6000 to complain when compliling
C...but it will compile and still run correctly.  JLS 9/27/99
C31             commnd(ii,ccnt)='\0'
C
31             commnd(ii,ccnt)=' '
         endif
         ccnt=ccnt+1
C
C...Set q equal to 3 since q(1) is the major word and q(2) is '/'.
C
         q=3
         imaj=1
      endif
C
C...Set aflag to 1 so that if a postprocessor word is encountered before
C...aflag is set back to 0, then it is part of the same command.
C
      aflag=1
C
C...if imaj is equal to one, then we have the first word in the
C...pp command and therefore it is a major word.  It is then necessary
C...to go down to the else and put p(i) in the appropriate location
C...of temp so that the ist number can be retrieved.  If imaj is not
C...equal to one then it is either a minor word or a number.
C
      if (imaj.ne.1) then
         temp1=p(i)
         ist=itemp1(is4)
         if (ist.lt.1.or.ist.gt.1100) then
C
C...Need to change the real number into a character so
C...that it may be stored in the array.
C
            call num2char(p(i),hope)
            nc = strlen1(hope)
            commnd(q,ccnt)=hope(1:nc)
            q=q+1
C
C...If p(i) is equal to 0 then we have reached the end of this
C...particular command, so set aflag back to 0.
C
 
            if(p(i).eq.0.0) aflag=0
            lstcmd = p(i)
         else
            call asvoc(ist,1,pvoc)
            commnd(q,ccnt)=pvoc
            q=q+1
            lstcmd = -1
         endif
      else
         temp=p(i)
         ist=itemp(1)
         call asvoc(ist,1,pvoc)
         commnd(1,ccnt)=pvoc
C
C...After the major postprocessor word, there should be a slash.
C...Unless the word is either CLW or CCLW.
C
C
        if((ist.ne.59).and.(ist.ne.60)) commnd(2,ccnt)='/'
C         commnd(2,ccnt)='/'
         imaj = 0
      endif
      goto 30
C
C...All done.
C
35    goto 99
C
C...Line.
C
40    aflag=0
      i=i+2
      goto 30
C
C...Circle.
C
50    aflag=0
55    n=n+1
      if(n.lt.npts)goto 55
      i=i+6
      goto 30
 
99    continue
C
C...If there is an extra character 0.0000 at the end of the
C...last command, erase it.
C
cc      if (commnd((q-1),ccnt).eq.'0.0000')
      if (lstcmd .eq. 0.0) then
          commnd((q-1),ccnt)=' '
          if (q .eq. 4) commnd((q-2),ccnt)=' '
      endif
      return
      end
c
C*********************************************************************
C*    E_SUBROUTINE     : subroutine nshent (shpkey,nents)
c*       Returns the actual number of entities in a shape.
C*    PARAMETERS
C*       INPUT  :
C*          shpkey   = Shape key id.
C*       OUTPUT :
C*          nent     = Number of entities in a shape.
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
c
      subroutine nshent (shpkey,nents)
 
      include 'com8a.com'
 
      integer*4 shpkey,nents
 
      common/pblok/p(400)
      integer*2 kp(1600)
      equivalence(p,kp)
 
      integer*2 np,i,j,k
c
c...Initialize routine
c
      nents = 0
C
C...Get the shape information
C
      call gtshap (p, shpkey)
C
C...Save the key number of the shape.
C
      np = kp(3)
C
C...Find stpt (on dummy lim1) .
C
      do 15 i=8,np,1
          j=i
          k=4*i
          if (kp(k).eq.5) goto 20
   15 continue
C
C...Run thru p-list.
C
   20 i=j+2
   30 i=i+1
      k=4*i
C
C.......At end of shape
C
      if (i .ge. np) goto 8000
c
c.......Line
c
      if (kp(k).eq.5) then
          nents = nents + 1
          i=i+2
C
C......Circle.
C
      else if (kp(k).eq.7) then
          nents = nents + 1
          i=i+6
c
c......Post-processor command
c
      else if (kp(k).eq.2000) then
          i=i+kp(k-1)
      endif
      goto 30
c
c...End of routine
c
 8000 return
      end
c
C*******************************************************************
C*
C*        FUNCTION:  pwddel(shpkey,pickid,cmmnd,error)
C*
C*        PURPOSE:   To delete a postprocessor command from the
C*                   shape array.
C*
C********************************************************************
 
      subroutine pwddel(shpkey,pickid,cmmnd,error)
 
      include 'com8a.com'
 
      common/pblok/p(400)
      integer*2 kp(1600),kpbuf(200),error
      integer*4 shpkey, pickid,ifind
      integer*4 savncl
      integer*2  np,itemp(4),itemp1(4),is0,is1,is3,is4
      character*1 cmmnd(100)
      integer*2 ksn(4),numwds
      real*8 buf(3),asn,pang,pi2
      real*8 temp,temp1,pbuf(80)
      real*4 ang(2)
      equivalence(p,kp),(asn,ksn),(pang,ang),(pbuf,kpbuf)
      equivalence(temp1,itemp1),(temp,itemp)
      pi2=6.283185307d0
      data is0 /3/, is1 /4/, is3 /0/, is4 /1/
      ifind=0
      istrt=0
      inx=1
      icnt=1
      icnt2=0
      npts = 0
 
 
C
C...Clear out ain so that no extra characters are left in there.
C...Load ain with the uppercase postprocessor command cmmnd.
C
      do 1,i=1,81
1        ain(i)=' '
      do 2,i=1,80
2        call convrt(cmmnd(i),ain(i),1)
 
C...Call parser to get the asn numbers for the command.
C...and put them in pbuf for insertion into the p array.
C
3     call parser
C
C...If ityp.is equal to 7 and istrt is equal to 0 then there are spaces
C...at the begining of the command and need to call parser until the
C...command begins.
C
      if(ityp.eq.7.and.istrt.eq.0) then
        inx=inx+1
        goto 3
      endif
      if (ityp.eq.1.and.icnt.ne.1) then
C
C...Minor Word
C
         itemp1(is1)=0
         itemp1(is4)=ist
         pbuf(icnt)=temp1
         icnt=icnt+1
         icnt2=icnt2+1
      else if (ityp.eq.1) then
C
C...Major Word
C
         itemp(1)=ist
         itemp(4)=2000
         pbuf(icnt)=temp
         icnt=icnt+1
         icnt2=icnt2+1
         istrt=1
      else if(ityp.eq.2.or.ityp.eq.3.or.ityp.eq.4) then
C
C...Number
C
         pbuf(icnt)=tv
         icnt=icnt+1
         icnt2=icnt2+1
      end if
C
C...if the next token is a / ignore it and continue, if it is the end
C...of the command, continue with the routine, otherwise, go back
C...and get the next token.
C
      if(nextyp.eq.5) inx=inx+1
      if(nextyp.ne.0.and.nextyp.ne.11) then
         goto 3
      else
         pbuf(icnt) = 0
      endif
C
C...Clear out P. Get the shape information into p.
C
      do 5, i=1,400
5        p(i)=0
      call gtshap(p, shpkey)
C
C...Save the key number of the shape.
C
      savncl=shpkey
      np=kp(3)
      buf(3)=0.
      tol=sc(27)*4.
 
C
C...Find stpt (on dummy lim1).
C
      do 15 i=8,np
      j=i
      k=4*i
15    if(kp(k).eq.5)goto 20
 
20    continue
C
C...Run thru p-list.
C
      i=j+2
30    i=i+1
      k=4*i
C
C...If line or circle. Increment ifind.
C
      if (i.ge.np) goto 35
      if (kp(k).eq.5) then
         ifind=ifind+1
         goto 40
      endif
      if (kp(k).eq.7) then
         ifind=ifind+1
         goto 50
      endif
C
C...Have found the correct segment and need to try and find
C...the command that needs to be deleted and then delete it.
C
      if (ifind.ne.pickid) goto 30
      intj=i
11    do 12, i=intj,400
         if(kp(i*4).eq.5.or.kp(i*4).eq.7) goto 125
C
C...Searching for a match for the first word of the command.
C
12       if(kp(i*4-3).eq.kpbuf(1)) goto 13
C
C...Command is not part of the segment, call error message.
C
125   error=1
      return
C
C...The first word matches, now check to see if the rest of the
C...command is a match. If no match is found go back to 11 and
C...search for the command.
C
13    do 14, j=2,icnt2
         if(kp(((i+(j-1))*4)-is3).ne.0) then
cc         if (p(i+(j-1)).gt..001) then
            if (p(i+(j-1)).ne.pbuf(j)) then
               intj=i+1
               goto 11
            endif
         else if(kp(((i+(j-1))*4)-is0).ne.kpbuf(j*4-is0)) then
            intj=i+1
            goto 11
         endif
14    continue
C
C...Found the command so now delete it.
C...If only one one, make sure icnt is equal
C...to one also.  Otherwise it will create
C...junk in the postprocessor word list.
C...JLS 9/27/99
C
cc      if (icnt2.eq.1) icnt = 1
      do 16, ij=i,397
16       p(ij)=p(ij+icnt)
C
C...All done.
C
35    goto 99
C
C...Line.
C
40    aflag=0
      i=i+2
      goto 30
C
C...Circle.
C
50    aflag=0
55    n=n+1
      if(n.lt.npts)goto 55
      i=i+6
      goto 30
 
99    continue
C
C...Reduce kp and the numwds to the new number of elements
C...in the p array.
C
      kp(3)=kp(3)-icnt
      numwds=kp(3)
C
C...Save the p array with the new information in it.
C
      call ptshap(p,shpkey,numwds)
      return
      end
