C*********************************************************************
C*    NAME         :  declsh.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL
C*       declsh.for , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:09:32
C*********************************************************************
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine declsh
C*     this routine handles shape declaration.  it parses a shape
C*     statement into an array of descriptors which point to the
C*     elements of the shape.  these elements may be:
C*
C*                   -    lines
C*                   -    circles
C*                   -    curves (including splines)
C*                   -    shapes
C*                   -    modifiers
C*                            xlarge
C*                            xsmall
C*                            ylarge
C*                            ysmall
C*                            clw
C*                            cclw
C*                   -    postprocessor commands
C*                            fedrat
C*                                ipr
C*                                ipm
C*                            coolnt
C*                                off
C*                                on
C*                                flood
C*
C*
C*     the first real*8 in the descriptor list is the shape descriptor.
C*     it is subdivided into 4 integer*2 elements:
C*
C*                      first  i*2 = in/out
C*                      second i*2 = right/left
C*                      third  i*2 = number of r*8 words in the list
C*                                        including the header
C*                      fourth i*2 = not used
C*
C*     the descriptor for a line, circle,curve, or shape is the associated
C*     word for that entity.  the descriptor for a modifier is:
C*
C*                     first  i*2 = vocab num of the modifier
C*                     second i*2 = not used
C*                     third  i*2 = not used
C*                     fourth i*2 = 19
C*
C*     the descriptor  for a post word is:
C*
C*                     first  i*2 = vocab num of the post word
C*                     second i*2 = not used
C*                     third  i*2 = number of total words in the string
C*                     fourth i*2 = 2000
C*
C*     the following words contain each item in the statement in r*8 form.
C*
C*
C*     the descriptor list is passed to geogn2 in the next available
C*     ranfil record.
C*
C**************************************************************************
C*    PARAMETERS
C*       INPUT  :
C*          none
C*       OUTPUT :
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C**************************************************************************
C
       subroutine declsh
 
       include 'com8a.com'
 
       real*8 buff(72),temp,temp1
       integer*2 ibuff(4),icount,itemp(4),itemp1(4),irest(4),i2buff(288)
       equivalence (buff,ibuff,i2buff),(temp,itemp),(rest,irest)
       equivalence (temp1,itemp1)
       logical bc
 
       integer*2 is1,is4
       integer*2 kf(1600)
       real*8 f(400)
       equivalence (f,kf)
c
       data is1 /4/, is4 /1/
 
 
       bc=.false.
       temp=0
       temp1=0
       if (ityp .eq. 2 .and. ist .eq. 18 .and. NXTEOS ) then
            isc10(1)=600
            isc10(2)=2
            goto 99999
       endif
c
       if (ityp.eq.1.and.(ist.eq.653.or.ist.eq.652)) then
c               it is IN or OUT
           ibuff(1)=ist
           call parsit
       else
c               default is OUT
           ibuff(1)=653
       endif
       if (ityp.eq.1.and.(ist.eq.8.or.ist.eq.24)) then
c               it is LEFT or RIGHT
           ibuff(2)=ist
           call parsit
       else
c               default is RIGHT
           ibuff(2)=24
       endif
c
       icount=1
       do 100 i=1,72
       icount=icount+1
       temp=0
       if (ityp.eq.2) then
           if (ist.eq.5.or.ist.eq.7.or.ist.eq.8.or.ist.eq.18) then
c                       it is a line, circle, curve, or shape
               if (icount.gt.100) then
                   call error(15)
                   goto 99999
               endif
c                  store the assoc word in the descriptor list
               buff(icount)=tv
           else
               call error(231)
               goto 99999
           endif
       else if (ityp.eq.1) then
c                 it is a vocab word-either a modifier or a post word
           if (ist.eq.638.or.ist.eq.639.or.ist.eq.641.or.ist.eq.642
     1         .or.ist.eq.59.or.ist.eq.60) then
c                    it is xs,xl,ys,yl,clw or cclw
               itemp(1)=ist
               itemp(4)=19
               buff(icount)=temp
           else if (ist.lt.500.or.ist.gt.999) then
c                     it is a post processor command
               if (nextyp.eq.5) then
c                   skip the '/'
                   inx=inx+1
c
c...If the next type is a ',' then
c...this is a standalone post-processor command
c
cc               else if (nextyp .eq. 9) then
cc                   ii = 1
cc                   itemp(1)=ist
cc                   itemp(4)=2000
cc                   goto 51
               else
                   isvinx=inx
                   call error(22)
                   goto 99999
               endif
               itemp(1)=ist
               itemp(4)=2000
               if (nextyp.ne.11) call parsit
               do 50 ii=1,71
                   if (ityp.eq.1) then
c                        it is a vocab word
                       if (ist.eq.638.or.ist.eq.639.or.ist.eq.641.or.
     1                       ist.eq.642.or.ist.eq.59.or.ist.eq.60.or.
     2                       nextyp .eq. 5) then
cc     2                       (ist.gt.999 .and. ii .ne. 1)) then
                           bc=.true.
                           go to 51
                       endif
                       itemp1(is1)=0
                       itemp1(is4)=ist
                       buff(icount+ii)=temp1
                   else if (ityp.eq.2.and.ist.eq.2.or.ityp.eq.3
     1                  .or.ityp.eq.4) then
                       buff(icount+ii)=tv
                   else if ((nextyp.eq.11.and.ifl(111).eq.0).or.
     1                  (ifl(111).ne.0.and.nextyp.eq.7)) then
                       bc=.true.
                       goto 51
                   else
                       if (ityp .ne. 5 .or. ist .ne. 9) bc=.true.
                       goto 51
                   endif
                   call parsit
50             continue
51             itemp(3)=ii-1
               buff(icount)=temp
               icount=icount+itemp(3)
           endif
       else if (ityp.eq.7) then
           icount=icount-1
           goto 200
       else
           call error(234)
           goto 99999
       endif
       if (bc) then
           bc=.false.
       else
            if ((ifl(111).eq.0.and.nextyp.eq.11).or.
     1          (ifl(111).ne.0.and.nextyp.eq.7)) go to 200
           call parsit
       endif
100    continue
200    call putran(i2buff,ifl(4)+1)
       if (icount.gt.35) call putran(i2buff(141),ifl(4)+2)
       isc10(1)=600
       isc10(2)=1
       isc10(3)=icount
 
       if (debug) then
           do 201 i=1,icount
               write (cout,202)i,buff(i)
202            format ('declsh: buff(',i2,')=',f12.4)
               call putmsg(cout,80,17,0)
201        continue
       endif
99999  continue
       return
       end
 
 
