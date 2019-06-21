C*********************************************************************
C*    NAME         :  vstchk.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       vstchk.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:54
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine vstchk
c*       check the variable symbol table for   
c*       the existence of the symbol passed in 'token'.                
c*                                                                   
c*   input: variable name to be checked in common variable 'token'   
c*   output: ityp - type of variable                                 
c*           ist - subtype of variable                               
c*           tv - token value (contents of 'b' record for variable)  
c*           ifl(11) - ranfil record number of variable              
c*           ifl(12) - ranfil element number within the record       
c*                                                                   
c*   if the variable name is found, the location (ifl(11&12)) is of  
c*     the name in the ranfil.                                       
c*   if the variable name is not found, the location (ifl(11&12)) is 
c*     of the next available spot in the ranfil.  the value in 'tv'  
c*     has no meaning.                                               
C*    PARAMETERS   
C*       INPUT  : 
C*          none
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine vstchk

      include 'com8a.com'

      common/keycom/keyold, keyhld, istold
      integer*4 keyold, keyhld,ipg, iel, strlen1
      integer*2 istold

      integer*2 ksn(4)
      integer*2 nwds, ietyp
      integer*4 nclkey
C      logical found
      real*8 asn
      equivalence (asn,ksn)
      character*64 mtok
      integer*4 msub

c          set for unknown identifier and init cont safecount.
      ityp=2
      ipg = 0
      iel = 1
5     call vxchk (token2, ivxsub, nclkey, ipg, iel, nwds, ist)
      call ptdsc3 (nclkey, nwds, ist, asn)
      if (ist.eq.1) goto 70
      if (nclkey.eq.0) then
        ksn(1) = ipg
        ksn(2) = iel
      endif
C      goto 70

C10    ksn(4)=1
C      found=.false.
C
Cc         calculate record number from the 1st 4 characters of token
Cc           ipg=remf(iword/20)*2+1
Cc           recs 1-40 are initial storage area ranfil
C
C      ipg=iabs((iword+isub(4))-(iword+isub(4))/20*20)*2+1
C25    call getran (jb,ipg)
c          if no variable symbols entries on this record then exit
C      if (jb(141).gt.0) goto 35
Cc          no variable symbol names this record
C      iel=1
C      goto 70
Cc          check patwd (token) against existing vnames
C35    nvs=jb(141)
C      do 40 i=1,nvs
C          if(apatw.ne.vs(i))goto 40
c              match. get asn from assoc page.
C          found=.true.
C          iel=i
C          jpg=ipg+1
C          call getran(jb,jpg)
C          asn=vs(i)
C          goto 70
C40    continue
Cc          no match.  if record not full go update quick reference table
Cc          put iel at next open spot.
C      iel=jb(141)+1
C      if (nvs.lt.35) goto 70
c          record full, no match. if cont'd keep searching.
C      if (jb(143).eq.0) goto 50
C      ipg=jb(143)
C      goto 25
Cc          record full but not cont'd. get next record pair if count ok
Cc          add 2 records to vst and write-off this record
C50    jb(143)=ifl(4)+1
C      call putran (jb,ipg)
c          now initialize continuation record
C      ipg=jb(143)
C      jb(141)=0
C      jb(143)=0
C      call putran (jb,ipg)
Cc         also write next record to create it
C      jpg=ipg+1
C      call putran (jb,jpg)
C      ifl(4)=ifl(4)+2
C      iel=1

c          if subtype is macro parameter go get associated word of
c          the variable that is pointed to by the macro parameter
c          name's associated word
70    if (ksn(4).eq.12) then
          if (ifl(145).ne.1) then
c
c...Verify that this parameter
c...is part of the currently active Macro
c...6/26/91  -  Bobby
c......Added data statements and text strings to accepted macro
c......paramters - ASF 12/19/13.
c
              mtok = ' '
              call mclfnd (token2, ivxsub, ifnd, ityp, ist,
     x                     asn, mtok, msub, nclkey)
              if (ifnd.eq.0) goto 99
              if (ityp.eq.2 .and. ist.eq.2 .or.
     x             ityp.eq.3 .or. ityp.eq.4) then
                call rtoi(asn,itv)
                if (ityp.eq.3 .or. ityp.eq.4) goto 95
              endif
              if (ityp.ne.2. and. ityp .ne. 9) goto 95
              if (mtok(1:1) .eq. ' ') goto 95
c
c... If parameter is set to a scalar constant, a temporary scalar entity named @UN
c... was created in unibase but not in name list. We have its value now & don't need
c... to look it up.
c... 31 Oct 96 - IJD
c
              if (mtok(1:3) .eq. '@UN') goto 95
              token2 = mtok
              ivxsub = msub
              if (ityp .eq. 9) then
                  call rtoi(asn,length)
                  goto 95
              endif
              goto 5
          endif
      endif

c          put vst location in ifl(11,12)
      ifl(11)=ipg
      ifl(12)=iel

c          do return data per asn. last nibble sets ist.
cc80    ist=ksn(4)-ksn(4)/32*32
c
c...vp 24-may-94 don't get scalar value when working with unibase
c...since tv holds nclkey and should remain its value. 
c
90    if (ist .eq. 2 .and. ifl(299) .eq. 0) then
          call gtdesc(asn, nclkey, nwds, ietyp)
          call gtgeo(nclkey, asn)
          call rtoi(asn,itv)
          if (ldtflg) then
            ldtflg = .false.
          else
            call sclvoc (nclkey, i)
            if (i.eq.1) then
              ityp = 1
              ist = itv
            endif
          endif
      else if (ist.eq.16) then
c            it's a macro param that is a vocabulary word
          ityp=1
          ist=ksn(3)
      else if (ist.eq.17) then
c            it's a macro param that is unknown or subscripted
          ist=1
      endif
95    tv=asn
      keyhld = nclkey
      return
c
c...Undefined Macro parameter
c
99    ityp   = 2
      ist = 1
      keyhld = 0
      return
      end
