C*********************************************************************
C*    NAME         :  ushow.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       ushow.f , 15:10:51
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 25.1
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine ushow (stat)
c*       this routine parses all show commands    
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
      subroutine ushow (stat)

      include 'com8a.com'
      include 'comgt.com'
      include 'const.com'
      include 'cutter.com'
      include 'wrksys.com'

      character*6 tkn,atemp, atemp2
      character*14 atmp14
      character*50 fnam
      integer*2 stat, isc162(4), iflsv,nlin,tlin,mlin,i,iblnk
      integer*4 nup,nupt,nvp,nvpt,nci,maxc,strlen1,ic,ipg, iel
      real*8 rads,rnum,cutr(6),origin(3)
      real*4 asc(400),asc162(2)
      equivalence (sc,asc)
      equivalence (sc(162),asc162,isc162)

      equivalence (tkn,token2)
      character*1 cout1(80)
      equivalence (cout,cout1)
      character*40 cout2
      equivalence (cout1(41),cout2)
      character*25 cout2a
      equivalence (cout1(56),cout2a)
      character*50 cout3
      equivalence (cout1(30),cout3)
      character*20 lbuf
      integer*4 nclkey
c
      character*12 ltbuf(5)
c
      data ltbuf /'SQUARE','DIAMOND','TRIANGLE','CIRCLE','GROOVE'/
C

      rads = 57.29577951
C
      nlin   = 15
      mlin   = 9
      tlin   = 15

      if (tkn.eq.'DEBUG') then
        ifl(44) = 9
        call parsit
        call gtdesc (tv,nup,nwds,ietype)
        call parsit
        call gtdesc (tv,nvp,nwds,ietype)
        nupt = 1
        if (.not.NXTEOS) then
           call parsit
           if (.not.SCALAR) then
              call error (3)
              go to 99999 
           end if
           nupt = TV
        end if
        t    = sc(27)*5.0
        call ncl_2sfbnry (nup,nvp,sc(27),t,ifl(2))
        if (ifl(2) .eq. 0) call ncl_2sfbnry_disp (nupt) 
        call ncl_2sfbnry_free 
        goto 9999
      endif
C
C...open the scrolling window if it is not already open
C
      if (ifl(35) .eq. 0 .and. tkn .ne. 'STATLN') call opnwin()

C                                               *** source
          if (.not.((tkn.eq.'SRC'.and.length.eq.3).or.
     1        (tkn.eq.'SOURCE'.and.length.eq.6))) go to 199
              call ssrc(stat)
              go to 9999
199   continue
          if ((lstrng .and. (ityp.ne.2 .or. ist .ne. VANOTE)) .or.
     1        (ityp .eq. 2 .and. ist .eq. TEXTVAR)) then
            call shtext
            goto 9999
          endif
c
c...check if it is voc word (voc word could be saved as scalar when inside
c...a data statement)
c
      if ((ityp.eq.2).and.(ist.eq.2)) then
5         call vxchk (token2, ivxsub, nclkey, ipg, iel, nwds, ist)
          call sclvoc (nclkey, i)
          if (i.eq.1) then
             ityp = 1
             call rtoi(tv,itv)
             ist = itv
          else
             ist = 2
             ityp = 2  
          endif
      endif

C                                               *** geo
          if (.not.((ityp.eq.2.and.ist.ne.1.).or.ityp.eq.3.or.
     1              (ityp.eq.4.and.tkn.ne.'TAPEFT'))) go to 299
              call sgeo
              go to 9999

C                                               *** cutter
299       if (.not.(ityp.eq.1.and.ist.eq.716)) go to 399
              call obcutr (cutr,ic)
c
c......CUTTER/BLADE
c
              if (ic .eq. 3) then
                call putmsg('CUTTER:  BLADE',14,15,0)
                write (cout,1101) cutr(1)
1101            format ('           WIDTH:',F16.5)
                call putmsg(cout,34,16,0)
                write (cout,1102) cutr(2)
1102            format ('             TIP:',F16.5)
                call putmsg(cout,34,17,0)
                write (cout,1103) cutr(3)
1103            format ('          HEIGHT:',F16.5)
                call putmsg(cout,34,18,0)
                write (cout,1104) cutr(4)
1104            format ('           ANGLE:',F16.5)
                call putmsg(cout,34,19,0)
                iblnk=20
                goto 115
c
c......CUTTER/LATHE
c
              else if (ic .eq. 4) then
                write (cout,1210) ltbuf(lthbit)
1210            format ('CUTTER:  LATHE ',A)
                call putmsg(cout,34,15,0)
                write (cout,1211) cutr(1)
1211            format ('          RADIUS:',F16.5)
                call putmsg(cout,34,16,0)
                if (lthbit .eq. 5) then
                    write (cout,1213) cutr(2)
                else
                    write (cout,1212) cutr(2)
                endif
1212            format ('        DIAMETER:',F16.5)
1213            format ('           WIDTH:',F16.5)
                call putmsg(cout,34,17,0)
                write (cout,1214) cutr(3)
1214            format ('          HEIGHT:',F16.5)
                call putmsg(cout,34,18,0)
                write (cout,1215) cutr(4)
1215            format ('           ANGLE:',F16.5)
                call putmsg(cout,34,19,0)
                if (lthbit .eq. 5) then
                    write (cout,1217) cutr(5)
                else
                    write (cout,1216) cutr(5)
                endif
1216            format ('     MOUNT ANGLE:',F16.5)
1217            format ('          LENGTH:',F16.5)
                call putmsg(cout,34,20,0)
                iblnk=21
                goto 115
              endif
c
c......Standard CUTTER
c
              call putmsg('CUTTER:',7,15,0)
              write (cout,101) cutr(1)
              call putmsg(cout,34,16,0)
101           format ('        DIAMETER:',F16.5)
              write (cout,102) cutr(2)
              call putmsg(cout,34,17,0)
102           format ('   CORNER RADIUS:',F16.5)
              write (cout,103) cutr(3)
              call putmsg(cout,34,18,0)
103           format ('          HEIGHT:',F16.5)
              if (ifl(282).eq.1) then
                  write (cout,104) cutr(4)
                  call putmsg(cout,34,19,0)
104               format ('     SIDE RADIUS:',F16.5)
                  write (cout,1055) cutr(5)
                  call putmsg(cout,34,20,0)
1055              format ('        Z-HEIGHT:',F16.5)
                  write (cout,1066) cutr(6)
                  call putmsg(cout,34,21,0)
1066              format ('      FLAT ANGLE:',F16.5) 
                  iblnk=22
              else
                  write (cout,1066) cutr(4)
                  call putmsg(cout,34,19,0)
                  iblnk=20
              endif
C
C...Added check for NCL-VT mode
C...Paul  -  10/3/91
C...Old version was:
C   115           if (ifl(35).eq.0) call ersw3 (iblnk,1)
C
115           if (ifl(35) .eq. 0 .or. ifl(35) .eq. 2) 
     x        call ersw3 (iblnk,1)

              go to 9999
C                                               *** thick
399       if (.not.(ityp.eq.1.and.ist.eq.717)) go to 499
              call putmsg('THICK:',5,15,0)
              write (cout,105) sc(23)
              call putmsg(cout,24,16,0)
105           format ('   PART: ',F18.7)
              write (cout,106) sc(24)
              call putmsg(cout,24,17,0)
106           format ('   DRIVE:',F18.7)
              write (cout,107) sc(25)
              call putmsg(cout,24,18,0)
107           format ('   CHECK1:',F18.7)
              write (cout,108) sc(177)
              call putmsg(cout,24,19,0)
C
C...Added four more check surface formats since adding
C...the ablity to do multiple thick.
C
108           format ('   CHECK2:',F18.7)
              write (cout,109) sc(178)
              call putmsg(cout,24,20,0)
109           format ('   CHECK3:',F18.7)
              write (cout,110) sc(179)
              call putmsg(cout,24,21,0)
110           format ('   CHECK4:',F18.7)
              write (cout,111) sc(180)
              call putmsg(cout,24,22,0)
111           format ('   CHECK5:',F18.7)
              iblnk=19
              go to 115

C                                               *** tool
499       if (.not.(tkn.eq.'TOOL'.and.length.eq.4)) go to 599
              call putmsg('TOOL:',5,15,0)
              write(cout,300)sc(1),sc(2),sc(3)
300           format(6x,'TOOL END POINT',f14.7,1x,f14.7,1x,f14.7)
              call putmsg(cout,64,16,0)
              call putmsg(' ',1,17,0)
              write(cout,310)sc(4),sc(5),sc(6)
310           format(6x,'TLAXIS  VECTOR',f14.7,1x,f14.7,1x,f14.7)
              call putmsg(cout,64,18,0)
              call putmsg(' ',1,19,0)
              write(cout,320)sc(7),sc(8),sc(9)
320           format(6x,'FORWARD VECTOR',f14.7,1x,f14.7,1x,f14.7)
              call putmsg(cout,64,20,0)
              iblnk=21
              go to 115

C                                               ***   tlaxis
599       if (.not.(tkn.eq.'TA'.and.length.eq.2.or.
     1        tkn.eq.'TLAXIS'.and.length.eq.6)) go to 699
              cout='TLAXIS: same'
			  if (ifl(23).eq.0 .and. ifl(293).eq.1) then
				  cout = 'TLAXIS: same,normal'
              endif
              if (ifl(23).eq.1) then
                cout='TLAXIS: normal,ps'
              else if (ifl(23).eq.2) then
                cout='TLAXIS: atangl,ps'
              else if (ifl(23).eq.3) then
                cout='TLAXIS: tanto,ds'
              else if (ifl(23).eq.4) then
                cout='TLAXIS: tanto,ds,fan'
              else if (ifl(23).eq.5) then
                cout='TLAXIS: tanto,ds,perpto,ve'
              else if (ifl(23).eq.6) then
                cout='TLAXIS: tanto,ds,parelm'
              else if (ifl(23).eq.7) then
                cout='TLAXIS: normal,ps,perpto,ve'
              ELSE IF (IFL(23).EQ.8) THEN
                COUT='TLAXIS: COMBIN'
              ELSE IF (IFL(23).EQ.9) THEN
                COUT='TLAXIS: COMBIN,PARELM'
              ELSE IF (IFL(23).EQ.10) THEN
                COUT='TLAXIS: ATANGL,PS,PERPTO,VE'
              ELSE IF (IFL(23).EQ.11) THEN
                COUT='TLAXIS: ATANGL,PS,CLDIST'
              ELSE IF (IFL(23).EQ.12) THEN
                COUT='TLAXIS: ATANGL,PS,CLDIST,PERPTO,VE'
              endif
              call putmsg(cout,35,15,0)
              iblnk=16
              go to 115

C                                               ***  toler
699       if (.not.(tkn.eq.'TOLER'.and.length.eq.5)) go to 799
              write(cout,360)sc(27),sc(167),sc(168)
360           format('TOLER:',f14.8,',',f14.8,',',f14.8)
              call putmsg(cout,50,15,0)
              write(cout,361)sc(91),dacos(sc(92))*RADIAN
361           format('AUTOST TOLER:',f14.8,',',f14.8)
              call putmsg(cout,50,16,0)
              iblnk=17
              go to 115

C                                               ***  tapeft
799       if (.not.(tkn.eq.'TAPEFT' .and. length.eq.6)) go to 899
              write (cout,1010) sc(55)
1010          format ('Amount of tape generated since last break is: ',
     1                 f12.1)
              call putmsg(cout,53,15,0)
              iblnk=16
              go to 115

C                                               ***  files
899       if (.not.(tkn.eq.'FILES' .and. length.eq.5)) go to 999
C
C...Added check for NCL-VT mode
C...Paul  -  10/24/91
C
              if (ifl(35) .eq. 2) call ersw3(15,1)

              maxc = 49
              nci = strlen1(ppfnam)
              call shfile (ppfnam,nci,fnam,maxc)
              write (cout,1020) fnam
1020          format ('Part program name: ',a49)
              call putmsg (cout,68,15,0)
              if (ifl(69).eq.1) then
                  write (cout,1030)
1030              format ('CL file being created',24x)
              else if (ifl(69).eq.2) then
                  write (cout,1037)
1037            format ('CL file being created for apt source use only')
              else
                  write (cout,1040)
1040              format ('No cl file being created',21x)
              endif
              call putmsg (cout,45,16,0)
              if (ifl(88).eq.1) then
                  write (cout,1035)
1035              format ('APT source file being created   ')
              else
                  write (cout,1045)
1045              format ('No apt source file being created')
              endif
              call putmsg (cout,32,17,0)
              iblnk=18
              go to 115


999       if (.not.(ityp.eq.1.and.ist.eq.737)) go to 1099
C                                               ***  numpts
              write (cout,2010)ifl(368)
              if (ifl(91).ne.ifl(368)) then
                write (cout(15:),2011)ifl(91)
              endif
2010          format ('NUMPTS:',i5)
2011          format ('(ONCE:',i5,')')
              call putmsg(cout,30,15,0)
              iblnk=16
              go to 115
1099      if (.not.(ityp.eq.1.and.ist.eq.736)) go to 1199
C                                               ***  maxdp
              if (lstep) then
                  lbuf = 'STEP=ON '
              else
                  lbuf = 'STEP=OFF'
              endif
              write (cout,2020) sc(105),lbuf
2020          format (5x,'MAXDP STEP:',f15.7,4x,a)
              call putmsg(cout,52,15,0)
              cout = ' '
C                                      auto maxdp
              if (isc162(1).eq.0 .and. asc162(2).eq.0.0) then
                  write (cout,2925)
              else
                  write (cout,2926)
              endif
              call putmsg(cout,40,16,0)
              cout = ' '
C                                      max-loop
              write (cout,2929)isc162(1)
              call putmsg(cout,40,17,0)
              cout = ' '
C                                      mindp
              write (cout,2928)asc162(2)
              call putmsg (cout,80,18,0)
              cout = ' '
C                                      warning flag
              if (isc162(2).eq.0) then
                  write (cout,2927)
              else
                  write (cout,2924)
              endif
              if (sc(54).ne.sc(105)) then
                write(cout2,2025) sc(54)
              endif
2025          format('(ONCE: ',f12.6,')')
C
              call putmsg(cout,60,30,0)
              iblnk=16
              go to 115
1199      if (.not.(ityp.eq.1.and.ist.eq.741)) go to 1299
C                                               ***  maxang
              write (cout,2030) sc(201)
2030          format ('MAXANG:',f18.7)
              if (sc(80).ne.sc(201)) then
                write(cout2,2025) sc(80)
              endif
              call putmsg(cout,60,15,0)
              iblnk=16
              go to 115
1299      if (.not.(ityp.eq.1.and.ist.eq.841)) go to 1399
C                                               ***  units
              if (ifl(264).eq.0) then
                   write (cout,2040)'inches'
              else if (ifl(264).eq.1) then
                   write (cout,2040)'millimeters'
              endif
2040          format ('UNITS: ',a11)
              call putmsg(cout,30,15,0)
              iblnk=16
              go to 115
1399      if (.not.(ityp.eq.1.and.ist.eq.1009)) go to 1499
C                                               ***  feed rate
              if (rpfron) then
                  write (cout, 2044)
2044              format ('PRIMARY FEDRAT: RAPID')
                  call putmsg (cout,80,15,0)
              else
                  write (cout, 2045) sc(123)
2045              format ('PRIMARY FEDRAT:',f10.3)
C
C...                              FR/AT...
C
                  if (ifl(314).eq.0) then
                      cout3 = 'SECONDARY FEDRAT: none'
                  else 
                      if (ifl(314) .eq. 1) then
                         atemp = ' modal'
                      else if (ifl(314) .eq. 2) then
                         atemp = ' once '
                      endif
                      if (sclat) then
                          atemp2 = ' scale'
                      else
                          atemp2 = '      '
                      endif
                      write (cout3, 2046) sc(126),sc(125),atemp,atemp2
2046                  format ('SECONDARY FEDRAT:',f10.3,
     x                                     ' at ',f7.3,a6,a6)
                  endif
                  call putmsg (cout,80,15,0)
C
C...                              FR/OUT...
C
                  cout = ' '
                  if (ifl(315) .eq. 0) then      
                      cout3 = 'ACCEL FEDRAT:     none'
                  else
                      if (ifl(315) .eq. 1) then
                         atemp = ' modal'
                      else if (ifl(315) .eq. 2) then
                         atemp = ' once '
                      endif
                      if (sclout) then
                          rnum = FEEDC(5)
                          atemp2 = ' scale'
                      else
                          rnum = FEEDC(2)
                          atemp2 = '      '
                      endif
                      write (cout3, 2146) rnum,FEDIS(2),atemp,atemp2
2146                  format ('ACCEL FEDRAT:    ',f10.3,
     x                                     ' out',f7.3,a6,a6)
                  endif
                  call putmsg (cout,80,16,0)
              endif
              iblnk = 17
              go to 115
1499      if (.not.(tkn.eq.'ADISPL'.and.length.eq.6)) go to 1599
C                                               ***  adispl
              call gtsfdp (nup,nupt,nvp,nvpt)
              if (ifl(136) .gt. 0) then
                 write (cout,2047) ifl(136),nvpt,nvp,nupt,nup
              else
                 rnum = sc(175)
                 if (ifl(264) .eq. 1) rnum = rnum * 25.4
                 write (cout,2048) rnum,nvp,nup 
              end if
2047          format('ADISPL:  CV-pts ',I3,', SF U-pts ',i3,
     1            ', SF U-lns ',i3,', SF V-pts ',I3,', SF V-lns ',I3)
2048          format('ADISPL:  CV-toler ',f7.5, 
     1            ', SF U-lns ',i3,', SF V-lns ',I3)
              call putmsg (cout,80,15,0)
              iblnk=16
              go to 115
1599      if (.not.(ityp.eq.1.and.ist.eq.667)) go to 1699
C                                               ***  modals
C                                      toler
              write (cout,360)sc(27),sc(167),sc(168)
C                                      numpts
              if (ifl(91).ne.ifl(368)) then
                write (cout(52:),2010)ifl(368)
                write (cout(65:),2011)ifl(91)
              else
                write (cout2a,2010)ifl(91)
              endif
C             call putmsg (cout,80,15,0)
              call wypstr (cout,80,nlin,tlin,mlin)
              cout = ' '
C                                      autost toler
              write(cout,361)sc(91),dacos(sc(92))*RADIAN
              call wypstr (cout,80,nlin,tlin,mlin)
              cout = ' '
C                                      maxdp
              write (cout,2023)
C             call putmsg (cout,80,16,0)
              call wypstr (cout,80,nlin,tlin,mlin)
              cout = ' '
              if (lstep) then
                  lbuf = 'STEP=ON '
              else
                  lbuf = 'STEP=OFF'
              endif
              write (cout,2022)sc(105),lbuf
C                                      auto maxdp
              if (isc162(1).eq.0 .and. asc162(2).eq.0.0) then
                  write (cout2,2925)
              else
                  write (cout2,2926)
              endif
2023          format ('MAXDP:')
2022          format (5x,'STEP:',3x,f15.7,2x,a)
2926          format (5x,'AUTO MAXDP:',4x,'  AUTO')
2925          format (5x,'AUTO MAXDP:',4x,'  OFF')
C             call putmsg (cout,80,17,0)
              call wypstr (cout,80,nlin,tlin,mlin)
              cout = ' '
C                                      max-loop
              write (cout,2929)isc162(1)
2929          format (5x,'MAX-LOOPS:',i9)
C                                      mindp
              write (cout2,2928)asc162(2)
2928          format (5x,'MIN-DP:',f19.7)
C             call putmsg (cout,80,18,0)
              call wypstr (cout,80,nlin,tlin,mlin)
              cout = ' '
C                                      warning flag
              if (isc162(2).eq.0) then
                  write (cout,2927)
              else
                  write (cout,2924)
              endif
2927          format (5x,'WARNING:',7x,'  NOWARN')
2924          format (5x,'WARNING:',7x,'  WARN')
              if (sc(54).ne.sc(105)) then
                write(cout2,2025) sc(54)
              endif
C             call putmsg (cout,80,19,0)
              call wypstr (cout,80,nlin,tlin,mlin)
              cout = ' '
C                                      maxang
              write (cout,2030)sc(201)
              if (sc(80).ne.sc(201)) then
                write(cout(30:),2025) sc(80)
              endif
              if (lcntct) then
                cout(60:) = 'CONTCT/ON'
              else
                cout(60:) = 'CONTCT/OFF'
              endif
C             call putmsg (cout,80,20,0)
              call wypstr (cout,80,nlin,tlin,mlin)
              cout = ' '
C                                      tlaxis
              cout='TLAXIS: same'
              if (ifl(23).eq.0 .and. ifl(293).eq.1) then
                cout = 'TLAXIS: same,normal'
              endif
              if (ifl(23).eq.1) then
                cout='TLAXIS: normal,ps'
              else if (ifl(23).eq.2) then
                cout='TLAXIS: atangl,ps'
              else if (ifl(23).eq.3) then
                cout='TLAXIS: tanto,ds'
              else if (ifl(23).eq.4) then
                cout='TLAXIS: tanto,ds,fan'
              else if (ifl(23).eq.5) then
                cout='TLAXIS: tanto,ds,perpto,ve'
              else if (ifl(23).eq.6) then
                cout='TLAXIS: tanto,ds,parelm'
              else if (ifl(23).eq.7) then
                cout='TLAXIS: normal,ps,perpto,ve'
              ELSE IF (IFL(23).EQ.8) THEN
                COUT='TLAXIS: COMBIN'
              ELSE IF (IFL(23).EQ.9) THEN
                COUT='TLAXIS: COMBIN,PARELM'
              ELSE IF (IFL(23).EQ.10) THEN
                COUT='TLAXIS: ATANGL,PS,PERPTO,VE'
              ELSE IF (IFL(23).EQ.11) THEN
                COUT='TLAXIS: ATANGL,PS,CLDIST'
              ELSE IF (IFL(23).EQ.12) THEN
                COUT='TLAXIS: ATANGL,PS,CLDIST,PERPTO,VE'
              endif
C                                     tool condition
              if (ifl(21).eq.0) then
                  atmp14 = 'tlon'
              else if (ifl(21).eq.1) then
                  atemp = 'tlrgt'
              else
                  atmp14 = 'tllft'
              endif
              if (ifl(342) .eq. 0) then
                  atmp14(6:) = ' , tlofps'
              else
                  atmp14(6:) = ' , tlonps'
              endif
              write (cout2,2050)atmp14
2050          format ('TOOL CONDITION: ',a14)
C             call putmsg (cout,80,21,0)
              call wypstr (cout,80,nlin,tlin,mlin)
              cout = ' '
C                                      units
              if (ifl(264).eq.0) then
                  cout = 'UNITS: inches'
              else
                  cout = 'UNITS: millimeters'
              endif
C             call putmsg (cout,80,22,0)
              call wypstr (cout,80,nlin,tlin,mlin)
              cout = ' '
C                                      feed rate
              if (rpfron) then
                  write (cout, 2044)
              else
                  write (cout,2045)sc(123)
                  if (ifl(314) .eq. 0) then
                      cout3 = 'SECONDARY FEDRAT: none'
                  else 
                      if (ifl(314) .eq. 1) then
                         atemp = ' modal'
                      else if (ifl(314) .eq. 2) then
                         atemp = ' once '
                      endif
                      if (sclat) then
                          atemp2 = ' scale'
                      else
                          atemp2 = '      '
                      endif
                      write (cout3,2046) sc(126),sc(125),atemp,atemp2
                  endif
              endif
C             call putmsg(cout,80,23,0)
              call wypstr (cout,80,nlin,tlin,mlin)
              cout = ' '
              if (ifl(315) .eq. 0) then
                  cout3 = 'ACCEL FEDRAT:     none'
              else
                  if (ifl(315) .eq. 1) then
                      atemp = ' modal'
                  else if (ifl(315) .eq. 2) then
                      atemp = ' once '
                  endif
                  if (sclout) then
                      rnum = FEEDC(5)
                      atemp2 = ' scale'
                  else
                      rnum = FEEDC(2)
                      atemp2 = '      '
                  endif
                  write (cout3, 2146) rnum,FEDIS(2),atemp,atemp2
              endif
              call wypstr (cout,80,nlin,tlin,mlin)
C                                      thick
              write (cout,2060)sc(23),sc(24),sc(25)
2060          format ('THICK  PS:',F18.7,
     X                   '    DS:',F18.7,
     X                   '    CS:',F18.7)
C             call putmsg(cout,80,24,0)
              call wypstr (cout,80,nlin,tlin,mlin)

C                                      cutter
              call obcutr (cutr,ic)
              if (ic .eq. 1) then
                  write (cout,2070) (cutr(i),i=1,4)
              else if (ic .eq. 2) then
                  write (cout,2070) (cutr(i),i=1,6)
              else if (ic .eq. 3) then
                  write (cout,2071) (cutr(i),i=1,4)
              else if (ic .eq. 4) then
                  write (cout,2072) (cutr(i),i=1,5)
              endif
2070          format('CUTTER:',6f12.5)
2071          format('CUTTER/BLADE:',6f12.5)
2072          format('CUTTER/LATHE:',6f12.5)

C             call putmsg(cout,80,25,0)
              call wypstr (cout,80,nlin,tlin,mlin)
C                                      adispl
              call gtsfdp (nup,nupt,nvp,nvpt)
              if (ifl(136) .gt. 0) then
                 write (cout,2047) ifl(136),nvpt,nvp,nupt,nup
              else
                 rnum = sc(175)
                 if (ifl(264) .eq. 1) rnum = rnum * 25.4
                 write (cout,2048) rnum,nvp,nup 
              end if
C             call putmsg (cout,80,26,0)
              call wypstr (cout,80,nlin,tlin,mlin)
C...
C...Changed by Paul 11/06/91
C...Old version was : iblnk=23
C...vp 23-feb-94 wypstr returns next nlin  
C
              iblnk=nlin
              go to 115
C                                               ***  sc entries
1699      if (.not.(tkn.eq.'SC')) go to 1799
              call ssc
              go to 9999

C                                               ***  ifl entries
1799      if (.not.(tkn.eq.'IFL')) go to 1899
              call sifl
              go to 9999

C                                               ***  statln (status line)
1899      if (.not.(tkn.eq.'STATLN')) go to 1999
              iflsv = ifl(303)
C                 ifl(303)=2 forces STATLN.F to display the status line
              ifl(303) = 2
              call statln
              ifl(303) = iflsv
              go to 9999
1999      if (.not.(tkn.eq.'KEYS')) go to 2000
              call ranlst (nlin,tlin,mlin)
              go to 9999
2000      if (tkn.eq.'RS' .or. tkn.eq.'REFSYS') then
            if (ifl(72).eq.0) then
              call putmsg('REFSYS/OFF',10,15,0)
              go to 9999
            endif
            cout = 'REFSYS/ ' // rsname
            call putmsg(cout,80,16,0)

            write (cout,1080)
1080        format (15x,'x(1)',15x,'y(1)',15x,'z(1)',11x,
     1              'translation')
            call putmsg (cout,80,17,0)
            write(cout,1090) sc(56),sc(57),sc(58),sc(59)
1090        format ('x(2):',4(f18.7,1x))
            call putmsg(cout,80,18,0)
            write(cout,1100) sc(60),sc(61),sc(62),sc(63)
1100        format ('y(2):',4(f18.7,1x))
            call putmsg(cout,80,19,0)
            write(cout,1110) sc(64),sc(65),sc(66),sc(67)
1110        format ('z(2):',4(f18.7,1x))
            call putmsg(cout,80,20,0)
            go to 9999
          endif
          if (tkn.eq.'TC' .or. tkn.eq.'TRACUT') then
            if (ifl(73).eq.0) then
              call putmsg('TRACUT/OFF',10,15,0)
              go to 9999
            endif
            cout = 'TRACUT/ ' // tcname
            call putmsg(cout,80,16,0)
            write (cout,1080)
            call putmsg (cout,80,17,0)
            write(cout,1090) sc(41),sc(42),sc(43),sc(44)
            call putmsg(cout,80,18,0)
            write(cout,1100) sc(45),sc(46),sc(47),sc(48)
            call putmsg(cout,80,19,0)
            write(cout,1110) sc(49),sc(50),sc(51),sc(52)
            call putmsg(cout,80,20,0)
            go to 9999
          endif
          if (tkn.eq.'MODSYS') then
            if (.not.lwrk) then
              call putmsg('MODSYS/OFF',10,15,0)
              go to 9999
            endif
            cout = 'MODSYS/ ' // modlab
            call putmsg(cout,80,16,0)
            write (cout,1080)
            call putmsg (cout,80,17,0)
            write(cout,1090) wrkmx(1),wrkmx(2),wrkmx(3),wrkmx(4)
            call putmsg(cout,80,18,0)
            write(cout,1100) wrkmx(5),wrkmx(6),wrkmx(7),wrkmx(8)
            call putmsg(cout,80,19,0)
            write(cout,1110) wrkmx(9),wrkmx(10),wrkmx(11),wrkmx(12)
            call putmsg(cout,80,20,0)
            go to 9999
          endif
2999      continue
C                                               ***  vocab word
          if (ityp.eq.1) then
            cout = token2
            len = strlen1(token2)
            if (len.lt.14) then
                cout(15:) = 'vocabulary word'
            else
                cout(len+2:) = 'vocabulary word'
            endif
            call putmsg(cout,80,15,0)
            i = iabs(ist)
            write (cout,3010) i
3010        format (i6)
C
C...change the parameter because function asvoc
C...changed ( 0 mean it use minor word, if not use 1)
C...Yurong 7/18/97
C
C            call asvoc (i, cout(20:))
            call asvoc (i, 0, cout(20:))
            call putmsg(cout,80,15,0)
            go to 9999
          endif
c
c...Symbol
c
          call ub_symbol_name(token2,ivxsub,nclkey,origin,i)
          if (i .ne. 0) then
              call expnam4 (token2,ivxsub,cout,nc)
              if (i .eq. 1) then
                  cout(nc+5:) = 'symbol'
              else if (i .eq. 2) then
                  cout(nc+5:) = 'symbol instance'
                  call putmsg (cout,80,15,0)
                  cout = ' '
                  call putmsg (cout,80,15,0)
                  write (cout,3100) origin(1),origin(2),origin(3)
 3100             format ('     Origin: ',3(f18.7,1x))
              endif
              call putmsg (cout,80,15,0)
              go to 9999
          endif
c
c...Unrecognized command
c
          call error(25)
          go to 99999

9999      continue

99999 return
      end
C
C****************************************************************
      subroutine wypstr (buff,len,lin,toplin,mlin)
C
      include 'com8a.com'
c
      character*(*) buff
      character*80 locb1,locb2 
C
      integer*2 len,lin,toplin
      integer*4 knc, kmxc, klin, kcol, nc1, strlen1
C
      if (ifl(35) .ne. 1) then
          if (lin .gt. mlin+toplin) then
              locb1 = 'Hit any key to continue'
C
C...VX
C
              if (ifl(35) .eq. 2 .and. ifl(322) .eq. 0) then
                  call putmsg (locb1,23,2,2)
                  knc  = 0
                  kmxc = 2
                  kcol = 9
                  klin = 1
                  call gvtlin (locb2,knc,kmxc,klin,kcol)
              else
                  nc1 = strlen1(locb1)
                  call nclpmt (locb1,nc1,locb2,knc)
              end if
              call ersw3 (toplin,1) 
              lin  = toplin
          end if
      endif
      call putmsg(buff,len,lin,0)
      lin   = lin + 1
      return
      end
