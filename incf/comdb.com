c**********************************************************************
c**
c**    NAME         :  comdb.com 
c**
c**    MODULE NAME AND RELEASE LEVEL 
c**       comdb.com , 25.1
c**    DATE AND TIME OF LAST MODIFICATION
c**       04/29/15 , 15:07:30
c**
c*********************************************************************
       integer*2 wdbnpg,wdbgpg,wdbel,wdbio,cdbio
       integer*2 dbfn1,dbfn2,dbfn3
       equivalence (wdbnpg,ifl(59)),(wdbgpg,ifl(60))
       equivalence (wdbel,ifl(61)),(wdbio,ifl(62))
       equivalence (cdbio,ifl(63)),(dbfn1,ifl(64))
       equivalence (dbfn2,ifl(65)),(dbfn3,ifl(66))
