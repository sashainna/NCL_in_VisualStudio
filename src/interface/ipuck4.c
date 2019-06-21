#include     "usysdef.h"
#include     "dmark.h"
#include     "bsym.h"

	static int tbindex = 4;


static char *tbstr[]= {
"",
"!tog",
"\\\\2",
"\\\\3",
""
};

static int  tbinfor[][2]= {
{1,0}, {1,1}, {1,2}, {1,3}, };


static tbcases (op)
int  op;

{
	switch(op)
    {
   }
};




ipuck4 (num, index, xflag)
int  num, xflag;
char **index;

{
int  i1;
char  us[100];
if (num <= tbindex)
 switch (tbinfor[num-1][0])
       {
    case  1 :
      *index = tbstr[tbinfor[num-1][1]];
      return (1);

    case  0 :
      if (xflag)  {
       		UD_MARK(i1,UU_FALSE);
      		if (i1==0) 	{
       	ud_lpsh(UU_FALSE);
       	tbcases(tbinfor[num-1][1]);	}
       	ud_lpop();  
       	UD_UNMARK(i1);	}
      return (0);

    default :  return(2);
       }
else
   return(2);
}
