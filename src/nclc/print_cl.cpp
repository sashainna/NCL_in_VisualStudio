/*-------------------------------------------------------*/
/* function : print_cl                                   */
/*-------------------------------------------------------*/
#include <iostream>
#include "nclx.h"
#include "nclxmdl.h"
#include "nclxmot.h"
#include <stdio.h>
#include <math.h>
#include <stdlib.h>
extern "C" void NclxMotClRead(NCLX_mot_clrec  *);

int print_cl(NCLX_mot_clrec * clrec)
{
   using namespace std;
   int inc(0),i(0);
   int index = 1;
	int j(0);
	double tc[12];
 	char * CLREC_ToolPathBeginning=NULL;
 	char * CLREC_ToolPathEnd=NULL;
	char buf[200],com[200];


   CLREC_ToolPathBeginning = clrec->start;
   CLREC_ToolPathEnd       = clrec->end  ;

   /*
.....Print clfile
*/
   NclxMotClRewind((*clrec));
	if (clrec->start != clrec->end)
   do
   {
		NclxMotClRead(clrec);
     
		if (clrec->type == 5000 || 
			clrec->type == 5200 || 
			clrec->type == 5210 ||
			clrec->type == 5220)
		{
			inc = 6;
			if (clrec->type == 5200) 
			{
				cout << "$$ 5200 ........" << endl;
				inc = 21;
			}
			else if (clrec->type == 5210)
			{
				cout << "$$ 5210 ........" << endl;
			} 
			else if (clrec->type == 5220)
			{
				cout << "$$ 5220 ........" << endl;
			} 
			else if (clrec->type == 5000)
			{
				cout << "$$ 5000 ........" << endl;
			}
                
			for (i = 0; i < clrec->mxcl; i += inc)
			{
				if (clrec->type == 5210 || clrec->type == 5220)
				{
					sprintf(com,"FROM/ "); 
				}
				else 
				{
					sprintf(com,"GOTO/ ");
				}
/*	  
				sprintf (buf,"%s%2.2f,%2.2f,%2.2f,   %2.2f,%2.2f,%2.2f",com,
						clrec->cldata[i],
						clrec->cldata[i+1],
						clrec->cldata[i+2],
						clrec->cldata[i+3],
						clrec->cldata[i+4],
						clrec->cldata[i+5]);
*/
				for (j = 0; j < 6; j++)
				{
					tc[j] = clrec->cldata[i+j];
					if (fabs(tc[j]) < 0.005) tc[j] = 0;
				}

				sprintf (buf,"%s%2.2f,%2.2f,%2.2f,   %2.2f,%2.2f,%2.2f",com,
						tc[0],tc[1],tc[2],tc[3],tc[4],tc[5]);
				cout << buf << endl;
				if (clrec->type == 5210 || clrec->type == 5220) break;
			}
		}
		else if (clrec->type == 2000)
		{
			if (clrec->subtype == 1009)
			{
				sprintf  (buf,"FEDRAT/%2.2f",clrec->cldata[0]);
				cout << buf << endl;
			}
			else if (clrec->subtype == 5)
			{
				sprintf  (buf,"RAPID");
				cout << buf << endl;
			}
		}
		else if (clrec->type == 3000)
		{
			printf("$$........\n");

			for (j = 5; j < 12; j++)
			{
				tc[j] = clrec->cldata[j];
				if (fabs(tc[j]) < 0.005) tc[j] = 0;
			}
			sprintf(buf,"$$ CIRCLE/%2.2f,%2.2f,%2.2f,%2.2f,%2.2f,%2.2f,%2.2f",
				tc[5],tc[6],tc[7],tc[8],tc[9],tc[10],tc[11]);
/*
				 clrec->cldata[5],
				 clrec->cldata[6],
				 clrec->cldata[7],
				 clrec->cldata[8],
				 clrec->cldata[9],
				 clrec->cldata[10],
				 clrec->cldata[11]);
*/
			cout << buf << endl;
		}
		else if (clrec->type == 6000)
		{
			sprintf(buf,"CUTTER/%2.2f,%2.2f,%2.2f",clrec->cldata[0],
				clrec->cldata[1],clrec->cldata[2]);
			cout << buf << endl;
		}
		else
		{
/*			printf  (" OTHER/\n");*/
		}
		index++;
	}
   while (clrec->current != 0);

   clrec->start = CLREC_ToolPathBeginning ;
   clrec->end   = CLREC_ToolPathEnd ;

   return 0;
}

