/*********************************************************************
**  NAME:  PtdFunc.c  
**  Description:
**          Function for Pted called from C++ and fortran.
**    CONTAINS:
**         static void Ptd_list_copy(from, to, n)
**         static void Ptd_list_adjust(list, count)
**         void Ptd_list_init(list, item_size, init_cnt, exp_cnt)
**         void Ptd_list_free(list)
**         void Ptd_list_push(list, item)
**         void Ptd_list_push_multiple(list, count, items)
**         void Ptd_list_compact(list)
**         void ptd_addapt(char *aptstr, int *nc)
**         void Ptd_cllist_init()
**         void ptd_cllist_push_multiple (int nc, char *cstr)
**         void ptd_cllist_finish (char **cstr)
**         void Ptd_setreg(char *setstring)
**         void Ptd_Appinit ()
**         int Ptd_Edit_input()
**         int Ptd_Edit_output()
**         int Ptd_strcat (**in, *out)
**         int Ptd_LoadInput(fil,msg1,msg2)
**         int Ptd_LoadOuput(fil,msg1,msg2)
**         int Ptd_LoadMDF(fil,msg1,msg2)
**         int Ptd_GetAdds(in, out, bcase, fval)
**         int Ptd_GetFindStr(in, out, bcase, type)
**         int Ptd_Convert(char*inputdata, char**outputdata)
**         int Ptd_Format(char*inputdata, char**outputdata)
**         int Ptd_Unformat(char*inputdata, char**outputdata)
**         int Ptd_BadCL(char*out, int sln, int eln, int *bufpt)
**         int Ptd_BadAPT(char*out, int sln, int eln, int *bufpt)
**         int Ptd_BadCTL(char*inputdata, char**outputdata)
**         int Ptd_Resequence(char* cin, int bseq, int seqinc,
**         int Ptd_Reverse(char* cin, int len, char**cout)
**         int Ptd_GetLine(fdata, line_num, line_str)
**         int Ptd_NCtoAPT(char*inputdata, char**outputdata)
**         int Ptd_initreglist()
**         int Ptd_freereglist()
**         int ptd_updreglist()
**         int ptd_schreglist()
**         int add_badstr(char *badstr, int *nc)
**         int Ptd_set_tllen(tlno, tllen, tnum, ton)
**         int Ptd_set_units(iun)
**         Ptd_Add(char *cin, char **adds, int num, 
**         Ptd_Mirror(char *cin, char **adds, int num, 
**         Ptd_Multiply(char *cin, char **adds, int num, 
**         Ptd_Rotate(char *cin, char **adds, int num, 
**         Ptd_Scale(char *cin, char **adds, int num, 
**         Ptd_Trans(char *cin, char **adds, int num, 
**         Ptd_BadCommand(char*inputdata, char**outdata, int fsave)
**
**    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       PtdcFunc.c , 24.1
**    DATE AND TIME OF LAST  MODIFICATION
**       09/11/13 , 12:59:19
*********************************************************************/
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <time.h>
#include "pwenv.h"
#include "PtdFunc.h"

/*
.....These declarations fix undefined symbols on Unix
.....but there must be a better way.
*/
int f77argc;
int f77argv[256];

typedef struct
{
	int item_size;          /* storage required by each item in bytes */
	int cur_cnt;            /* current item count */
	int max_cnt;            /* maximum item count */
	int exp_cnt;            /* expansion item count */
	char *data;             /* pointer to data array */
} Ptd_list;

static Ptd_list PTD_BAD_STR;

static Ptd_list Ptd_aptlist;
static Ptd_list Ptd_cllist;
static Ptd_list Ptd_flist;
static char PTD_badstr[512];

/* #define PTD_LIST_LENGTH(list) ((*list).cur_cnt) */
#define PTD_LIST_ARRAY(list) ((*list).data)
#define PTD_LIST_INDEX(list,index) (&((list)->data[(index)*((list)->item_size)]))
#define PTD_END_OF_LIST(list) PTD_LIST_INDEX((list),((list)->cur_cnt))

typedef struct
{
	double inval;
	double outval;
	int    outreg;
	int    dummy;
} Ptd_regstruct;

static Ptd_list Ptd_reglist[22];

#ifndef WNT
#ifndef VAXVMS
#ifndef HP
#ifndef IBM
#define add_badstr      add_badstr_
#define ptd_gtregv      ptd_gtregv_
#define ptd_badcla      ptd_badcla_
#define ptd_badapt      ptd_badapt_
#define ptdf_badctl		ptdf_badctl_ 
#define ptd_init        ptd_init_
#define ptd_edit_mdf    ptd_edit_mdf_
#define ptd_fmticod     ptd_fmticod_
#define ptd_defmtcod    ptd_defmtcod_
#define ptd_load_mdf    ptd_load_mdf_
#define ptd_save_mdf      ptd_save_mdf_
#define ptd_inendchar   ptd_inendchar_
#define ptd_outendchar  ptd_outendchar_
#define ptd_outpack     ptd_outpack_
#define ptd_inpack      ptd_inpack_
#define ptd_ctocd       ptd_ctocd_
#define ptd_tapblk      ptd_tapblk_
#define ptd_cparse      ptd_cparse_
#define ptd_getseq      ptd_getseq_
#define ptd_rmopt       ptd_rmopt_
#define ptdf_stoval     ptdf_stoval_
#define set_tllen       set_tllen_
#define set_units       set_units_
#define ptd_initregtab  ptd_initregtab_
#define ptd_updreglist  ptd_updreglist_
#define ptd_schreglist  ptd_schreglist_
#define ptd_aptsrc      ptd_aptsrc_
#define ptdf_loddef     ptdf_loddef_
#define ptd_addapt      ptd_addapt_
#define ptdf_ledout     ptdf_ledout_
#define ptd_simhdr      ptd_simhdr_
#define ptd_nctosim     ptd_nctosim_
#define ptd_tolinit     ptd_tolinit_
#define ptd_toldo       ptd_toldo_
#define ptd_savmch		ptd_savmch_
#define pted_version		pted_version_
#define pw_getver_info	pw_getver_info_
#define datcnvc		datcnvc_
#define pwdaut			pwdaut_

#endif
#endif
#endif
#endif

#ifdef WNT
static char *Ptd_eol  = "\r\n";
static int   Ptd_leol = 2;
#else
static char *Ptd_eol  = "\n";
static int   Ptd_leol = 1;
#endif

#ifndef WNT
#ifdef TIME_CODE
/*   ------   TIMER CODE   ------   */
#include <sys/times.h>
#include <limits.h>
static int stim[10],etim[10] = {0,0,0,0,0,0,0,0,0,0};
void gtimstx_(int *ix)
{
	struct tms buffer;
	stim[*ix] = times(&buffer) * CLK_TCK;
}
void gtimenx_(int *ix)
{
	struct tms buffer;
	etim[*ix] += times(&buffer) * CLK_TCK - stim[*ix];
}
void gtcnt_(int *ix)
{
	etim[*ix] += 1;
}
/*   ------   TIMER CODE   ------   */
#endif
#endif
/*********************************************************************
**    E_FUNCTION     : static void Ptd_list_copy(from, to, n)
**       Copy data down in-line
**    PARAMETERS
**       INPUT  :
**          from           pointer to copy from
**          n              number of bytes to copy.
**       OUTPUT :
**          to             pointer to copy to
**    RETURNS      : nothing
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

static void Ptd_list_copy_down(from, to, n)
char *from;
char *to;
int  n;
{
	while(n--)
		*to++ = *from++ ;
}

/*********************************************************************
**    E_FUNCTION     : static void Ptd_list_adjust(list, count)
**       Increase list size.
**    PARAMETERS
**       INPUT  :
**          list           pointer to list to adjust.
**          count          number of bytes to increase list by.
**       OUTPUT :
**          none
**    RETURNS      : nothing
**    SIDE EFFECTS : List storage is increased.
**    WARNINGS     : none
*********************************************************************/

static void Ptd_list_adjust(list, count)
Ptd_list *list;
int      count;
{
	char *new_data = NULL;

/*
...compute new max count
*/
	list->max_cnt = list->cur_cnt + count + list->exp_cnt;

/*
...allocate new space
*/
	new_data = (char *)malloc(list->item_size * list->max_cnt);

/*
...move the data and free the old space
*/
	if (list->data && new_data)
	{
		Ptd_list_copy_down(list->data, new_data, list->item_size * list->cur_cnt);
		free(list->data);
	}

/*
...update the data pointer
*/
	list->data = new_data;

}

/*********************************************************************
**    E_FUNCTION     : void Ptd_list_init(list, item_size, init_cnt, exp_cnt)
**       Initialize a list
**    PARAMETERS
**       INPUT  :
**          list           pointer to Ptd_list structure
**          item_size      storage required for each item in bytes
**          init_cnt       number of items initially allocated
**          exp_cnt        number of items to expand by on overflow
**       OUTPUT :
**          list           filled in
**    RETURNS      : nothing
**    SIDE EFFECTS : storage is allocated
**    WARNINGS     : storage already held by 'list' will not be deallocated
*********************************************************************/

void Ptd_list_init(list, item_size, init_cnt, exp_cnt)
Ptd_list       *list;
int            item_size;
int            init_cnt;
int            exp_cnt;
{

/*
...fill in list struct
*/
   list->item_size = item_size;
   list->cur_cnt = 0;
   list->max_cnt = init_cnt;
   list->exp_cnt = exp_cnt;

/*
...if space to allocate, allocate space
*/
   if(list->max_cnt)
      list->data = (char *)malloc(item_size * list->max_cnt);
   else
      list->data = 0;
}
/*********************************************************************
**    E_FUNCTION     : void Ptd_list_free(list)
**       Free (dellocate) a list
**    PARAMETERS
**       INPUT  :
**          list           pointer to Ptd_list structure
**       OUTPUT :
**          list           data pointer is NULLED and current count is zeroed
**    RETURNS      : nothing
**    SIDE EFFECTS : list storage is free'd
**    WARNINGS     : No check for a valid list is made
*********************************************************************/

void Ptd_list_free(list)
Ptd_list *list;
{
/*
...free the storage if any.
*/
   if(list->data)
      free(list->data);
/*
...clear counts
*/
   list->data = 0;
   list->cur_cnt = 0;
}
/*********************************************************************
**    E_FUNCTION     : void Ptd_list_push(list, item)
**       Push (append) an item onto a list
**    PARAMETERS   
**       INPUT  : 
**          list           pointer to Ptd_list structure
**          item           pointer to item
**       OUTPUT :
**          list           list is modified
**    RETURNS      : nothing
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void Ptd_list_push(list, item)
Ptd_list *list;
char     *item;
{

/*
...Increase the list storage if necessary.
*/
	if(list->cur_cnt == list->max_cnt)
		Ptd_list_adjust(list, 1);
/*
...Copy new data to end of list.
*/
	Ptd_list_copy_down(item, PTD_END_OF_LIST( list), list->item_size );

	list->cur_cnt++ ;

}

/*********************************************************************
**    E_FUNCTION     : void Ptd_list_push_multiple(list, count, items)
**       Push (append) an array of items onto a list
**    PARAMETERS   
**       INPUT  : 
**          list            pointer to Ptd_LIST structure
**          count           number of items to push
**          items           item array
**       OUTPUT :  
**          list            list is modified
**    RETURNS      : nothing
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void Ptd_list_push_multiple(list, count, items)
Ptd_list *list;
int      count;
char     *items;
{
	int left = list->max_cnt - list->cur_cnt;

/*
...Increase the list storage if necessary.
*/
	if(count > left)
		Ptd_list_adjust(list, count);
/*
...Copy new data to end of list.
*/
	Ptd_list_copy_down(items, PTD_END_OF_LIST( list), count * list->item_size );

	list->cur_cnt += count;

}

/*********************************************************************
**    E_FUNCTION     : void Ptd_list_compact(list)
**       Compact a list (allocate exactly the amount of memory required).
**    PARAMETERS
**       INPUT  :
**          list           pointer to list to adjust.
**       OUTPUT :
**          list           adjusted list.
**    RETURNS      : nothing
**    SIDE EFFECTS : List storage may be decreased.
**    WARNINGS     : none
*********************************************************************/

void Ptd_list_compact(list)
Ptd_list *list;
{
	char  *new_data = NULL;
	int   len;

/*
...compute new max count
*/
	len = list->cur_cnt*list->item_size;
	if (list->max_cnt > len && list->data)
	{
/*
...allocate new space
*/
		if (len > 0) new_data = (char *)malloc(len);
/*
...move the data
*/
		if (new_data)
		{
			Ptd_list_copy_down(list->data, new_data, len);
/*
...free the old space
*/
			free(list->data);
/*
...update the data pointer
*/
			list->data = new_data;
			list->max_cnt = len;
		}
	}
}
/*********************************************************************
**    E_FUNCTION     : void ptd_addapt(char *aptstr, int *nc)
**       Add a string to the aptsrc list.
**    PARAMETERS
**       INPUT  :
**          aptstr         pointer to apt string.
**          nc             length of aptstr.
**       OUTPUT :
**          none
**    RETURNS      : nothing
**    SIDE EFFECTS : Sring is added to apt source list.
**    WARNINGS     : None.
*********************************************************************/
void ptd_addapt(aptstr, nc)
char *aptstr;
int *nc;
{
	int knc = *nc;
	Ptd_list_push_multiple (&Ptd_aptlist, knc, aptstr);
	Ptd_list_push_multiple (&Ptd_aptlist, Ptd_leol, Ptd_eol);
}
/*********************************************************************
**    E_FUNCTION     : void Ptd_cllist_init()
**       Initialize the clfile list.
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      : nothing
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void Ptd_cllist_init()
{
	Ptd_list_init(&Ptd_cllist, sizeof(char), 10000,10000);
}
/*********************************************************************
**    E_FUNCTION     : void ptd_cllist_push_multiple (int nc, char *cstr)
**       Add a string to the cl list.
**    PARAMETERS
**       INPUT  :
**          nc             length of string.
**          cstr           pointer to string.
**       OUTPUT :
**          none
**    RETURNS      : nothing
**    SIDE EFFECTS : Sring added to cl list.
**    WARNINGS     : none
*********************************************************************/
void Ptd_cllist_push_multiple(nc, cstr)
int nc;
char *cstr;
{
	Ptd_list_push_multiple (&Ptd_cllist, nc, cstr);
}
/*********************************************************************
**    E_FUNCTION     : void ptd_cllist_push_eol()
**       Add end of line character(s) to the cl list.
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      : nothing
**    SIDE EFFECTS : Sring added to cl list.
**    WARNINGS     : none
*********************************************************************/
void Ptd_cllist_push_eol()
{
	Ptd_list_push_multiple (&Ptd_cllist, Ptd_leol, Ptd_eol);
}
/*********************************************************************
**    E_FUNCTION     : void ptd_cllist_finish (char **cstr)
**       Finish the cl list and return a pointer to the data.
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          cstr           pointer to cl data.
**    RETURNS      : nothing
**    SIDE EFFECTS : none.
**    WARNINGS     : none
*********************************************************************/
void Ptd_cllist_finish(cstr)
char **cstr;
{
	Ptd_list_push(&Ptd_cllist,"\0");
	Ptd_list_compact(&Ptd_cllist);
	*cstr = NULL;
	*cstr = PTD_LIST_ARRAY(&Ptd_cllist);
}
/*********************************************************************
**    E_FUNCTION     : void Ptd_flist_init()
**       Initialize the find list.
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      : nothing
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void Ptd_flist_init(len1,len2)
int len1,len2;
{
	Ptd_list_init(&Ptd_flist, sizeof(char), len1, len2);
}
/*********************************************************************
**    E_FUNCTION     : void ptd_flist_push_multiple (int nc, char *cstr)
**       Add a string to the find list.
**    PARAMETERS
**       INPUT  :
**          nc             length of string.
**          cstr           pointer to string.
**       OUTPUT :
**          none
**    RETURNS      : nothing
**    SIDE EFFECTS : Sring added to find list.
**    WARNINGS     : none
*********************************************************************/
void Ptd_flist_push_multiple(nc, cstr)
int nc;
char *cstr;
{
	Ptd_list_push_multiple (&Ptd_flist, nc, cstr);
}
/*********************************************************************
**    E_FUNCTION     : void ptd_flist_push_eol()
**       Add end of line character(s) to the find list.
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      : nothing
**    SIDE EFFECTS : Sring added to find list.
**    WARNINGS     : none
*********************************************************************/
void Ptd_flist_push_eol()
{
	Ptd_list_push_multiple (&Ptd_flist, Ptd_leol, Ptd_eol);
}
/*********************************************************************
**    E_FUNCTION     : void ptd_flist_finish (char **cstr)
**       Finish the find list and return a pointer to the data.
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          cstr           pointer to find data.
**    RETURNS      : nothing
**    SIDE EFFECTS : none.
**    WARNINGS     : none
*********************************************************************/
void Ptd_flist_finish(cstr)
char **cstr;
{
	Ptd_list_push(&Ptd_flist,"\0");
	Ptd_list_compact(&Ptd_flist);
	*cstr = PTD_LIST_ARRAY(&Ptd_flist);
}
/*********************************************************************
**    E_FUNCTION     : void Ptd_setreg(char *setstring)
**       Store a list of registers.
**    PARAMETERS
**       INPUT  :
**          setstr   - Text list of registers separated by tabs.
**       OUTPUT :
**          none
**    RETURNS      : nothing
**    SIDE EFFECTS : none.
**    WARNINGS     : none.
*********************************************************************/
int Ptd_setreg(char* setstring)
{
	char *tok;
	int i, nc, kreg,kerr;
	double gval;
	double valsto[66];
	int regsto[66];

	i = 0;
	tok = strtok(setstring, " \t,");
	while(tok!=NULL)
	{
		nc = strlen(tok);
		ptd_ctocd (tok,&nc,&kreg,&gval,&kerr);
		if (kerr != 0) return(kerr);
		regsto[i] = kreg;
		valsto[i] = gval;
		tok = strtok(NULL, " \t,");
		i++;
	}
	ptdf_stoval(regsto, valsto, &i);
	return (0);
}
/*********************************************************************
**    E_FUNCTION     : void Ptd_Appinit ()
**       Initialize pted.
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      : nothing
**    SIDE EFFECTS : none.
**    WARNINGS     : none.
*********************************************************************/
int Ptd_Appinit()
{
	ptd_init();
	Ptd_initreglist();
	return(0);
}

/*********************************************************************
**    E_FUNCTION     : int Ptd_Edit_input()
**       Edit the input MDF file.
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      : non-zero for error
**    SIDE EFFECTS : none.
**    WARNINGS     : none.
*********************************************************************/
Ptd_Edit_input()
{
	int err,ifl;
	ifl = 1;
	ptd_edit_mdf(&ifl,&err);
	Ptd_initreglist();
	return err;
}
/*********************************************************************
**    E_FUNCTION     : int Ptd_Edit_output()
**       Edit the output MDF file.
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      : non-zero for error
**    SIDE EFFECTS : none.
**    WARNINGS     : none.
*********************************************************************/
Ptd_Edit_output()
{
	int err,ifl;
	ifl = 2;
	ptd_edit_mdf(&ifl,&err);
	Ptd_initreglist();
	return err;
}

/*********************************************************************
**    E_FUNCTION     : Ptd_Get_NxtBk(char*inputdata, char *bkstr, int start, int *next)
**       Return next block from the input text string
**    PARAMETERS
**       INPUT  :
**          inputdata      - Pointer to current string.
**			start:			- start point of the inputdata to get teh next block
**       OUTPUT :
**          bkstr     - next block text string.
**          next      - next point of the string to pass.
**    RETURNS      : Zero if valid block found else -1.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int Ptd_Get_NxtBk(char*inputdata, char *bkstr, int start, int *next)
{
	int pack, indx, i, j, len;
	char endbk[5];
	char *temp, line_char[500], tempstr[500];
	ptd_inpack(&pack);
	ptd_inendchar(endbk);
	if (pack==2)
/*
.....unpacked
*/
	{
		temp = strchr(&(inputdata[start]), '\n');
		if (temp==NULL)
		{
			temp = strchr(&(inputdata[start]), '\0');
		}
		indx = temp - &(inputdata[start]) + 1;
		
		if (indx>0)
		{
			*next = start + indx;
/*
.....maxinum 256 chars per line
*/
			if (indx>256) indx = 256;
			strncpy(line_char, &(inputdata[start]), indx);

			indx--;
			while (indx>-1 && (line_char[indx]=='\r' || line_char[indx]=='\n'))
				indx--;
			line_char[++indx] = '\0';
		}
		else
		{
			strcpy(bkstr, "\n");
			*next = start + 1;
			return -1;
		}
/*
......then search for end block characters
*/
/*
		if (strlen(endbk)!=0)
		{
			temp = strstr(line_char, endbk);
			if (temp==NULL)
				indx = 0;
			else
				indx = temp - line_char;
		}
		if (indx != 0)
		{
			strncpy(bkstr, line_char, indx);
			bkstr[indx] = '\0';
		}
		else
*/
		{
			strcpy(bkstr, line_char);
		}
	}
	if (pack==1)
	{
/*
......If it is packed punch format
......then search for end block characters
*/
		if (strlen(endbk)!=0)
		{
			temp = strstr(&(inputdata[start]), endbk);
			indx = temp - &(inputdata[start]);
			*next = start + indx;
			if (indx>256) indx = 256;
			strncpy(tempstr, &(inputdata[start]), indx);
			tempstr[indx] = '\0';
/*
.....remove new line character if have any
*/
			j = 0;
			len = strlen(tempstr);
			for (i=0; i<len; i++)
			{
				if (tempstr[i]!='\n')
					bkstr[j++] = tempstr[i];
			}
			bkstr[j] = '\0';
		}
		else
		{
			bkstr[0] = '\0';
			return -1;
		}
	}
	return 0;
}
/*********************************************************************
**    E_FUNCTION     : int Ptd_LoadInput(fil,msg1,msg2)
**       Load the input machine description.
**    PARAMETERS
**       INPUT  :
**          fil        - Filename to load
**       OUTPUT :
**          msg1       - Error message
**          msg2       - Error message
**    RETURNS      : Non-zero on error.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int Ptd_LoadInput(fil,msg1,msg2)
char *fil,*msg1,*msg2;
{
	int err,ifl;
	Ptd_freereglist();
	ifl = 1;
	ptd_load_mdf(fil,&ifl,msg1,msg2,&err);
	Ptd_initreglist();
	return(err);
}
/*********************************************************************
**    E_FUNCTION     : int Ptd_LoadOuput(fil,msg1,msg2)
**       Load the ouput machine description.
**    PARAMETERS
**       INPUT  :
**          fil        - Filename to load
**       OUTPUT :
**          msg1       - Error message
**          msg2       - Error message
**    RETURNS      : Non-zero on error.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int Ptd_LoadOutput(fil,msg1,msg2)
char *fil,*msg1,*msg2;
{
	int err,ifl;
	Ptd_freereglist();
	ifl = 2;
	ptd_load_mdf(fil,&ifl,msg1,msg2,&err);
	Ptd_initreglist();
	return(err);
}
/*********************************************************************
**    E_FUNCTION     : int Ptd_LoadMDF(fil,msg1,msg2)
**       Load the standard machine description.
**    PARAMETERS
**       INPUT  :
**          fil        - Filename to load
**       OUTPUT :
**          msg1       - Error message
**          msg2       - Error message
**    RETURNS      : Non-zero on error.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int Ptd_LoadMDF(fil,msg1,msg2)
char *fil,*msg1,*msg2;
{
	int err,ifl;
	Ptd_freereglist();
	ifl = 3;
	ptd_load_mdf(fil,&ifl,msg1,msg2,&err);
	Ptd_initreglist();
	return(err);
}
/*********************************************************************
**    E_FUNCTION     : int Ptd_GetAdds(in, out, bcase, fval)
**       Get address.
**    PARAMETERS
**       INPUT  :
**          in         - 
**          bcase      - respect or ignore case.
**          fval       - register value.
**       OUTPUT :
**          out        - register string
**    RETURNS      : Non-zero on error.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int Ptd_GetAdds(char*in, char**out, int bcase, double fval)
{
	int nc, kerr, kreg;
	double gval;
	char out1[500];
	char *outpt = *out;

	if (in == NULL || in[0] == '\0')
	{
		*out[0] = '\0';
		return 0;
	}
/*
.....Letter address with values
*/
	nc = strlen(in);
	ptd_defmtcod(&kreg, &gval, in, &nc, &bcase, &kerr);
	if (kerr!=-1)
	{
/*
.....if Letter address without value
.....use fval instead of gval
.....12/09/99
*/
		if (kerr==2)
			ptd_fmticod(&kreg, &fval, out1, &nc);
		else
			ptd_fmticod(&kreg, &gval, out1, &nc);
		out1[nc] = '\0';
		strncpy(outpt, out1, nc);
		outpt[nc] = '\0';
		return 0;
	}
/*
.....if not letter address
.....check for text representation of 
.....a register number and value
*/
	if (kerr==-1)
	{
		ptd_ctocd (in,&nc,&kreg,&gval,&kerr);
/*
.....only conside G, M group and registers
.....12/09/99
		if (kerr==0)
*/
		if ((kerr==0)&&((kreg>0)||(kreg==-1)||(kreg==-2)))
		{
			ptd_fmticod(&kreg, &gval, out1, &nc);
			out1[nc] = '\0';
			strncpy(outpt, out1, nc);
			outpt[nc] = '\0';
			return 0;
		}
	}
	strcpy(*out, in);
	return 0;
}
/*********************************************************************
**    E_FUNCTION     : int Ptd_GetFindStr(in, out, bcase, type)
**       Find string.
**    PARAMETERS
**       INPUT  :
**          in         - 
**          bcase      - respect or ignore case.
**          type       - 
**       OUTPUT :
**          out        - find string
**    RETURNS      : Non-zero on error.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int  Ptd_GetFindStr(char*in, char**out, int bcase, int *type)
{
	int nc, kerr, kreg;
	double gval;
	char out1[500];
	char *outpt = *out;

	if (in == NULL || in[0] == '\0')
	{
		*out[0] = '\0';
		return 0;
	}
/*
.....Letter address with values
*/
	nc = strlen(in);
	ptd_defmtcod(&kreg, &gval, in, &nc, &bcase, &kerr);
	if (kerr!=-1)
	{
		*type = 2;
		ptd_fmticod(&kreg, &gval, out1, &nc);
		out1[nc] = '\0';
		strncpy(outpt, out1, nc);
		outpt[nc] = '\0';
		return 0;
	}
/*
.....if not letter address
.....check for text representation of 
.....a register number and value
*/
	if (kerr==-1)
	{
		ptd_ctocd (in,&nc,&kreg,&gval,&kerr);
/*
.....only conside G, M group and registers
.....12/09/99
		if (kerr==0)
*/
		if ((kerr==0)&&((kreg>0)||(kreg==-1)||(kreg==-2)))
		{
			*type = 2;
			ptd_fmticod(&kreg, &gval, out1, &nc);
			out1[nc] = '\0';
			strncpy(outpt, out1, nc);
			outpt[nc] = '\0';
			return 0;
		}
	}
	*type = 1;
	strcpy(*out, in);
	return 0;
}
/*********************************************************************
**    E_FUNCTION     : int Ptd_Convert(char*inputdata, char**outputdata, int flag)
**       Convert input nc data to output nc data.
**    PARAMETERS
**       INPUT  :
**          inputdata    - Input data.
**          istrt        - 1=starting at beginning of file, else 0
**          iend         - 1=ending at end of file, else 0
**			flag: 1: added newline character in the end
**       OUTPUT :
**          outputdata   - Output data.
**    RETURNS      : Zero
**    SIDE EFFECTS : Memory for output data is allocated.
**    WARNINGS     : none
*********************************************************************/
int Ptd_Convert(char*inputdata,int istrt,int iend, char**outputdata, int flag)
{
	int i, datalen, stat, len, knc, start, next;
	char bkstr[256];
	char outdat[256];
	int total, left;
	char *tempdata = *outputdata;
	tempdata[0] = '\0';

	datalen = strlen (inputdata);
/*
.....Initialize conversion
*/
	if (istrt == 1) ptdf_init_convert();
/*
.....Add leader if necessary.
*/
/*
	if (istrt == 1)
	{
		stat = 0;
		ptdf_ledout(&stat, &len, outdat, &knc);
		if (len>0)
		{
			if (knc>500) knc = 500;
			for (i=1; i<knc; i++) outdat[i] = outdat[0];
			while (len>0)
			{
				strncat(tempdata, outdat, knc);
				strncat(tempdata, Ptd_eol, Ptd_leol);
				len -= knc;
			}
		}
	}
*/
	total = left = datalen;
	start = 0;
	while (start<datalen)
	{
		stat = Ptd_Get_NxtBk(inputdata, bkstr, start, &next);
		len = strlen(bkstr);
		start = next;
		left = left - len;
		if (stat==-1)
		{
			if (len > 0)
			{
				strncat(tempdata, bkstr, len);
				strncat(tempdata, Ptd_eol, Ptd_leol);
			}
		}
		else if (len > 0)
		{
			knc = len;
			ptd_tapblk(bkstr, outdat, &knc);
			if (knc > 0)
			{
				strncat(tempdata, outdat, knc);
				strncat(tempdata, Ptd_eol, Ptd_leol);
			}
		}
	}
/*
.....Add trailer if necessary.
*/
/*
	if (iend == 1)
	{
		stat = 1;
		ptdf_ledout(&stat, &len, outdat, &knc);
		if	(len>0)
		{
			if (knc>500) knc = 500;
			for (i=1; i<knc; i++) outdat[i] = outdat[0];
			while (len>0)
			{
				strncat(tempdata, outdat, knc);
				strncat(tempdata, Ptd_eol, Ptd_leol);
				len -= knc;
			}
		}
	}
*/
	strncat(tempdata, "\0", 1);
/*
.....Copy output MDF to input MDF
*/
	if (iend == 1)
	{
		i = 3;
		ptdf_copy_mdf(&i);
		i = 2;
		ptdf_copy_mdf(&i);
	}
	return 0;
}

/*********************************************************************
**    E_FUNCTION     : int Ptd_Format(char*inputdata, char**outputdata)
**       Format input nc data by inserted blanks between registers.
**    PARAMETERS
**       INPUT  :
**          inputdata    - Input data.
**			flag: 1: added newline character in the end
**       OUTPUT :
**          outputdata   - Output data.
**    RETURNS      : Zero
**    SIDE EFFECTS : Memory for output data is allocated.
**    WARNINGS     : none
*********************************************************************/
int Ptd_Format(char *inputdata, char **outputdata, int flag)
{
	int stat, len, kreg, end, start, next, datalen;
	double gval;
	char bkstr[256], *p1;
	int lstart;
	int bcase;
	char *tempdata = *outputdata;
	tempdata[0] = '\0';

	bcase = 1;
	lstart = 1;
	datalen = strlen (inputdata);

	start = 0;
	while (start<datalen)
	{
		stat = Ptd_Get_NxtBk(inputdata, bkstr, start, &next);
		p1 = bkstr;
		len = strlen(bkstr);
		start = next;
		if (stat==-1)
		{
/*
.....not a valid block, copy it as it is
*/
			if (len > 0)
			{
				strncat(tempdata, bkstr, len);
				strncat(tempdata, Ptd_eol, Ptd_leol);
			}
		}
		else if (len > 0)
		{
			while (len>0 && *p1==' ')
			{
				len--;
				p1++;
			}
			for(;;)
			{
				ptd_gtregv (&kreg,&gval,p1, &len, &lstart, &end, &bcase);
				if (kreg!=-1000)
/*
.....Get a letter address
*/
				{
					if (end>0)
					{
						strncat(tempdata, p1, end);
						strncat(tempdata, " ", 1);
					}
					p1 += end;
					while (end<len && *p1==' ')
					{
						end++;
						p1++;
					}
					if (end >= len) break;
					len -= end;
				}
				else
				{
					strncat(tempdata, p1, len);
					break;
				}
			}
/*
.....add new line char(s).
*/
			if (flag)
				strncat(tempdata, Ptd_eol, Ptd_leol);
		}
	}
	strncat(tempdata, "\0", 1);
	return 0;
}
/*********************************************************************
**    E_FUNCTION     : int Ptd_Unformat(char*inputdata, char**outputdata)
**       Unformat input nc data by deleting blanks between registers.
**    PARAMETERS
**       INPUT  :
**          inputdata    - Input data.
**       OUTPUT :
**          outputdata   - Output data.
**    RETURNS      : Zero
**    SIDE EFFECTS : Memory for output data is allocated.
**    WARNINGS     : none
*********************************************************************/
int Ptd_Unformat(char *inputdata, char **outputdata, int flag)
{
	int stat, len, kreg, end, start, next, datalen;
	double gval;
	char bkstr[256], *p1;
	int lstart;
	int bcase;
	char *tempdata = *outputdata;
	tempdata[0] = '\0';

	bcase = 1;
	lstart = 1;

	datalen = strlen (inputdata);
	start = 0;
	while (start<datalen)
	{
		stat = Ptd_Get_NxtBk(inputdata, bkstr, start, &next);
		p1 = bkstr;
		len = strlen(bkstr);
		start = next;
		if (stat==-1)
		{
/*
.....not a valid block, copy it as it is
*/
			if (len > 0)
			{
				strncat(tempdata, bkstr, len);
				strncat(tempdata, Ptd_eol, Ptd_leol);
			}
		}
		else if (len > 0)
		{
			while (len>0 && *p1==' ')
			{
				len--;
				p1++;
			}
			for(;;)
			{
				ptd_gtregv (&kreg,&gval,p1, &len, &lstart, &end, &bcase);
				if (kreg!=-1000)
/*
.....Get a letter address
*/
				{
					if (end>0)
					{
						strncat(tempdata, p1, end);
					}
					p1 += end;
					while (end<len && *p1==' ')
					{
						end++;
						p1++;
					}
					if (end >= len) break;
					len -= end;
				}
				else
				{
					strncat(tempdata, p1, len);
					break;
				}
			}
/*
.....add end of line char(s).
*/
			if (flag)
				strncat(tempdata, Ptd_eol, Ptd_leol);
		}
	}
	strncat(tempdata, "\0", 1);
	return 0;
}

/*********************************************************************
**    E_FUNCTION     : int Ptd_BadAPT(char*cout, 
**						int sln, int eln, int *bufpt)
**       Create a list of blocks containing errors.
**    PARAMETERS   
**       INPUT  : 
**				bufpt: text buffer pointer
**          sln, eln:      start and end line of the text buffer.
**       OUTPUT :  
**          cout:     List of bad blocks.
**    RETURNS      : Zero
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
Ptd_BadAPT(char** cout, int sln, int eln, int *bufpt)
{
	int datalen;
	datalen = (eln - sln)*80;
	Ptd_list_init (&PTD_BAD_STR, sizeof(char), datalen, datalen);
	ptd_badapt(bufpt,&sln, &eln);
	Ptd_list_push (&PTD_BAD_STR, "\0");
	Ptd_list_compact (&PTD_BAD_STR);
	*cout = PTD_LIST_ARRAY(&PTD_BAD_STR);
	return 0;
}

/*********************************************************************
**    E_FUNCTION     : int Ptd_BadCTL(char*line_str, char**cout)
**       pass into a string line to see if it is a error block
**		if it is put into output string
**    PARAMETERS   
**       INPUT  : 
**          line_str      input string to check.
**       OUTPUT :  
**          cout     bad block string.
**    RETURNS      : Zero
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
Ptd_BadCTL(char*line_str, char** cout)
{
	int datalen, start, next, stat, len,  svlen;
	char bkstr[256], tempdata2[500];
	int lstart, knc,flag;
	int bcase;
	int total,seqnum, seqreg;
	char *tempdata = *cout;

	bcase = 1;
	lstart = 1;
	seqnum = -1;
	ptd_getseq(&seqreg);

	datalen = strlen (line_str);
	total = datalen;
	start = 0;
	while (start<datalen)
	{
		stat = Ptd_Get_NxtBk(line_str, bkstr, start, &next);
		svlen = len = strlen(bkstr);
		start = next;
		if (stat==-1)
		{
			if (len > 0)
			{
				strncat(tempdata, bkstr, len);
				strncat(tempdata, Ptd_eol, Ptd_leol);
			}
		}
		else if (len>0)
		{
			knc = len;
			strncpy(tempdata2, bkstr, knc);
//			ptdf_badctl(bkstr, &knc, &flag);
			seqnum = -1;
            ptdf_seqctl (bkstr,&knc, &seqreg, &seqnum, &flag);
			if (flag==-1)
			{
				strncat(tempdata, tempdata2, knc);
				strncat(tempdata, Ptd_eol, Ptd_leol);
			}
		}
	}
	return 0;
}
/*********************************************************************
**    E_FUNCTION     : int Ptd_BadCL(char*cout, 
**						int sln, int eln, int *bufpt)
**       Create a list of blocks containing errors.
**    PARAMETERS   
**       INPUT  : 
**			bufpt: text buffer pointer
**          sln, eln:      start and end line of the text buffer.
**       OUTPUT :  
**          cout:     List of bad blocks.
**    RETURNS      : Zero
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
Ptd_BadCL(char** cout, int sln, int eln, int *bufpt)
{
	int datalen;
	datalen = (eln - sln)*80;
	Ptd_list_init (&PTD_BAD_STR, sizeof(char), datalen, datalen);
	ptd_badcla(bufpt,&sln, &eln);
	Ptd_list_push (&PTD_BAD_STR, "\0");
	Ptd_list_compact (&PTD_BAD_STR);
	*cout = PTD_LIST_ARRAY(&PTD_BAD_STR);
	return 0;
}
/*********************************************************************
**    E_FUNCTION     : int Ptd_Resequence(char* cin, int bseq, int seqinc,
**                                        int seqn, int nonly, char**cout)
**       Resequence the file.
**    PARAMETERS   
**       INPUT  : 
**          cin            Data to resequence.
**          bseq           Start sequence number.
**          seqinc         Sequence increment.
**          seqn           Resequence every seqn'th block.
**          nonly          Resequence only blocks with seqence numbers.
**       OUTPUT :  
**          cout           Resequenced data.
**    RETURNS      : Zero
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int Ptd_Resequence(int *bufpt, int sln, int eln, int bseq, int seqinc, int seqn, int nonly,
	char**cout, int *parent)
{
	int i, lnlen, nc, datalen, start, next, stat, len, knc;
	char bkstr[256], savbkstr[256];
	char tempsav[256];
	char *p1;
	int lstart;
	int bcase;
	int tmpnum, sreqnum, seqnum, seqtime;
	Ptd_list l1;
	int total, perc, ret;
	char linestr[256];

	bcase = 1;
	lstart = 1;
	if (seqn<1) seqn = 1;

	ptd_getseq(&sreqnum);
	seqnum = bseq;
	seqtime = 0;
	datalen = (eln - sln)*40;
	total =  (eln - sln);
	Ptd_list_init (&l1, sizeof(char), datalen*2, datalen);
	for (i=sln; i<eln; i++)
	{
		ptd_getstr(bufpt, &i, linestr, &nc);
		start = 0;
		lnlen = strlen(linestr);
		while (start<lnlen)
		{		
			tempsav[0] = '\0';
			stat = Ptd_Get_NxtBk(linestr, bkstr, start, &next);
			strcpy(savbkstr, bkstr);
			len = strlen(bkstr);
			start = next;
			p1 = bkstr;
			if (stat==-1)
			{
/*
.....not a valid block, copy it as it is
*/
				if (len>0)
				{
					Ptd_list_push_multiple (&l1, len, p1);
					Ptd_list_push_multiple (&l1, Ptd_leol, Ptd_eol);
				}
			}
			else if (len>0)
			{
				knc = len;
				if (seqtime%seqn!=0)
				{
					tmpnum = -1000;
					ptdf_seqctl (bkstr, &knc, &sreqnum, &tmpnum, &ret);
				}
				else
				{
					ptdf_seqctl (bkstr, &knc, &sreqnum, &seqnum, &ret);
					if (ret==1)
						seqnum = seqnum + seqinc;
				}
				if ((ret!=-1) && (ret!=2))
					seqtime++;
				Ptd_list_push_multiple (&l1, strlen(bkstr), bkstr);
				Ptd_list_push_multiple (&l1, Ptd_leol, Ptd_eol);
			}
		}
		perc = (i-sln)*100/(eln-sln);
		Pted_Display_as_percent2(perc, parent);
	}
	Ptd_list_push (&l1, "\0");
	Ptd_list_compact (&l1);
	*cout = PTD_LIST_ARRAY(&l1);
	return 0;
}
/*********************************************************************
**    E_FUNCTION     : int Ptd_Reverse(char* cin, int len, char**cout)
**       Reverse the file.
**    PARAMETERS   
**       INPUT  : 
**          cin            Data to Reverse.
**          len            Length of data.
**       OUTPUT :  
**          cout           Reversed data.
**    RETURNS      : Zero
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int Ptd_Reverse(char* cin, int len, char**cout)
{
	int len1, len2, crflg=0;
	char *p1;
	Ptd_list l1;
	int total;

	len2 = len;
	len1 = len-1;
	total =  len;
	p1 = cin+len1;
/*
.....If last line does not end with a cr, set flag so one will be inserted.
*/
	if (*p1 != '\n') crflg = 1;
	Ptd_list_init (&l1, sizeof(char), len+crflg+1, 1);
	while (len1)
	{
		p1--;
		if (*p1 == '\n')
		{
			Ptd_list_push_multiple (&l1, len2-len1, p1+1);
			if (crflg)
			{
				Ptd_list_push_multiple (&l1, Ptd_leol, Ptd_eol);
				crflg = 0;
			}
			len2 = len1;
		}
		len1--;
	}
	Ptd_list_push_multiple (&l1, len2, p1);
	Ptd_list_push (&l1, "\0");
	Ptd_list_compact (&l1);
	*cout = PTD_LIST_ARRAY(&l1);
	return(0);
}

/*********************************************************************
**    E_FUNCTION     : int Ptd_NCtoAPT(char*inputdata, char**outputdata)
**       Convert input nc data to APT source
**    PARAMETERS
**       INPUT  :
**          inputdata    - Input data.
**       OUTPUT :
**          outputdata   - Output data.
**    RETURNS      : Zero
**    SIDE EFFECTS : Memory for output data is allocated.
**    WARNINGS     : none
*********************************************************************/
void Ptd_NCtoAPT(char *indata, char **outdata, int *parent)
{
	int stat, len, datalen, start, next;
	char bkstr[256];
	char cerr[16];
	int total, left, perc;

	ptdf_loddef();
	strcpy (cerr, "$$ ERROR - ");
	len = strlen (indata);
	Ptd_list_init (&Ptd_aptlist, sizeof(char), len*4, len*2);
	total =  len;
	left = total;
	datalen = len;
	start = 0;
	while (start<datalen)
	{
		stat = Ptd_Get_NxtBk(indata, bkstr, start, &next);
		len = strlen(bkstr);
		start = next;
		left = left - len;
		if (stat==-1)
		{
/*
.....not a valid block, copy it as a comment
*/
			if (len>0)
			{
				Ptd_list_push_multiple (&Ptd_aptlist, 11,  cerr);
				Ptd_list_push_multiple (&Ptd_aptlist, len, bkstr);
				Ptd_list_push_multiple (&Ptd_aptlist, Ptd_leol, Ptd_eol);
			}
		}
		else if (len > 0)
		{
/*
.....Valid block, call ptd_aptsrc to convert to apt source and
.....push onto apt source list.
*/
			ptd_aptsrc(bkstr, &len);
		}
		perc = 100 - 100*left/total;
		Pted_Display_as_percent2(perc, parent);
	}
	Ptd_list_push (&Ptd_aptlist, "\0");
	Ptd_list_compact (&Ptd_aptlist);
	*outdata = PTD_LIST_ARRAY(&Ptd_aptlist);
	Pted_Display_as_percent2(100,parent);
}

/*********************************************************************
**    E_FUNCTION     : int Ptd_nctosimfile(char*inputdata, char**outputdata)
**       Convert input nc data to simulation file data and write
**       to simulation file.
**    PARAMETERS
**       INPUT  :
**          inputdata    - Input data.
**          fname        - file name of nc data.
**       OUTPUT :
**          outputdata   - Input data not converted.
**    RETURNS      : Zero
**    SIDE EFFECTS : Memory for output data is allocated.
**    WARNINGS     : none
*********************************************************************/
void Ptd_nctosimfile(char *indata, char *fname, char **outdata, int *parent)
{
	int stat, len, start, next, datalen, cnt = 0, j;
	char bkstr[256];
	char partno[80];
	int total, left, perc, open_error;

	ptdf_loddef();
	datalen = len = strlen (indata);
	Ptd_list_init (&Ptd_aptlist, sizeof(char), len*4, len*2);
	j = 0;
	start = 0;
	while (start<datalen && !j)
	{
		stat = Ptd_Get_NxtBk(indata, bkstr, start, &next);
		len = strlen(bkstr);
		start = next;
		if (stat!=-1 && len>6)
		{
			if (strncmp(bkstr,"PARTNO",6) == 0)
			{
				j = len-6;
				if (j<0)  j=0;
				if (j>66) j=66;
				strncpy(partno,bkstr+6,j);
			}
		}
	}
	ptd_simhdr(&j, partno, fname, &open_error);
	total =  left = strlen (indata);
	start = 0;
	while (start<datalen)
	{
		cnt++;
		stat = Ptd_Get_NxtBk(indata, bkstr, start, &next);
		len = strlen(bkstr);
		start = next;
		left = left - len;
		if (stat==-1)
		{
/*
.....not a valid block, copy it as a comment
*/
			if (len > 0)
			{
				Ptd_list_push_multiple (&Ptd_aptlist, len, bkstr);
				Ptd_list_push_multiple (&Ptd_aptlist, Ptd_leol, Ptd_eol);
			}
		}
		else if (len > 0)
		{
/*
.....Valid block, call ptd_aptsrc to convert to apt source and
.....push onto apt source list.
*/
			ptd_nctosim(bkstr, &len, &cnt);
		}
		perc = 100 - 100*left/total;
		Pted_Display_as_percent2(perc,parent);
	}
	if (open_error==0)
		ptd_clssim();
	Ptd_list_push (&Ptd_aptlist, "\0");
	Ptd_list_compact (&Ptd_aptlist);
	*outdata = PTD_LIST_ARRAY(&Ptd_aptlist);
	Pted_Display_as_percent2(100,parent);
}

/*********************************************************************
**    E_FUNCTION     : int Ptd_docutter(char *inputdata)
**       Pass cutter file data to fortran routines for storage.
**    PARAMETERS
**       INPUT  :
**          inputdata    - Input data.
**       OUTPUT :
**          none.
**    RETURNS      : Zero
**    SIDE EFFECTS : Memory for output data is allocated.
**    WARNINGS     : none
*********************************************************************/
void Ptd_docutter(char *indata, int *kerr, char *emsg)
{
	int j;
	char *bkptr, *endptr;

	ptd_tolinit();
	endptr = indata;
	*kerr = 0;
	while(*endptr && ! *kerr)
	{
		bkptr = endptr;
		j = 0;
		while (*endptr && *endptr!= '\r' && *endptr!='\n')
		{
			endptr++;
			j++;
		}
		ptd_toldo(bkptr, &j, kerr);
		if (*kerr)
		{
			if (j > 80) j = 80;
			strcpy (emsg, "Syntax error: ");
			strncat(emsg,bkptr,j);
			emsg[j+14] = 0;
		}
		while (*endptr == '\r' || *endptr=='\n') endptr++;
	}
}

/*********************************************************************
**    E_FUNCTION     : int Ptd_initreglist()
**       Initialize register conversion lists.
**    PARAMETERS
**       INPUT  : none
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int Ptd_initreglist()
{
	int i;
	for (i=0;i<22;i++)
		Ptd_list_init (&Ptd_reglist[i], sizeof(Ptd_regstruct), 0, 10);
	ptd_initregtab ();
	return(0);
}

/*********************************************************************
**    E_FUNCTION     : int Ptd_freereglist()
**       Free register conversion lists.
**    PARAMETERS
**       INPUT  : none
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int Ptd_freereglist()
{
	int i;
	for (i=0;i<22;i++)
		Ptd_list_free (&Ptd_reglist[i]);
	return(0);
}

/*********************************************************************
**    E_FUNCTION     : int ptd_updreglist()
**       Update a register conversion list for G and M code registers.
**    PARAMETERS
**       INPUT  : lix     - index of list to update, 1-11 for G0 thru
**                          GA, 12-22 for M0 thru MA.
**                val1    - G or M code input value.
**                val2    - G or M code output value.
**                kreg    - Output register index.
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : Updates register conversion list.
**    WARNINGS     : none
*********************************************************************/
int ptd_updreglist (int *lix, double *val1, double *val2, int *kreg)
{
	int i;
	Ptd_regstruct r1;

	r1.inval  = *val1;
	r1.outval = *val2;
	r1.outreg = *kreg;
	i = *lix-1;
	Ptd_list_push (&Ptd_reglist[i], &r1);
	return 0;
}
/*********************************************************************
**    E_FUNCTION     : int ptd_schreglist()
**       Search a register conversion list.
**    PARAMETERS
**       INPUT  : lix     - index of list to search.
**                val1    - G or M code input value to search for.
**       OUTPUT : val2    - G or M code output value.
**                kreg    - Output register index.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ptd_schreglist (int *lix, double *val1, double *val2, int *kreg)
{
	int i, n;
	Ptd_regstruct *r1;

	i = *lix-1;
	r1 = (Ptd_regstruct *)PTD_LIST_ARRAY(&Ptd_reglist[i]);
	n =  Ptd_reglist[i].cur_cnt;
	for (i=0;i<n;i++)
	{
		if (r1->inval == *val1)
		{
			*val2 = r1->outval;
			*kreg = r1->outreg;
			break;
		}
		r1++;
	}
	return (0);
}
/*********************************************************************
**    E_FUNCTION     : int add_badstr(char *badstr, int *nc)
**       Push a string onto the bad list.
**    PARAMETERS
**       INPUT  :
**            badstr     - string to push onto badlist.
**            nc         - Number of characters to push.
**       OUTPUT :
**            none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void add_badstr(char *badstr, int *nc)
{
	int knc = *nc;
	Ptd_list_push_multiple (&PTD_BAD_STR, knc, badstr);
	Ptd_list_push_multiple (&PTD_BAD_STR, Ptd_leol, Ptd_eol);
}
/*********************************************************************
**    E_FUNCTION     : int Ptd_set_tllen(tlno, tllen, tnum, ton)
**       Save tool numbers and tool lengths in fortran common array.
**    PARAMETERS
**       INPUT  :
**            tlno       - Array of tool numbers.
**            tllen      - Array of tool lengths.
**            tnum       - Number of tool numbers.
**            ton        - Number of tool lengths.
**       OUTPUT :
**            none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int Ptd_set_tllen(int tlno[], double tllen[], int tnum, int ton)
{
	set_tllen(tlno, tllen, &tnum, &ton);
	return(0);
}

/*********************************************************************
**    E_FUNCTION     : int Ptd_set_units(kun)
**       Set working units (INCH/MM).
**    PARAMETERS
**       INPUT  :
**            kun        - 1 = Inches, 2 = Millimeters.
**       OUTPUT :
**            none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int Ptd_set_units(int kun)
{
	set_units(&kun);
	return(0);
}

/*********************************************************************
**    E_FUNCTION     : Ptd_Add(char *cin, char **adds, int num, 
**                             char**cout, char *msg)
**       Doing the "ADD" command
**    PARAMETERS
**       INPUT  : 
**            adds   - an arrays of letter address
**            num    - number of letter address
**            cin    - input string to be convert
**
**       OUTPUT :
**            cout   - output string after doing add
**            msg    -  error msg
**    RETURNS      : -1: if err
**                    0: success
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
Ptd_Add(char *cin, char **adds, int num, char**cout, char *msg)
{
	*cout = cin; /* TEMP - to eliminate warning. */
/*
.....we need write add function */
	return 0;
}

/*********************************************************************
**    E_FUNCTION     : Ptd_Mirror(char *cin, char **adds, int num, 
**                     char**cout, char *msg)
**       Doing the "ADD" command
**    PARAMETERS
**       INPUT  :
**            adds   - an arrays of input token(not include range)
**            num    - number of input token
**            cin    - input string to be convert
**
***       OUTPUT :
**            cout   - output string after doing add
**            msg    -  error msg
**    RETURNS      : -1: if err
**                 0: success
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
Ptd_Mirror(char *cin, char **adds, int num, char**cout, char *msg)
{
	double i, j, k, d;
	*cout = cin; /* TEMP - to eliminate warning. */
#ifdef WNT
	if ((_stricmp(adds[0], "X")==0) ||
		(_stricmp(adds[0], "Y")==0) ||
		(_stricmp(adds[0], "Z")==0))
#else
	if ((strcasecmp(adds[0], "X")==0) ||
		(strcasecmp(adds[0], "Y")==0) ||
		(strcasecmp(adds[0], "Z")==0))
#endif
	{
		if (num!=1)
		{
			sprintf(msg, "Invalid parameters for MIRror Command!");
			return -1;
		}
/*
.....set i,j,k,d
*/
#ifdef WNT
		if (_stricmp(adds[0], "X")==0)
#else
		if (strcasecmp(adds[0], "X")==0)
#endif
		{
			i = 1;
			j = 0;
			k = 0;
			d = 0;
		}
#ifdef WNT
		else if (_stricmp(adds[0], "Y")==0)
#else
		else if (strcasecmp(adds[0], "Y")==0)
#endif
		{
			i = 0;
			j = 1;
			k = 0;
			d = 0;
		}
#ifdef WNT
		else if (_stricmp(adds[0], "Z")==0)
#else
		else if (strcasecmp(adds[0], "Z")==0)
#endif
		{
			i = 0;
			j = 0;
			k = 1;
			d = 0;
		}
	}
	else
	{
		if (num!=4)
		{
			sprintf(msg, "Invalid parameters for MIRror Command!");
			return -1;
		}
/*
.....get i, j, k, d
*/
		i = atof(adds[0]);
		j = atof(adds[1]);
		k = atof(adds[2]);
		d = atof(adds[3]);
	}
/*we haven't write mirror function yet
	ptdf_mirror(i,j,k,d);
*/
	return 0;
}

/*********************************************************************
**    E_FUNCTION     : Ptd_Multiply(char *cin, char **adds, int num, 
**                                  char**cout, char *msg)
**       Doing the "Multiply" command
**    PARAMETERS
**       INPUT  :
**            adds   - an arrays of letter address
**            num    - number of letter address
**            cin    - input string to be convert
**
***       OUTPUT :
**            cout   - output string after doing add
**            msg    -  error msg
**    RETURNS      : -1: if err
**                    0: success
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
Ptd_Multiply(char *cin, char **adds, int num, char**cout, char *msg)
{
	*cout = cin; /* TEMP - to eliminate warning. */
	/*we need write rotate function*/
	return 0;
}
/*********************************************************************
**    E_FUNCTION     : Ptd_Rotate(char *cin, char **adds, int num, 
**                                char**cout, char *msg)
**       Doing the "Rotate" command
**    PARAMETERS
**       INPUT  :
**            adds   - an arrays of input token (x,y,z)
**            num    - number of input token
**            cin    - input string to be convert
**
***       OUTPUT :
**            cout   - output string after doing add
**            msg    -  error msg
**    RETURNS      : -1: if err
**                    0: success
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

Ptd_Rotate(char *cin, char **adds, int num, char**cout, char *msg)
{
/*we need write rotate function*/

	*cout = cin; /* TEMP - to eliminate warning. */
	return 0;
}
/*********************************************************************
**    E_FUNCTION     : Ptd_Scale(char *cin, char **adds, int num, 
**                               char**cout, char *msg)
**       Doing the "Scale" command
**    PARAMETERS
**       INPUT  :
**            adds   - an arrays of input token (x,y,z scale)
**            num    - number of input token
**            cin    - input string to be convert
**
***       OUTPUT :
**            cout   - output string after doing add
**            msg    -  error msg
**    RETURNS      : -1: if err
**                    0: success
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
Ptd_Scale(char *cin, char **adds, int num, char**cout, char *msg)
{
	double x,y,z;

	*cout = cin; /* TEMP - to eliminate warning. */

	if ((num>3)||(num<1))
	{
		sprintf(msg, "Invalid parameters for Scale Command!");
		return -1;
	}
	if (num==3)
	{
		x = atof(adds[0]);
		y = atof(adds[1]);
		z = atof(adds[2]);
	}
	else if (num==2)
	{
		x = atof(adds[0]);
		y = atof(adds[1]);
		z = 1;
	}
	else if (num==1)
	{
		x = atof(adds[0]);
		y = 1;
		z = 1;
	}
/*haven't write this function yet
	ptdf_scale(x,y,z);
*/
	return 0;
}
/*********************************************************************
**    E_FUNCTION     : Ptd_Trans(char *cin, char **adds, int num, 
**                               char**cout, char *msg)
**       Doing the "Translate" command
**    PARAMETERS
**       INPUT  :
**            adds   - an arrays of input token (x,y,z)
**            num    - number of input token
**            cin    - input string to be convert
**
**        OUTPUT :
**            cout   - output string after doing add
**            msg    -  error msg
**    RETURNS      : -1: if err
**                    0: success
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
Ptd_Trans(char *cin, char **adds, int num, char**cout, char *msg)
{
	double x,y,z;

	*cout = cin; /* TEMP - to eliminate warning. */

	if ((num>3)||(num<1))
	{
		sprintf(msg, "Invalid parameters for Translate Command!");
		return -1;
	}
	if (num==3)
	{
		x = atof(adds[0]);
		y = atof(adds[1]);
		z = atof(adds[2]);
	}
	else if (num==2)
	{
		x = atof(adds[0]);
		y = atof(adds[1]);
		z = 0;
	}
	else if (num==1)
	{
		x = atof(adds[0]);
		y = 0;
		z = 0;
	}
/*haven't write this function yet
	ptdf_translate(x,y,z);
*/
	return 0;
}
/*********************************************************************
**    E_FUNCTION     : Ptd_BadCommand(char*inputdata, char**outdata, int fsave)
**       
**    PARAMETERS   
**       INPUT  : 
**           inputdata     - 
**           fsave         - 
**       OUTPUT :  
**           outdata       -
**    RETURNS      : 1
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
/*
.....fsave is a flag for save checked data into global values
*/
Ptd_BadCommand(char*inputdata, char**outdata, int fsave)
{
	if (fsave) *outdata = inputdata; /* TEMP to eliminate warning. */
	return 1;
}
/*********************************************************************
**    E_FUNCTION     : Ptd_checkkey()
**       
**    PARAMETERS   
**       INPUT  : 
**           None
**       OUTPUT :  
**           None
**    RETURNS      : 1
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
Ptd_checkkey(errmsg)
char *errmsg;
{
	char program[11], revdate[9], rightdate[10], ldat[12];
	int iop[8], ierr, imaj, imin;
	char lmsg[80];

	pted_version();
	pw_getver_info(program, revdate, rightdate, &imaj, &imin);
	revdate[8] = '\0';
	datcnvc (revdate,ldat);
	pwdaut("POSTWORKS","PTED",ldat,iop,lmsg,&ierr);
	if (ierr!=0 || iop[0] == 0)
	{
		if (ierr == 0)
			strcpy(errmsg,"You are not authorized to run Pted.");
		else
			strcpy(errmsg, lmsg);
		return -1;
	}
/*
.....Allocate Pted user
*/
	pwdall ("PTED",lmsg,&ierr);
	if (ierr != 0)
	{
		strcpy(errmsg,lmsg);
		return -1;
	}
	return 1;
}

/*********************************************************************
**    E_FUNCTION     : Ptd_releasekey()
**       
**    PARAMETERS   
**       INPUT  : 
**           None
**       OUTPUT :  
**           None
**    RETURNS      : 1
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void Ptd_releasekey()
{
	pwddea("PTED");
	pwddea("");
}

/*********************************************************************
**    E_FUNCTION     : Pted_getparse_word(char *msgbblk, char *msgeblk, char *eob, char *opskip, 
**						char *tab, char *rewind)
**			This function get the MSG begin block, end block, End of Block, ... string defined in 
**			Fortran file.
**
**    PARAMETERS   
**       INPUT  : None 
**       OUTPUT :  
**           msgbblk, msgeblk: MSG begin block, end block string
**			  eob:             End of block string
**			  opskip:          OPSKIP charcter string
**				tab:           TAB character
**				rewind:        REWIND 	charcter string
**    RETURNS      : None
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void Pted_getparse_word(char *msgbblk, char *msgeblk, char *eob, char *opskip, 
						char *tab, char *rewind)
{
	int nc1, nc2, nc3, nc4, nc5, nc6;
	ptd_getparse_word(msgbblk, &nc1, msgeblk, &nc2, eob, &nc3, 
				opskip, &nc4, tab, &nc5, rewind, &nc6);
	msgbblk[nc1] = '\0';
	msgeblk[nc2] = '\0';
	eob[nc3] = '\0';
	opskip[nc4] = '\0';
	tab[nc5] = '\0';
	rewind[nc6] = '\0';
}

/*********************************************************************
**    E_FUNCTION     : IsRegister(const char *str, int knc, int *regnum, int *nc)
**		This routine receives as input a character string and
**          check to see if it matches (from beginning)
**          any Register, it must match from beginning of the string, and return the 
**          register (include value) string length
**
**    PARAMETERS   
**       INPUT  : 
**           str: string to be checked
**			nc: input string length
**       OUTPUT :  
**           regnum: register number
**			nc: register string length
**
**    RETURNS      : 1: if match
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int IsRegister(const char *str, int knc, int *regnum, int *nc)
{
	double gval;
	int kreg = -1000;
	int iend, bcase = 1, istart = 1;
	char pstr[256];
	if (knc>=256) knc = 255;
	strncpy(pstr, str, knc);
	pstr[knc] = '\0';

	ptd_gtregv (&kreg,&gval,pstr, &knc, &istart, &iend, &bcase);
	if (kreg==-1000) return 0;
	*regnum = kreg;
	*nc = iend - istart;
	return 1;
}
/*********************************************************************
**    E_FUNCTION: IsMajorMinorWord(const char *str, int knc, int *nc, int *flag)
**       check the see if the input character is a Major/Minor word (must
**			from beginning of the string)
**
**    PARAMETERS   
**       INPUT  : 
**           str: input string to be checked
**			knc: input string length
**       OUTPUT :  
**           nc: the length of Major/Minor word matched
**			flag: 1: major word.
**					2: minor word	
**    RETURNS      : 1: if match
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
IsMajorMinorWord(const char *str, int knc, int *nc, int *flag)
{
	char pstr[256];
	if (knc>=256) knc = 255;
	strncpy(pstr, str, knc);
	pstr[knc] = '\0';

	ptdf_getvwd(pstr, &knc, nc, flag);
	if (*nc>0) return 1;
	else
		return 0;
}

/*********************************************************************
**    E_FUNCTION     : IsSymbols(char sym)
**       check the see if the input character is a symbol character
**
**    PARAMETERS   
**       INPUT  : 
**           sym: character to be checked
**       OUTPUT :  
**           None
**    RETURNS      : 1: if match
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
IsSymbols(char sym)
{
	char symbols[32] = "!@#$%^&*()_-+=|\\~`{}[]\"':;<>,/?";
	int i, stat, total = 31;
	stat = 0;
	for (i=0; i<total; i++)
	{
		if (sym==symbols[i])
		{
			stat = 1;
			break;
		}
	}
	return stat;
}
/*********************************************************************
**    E_FUNCTION     : Ptd_getMDF_unit()
**       Get current MDF units
**
**    PARAMETERS   
**       INPUT  : 
**           None
**       OUTPUT :  
**           None
**    RETURNS      : 1: INCH   2. MM
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
Ptd_getMDF_unit()
{
	int unit;
	ptd_getunit (&unit);
	return unit;
}
