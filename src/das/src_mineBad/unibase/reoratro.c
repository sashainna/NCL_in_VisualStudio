/*********************************************************************
**    NAME         :  reoratro.c
**       CONTAINS:
**       ur_trans_out_oracle() (and stub version)
**    COPYRIGHT 1985 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       reoratro.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:33
*********************************************************************/

#include "udebug.h"
#include "usysdef.h"
#include "uhep.h"
#include "rerrdef.h"

#ifdef UR_ORACLE

#include "rbase.h"
#include "ribase.h"
#include "rmtuple.h"
#include "riddldef.h"

/*********************************************************************
**    E_FUNCTION     :  ur_trans_out_oracle(dbname, partnm, keepold)
**       transcribe a part out to oracle database
**    PARAMETERS   
**       INPUT  : 
**				dbname	char*			oracle database name
**				partnm	char*			part name to save as
**				keepold	UU_LOGICAL	true = keep old if part exists
**											false = replace old
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ur_trans_out_oracle(dbname, partnm, keepold)
char			*dbname;				/* database name for oracle */
char			*partnm;				/* part name to save as */
UU_LOGICAL	keepold;				/* old or new priority flag */
{
#define MAX_CHAR_COL_WIDTH 240
	UU_LOGICAL	partexists;			/* part exists in the database */
	int		partid;					/* part id of the part */
	char		relnm[16];				/* relation name for oracle append */
	char		atrnam[32];				/* build attribute names for Oracle binds */
	char		strnam[32];				/* save string attribute's name for binds */
	char		joinnm[16];				/* join relation's name for oracle */
	char		targstr[8000];			/* target string for oracle calls */
	char		v_targstr[8000];		/* target string for oracle calls */
	int		status;					/* holds status of unibase calls	*/
	int		get_nxt_status;		/* holds status of get next key_id calls	*/
	int		rel;						/* relationship counter	*/
	struct UR_data	tuple;			/* holds largest tuple	*/
	struct UR_data	atom;				/* holds largest atom	*/
	struct UR_data	*tuple_ptr;		/* a pointer to an entry	*/
	char		*attr_ptr;				/* ptr to attributes within tuple*/
	char		*data_ptr;				/* ptr to data within attribute	*/
	char		*bindptr;				/* ptr to data to bind for oracle */
	int		bindsiz;					/* size of data field being bound */
	char		strbufr[10000];		/* buffer for string data type */
	int		data_offset;			/* offset to next data item	*/
	int		extra_offset;			/* extra offset to next data item */
	struct	UR_data	*atom_ptr;	/* a pointer to an atom	*/
	int		nxt_ent;					/* next entry in relation */
	int		i,j,m,n;					/* indexes */
	unsigned int	*w_ptr;			/* a pointer to an integer */
	int		atom_size;				/* atom size for var length data */
	int		atom_ct;					/* num of atoms in a particular list		*/
	int		displ;					/* displacement into varlist					*/
	int		lst_len;					/* length of a list in bytes					*/
#define	atmax	64
	struct attr_def	atdefs[atmax];	/* attribute definitions					*/
	int		num_attr;				/* number of attributes parsed				*/
	int		atndx;					/* index into atdefs								*/
	int		atype;					/* the data type of the attribute			*/
	int		rndx;						/* row index of attribute array				*/
	int		cndx;						/* col index of attribute array				*/
	int		join_cnt;				/* number of joins in the rel					*/
	UU_LOGICAL	first_key;			/* is this the 1st key boolean				*/
	struct attr_def	v_adefs[atmax];/* attribute definitions - varlist		*/
	int		v_nattr;					/* number of attributes parsed				*/
	int		v_andx;					/* index into atdefs								*/
	int		v_atype;					/* the data type of the attribute			*/
	int		v_rndx;					/* row index of attribute array				*/
	int		v_cndx;					/* col index of attribute array				*/
	int		atrtyp;					/* oracle type of attribute					*/
	char	tmpstr[20];
#define URI_MAXKEYS 100
	int		key_rels[URI_MAXKEYS]; /* space to split keys in					*/
	int		key_keys[URI_MAXKEYS];
	int		key_ndx;					/* index to above space							*/
	int		v_key_rels[URI_MAXKEYS]; /* space to split keys in					*/
	int		v_key_keys[URI_MAXKEYS];
	int		v_key_ndx;					/* index to above space							*/
	int	rel_typ;					/* relation type from data dictionary */
		
	/* ORACLE DATA AREA */

	extern short	cur[5][32];	/* logon data area and 4 cursors	*/
	static char 	part_exist[] =
		"SELECT PART_ID FROM PART_LIS WHERE PART_NAME = :PART_NAME ";
	static char 	new_partid[] = "SELECT NVL(MAX(PART_ID),0) FROM PART_LIS";
	static char 	in_part[] =
		"INSERT INTO PART_LIS(PART_NAME,PART_DATE,PART_ID) \
		VALUES (:PART_NAME,SYSDATE,:PART_ID)";
	short				ret_len,
						ret_code;	/* returned length & code from odefin	*/
	short				indp;

/*--------------------------------------------------------------------
**    Start of Executable Code
**--------------------------------------------------------------------
*/
	uu_denter(UU_RTRC,(us,"ur_trans_out_oracle"));
	status = ur_chk_data_dict();		/* make sure data dictionary inited */
	if (status != 0)
	{
		uu_dprint(-1,(us,
				"ERROR:ur_trans_out_oracle unable to initialize data dictionary"));
		uu_dexit;
		return(-1);				/* return error code */
	}
	/* open the database and start up oracle */
	ur_oracle_init(dbname);
 
	/* see if part already exists -- try to retrieve */
	uu_dprint(UU_RITRC,(us,"go see if part(%s) is in oracle db",partnm));
	if(osql3(cur[1],part_exist,-1) ||
		odefin(cur[1],1,&partid,sizeof(partid),3,-1,&indp,-1,-1,-1,&ret_len,
			&ret_code)	||
		obndrv(cur[1],":PART_NAME",-1,partnm,-1,5,-1,(short *)-1)	||
		oexec(cur[1])
		)
	{
		uu_dprint(-1,(us,
				"ERROR:ur_trans_out_oracle error checking part existance"));
		status = URM_RDBMS_ERR;
		goto shutdown;
	}
	status = ofetch(cur[1]);
	if(status == 0)		/* part exists, what does user want to do			*/
	{
		uu_dprint(UU_RITRC,(us,"part exists!"));
		if (keepold)
		{
			/* old priority -- leave oracle and return error */
			uu_dprint(UU_RITRC,(us, "keeping old part"));
			status = UR_PRTXISTS;	/* return part_exists_error */
			goto normal;			/* but use normal exit */
		}
		else	/* new priority */
		{
			/* delete the old part */
			uu_dprint(UU_RITRC,(us, "accepting new part"));
			status = uri_del_part_oracle(partnm);
			if(status != 0)					/* error deleting part */
			{
				uu_dprint(-1,(us,
						"ERROR:ur_trans_out_oracle cannot delete part"));
				status = URM_RDBMS_ERR;
				goto shutdown;
			}
		}
	}
	else if(status !=4)					/* error reading part id	*/
	{
		uu_dprint(-1,(us,
				"ERROR:ur_trans_out_oracle error determining part existence"));
		status = URM_RDBMS_ERR;
		goto shutdown;
	}
	ur_flush_del_stack();	/* must dump any phantoms now - don't want saved */

	/* the part does not exist. either it is a new part or the old is deleted.*/
	/* enter the part in the part list with a unique part id	*/
	if(osql3(cur[1],new_partid,-1)	||
	  	odefin(cur[1],1,&partid,sizeof(partid),3,-1,&indp,-1,-1,-1,
		&ret_len,&ret_code)	||
		oexec(cur[1])	||
		ofetch(cur[1])
		)
	{
		uu_dprint(-1,(us,"ERROR:ur_trans_out_oracle unable to get part id"));
		status = URM_RDBMS_ERR;
		goto shutdown;
	}
	/* insert part list data into part list */
	partid++;						/* next unused part id */
	uu_dprint(UU_RITRC,(us,"add part (# %d) to part-list", partid));
	if(osql3(cur[1],in_part,-1)	||
		obndrv(cur[1],":PART_NAME",-1,partnm,-1,5,-1,(short *)-1)	||
		obndrv(cur[1],":PART_ID",-1,&partid,sizeof(partid),3,-1,(short *)-1)	||
		oexec(cur[1]) 
		)
	{
		uu_dprint(-1,(us,
				"ERROR:ur_trans_out_oracle unable to insert into part list"));
		status = URM_RDBMS_ERR;
		goto shutdown;
	}
	/* assign pointers for buffers to hold largest tuple & atom */
	tuple_ptr = &tuple;
	uu_dprint(UU_RITRC,(us,"adrs of retrieve buffer 0x%x",tuple_ptr));
	uu_dprint(UU_RITRC,(us,"adrs of key_rel buffer 0x%x",key_rels));
	uu_dprint(UU_RITRC,(us,"adrs of key_key buffer 0x%x",key_keys));
	atom_ptr = &atom;

	/* iterate through the relations to determine if it is active(status >= 0) */
	/* and there are active tuples in it(last_active_index > 0) */
	for(rel = 0; rel <= UR_MAX_REL; rel++)
	{
		if((UR_rcb[rel].status >= 0) && (UR_rcb[rel].last_active_index > 0))
		{
			/* get the data definition for the relation */
			strcpy(relnm, UR_rcb[rel].relname);
			num_attr = ur_data_dict(atdefs, atmax, relnm, &rel_typ);
			uu_dprint(UU_RITRC,(us,"active:%s(rel_num %d) - %d attributes",
					relnm, rel, num_attr));
			if(num_attr <= 0)
			{
				/* relation not in data dictionary error */
				uu_dprint(-1,(us,
					"ERROR:ur_trans_out_oracle don't know rel '%s'", relnm));
				status = URM_RELNTFND;
				goto shutours;
			}
			/* relation definition found */
			join_cnt = 0;
			for(i=0; i < num_attr; i++)
			{
				if(atdefs[i].attr_type == JOIN) join_cnt++;
			}
			/* build the SQL command for insert into the relation */
			uri_nsrt_sql_str(relnm, atdefs, num_attr, targstr);

			/* give the SQL statement to ORACLE */
			if(osql3(cur[1],targstr,-1))
			{
				uu_dprint(-1,(us,
					"ERROR:ur_trans_out_oracle sql command not parsed"));
				status = URM_RDBMS_ERR;
				goto shutdown;
			}

			/* bind the program vars */
			/* first the part id which isn't in the Unibase relation */
			if(obndrv(cur[1], ":PART_ID", -1, &partid, 4, 3, -1, (short *)-1))
			{
				uu_dprint(-1,(us,
					"ERROR:ur_trans_out_oracle can't bind part_id"));
				status = URM_RDBMS_ERR;
				goto shutdown;
			}
			uu_dprint(UU_RITRC,(us,"bound :PART_ID to 0x%x(%d)",&partid,partid));
			first_key = UU_TRUE;
			key_ndx = 0;
			attr_ptr = (char *) &tuple;		/* make a pointer to the tuple */
			uu_dprint(UU_RITRC,(us,"chk pnt 1 : no exception yet"));
			for(atndx=0; atndx<num_attr-join_cnt; atndx++)
			{
				atype = atdefs[atndx].attr_type;

				/* set array indexs if any, non array has 1 row, 1 col */
				rndx = atdefs[atndx].num_rows;
				cndx = atdefs[atndx].num_cols;
				data_ptr = attr_ptr;
				extra_offset = 0;
				uu_dprint(UU_RITRC,(us,"chk pnt 2 : no exception yet"));
				if(atype == CHARACTER)
				{
					uu_dprint(UU_RITRC,(us,"chk pnt 2.1 : no exception yet"));
					data_offset = atdefs[atndx].attr_off / rndx;

					/* compute the actual number of columns - columns for */
					/* character arrays are only for size of array > max */
					/* column width for character columns */
					cndx = (cndx + MAX_CHAR_COL_WIDTH - 1) / MAX_CHAR_COL_WIDTH;
					uu_dprint(UU_RITRC,(us,"chk pnt 2.2 : no exception yet"));
					if(cndx > 1)
					{
						data_offset = MAX_CHAR_COL_WIDTH;
					}
				}
				else
				{
					uu_dprint(UU_RITRC,(us,"chk pnt 2.3 : no exception yet"));
					uu_dprint(UU_RITRC,(us,"atndx=%d atype=%d",atndx,atype));
					uu_dprint(UU_RITRC,(us,"name = '%s'",atdefs[atndx].attr_name));
					uu_dprint(UU_RITRC,(us,"rndx=%d cndx=%d",rndx,cndx));
					data_offset = atdefs[atndx].attr_off / (rndx * cndx);
				}
				uu_dprint(UU_RITRC,(us,"chk pnt 3 : no exception yet"));
				for(j = 1; j <= rndx; j++)
				{
					for(i = 1; i <= cndx; i++)
					{
						atrnam[0] = ':'; atrnam[1] = '\0';
						strcat(atrnam, atdefs[atndx].attr_name);	/* SQL name */
						if(rndx > 1)							/* to fix for arrays */
						{											/* add the digits */
							sprintf(tmpstr, "_%d", j-1);
							strcat(atrnam, tmpstr);
						}
						if(cndx > 1)
						{
							sprintf(tmpstr, "_%d", i-1);
							strcat(atrnam, tmpstr);
						}
						bindptr = data_ptr;
						bindsiz = atdefs[atndx].attr_size;
						switch (atype)
						{
						case INT:
							atrtyp = 3;
							break;
						case FLOAT:
							atrtyp = 4;
							break;
						case DOUBLE:
							atrtyp = 4;
							break;
						case REAL:
							atrtyp = 4;
							break;
						case KEY_ID:
						case REL_ID:
							atrtyp = 3;
							strcpy(tmpstr, atrnam);		/* save base name */
							strcat(atrnam, "_rel");		/* rel part of key */
							if(obndrv(cur[1], atrnam, -1, &key_rels[key_ndx],
									atdefs[atndx].attr_size, atrtyp, -1, (short *)-1))
							{
								uu_dprint(-1,(us,
									"ERROR:ur_trans_out_oracle can't bind field %s",
									atrnam));
								status = URM_RDBMS_ERR;
								goto shutdown;
							}
							uu_dprint(UU_RITRC, (us,
									"bound %s to 0x%x, size %d, type %d", atrnam,
									&key_rels[key_ndx], atdefs[atndx].attr_size, atrtyp));
							strcpy(atrnam, tmpstr);		/* get base back */
							strcat(atrnam, "_key");		/* key part of key */
							bindptr = (char *)&key_keys[key_ndx++];
							break;
						case LOGICAL:
							atrtyp = 3;
							break;
						case STRING:
							atrtyp = 8;
							strcpy(strnam, atrnam);		/* save name to rebind later */
							bindptr = strbufr;
							bindsiz = 1;				/* !!!!!can't bind size yet!!!! */
							break;
						case CHARACTER:
							atrtyp = 5;
							bindsiz = (i < cndx) ? MAX_CHAR_COL_WIDTH
														: atdefs[atndx].attr_off -
															((cndx-1)*MAX_CHAR_COL_WIDTH);
							break;
						case JOIN:
							atrtyp = 10;
							break;
						}
						if(obndrv(cur[1],atrnam,-1,bindptr,bindsiz, atrtyp,-1,
									(short *)-1))
						{
							uu_dprint(-1,(us,
								"ERROR:ur_trans_out_oracle can't bind field %s",atrnam));
							status = URM_RDBMS_ERR;
							goto shutdown;
						}
						uu_dprint(UU_RITRC, (us,
								"bound %s to 0x%x, size %d, type %d", atrnam,
								bindptr,bindsiz, atrtyp));
						if((atype == KEY_ID) || (atype == REL_ID))
						{
							/* if first key, then displace past hidden rel_num */
							if((first_key)&&(rel != UR_MTUPLE_REL))
							{
								extra_offset = sizeof(long);
								first_key = UU_FALSE;
							}
						}
						data_ptr += data_offset + extra_offset;
					}	/* i loop */
				}	/* j loop */
				uu_dprint(UU_RITRC,(us,"chk pnt 4 : no exception yet"));
				attr_ptr += atdefs[atndx].attr_off + extra_offset;
				extra_offset = 0;		/* be careful with this way out here */
			}	/* end of fixed data attributes index for loop	*/

			/* now output the active tuples of the relation */
			nxt_ent	= 1;
			get_nxt_status	= ur_get_next_tuple_index(rel,&nxt_ent);
			uu_dprint(UU_RITRC,(us,"chk pnt 5 : no exception yet"));
			while(get_nxt_status == 0)
			{
				status = ur_retrieve_tuple(rel,nxt_ent,&tuple);	/* get the tuple */
				if(!uu_tst_bit(&UR_rcb[rel].rel_flags, UR_MTUPLE_REQD))
				{
					/* no master tuple for this relation - must supply a key */
					ur_rt2k(rel, nxt_ent, &tuple.key_id);
				}
				attr_ptr = (char *) &tuple;		/* make a pointer to the tuple */
				first_key = UU_TRUE;
				key_ndx = 0;

				/* index through the fixed data portion of the tuple and convert */
				/* any key_id's in the tuple from their absolute to relative form */
				for(atndx=0; atndx<num_attr-join_cnt; atndx++)
				{
					atype = atdefs[atndx].attr_type;

					/* set array indexs if any, non array has 1 row, 1 col */
					rndx = atdefs[atndx].num_rows;
					cndx = atdefs[atndx].num_cols;
					data_ptr = attr_ptr;
					if(atype == CHARACTER)
					{
						data_offset = atdefs[atndx].attr_off / rndx;
						cndx = 1;			/* no names for each column */
					}
					else
					{
						data_offset = atdefs[atndx].attr_off / (rndx * cndx);
					}
					for(i = 1; i <= cndx; i++)
					{
						for(j = 1; j <= rndx; j++)
						{
							switch (atype)
							{
							case INT:
								break;
							case FLOAT:
								break;
							case DOUBLE:
								break;
							case REAL:
								break;
							case KEY_ID:
							case REL_ID:
								uri_sp02a(data_ptr);
								ur_k2rt(*((long *)data_ptr),&key_rels[key_ndx],
										&key_keys[key_ndx]);
								key_ndx++;
								/* if first key, then displace past hidden rel_num */
								if((first_key)&&(rel != UR_MTUPLE_REL))
								{
									extra_offset = sizeof(long);
									first_key = UU_FALSE;
								}
								break;
							case LOGICAL:
								if(*(UU_LOGICAL *)data_ptr)
								{
									*(UU_LOGICAL *)data_ptr = 1;
								}
								break;
							case STRING:
								/* must rebind the string field for each tuple */
								/* so bind directly to the varlist data */
								status=ur_get_varlist_ptr(rel,nxt_ent,1,&w_ptr,
									&lst_len);
								if(obndrv(cur[1],strnam,-1, w_ptr, lst_len, 8,-1,
									(short *)-1))
								{
									uu_dprint(-1,(us,
										"ERROR:ur_trans_out_oracle can't bind field %s",
										strnam));
									status = URM_RDBMS_ERR;
									goto shutdown;
								}
								break;
							case CHARACTER:
								break;
							case JOIN:
								break;
							}
							data_ptr += data_offset + extra_offset;
						}	/* i loop */
					}	/* j loop */
					attr_ptr += atdefs[atndx].attr_off + extra_offset;
					extra_offset = 0;		/* be careful with this way out here */
				}	/* end for each fixed data attribute	*/

				/* output the modified relation tuple, along with the variable */
				/* length lists' atom size, and byte count (are these set up?) */
				uu_dprint(UU_RITRC,(us,"insert into %s",relnm));
				if(oexec(cur[1]))						/* perform the insert */
				{
					uu_dprint(-1,(us,
						"ERROR:ur_trans_out_oracle can't insert into %s",
						relnm));
					status = URM_RDBMS_ERR;
					goto shutdown;
				}
				uu_dprint(UU_RITRC,(us,"insert done"));

				/* now process joins, if any. First check that join count agrees */
				/* with curent varl count for this relation */
				uu_dprint(UU_RITRC,(us,"%d joins",join_cnt));
				for(n = 1; n <= join_cnt; n++)	/* each join */
				{
					status = ur_get_varlist_ptr(rel, nxt_ent, n, &w_ptr, &lst_len);
					if(lst_len > 0 && status == 0)
					{
						/* get the data definition for the variable list */
						strcpy(joinnm, atdefs[atndx+n-1].attr_name);
						v_nattr = ur_data_dict(v_adefs, atmax, joinnm, &rel_typ);
						if(v_nattr <= 0)
						{
							uu_dprint(-1,(us,
								"ERROR:ur_trans_out_oracle don't know rel %s", joinnm));
							status = URM_RELNTFND;
							goto shutours;
						}

						/* build cmd string for insert into join's relation */
						uri_nsrt_sql_str(joinnm, v_adefs, v_nattr, v_targstr);

						/* give the SQL statement to ORACLE */
						if(osql3(cur[2],v_targstr,-1))
						{
							uu_dprint(-1,(us,
								"ERROR:ur_trans_out_oracle sql command not parsed"));
							status = URM_RDBMS_ERR;
							goto shutdown;
						}
						status = ur_get_atom_size(rel, n, &atom_size);
						if (status != 0)
						{
							uu_dprint(-1,(us,
								"ERROR:ur_trans_out_oracle can't get atom size!"));
							status = URM_NEXIST_VARL;
							goto shutdown;
						}
						atom_ct = lst_len / atom_size;
						displ = 1;

						/* now bind the variables for the insert */
						/* bind part id and key which aren't in the structure */
						if(obndrv(cur[2], ":PART_ID", -1,&partid,4,3,-1,(short *)-1))
						{
							uu_dprint(-1,(us,
								"ERROR:ur_trans_out_oracle can't bind field PART_ID"));
							status = URM_RDBMS_ERR;
							goto shutdown;
						}
						if(obndrv(cur[2],":KEY_REL",-1,key_rels,4,3,-1, (short *)-1))
						{
							uu_dprint(-1,(us,
								"ERROR:ur_trans_out_oracle can't bind field KEY_REL"));
							status = URM_RDBMS_ERR;
							goto shutdown;
						}
						if(obndrv(cur[2],":KEY_KEY",-1,key_keys,4,3,-1, (short *)-1))
						{
							uu_dprint(-1,(us,
								"ERROR:ur_trans_out_oracle can't bind field KEY_KEY"));
							status = URM_RDBMS_ERR;
							goto shutdown;
						}
						attr_ptr = (char *) &atom;

						/* start at attribute 1 not 0--key not in structure */
						for(v_andx=1; v_andx < v_nattr; v_andx++)
						{
							v_atype = v_adefs[v_andx].attr_type;

							/* set array indexs if any, non array has 1 row, 1 col */
							rndx = v_adefs[v_andx].num_rows;
							cndx = v_adefs[v_andx].num_cols;
							data_ptr = attr_ptr;
							if(atype == CHARACTER)
							{
								data_offset = v_adefs[v_andx].attr_off / rndx;

								/* compute the actual number of columns - columns for */
								/* character arrays are only for size of array > max */
								/* column width for character columns */
								cndx = (cndx+MAX_CHAR_COL_WIDTH-1) / MAX_CHAR_COL_WIDTH;
								if(cndx > 1)
								{
									data_offset = MAX_CHAR_COL_WIDTH;
								}
							}
							else
							{
								data_offset = v_adefs[v_andx].attr_off / (rndx * cndx);
							}
							for(i = 1; i <= cndx; i++)
							{
								for(j = 1; j <= rndx; j++)
								{
									atrnam[0] = ':'; atrnam[1] = '\0';	/* SQL name */
									strcat(atrnam, v_adefs[v_andx].attr_name);
									if(rndx > 1)						/* fix for arrays */
									{										/* add the digits */
										sprintf(tmpstr, "_%d", j-1);
										strcat(atrnam, tmpstr);
									}
									if(cndx > 1)
									{
										sprintf(tmpstr, "_%d", i-1);
										strcat(atrnam, tmpstr);
									}
									bindptr = data_ptr;
									switch (v_atype)
									{
									case INT:
										atrtyp = 3;
										break;
									case FLOAT:
										atrtyp = 4;
										break;
									case DOUBLE:
										atrtyp = 4;
										break;
									case REAL:
										atrtyp = 4;
										break;
									case KEY_ID:
									case REL_ID:
										atrtyp = 3;
										strcpy(tmpstr, atrnam);		/* save base name */
										strcat(atrnam, "_rel");		/* rel part of key */
										if(obndrv(cur[2], atrnam, -1, &key_rels[key_ndx],
												v_adefs[v_andx].attr_size, atrtyp, -1,
												(short *)-1))
										{
											uu_dprint(-1,(us,
												"ERROR:ur_trans_out_oracle can't bind field %s",
												atrnam));
											status = URM_RDBMS_ERR;
											goto shutdown;
										}
										strcpy(atrnam, tmpstr);	/* get base back */
										strcat(atrnam, "_key");	/* key part of key */
										bindptr = (char *)&key_keys[key_ndx++];
										break;
									case LOGICAL:
										atrtyp = 3;
										break;
									case STRING:
										atrtyp = 8;
										break;
									case CHARACTER:
										atrtyp = 5;
										bindsiz = (i < cndx) ? MAX_CHAR_COL_WIDTH
																	: atdefs[atndx].attr_off -
																((cndx-1)*MAX_CHAR_COL_WIDTH);
										break;
									case JOIN:
										atrtyp = 10;
										break;
									}
									if(obndrv(cur[2], atrnam, -1, bindptr,
										v_adefs[v_andx].attr_size,atrtyp,-1,(short *)-1))
									{
										uu_dprint(-1,(us,
											"ERROR:ur_trans_out_oracle can't bind field %s",
											atrnam));
										status = URM_RDBMS_ERR;
										goto shutdown;
									}
									data_ptr += data_offset;
								}	/* i loop */
							}	/* j loop */
							attr_ptr += v_adefs[v_andx].attr_off;
						}	/* for each join attribute	*/

						for(m = 1; m <= atom_ct; m++)		/* each atom in varlist */
						{
							ur_retrieve_tuple_varlist(rel, nxt_ent, n, displ,atom_size,
																&atom);

							/* index through the atom and convert any key_id's */
							key_ndx = 1;						/* skip first-already set */
							attr_ptr = (char *) &atom;

							/* start at attribute 1 not 0--key not in structure */
							for(v_andx = 1; v_andx < v_nattr; v_andx++)
							{
								v_atype = v_adefs[v_andx].attr_type;

								/* set array indexs if any, default:1 row, 1 col */
								rndx = v_adefs[v_andx].num_rows;
								cndx = v_adefs[v_andx].num_cols;
								data_ptr = attr_ptr;
								if(atype == CHARACTER)
								{
									data_offset = v_adefs[v_andx].attr_off / rndx;
									cndx = 1;			/* no names for each column */
								}
								else
								{
									data_offset = v_adefs[v_andx].attr_off/(rndx * cndx);
								}
								for(i = 1; i <= cndx; i++)
								{
									for(j = 1; j <= rndx; j++)
									{
										if((v_atype == KEY_ID) || (v_atype == REL_ID))
										{
											/* we skipped the first key to begin with */
											/* so any we find are real-translate it */
											uri_sp02a(data_ptr);
											ur_k2rt(*((long *)data_ptr),&key_rels[key_ndx],
													&key_keys[key_ndx]);
											key_ndx++;
										}
										else if(v_atype == LOGICAL)	/* force 0 or 1 */
										{
											if(*(UU_LOGICAL *)data_ptr)
											{
												*(UU_LOGICAL *)data_ptr = 1;
											}
										}
										data_ptr += data_offset;
									}	/* i loop */
								}	/* j loop */
								attr_ptr += v_adefs[v_andx].attr_off;
							}	/* end of var data attributes index for loop	*/
							uu_dprint(UU_RITRC,(us,"insert into join %s",joinnm));
							if(oexec(cur[2]))						/* perform the insert */
							{
								uu_dprint(-1,(us,
									"ERROR:ur_trans_out_oracle can't insert into %s",
									joinnm));
								status = URM_RDBMS_ERR;
								goto shutdown;
							}
							displ += atom_size;		/* setup for next atom */
						}	/* end - for each atom */
					}
				}
				nxt_ent++;
				get_nxt_status = ur_get_next_tuple_index(rel,&nxt_ent);
			}
		} /* if active rcb and used */
	} /* for rel */
	/* exit oracle */
	if (ocom(cur[0]))
	{
		uu_dprint(-1,(us, "ERROR:ur_trans_out_oracle commit failure"));
		status = URM_RDBMS_ERR;
		goto normal;	/* try normal exit anyway - don't want rollback */
	}
	status = 0;
	goto normal;

shutdown:	/* error bailout exit label */
	ur_o_erpt(cur[0],4);			/* print oracle error */

shutours:	/* error bailout when not an oracle error */
	orol(cur[0]);	/* roll back work */

normal:		/* normal exit label */
	uu_dexit;
	return(status);
}

#else
/* stub version for ORACLEless machines */
ur_trans_out_oracle(dbname, partnm, keepold)
char			*dbname;				/* database name for oracle */
char			*partnm;				/* part name to save as */
UU_LOGICAL	keepold;				/* old or new priority flag */
{
	uu_denter(UU_RTRC,(us,"ur_trans_out_oracle-----(not available)"));
	uu_dexit;
	return(URM_NO_RDBMS);
}
#endif
