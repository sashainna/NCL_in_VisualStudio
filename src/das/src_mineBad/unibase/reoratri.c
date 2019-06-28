/*********************************************************************
**    NAME         :  reoratri.c
**       CONTAINS:
**       ur_trans_in_oracle()
**    COPYRIGHT 1985 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       reoratri.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:33
*********************************************************************/

#include "udebug.h"
#include "uhep.h"
#include "rerrdef.h"

#ifdef UR_ORACLE

#include "usysdef.h"
#include "umoveb.h"
#include "rbase.h"
#include "rmtuple.h"
#include "ribase.h"
#include "riddldef.h"

UU_LOGICAL	ur_unibase_used();

/* global cursors for access to oracle */
short		cur[5][32];	/* logon data area and 4 cursors			*/

/*********************************************************************
**    E_FUNCTION     :  ur_trans_in_oracle(dbname, partnm)
**       transcribe a part in from oracle database
**    PARAMETERS   
**       INPUT  : 
**				dbname	char*			oracle database name
**				partnm	char*			part name to load
**       OUTPUT :  
**          none
**    RETURNS      : 0 if successful else error code
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ur_trans_in_oracle(dbname, partnm)
char			*dbname;				/* database name for oracle (users name/pswd) */
char			*partnm;				/* part name to save as */
{
#define MAX_CHAR_COL_WIDTH 240
#define atmax	64
#define URI_MAXSTR 10000
#define URI_MAXKEYS 100
	int				partid;			/* part id of the part */
	char				relnm[16];		/* relation name for oracle append */
	char				joinnm[16];		/* join relation's name for oracle */
	int				varndx;			/* field index for odefin calls */
	char				targstr[8000];	/* target string for oracle calls */
	int				status;			/* holds status of unibase calls	*/
	int				rel;				/* relationship counter	*/
	int				rel_num;
	int				rel_tuple_indx;
	struct UR_data	tuple;			/* holds largest tuple	*/
	struct UR_data	*tuple_ptr;		/* a pointer to an entry	*/
	struct UR_data	atom;				/* holds largest atom	*/
	struct UR_data	*atom_ptr;		/* a pointer to an atom	*/
	char				*attr_ptr;		/* ptr to attributes within tuple*/
	char				*data_ptr;		/* ptr to data within attribute	*/
	int				data_offset;	/* offset to next data item	*/
	int				extra_offset;	/* extra offset to next data item */
	UU_KEY_ID		key_id;			/* a master key */
	UU_REL_ID		rel_key;			/* a relation,tuple index key */
	UU_REL_ID		keyid;			/* a relation,tuple index key */
	int				tuple_indx;
	int				tupindx;
	int				i,j,m,n;			/* indexes */
	int				atom_size;		/* atom size for var length data */
	int				displ;			/* displacement into varlist */
	struct attr_def	atdefs[atmax];	/* attribute definitions */
	int				num_attr;		/* number of attributes parsed */
	int				atndx;			/* index into atdefs */
	int				atype;			/* the data type of the attribute */
	int				rndx;				/* row index of attribute array	*/
	int				cndx;				/* col index of attribute array	*/
	int				join_cnt;		/* number of joins in the rel		*/
	int				first_key;		/* is this the 1st key boolean	*/
	struct attr_def	v_adefs[atmax];/* attribute definitions - varlist */
	int				v_nattr;			/* number of attributes parsed	*/
	int				v_andx;			/* index into atdefs	*/
	int				v_atype;			/* the data type of the attribute	*/
	UU_LOGICAL		has_string;		/* flag existance of string field */
	int				num_rows;		/* count of rows selected */
	int				list_num;		/* var len list # */
	extern int 		uri_oraerr();	/* oracle error handler */
	int				prendx;
	int				precnt;			/* index and count for back to relative key */
	char				*bindptr;		/* ptr to data to bind for oracle */
	int				*bindlen;
	int				bindsiz;
	int				scnt;
	short				stringlng;		/* length of string type */
	char				stringbfr[URI_MAXSTR];
	int				key_rels[URI_MAXKEYS]; /* space to split keys in */
	int				key_keys[URI_MAXKEYS];
	int				key_ndx;			/* index to above space							*/
	int				frst_key_rel;	/* key to retrieve with */
	int				frst_key_key;
	int				reltyp;			/* relation type returned by data dictionary */
   UU_LOGICAL  	relocate_keys;	/* true if need to relocate key's   */
	UR_REL_NUM		keysrel;			/* a key's relation */
	UR_TUPLE_INDX	keysindx;		/* a key's tuple index */
		
	/* ORACLE DATA AREA */
	static char 	part_exist[] =
		"SELECT PART_ID FROM PART_LIS WHERE PART_NAME = :PART_NAME ";
	char				geom_exist[80];
	static char 	geom_exist1[] = "SELECT PART_ID FROM ";
	static char 	geom_exist2[] = " WHERE PART_ID = :PART_ID ";
	int				atrtyp;			/* oracle type code of attribute */
	short				ret_len,
						ret_code;		/* returned length & code from odefin	*/
	short				indp;

/*--------------------------------------------------------------------
**    Start of Executable Code
**--------------------------------------------------------------------
*/
	uu_denter(UU_RTRC,(us,"ur_trans_in_oracle"));
	status = ur_chk_data_dict() ;
	if (status != 0)
	{
		uu_dprint(-1,(us,
			"ERROR:ur_trans_in_oracle unable to init data dictionary"));
		uu_dexit;
		return(-1);				/* return error code */
	}

	/* open the database and start up oracle */
	/* initialize Oracle and warm start it */
	ur_oracle_init(dbname);

	/* see if part exists -- select the part id */
	uu_dprint(UU_RITRC,(us,"perform part_id select"));
	if(osql3(cur[1],part_exist,-1)	||
		odefin(cur[1],1,&partid,sizeof(partid),3,-1,&indp,-1,-1,-1,
			&ret_len,&ret_code)	||
		obndrv(cur[1],":PART_NAME",-1,partnm,-1,5,-1,(short *)-1)	||
		oexec(cur[1])
		)
	{
		uu_dprint(-1,(us,"ERROR:ur_trans_in_oracle checking part existance"));
		status = URM_RDBMS_ERR;
		goto shutdown;
	}
	status = ofetch(cur[1]) ;
	if(status != 0)			/* error on select								*/
	{
		uu_dprint(-1,(us,"ERROR:ur_trans_in_oracle checking part existance"));
		if(status == 4)
		{
			status = URM_NO_PART;
			goto normal;
		}
		else
		{
			status = URM_RDBMS_ERR;
			goto shutdown;
		}
	}
	/* the part exists. */
	ur_flush_del_stack();	/* clean out deleted but hangin' around */
	relocate_keys = ur_unibase_used();
	ur_svld_clear();			/* clear the save/load bit map */

	/* assign pointers for buffers to hold largest tuple & atom */
	tuple_ptr = &tuple;
	atom_ptr = &atom;

	/* must make a copy of each relation's bit map, by copy bit */
	/* map 0(ALLOC) into the delete bit map space(CALLOC), to use */
	/* in restoring the geometry later(for relocating keys) */
	for(i = 0; i <= UR_MAX_REL; i++)
	{
		uu_move_byte(UR_rcb[i].bmap_ptr,
					&(UR_rcb[i].bmap_ptr[UR_CALLOC_MAP*UR_rcb[i].bmap_size]),
					UR_rcb[i].bmap_size*sizeof(unsigned long));
	}
	/* for each possible geometry id (0-UR_MAX_REL) try to select the table */
	for(rel = 0; rel < UR_MAX_REL; rel++)
	{
		/* if the relation exists */
		if(UR_rcb[rel].status >= 0)
		{
			strcpy(relnm,UR_rcb[rel].relname);
			/* get the data definition for the relation */
			num_attr = ur_data_dict(atdefs, atmax, relnm, &reltyp);
			if(num_attr <= 0)
			{
				uu_dprint(-1,(us,"ERROR:ur_trans_in_oracle unknown relation %s",
						relnm));
				status = URM_RELNTFND;
				goto normal;
			}
			else if(num_attr > atmax)
			{
				uu_dprint(-1,(us,
					"ERROR:ur_trans_in_oracle too many fields in rel %s",
					relnm));
				status = URM_ILL_REL;
				goto normal;
			}
			/* relation definition found */
			join_cnt = 0;
			for(i=0; i < num_attr; i++)
			{
				if(atdefs[i].attr_type == JOIN) join_cnt++;
			}
			varndx = 1;

			/* first build a simple test case to see if there are any */
			/* bind up only the minimum necessary unless there is really */
			/* something to retrieve */
			strcpy(geom_exist,geom_exist1);	/* put sql stmt together */
			strcat(geom_exist,relnm);
			strcat(geom_exist,geom_exist2);

			/* parse,bind and execute */
			if(osql3(cur[1],geom_exist,-1)	||
				odefin(cur[1],1,&partid,sizeof(partid),3,-1,&indp,-1,-1,-1,
					&ret_len,&ret_code)  ||
				obndrv(cur[1],":PART_ID",-1,&partid,sizeof(partid),3,-1,
					(short *)-1)	||
				oexec(cur[1])
				)
			{
				/* error */
      		uu_dprint(-1,(us,
						"ERROR:ur_trans_in_oracle checking table existance"));
				status = URM_RDBMS_ERR;
				goto shutdown;
			}
			status = ofetch(cur[1]);
			if(status)
			{
				continue;	/* on to next relation */
			}

			/* build the target string for select of the relation */
			uri_sel_sql_str(relnm, atdefs, num_attr, targstr);
			if(osql3(cur[1],targstr,-1))
			{
				uu_dprint(-1,(us,
					"ERROR:ur_trans_in_oracle SQL command not parsed"));
				status = URM_RDBMS_ERR;
				goto shutdown;
			}

			/* bind variables for select operation */
			/* first attribute is part id, but we use it to select with */
			/* we dont retrieve that field */
			if(obndrv(cur[1], ":PART_ID", -1, &partid, 4, 3, -1, (short *)-1))
			{
				uu_dprint(-1,(us,
					"ERROR:ur_trans_in_oracle can't bind field PART_ID"));
				status = URM_RDBMS_ERR;
				goto shutdown;
			}
			first_key = UU_TRUE;
			key_ndx = 0;					/* reset index for key retrieval area */
			attr_ptr = (char *)tuple_ptr;		/* select into tuple and move? */
			extra_offset = 0;

			/* index through the fixed data portion of the tuple and */
			/* bind the address of each field to the "host variable" in */
			/* the oracle select command */
			has_string = UU_FALSE;
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

					/* compute the actual number of columns - columns for */
					/* character arrays are only for size of array > max */
					/* column width for character columns */
					cndx = (cndx + MAX_CHAR_COL_WIDTH - 1) / MAX_CHAR_COL_WIDTH;
					if(cndx > 1)
					{
						data_offset = MAX_CHAR_COL_WIDTH;
					}
				}
				else
				{
					data_offset = atdefs[atndx].attr_off / (rndx * cndx);
				}
				for(i = 1; i <= cndx; i++)
				{
					for(j = 1; j <= rndx; j++)
					{
						bindptr = data_ptr;
						bindlen = (int *)&ret_len;
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
							if(odefin(cur[1],varndx++,&key_rels[key_ndx],
								atdefs[atndx].attr_size, atrtyp,-1,&indp,-1,-1,
								-1,&ret_len,&ret_code))
							{
								uu_dprint(-1,(us,
									"ERROR:ur_trans_in_oracle unable to define field %d",
									varndx-1));
								status = URM_RDBMS_ERR;
								goto shutdown;
							}
							bindptr = (char *)&key_keys[key_ndx++];
							break;
						case LOGICAL:
							atrtyp = 3;
							break;
						case STRING:
							atrtyp = 8;
							has_string = UU_TRUE;
							bindlen = (int *)&stringlng;
							uu_dprint(UU_RITRC,(us,"String length %d.",stringlng));
							bindptr = stringbfr;
							bindsiz = URI_MAXSTR;
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
						if(odefin(cur[1],varndx++,bindptr,bindsiz,
							atrtyp,-1,&indp,-1,-1,-1,bindlen,&ret_code))
						{
							uu_dprint(-1,(us,
								"ERROR:ur_trans_in_oracle unable to define field %d",
								varndx-1));
							status = URM_RDBMS_ERR;
							goto shutdown;
						}
						if((atype == KEY_ID) || (atype == REL_ID))
						{
							/* if first key, then displace past hidden rel_num */
							/* but don't do this for master tuples ????? */
							if((first_key)&&(rel != UR_MTUPLE_REL))
							{
								extra_offset = sizeof(long);
								first_key = UU_FALSE;
							}
						}
						data_ptr = data_ptr + data_offset + extra_offset;
					}	/* i loop */
				}	/* j loop */
				attr_ptr += atdefs[atndx].attr_off + extra_offset;
				extra_offset = 0;
			}	/* end set up select addresses	*/

			/* now select the tuples of the relation */
			num_rows = 0;					/* initially no rows selected */
			uu_dprint(UU_RITRC,(us,"ready to select %s",relnm));
			if(oexec(cur[1]))				/* execute the select command */
			{
				uu_dprint(-1,(us,
					"ERROR:ur_trans_in_oracle unable to perform select"));
				status = URM_RDBMS_ERR;
				goto shutdown;
			}
			while(!(status = ofetch(cur[1])))	/* retrieve a row */
			{
				num_rows++;					/* bump count */
				uu_dprint(UU_RITRC,(us,"tuple selected adjusting keys"));
				/* massage the imbedded keys */
				first_key = UU_TRUE;
				key_ndx = 0;				/* reset index to key fetchery */
				attr_ptr = (char *)tuple_ptr;		/* select into tuple and move? */
				for (atndx=0; atndx<num_attr-join_cnt; atndx++)
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
							if((atype == KEY_ID) || (atype == REL_ID))
							{
								/* if first key, then displace past hidden rel_num */
								if((first_key)&&(rel != UR_MTUPLE_REL))
								{
									*((long *)(data_ptr+data_offset))=rel;	/* relnum */
									extra_offset = sizeof(long);
									first_key = UU_FALSE;
								}
								/* perform key jiggery-pokery and magic in general */
								ur_rt2k(key_rels[key_ndx], key_keys[key_ndx], data_ptr);
								key_ndx++;				/* set for next key */
								ur_k2rt(*((long *)data_ptr), &rel_num, &rel_tuple_indx);
								if(relocate_keys)
								{
									uri_lp02a(data_ptr,
										&(UR_rcb[rel_num].bmap_ptr[UR_CALLOC_MAP*UR_rcb[rel_num].bmap_size]));
								}
							}
							data_ptr = data_ptr + data_offset + extra_offset;
						}	/* i loop */
					}	/* j loop */
					attr_ptr += atdefs[atndx].attr_off + extra_offset;
					extra_offset = 0;
				} /* end massage keys */
				/* put the tuples into unibase find where to put the info */
				/* and initialize it */
				ur_alloc_rel_tuple(rel, &tuple_indx);					/* get index */
				/* move the information from select tuple area to unibase area */
				ur_update_tuple(rel, tuple_indx, tuple_ptr);
				if(has_string)
				{
					/* initialize the varlist info */
					ur_update_varlist_info(rel,tuple_indx,1,0,0) ;
					uu_dprint(UU_RITRC,(us,"String length %d.",stringlng));
					for(scnt = 0; scnt < stringlng; scnt++)
					{
						ur_update_tuple_varlist(rel, tuple_indx, 1, scnt+1, 1,
										&stringbfr[scnt]);
					}
				}
				ur_rt2k(rel,tuple_indx,&rel_key);
				ur_load_set(rel_key);	/* mark new geometry */
			}	/* retrieve a row of the select */
			if (num_rows != 0)			/* if something was selected */
			{
				/* see if there are joins to select into var len lists */
				/* for each joined table */
				list_num = 0;
				for(atndx = num_attr - join_cnt; atndx < num_attr; atndx++)
				{
					list_num++;
					ur_get_atom_size(rel,list_num,&atom_size);
					uu_dprint(UU_RITRC,(us,"processing join #%d.",list_num));
					if (atdefs[atndx].attr_type != JOIN)
						continue;		/* ?????? gen error?????? */
					/* for each tuple that was selected */
					for(tupindx=1; tupindx<=UR_rcb[rel].last_active_index; tupindx++)
					{
						if(uu_tst_bit(&(UR_rcb[rel].bmap_ptr[UR_CALLOC_MAP*UR_rcb[rel].bmap_size]), tupindx-1) == 0 &&
					 		uu_tst_bit(UR_rcb[rel].bmap_ptr, tupindx-1) != 0)
						{
							/* set up keyid for select */
							ur_rt2k(rel, tupindx, &rel_key);	/* get rel,entry key */
							ur_update_varlist_info(rel,tupindx,list_num,0,0) ;
							if(uu_tst_bit(&UR_rcb[rel].rel_flags, UR_MTUPLE_REQD))
							{
								/* get key for mast.tuple */
								ur_retrieve_data_tuple_key(rel_key, &keyid);
							}
							else
							{
								keyid = rel_key;
							}
							/* adjust key back to relative */
							/* rel = abs - 1's in old bitmap earlier than mast */
							precnt=0;
							ur_k2rt(keyid, &keysrel, &keysindx);	/* get key's rel */
							for (prendx=0; prendx<keysindx-1; prendx++)
							{
								if(uu_tst_bit(&(UR_rcb[keysrel].bmap_ptr[UR_CALLOC_MAP*UR_rcb[keysrel].bmap_size]), prendx) != 0)
								{
									precnt++;
								}
							}
							keyid = keyid - precnt;
							ur_k2rt(keyid,&frst_key_rel,&frst_key_key);
							key_ndx = 0;

							/* get the data definition for the relation */
							strcpy(joinnm, atdefs[atndx].attr_name);
							v_nattr = ur_data_dict(v_adefs, atmax, joinnm, &reltyp);
							if(v_nattr <= 0)
							{
								/* rel not in data dict error */
								uu_dprint(-1,(us,
									"ERROR:ur_trans_in_oracle %s not defined"));
								status = URM_RELNTFND;
								goto normal;
							}
							/* relation definition found */
							/* build the target string for select of the relation */
							uri_sel_sql_str(joinnm, v_adefs, v_nattr, targstr);

							/* append string to select by key as well as part_id */
							strcat(targstr,
								" AND key_rel=:KEY_REL AND key_key=:KEY_KEY");
							uu_dprint(UU_RITRC,(us,
								" AND key_rel=:KEY_REL AND key_key=:KEY_KEY"));
							if(osql3(cur[2],targstr,-1))
							{
								uu_dprint(-1,(us,
									"ERROR:ur_trans_in_oracle SQL command not parsed"));
								status = URM_RDBMS_ERR;
								goto shutdown;
							}

							/* set up addressing for select operation */
							/* select for joined table is with part_id and key */
							/* but we don't put either into the var len list atom */
							varndx = 1;					/* reset oracle attr index */
							attr_ptr = (char *)atom_ptr;	/* select into atom */
							if(obndrv(cur[2],":PART_ID",-1,&partid,4,3,-1,(short *)-1))
							{
								uu_dprint(-1,(us,
									"ERROR:ur_trans_in_oracle can't bind field PART_ID"));
								status = URM_RDBMS_ERR;
								goto shutdown;
							}
							if(obndrv(cur[2],":KEY_REL",-1,&frst_key_rel,4,3,-1,
								(short *)-1))
							{
								uu_dprint(-1,(us,
									"ERROR:ur_trans_in_oracle can't bind field KEY_REL"));
								status = URM_RDBMS_ERR;
								goto shutdown;
							}
							if(obndrv(cur[2],":KEY_KEY",-1,&frst_key_key,4,3,-1,
								(short *)-1))
							{
								uu_dprint(-1,(us,
									"ERROR:ur_trans_in_oracle can't bind field KEY_KEY"));
								status = URM_RDBMS_ERR;
								goto shutdown;
							}
							if(odefin(cur[2],varndx++,key_rels,4,3,-1,&indp,-1,-1,-1,
								&ret_len,&ret_code))
							{
								uu_dprint(-1,(us,
									"ERROR:ur_trans_in_oracle can't define field KEY_REL"));
								status = URM_RDBMS_ERR;
								goto shutdown;
							}
							if(odefin(cur[2],varndx++,key_keys,4,3,-1,&indp,-1,-1,-1,
								&ret_len,&ret_code))
							{
								uu_dprint(-1,(us,
									"ERROR:ur_trans_in_oracle can't define field KEY_KEY"));
								status = URM_RDBMS_ERR;
								goto shutdown;
							}
							/* index through the data of the tuple and define to */
							/* ORACLE where the fields are to be fetched. */
							for(v_andx = 1; v_andx < v_nattr; v_andx++)
							{
								v_atype = v_adefs[v_andx].attr_type;
								rndx = v_adefs[v_andx].num_rows;		/* array indexs */
								cndx = v_adefs[v_andx].num_cols;
								data_ptr = attr_ptr;
								if(v_atype == CHARACTER)
								{
									data_offset = v_adefs[v_andx].attr_off / rndx;

									/* compute the number of columns - columns for */
									/* character arrays only for size of array > max */
									/* column width for character columns */
									cndx = (cndx+MAX_CHAR_COL_WIDTH-1)/MAX_CHAR_COL_WIDTH;
									if(cndx > 1)
									{
										data_offset = MAX_CHAR_COL_WIDTH;
									}
								}
								else
								{
									data_offset = v_adefs[v_andx].attr_off/(rndx * cndx);
								}
								for(i = 1; i <= cndx; i++)
								{
									for(j = 1; j <= rndx; j++)
									{
										bindptr = data_ptr;
										bindsiz = v_adefs[v_andx].attr_size;
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
											if(odefin(cur[2],varndx++,&key_rels[key_ndx],
												v_adefs[v_andx].attr_size, atrtyp,-1,
												&indp,-1, -1,-1,&ret_len,&ret_code))
											{
												uu_dprint(-1,(us,
													"ERROR:ur_trans_in_oracle unable to define field %d",
													varndx-1));
												status = URM_RDBMS_ERR;
												goto shutdown;
											}
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
										if(odefin(cur[2],varndx++,bindptr,
											bindsiz,atrtyp,-1,&indp,-1,
											-1, -1,&ret_len,&ret_code))
										{
											uu_dprint(-1,(us,
												"ERROR:ur_trans_in_oracle unable to define field %d",
												varndx-1));
											status = URM_RDBMS_ERR;
											goto shutdown;
										}
										data_ptr += data_offset;
									}	/* row loop */
								}	/* col loop */
								attr_ptr += v_adefs[v_andx].attr_off;
							}	/* end set up select addresses	*/

							/* now select the tuples of the relation */
							uu_dprint(UU_RITRC,(us,"ready to select %s.",joinnm));
							if(oexec(cur[2]))	/* execute the select command */
							{
								uu_dprint(-1,(us,
									"ERROR:ur_trans_in_oracle unable to perform select"));
								status = URM_RDBMS_ERR;
								goto shutdown;
							}
							displ = 1;
							while(!(status = ofetch(cur[2])))	/* retrieve a row */
							{
								uu_dprint(UU_RITRC,(us,"join atom selected."));
								key_ndx = 0;
								/* massage the imbedded keys */
								attr_ptr = (char *)atom_ptr;	/* select into atom */
								for(v_andx = 1; v_andx < v_nattr; v_andx++)
								{
									v_atype = v_adefs[v_andx].attr_type;
									/* set array indexs if any, non array has 1,1 */
									rndx = v_adefs[v_andx].num_rows;
									cndx = v_adefs[v_andx].num_cols;
									data_ptr = attr_ptr;
									if(v_atype == CHARACTER)
									{
										data_offset = v_adefs[v_andx].attr_off / rndx;
										cndx = 1;			/* no names for each column */
									}
									else
									{
										data_offset=v_adefs[v_andx].attr_off/(rndx*cndx);
									}
									for(i = 1; i <= cndx; i++)
									{
										for(j = 1; j <= rndx; j++)
										{
											if((v_atype == KEY_ID) || (v_atype == REL_ID))
											{
												/* fix keys */
												ur_rt2k(key_rels[key_ndx],key_keys[key_ndx],
													data_ptr);
												key_ndx++;				/* set for next key */
												if(relocate_keys)
												{
													uri_lp02a(data_ptr,
														&(UR_rcb[rel_num].bmap_ptr[UR_CALLOC_MAP*UR_rcb[rel_num].bmap_size]));
												}
											}
											data_ptr += data_offset;
										}	/* i loop */
									}	/* j loop */
									attr_ptr += v_adefs[v_andx].attr_off;
								} /* end massage keys */
								/* put into unibase: find where to put the info */
								/* and initialize it */
								/* move the information into unibase */
								ur_update_tuple_varlist(rel, tupindx, list_num,
													displ, 1, atom_ptr);
								displ++;
							}	/* retrieve a row of the join select */
							uu_dprint(UU_RTRC,(us,"==--> varlist selected"));
						}	/* if newly selected */
					}	/* for each tuple */
				}	/* for each joined table */
			}	/* if rows were selected */
			uu_dprint(UU_RITRC,(us,"Fetch status = %d.",status));
			if (status != 4)
			{
				uu_dprint(-1,(us,
					"ERROR:ur_trans_in_oracle bad status from fetch"));
				status = URM_RDBMS_ERR;
				goto shutdown;
			}
		} /* if relation exists */
	} /* for each rel */
	/* clear the delete map which we have been using to relocate keys */
	ur_del_clear();	/* clearing bit maps */
	tuple_indx = 1 ;
	while(tuple_indx > 0)
	{
		ur_get_next_new_tuple_index(UR_MTUPLE_REL,&tuple_indx) ;
		if(tuple_indx > 0)
      {
      	ur_rt2k(UR_MTUPLE_REL,tuple_indx,&key_id) ;
			ur_update_disp_segid(key_id, -1);
         tuple_indx++ ;
		}
	}   
	status = 0;
	goto normal;

shutdown:	/* abnormal termination exit */
	ur_o_erpt(cur[0],4);			/* print oracle error */

normal:	/* normal exit comes here */
	uu_dexit;
	return(status);
}

#else
/* stub for ORACLEless machines */
ur_trans_in_oracle(dbname, partnm)
char			*dbname;				/* database name for oracle (users name/pswd) */
char			*partnm;				/* part name to save as */
{
	uu_denter(UU_RTRC,(us,"ur_trans_in_oracle-----(not available)"));
	uu_dexit ;
	return(URM_NO_RDBMS);
}
#endif
