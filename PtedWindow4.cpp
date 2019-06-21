/************************************************************************
c
c   FILE NAME: PtedWindow4.cpp 
c
c   CONTAINS:
c		CPtedWindow::OnConvertNctoapt() 
c		CPtedWindow::OnConvertTosim() 
c		CPtedWindow::OnConvertNctosim() 
c		CPtedWindow::OnConvertApttosim() 
c		CPtedWindow::OnConvertCltosim() 
c		CPtedWindow::OnConvertApttonc() 
c
c     COPYRIGHT 2001 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c           PtedWindow4.cpp , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c           09/11/13 , 12:59:30
c
c**********************************************************************
*/
#include "pwenv.h"
#include "pwstdafx.h"
#include "Pted.h"
#include "PtedWindow.h"
#include "PtedChildWindow.h"
#include "PtedMainWindow.h"
#include "PtdGlobal.h"
#include "Ptedres.h"
#include "PtdFunc.h"
#include "PtedTextBuffer.h"
#include <sys/stat.h>
#include <sys/types.h>
#include "PtedProcessDlg.h"

extern "C" char Pted_localdir[UX_MAX_PATH];
extern char PWINIT_FILE[UX_MAX_PATH];
extern CPtedMainWindow *Pted_MainDlg;
extern "C" void Pted_disply_ProcessWindow(char *title, CPtedWindow *parent);
extern "C" void Pted_Display_as_percent(int num, CPtedWindow* parent);
extern "C" void Pted_Close_ProcessWindow(CPtedWindow *parent);
extern int pted_get_subwindow_name(char *ofile, char *infile, char *fname, char * fext);

/***********************************************************************
c
c   SUBROUTINE:  OnConvertNctoapt() 
c
c   FUNCTION:  This function convert current Control data file into a APT file
c				and displayed in a seperate window
c
c   INPUT:  None
c			
c   OUTPUT: None
c
c***********************************************************************
*/
void CPtedWindow::OnConvertNctoapt() 
{
	char title[UX_MAX_PATH+40], fname[UX_MAX_PATH], oname[UX_MAX_PATH], *indx, title2[UX_MAX_PATH+40];
	char *outdata = NULL;
	if (m_processor)
	{
		return;
	}
	else
	{
		Pted_disply_ProcessWindow("Pted NCtoAPT Conversion", this);
		Pted_Display_as_percent(1, this);
	}
	CWaitCursor wait;

	strcpy(title, m_file);
	indx = strchr(title, '.');
	if (indx==NULL)
	{
		strcpy(oname, title);
		strcat(title, ".as");
	}
	else
	{
		*indx = '\0';
		strcpy(title2, title);
		if (m_convert)
		{
			indx = strrchr(title2, '_');
			*indx = '\0';
		}
		strcat(title, ".as");
		strcpy(oname, title2);
	}
	pted_get_subwindow_name(oname, title, fname, ".as");
	CPtedChildWindow* child;
	if (m_pParent==NULL)
		child = new CPtedChildWindow(Pted_MainDlg, fname, 0);
	else
		child = new CPtedChildWindow(m_pParent, fname, 0);
	child->Create(IDD_WINDOWDIALOG);
	
	int stat = m_TextView->ConvertNctoapt(child->m_TextView, this);	
	if (stat)
	{
		child->ShowWindow(TRUE);
		if (m_pParent==NULL)
		{
			Pted_MainDlg->Add_Child(child);
		}
		else
			((CPtedMainWindow*)m_pParent)->Add_Child(child);
		child->SetFtype(4);
		child->Reset_open();
		child->m_convert = 1;
		child->m_TextView->Get_TextBuffer()->SetModified(1);
	}
	else
	{
		delete child;
		Disp_Msg("No APT file created", 3);
	}

	Pted_Close_ProcessWindow(this);
}
/***********************************************************************
c
c   SUBROUTINE:  OnConvertTosim() 
c
c   FUNCTION:  This function converts the current open file
c              into a simultation file.
c
c...Now actually, we can convert NCL control data to simulate file,
c...also convert APT source and Binary CL files now.
c   INPUT:  None
c			
c   OUTPUT: None
c
c***********************************************************************
*/
void CPtedWindow::OnConvertTosim() 
{
	if (m_ftype==3)
		OnConvertCltosim();
	else if (m_ftype == 4)
		OnConvertApttosim();
	else if (m_ftype == 2)
		OnConvertNctosim();
}
/***********************************************************************
c
c   SUBROUTINE:  OnConvertApttosim() 
c
c   FUNCTION:  This function converts the current control data file
c              into a simultation file.
c
c   INPUT:  None
c			
c   OUTPUT: None
c
c***********************************************************************
*/
void CPtedWindow::OnConvertApttosim() 
{
	char title[UX_MAX_PATH+40], fname[UX_MAX_PATH], oname[UX_MAX_PATH], *indx, fext[20], title2[40+UX_MAX_PATH];
	FILE *fp;	
	int savtype;
	char buf[UX_MAX_PATH];
	char *p, *ext = NULL;
	char tempfile[UX_MAX_PATH], *tmp, sim_file[UX_MAX_PATH], print_file[UX_MAX_PATH], tpr_file[UX_MAX_PATH], tmp_print[UX_MAX_PATH];
	if (m_processor)
	{
		return;
	}
	else
	{
		Pted_disply_ProcessWindow("Pted APTtoSim Conversion", this);
		Pted_Display_as_percent(-50, this);
	}
	CWaitCursor wait;

/*
......don't use tmpnam since this just generate a unique tmpnam, but it could
......be in top directory (such as c:) but have problems since
......since a lot of machine don't allowed to create a new file by default, 
......so I changed to use _tempnam (which will generate a unique name too)
......to have a tempname in local Pted directory
*/
//	tmpnam(tempfile);
	tmp = (char*)malloc(UX_MAX_PATH*sizeof(char));
	tmp = _tempnam(Pted_localdir, "CRE");
	strcpy(tempfile, tmp);
	free(tmp);

	savtype = m_ftype;
	m_ftype = 1;
	p = strrchr(tempfile, '.');
	if (p!=NULL)
		*p = '\0';
	strcpy(sim_file, tempfile);
	strcat(sim_file, ".sim");
	strcpy(tpr_file, tempfile);
	strcat(tempfile, ".cla");
	Ptd_rmchar(tempfile, '-');
	ProgramSaveAs(tempfile);
	m_ftype = savtype;
	FILE *pfile;	
	char tmpstr[1000], inibuf[1000], comlin[1000], tmpinx[UX_MAX_PATH];
	int num;
	inibuf[0] = '\0';
	pfile = fopen(PWINIT_FILE , "r");
	if (pfile!=NULL)
	{
		fgets(inibuf, 1000, pfile);
		fclose(pfile);
	}
	if (inibuf[0]!='\0')
	{
		strcpy(tmpstr, inibuf);
		indx = strstr(tmpstr, "-MACHINE:");
	}
	else
		indx = NULL;
	if (indx!=NULL)
		sprintf(comlin, "Pworks %s -clfile:2 -noquiet -nopunch -noident -nolist -simulate:%s %s ",
					tempfile, sim_file, inibuf);
	else
	{
		strcpy(tmpstr,"0");
		if (((CPtedMainWindow*)Pted_MainDlg)->m_input_mdf[0]!='\0')
		{
			strcpy(tmpstr, ((CPtedMainWindow*)Pted_MainDlg)->m_input_mdf);
			indx = strstr(tmpstr, "PWORKS_");
			if (indx == NULL) indx = strstr(tmpstr, "pworks_");
			if (indx!=NULL)
			{
				indx += 6;
				strcpy(tmpstr, &(indx[1]));
				indx = strchr(tmpstr, '.');
				if (indx!=NULL)
					*indx = '\0';
			}
		}		
		sprintf(comlin, "Pworks %s -MACHINE:%s -clfile:2 -noquiet -nopunch -noident -nolist -simulate:%s %s ", 
									tempfile, tmpstr, sim_file, inibuf);
	}
	int result, len = strlen(comlin);
	csystem(comlin, &len, &result);
	if (result==-1)
	{
		sprintf(buf, "Error trying to run PWorks");
		Disp_Msg(buf, 1);
		Pted_Close_ProcessWindow(this);
		return;
	}

	indx = strstr(comlin, "-NOPR");
	if (indx!=NULL)
		print_file[0] = '\0';
	else
	{
		indx = strstr(comlin, "-PR");
		if (indx!=NULL)
		{
			strcpy(tmpinx, indx);
			indx = strchr(tmpinx, ':');
			if (indx!=NULL)
			{
				strcpy(tmpinx, &(indx[1]));
				indx = strtok(tmpinx, " \r\n\t");
				if (indx!=NULL)
					strcpy(print_file, indx);
			}
		}
		else
			strcpy(print_file, ".pr1");
	}
	if ((fp = fopen(sim_file, "r")) == NULL)
	{
		sprintf(buf, "Can't open %s\r\nSimulate file is not created.", sim_file);
		Disp_Msg(buf, 1);
		sim_file[0] = '\0';
		goto printfile;
	}
	else
		fclose(fp);

	CPtedChildWindow* child;
	strcpy(title, m_file);
	indx = strchr(title, '.');
	if (indx==NULL)
	{
		strcpy(oname, title);
		strcat(title, ".sim");
	}
	else
	{
		*indx = '\0';
		strcpy(title2, title);
		if (m_convert)
		{
			indx = strrchr(title2, '_');
			if (indx!=NULL)
				*indx = '\0';
		}
		strcat(title, ".sim");
		strcpy(oname, title2);
	}
	pted_get_subwindow_name(oname, title, fname, ".sim");
	if (m_pParent==NULL)
		child = new CPtedChildWindow(Pted_MainDlg, fname, 0);
	else
		child = new CPtedChildWindow(m_pParent, fname, 0);
	child->Create(IDD_WINDOWDIALOG);
	child->ShowWindow(TRUE);
	if (m_pParent==NULL)
	{
		Pted_MainDlg->Add_Child(child);
	}
	else
		((CPtedMainWindow*)m_pParent)->Add_Child(child);

	child->SetFtype(5);
	child->LoadProgram(sim_file, 1);
	child->SetFtype(5);
	child->Reset_open();
	child->m_TextView->Get_TextBuffer()->SetModified(1);
	m_convert = 1;
printfile:;
	CPtedChildWindow* child1 = NULL;
	tmp_print[0] = '\0';
	if (print_file[0]!='\0')
	{
		if (strncmp(print_file, ".pr", 3)==0)
		{
			strcpy(fext, print_file);
			strcat(tpr_file, fext);

			strcpy(tmp_print, tempfile);
			p = strrchr(tmp_print, '.');
			if (p!=NULL)
				*p = '\0';
			strcat(tmp_print, fext);

			strcpy(title, m_file);
			indx = strchr(title, '.');
			if (indx==NULL)
			{
				strcat(title, print_file);
			}
			else
			{
				*indx = '\0';
				strcat(title, print_file);
			}
			strcpy(print_file, title);
			pted_get_subwindow_name(oname, print_file, print_file, fext);
		}
		else
			strcat(tpr_file, print_file);
		if ((fp = fopen(tpr_file, "r")) == NULL)
		{
			sprintf(buf, "Can't open %s\r\nPrint file is not created.", tpr_file);
			Disp_Msg(buf, 1);
			tpr_file[0] = '\0';
			goto done;
		}
		else
			fclose(fp);
		if (m_pParent==NULL)
			child1 = new CPtedChildWindow(Pted_MainDlg, print_file, 0, 3);
		else
			child1 = new CPtedChildWindow(m_pParent, print_file, 0, 3);
		child1->Create(IDD_WINDOWDIALOG);
		child1->ShowWindow(TRUE);
		if (m_pParent==NULL)
		{
			Pted_MainDlg->Add_Child(child1);
		}
		else
			((CPtedMainWindow*)m_pParent)->Add_Child(child1);

		child1->LoadProgram(tpr_file, 0);
		child1->Reset_open();
		child1->SetFtype(1);
		child1->m_convert = 1;
		child1->m_TextView->Get_TextBuffer()->SetModified(1);
	}
done:;
	TRY
	{
		if (tempfile[0] != '\0')
			CFile::Remove(tempfile);
	}
	CATCH( CFileException, e )
	{
		sprintf(buf, "Can't delete %s", tempfile);
		Disp_Msg(buf, 1);
	}
	END_CATCH
	TRY
	{
		if (sim_file[0] != '\0')
			CFile::Remove(sim_file);
	}
	CATCH( CFileException, e )
	{
		sprintf(buf, "Can't delete %s", sim_file);
		Disp_Msg(buf, 1);
	}
	END_CATCH
	TRY
	{
		if (tmp_print[0]!='\0')
			CFile::Remove(tmp_print);
	}
	CATCH( CFileException, e )
	{
		sprintf(buf, "Can't delete %s", tmp_print);
		Disp_Msg(buf, 1);
	}
	END_CATCH
	Pted_Close_ProcessWindow(this);
}

/***********************************************************************
c
c   SUBROUTINE:  OnConvertCltosim() 
c
c   FUNCTION:  This function converts the current control data file
c              into a simultation file.
c
c   INPUT:  None
c			
c   OUTPUT: None
c
c***********************************************************************
*/
void CPtedWindow::OnConvertCltosim() 
{
	char title[UX_MAX_PATH+40], fname[UX_MAX_PATH], oname[UX_MAX_PATH], *indx, fext[20], title2[40+UX_MAX_PATH];
	FILE *fp;	
	int savtype;
	char buf[UX_MAX_PATH];
	char *p, *ext = NULL;
	char tempfile[UX_MAX_PATH], *tmp, sim_file[UX_MAX_PATH], print_file[UX_MAX_PATH], tpr_file[UX_MAX_PATH], tmp_print[UX_MAX_PATH];
	if (m_processor)
	{
		return;
	}
	else
	{
		Pted_disply_ProcessWindow("Pted CltoSim Conversion", this);
		Pted_Display_as_percent(-50, this);
	}
	CWaitCursor wait;

/*
......don't use tmpnam since this just generate a unique tmpnam, but it could
......be in top directory (such as c:) but have problems since
......since a lot of machine don't allowed to create a new file by default, 
......so I changed to use _tempnam (which will generate a unique name too)
......to have a tempname in local Pted directory
*/
//	tmpnam(tempfile);
	tmp = (char*)malloc(UX_MAX_PATH*sizeof(char));
	tmp = _tempnam(Pted_localdir, "CRE");
	strcpy(tempfile, tmp);
	free(tmp);

	savtype = m_ftype;
	m_ftype = 3;
	p = strrchr(tempfile, '.');
	if (p!=NULL)
		*p = '\0';
	strcpy(sim_file, tempfile);
	strcat(sim_file, ".sim");
	strcpy(tpr_file, tempfile);
	strcat(tempfile, ".cl");
	Ptd_rmchar(tempfile, '-');
	ProgramSaveAs(tempfile);
	m_ftype = savtype;

	FILE *pfile;	
	char tmpstr[1000], inibuf[1000], comlin[1000], tmpinx[UX_MAX_PATH];
	int num;
	inibuf[0] = '\0';
	pfile = fopen(PWINIT_FILE , "r");
	if (pfile!=NULL)
	{
		fgets(inibuf, 1000, pfile);
		fclose(pfile);
	}
	if (inibuf[0]!='\0')
	{
		strcpy(tmpstr, inibuf);
		indx = strstr(tmpstr, "-MACHINE:");
	}
	else
		indx = NULL;
	indx = strstr(tmpstr, "-MACHINE:");
	if (indx!=NULL)
		sprintf(comlin, "Pworks %s -clfile:1 -noquiet -nopunch -noident -nolist -simulate:%s %s ",
					tempfile, sim_file, inibuf);
	else
	{
		strcpy(tmpstr,"0");
		if (((CPtedMainWindow*)Pted_MainDlg)->m_input_mdf[0]!='\0')
		{
			strcpy(tmpstr, ((CPtedMainWindow*)Pted_MainDlg)->m_input_mdf);
			indx = strstr(tmpstr, "PWORKS_");
			if (indx == NULL) indx = strstr(tmpstr, "pworks_");
			if (indx!=NULL)
			{
				indx += 6;
				strcpy(tmpstr, &(indx[1]));
				indx = strchr(tmpstr, '.');
				if (indx!=NULL)
					*indx = '\0';
			}
		}		
		sprintf(comlin, "Pworks %s -MACHINE:%s -clfile:1 -noquiet -nopunch -noident -nolist -simulate:%s %s ",
									tempfile, tmpstr, sim_file, inibuf);
	}
	int result, len = strlen(comlin);
	csystem(comlin, &len, &result);
	if (result==-1)
	{
		sprintf(buf, "Error trying to run PWorks");
		Disp_Msg(buf, 1);
		Pted_Close_ProcessWindow(this);
		return;
	}

	indx = strstr(comlin, "-NOPR");
	if (indx!=NULL)
		print_file[0] = '\0';
	else
	{
		indx = strstr(comlin, "-PR");
		if (indx!=NULL)
		{
			strcpy(tmpinx, indx);
			indx = strchr(tmpinx, ':');
			if (indx!=NULL)
			{
				strcpy(tmpinx, &(indx[1]));
				indx = strtok(tmpinx, " \r\n\t");
				if (indx!=NULL)
					strcpy(print_file, indx);
			}
		}
		else
			strcpy(print_file, ".pr1");
	}
	if ((fp = fopen(sim_file, "r")) == NULL)
	{
		sprintf(buf, "Can't open %s\r\nSimulate file is not created.", sim_file);
		Disp_Msg(buf, 1);
		sim_file[0] = '\0';
		goto printfile;
	}
	else
		fclose(fp);
	CPtedChildWindow* child;
	strcpy(title, m_file);
	indx = strchr(title, '.');
	if (indx==NULL)
	{
		strcpy(oname, title);
		strcat(title, ".sim");
	}
	else
	{
		*indx = '\0';
		strcpy(title2, title);
		if (m_convert)
		{
			indx = strrchr(title2, '_');
			if (indx!=NULL)
				*indx = '\0';
		}
		strcat(title, ".sim");
		strcpy(oname, title2);
	}
	pted_get_subwindow_name(oname, title, fname, ".sim");
	if (m_pParent==NULL)
		child = new CPtedChildWindow(Pted_MainDlg, fname, 0);
	else
		child = new CPtedChildWindow(m_pParent, fname, 0);
	child->Create(IDD_WINDOWDIALOG);
	child->ShowWindow(TRUE);
	if (m_pParent==NULL)
	{
		Pted_MainDlg->Add_Child(child);
	}
	else
		((CPtedMainWindow*)m_pParent)->Add_Child(child);

	child->LoadProgram(sim_file, 1);
	child->Reset_open();
	child->SetFtype(5);
	child->m_convert = 1;
	child->m_TextView->Get_TextBuffer()->SetModified(1);

printfile:;
	CPtedChildWindow* child1 = NULL;
	tmp_print[0] = '\0';
	if (print_file[0]!='\0')
	{
		if (strncmp(print_file, ".pr", 3)==0)
		{
			strcpy(fext, print_file);
			strcat(tpr_file, fext);

			strcpy(tmp_print, tempfile);
			p = strrchr(tmp_print, '.');
			if (p!=NULL)
				*p = '\0';
			strcat(tmp_print, fext);

			strcpy(title, m_file);
			indx = strchr(title, '.');
			if (indx==NULL)
			{
				strcat(title, print_file);
			}
			else
			{
				*indx = '\0';
				strcat(title, print_file);
			}
			strcpy(print_file, title);
			pted_get_subwindow_name(oname, print_file, print_file, fext);
		}
		else
			strcat(tpr_file, print_file);

		if ((fp = fopen(tpr_file, "r")) == NULL)
		{
			sprintf(buf, "Can't open %s\r\nPrint file is not created.", tpr_file);
			Disp_Msg(buf, 1);
			tpr_file[0] = '\0';
			goto done;
		}
		else
			fclose(fp);

		if (m_pParent==NULL)
			child1 = new CPtedChildWindow(Pted_MainDlg, print_file, 0, 3);
		else
			child1 = new CPtedChildWindow(m_pParent, print_file, 0, 3);
		child1->Create(IDD_WINDOWDIALOG);
		child1->ShowWindow(TRUE);
		if (m_pParent==NULL)
		{
			Pted_MainDlg->Add_Child(child1);
		}
		else
			((CPtedMainWindow*)m_pParent)->Add_Child(child1);

		child1->LoadProgram(tpr_file, 0);
		child1->Reset_open();
		child1->SetFtype(1);
		child1->m_convert = 1;
		child1->m_TextView->Get_TextBuffer()->SetModified(1);
	}
done:;	
	TRY
	{
		if (tempfile[0] != '\0')
			CFile::Remove(tempfile);
	}
	CATCH( CFileException, e )
	{
		sprintf(buf, "Can't delete %s", tempfile);
		Disp_Msg(buf, 1);
	}
	END_CATCH
	TRY
	{
		if (sim_file[0] != '\0')
			CFile::Remove(sim_file);
	}
	CATCH( CFileException, e )
	{
		sprintf(buf, "Can't delete %s", sim_file);
		Disp_Msg(buf, 1);
	}
	END_CATCH
	TRY
	{
		if (tmp_print[0]!='\0')
			CFile::Remove(tmp_print);
	}
	CATCH( CFileException, e )
	{
		sprintf(buf, "Can't delete %s", tmp_print);
		Disp_Msg(buf, 1);
	}
	END_CATCH
	Pted_Close_ProcessWindow(this);
}

/***********************************************************************
c
c   SUBROUTINE:  OnConvertNctosim() 
c
c   FUNCTION:  This function converts the current control data file
c              into a simultation file.
c
c   INPUT:  None
c			
c   OUTPUT: None
c
c***********************************************************************
*/
void CPtedWindow::OnConvertNctosim() 
{
	char title[UX_MAX_PATH+40], title2[UX_MAX_PATH+40], *indx;
	char *tmp, tempfile[UX_MAX_PATH], oname[UX_MAX_PATH], simfile[UX_MAX_PATH];
	CString sText;

	if (m_processor)
	{
		return;
	}
	else
	{
		Pted_disply_ProcessWindow("Pted NctoSim Conversion", this);
		Pted_Display_as_percent(1, this);
	}
	CWaitCursor wait;
	if (m_pParent==NULL)
	{
		Pted_MainDlg->LoadCutterData();
	}
	else
		((CPtedMainWindow *)m_pParent)->LoadCutterData();

	CPtedChildWindow* child, *child1;
	sprintf(title, "Bad Blocks Creating Sim file for %s", m_file);	
	if (m_pParent==NULL)
		child = new CPtedChildWindow(Pted_MainDlg, title, 0, 3);
	else
		child = new CPtedChildWindow(m_pParent, title, 0, 3);
	child->Create(IDD_WINDOWDIALOG);
/*
.....we don't save the similation file into m_file.sim but save into a 
.....temp file, and open the file if success, just make the same as 'CTL to APT'
*/
/*	int stat = m_TextView->NctoSimfile(m_file, child->m_TextView, this); */
/*
......don't use tmpnam since this just generate a unique tmpnam, but it could
......be in top directory (such as c:) but have problems since
......since a lot of machine don't allowed to create a new file by default, 
......so I changed to use _tempnam (which will generate a unique name too)
......to have a tempname in local Pted directory
*/
//	tmpnam(tempfile);
	tmp = (char*)malloc(UX_MAX_PATH*sizeof(char));
	tmp = _tempnam(Pted_localdir, "CRE");
	strcpy(tempfile, tmp);
	free(tmp);

	char *p = strrchr(tempfile, '.');
	if (p!=NULL)
		*p = '\0';
	strcat(tempfile, ".sim");
	int stat = m_TextView->NctoSimfile(tempfile, child->m_TextView, this);

	if (stat==0)
	{
		child->ShowWindow(TRUE);
		if (m_pParent==NULL)
		{
			Pted_MainDlg->Add_Child(child);
		}
		else
			((CPtedMainWindow*)m_pParent)->Add_Child(child);
		child->SetFtype(1);
		sprintf(title, "Bad Blocks Creating Sim file for %s", m_file);	
		child->SetWindowText(title);
		child->m_TextView->Get_TextBuffer()->SetModified(0);
	}
//	else
//	{
//		sprintf(title, "Simulation file for %s created", m_file);
//		Disp_Msg(title, 3);
//	}
	
	strcpy(title, m_file);
	indx = strchr(title, '.');
	if (indx==NULL)
	{
		strcpy(oname, title);
		strcat(title, ".sim");
	}
	else
	{
		*indx = '\0';
		strcpy(title2, title);
		if (m_convert)
		{
			indx = strrchr(title2, '_');
			if (indx!=NULL)
				*indx = '\0';
		}
		strcat(title, ".sim");
		strcpy(oname, title2);
	}
	pted_get_subwindow_name(oname, title, simfile, ".sim");

	if (m_pParent==NULL)
		child1 = new CPtedChildWindow(Pted_MainDlg, simfile, 0);
	else
		child1 = new CPtedChildWindow(m_pParent, simfile, 0);
	child1->Create(IDD_WINDOWDIALOG);
	child1->ShowWindow(TRUE);
	if (m_pParent==NULL)
	{
		Pted_MainDlg->Add_Child(child1);
	}
	else
		((CPtedMainWindow*)m_pParent)->Add_Child(child1);

	child1->LoadProgram(tempfile, 0);
	child1->Reset_open();
	child1->SetFtype(5);
	child1->m_convert = 1;
	child1->m_TextView->Get_TextBuffer()->SetModified(1);
	Pted_Close_ProcessWindow(this);
	TRY
	{
		if (tempfile[0] != '\0')
			CFile::Remove(tempfile);
	}
	CATCH( CFileException, e )
	{
		sprintf(title, "Can't delete %s", tempfile);
		Disp_Msg(title, 1);
	}
	END_CATCH
	return;
}
/***********************************************************************
c
c   SUBROUTINE:  OnConvertApttonc() 
c
c   FUNCTION:  This function convert current APT file into
c				a control data file and displayed in a seperate window
c
c   INPUT:  None
c			
c   OUTPUT: None
c
c***********************************************************************
*/
void CPtedWindow::OnConvertApttonc() 
{
	FILE *fp;
	char title[UX_MAX_PATH+40], fname[UX_MAX_PATH], oname[UX_MAX_PATH], *indx, fext[20], title2[UX_MAX_PATH+40];
	int savtype;
	char buf[256];
	char *p, *ext = NULL;
	char tempfile[UX_MAX_PATH], *tmp, punch_file[UX_MAX_PATH], print_file[UX_MAX_PATH], tpr_file[UX_MAX_PATH], tmp_print[UX_MAX_PATH];
	if (m_processor)
	{
		return;
	}
	else
	{
		Pted_disply_ProcessWindow("Pted APTtoNC Conversion", this);
		Pted_Display_as_percent(-50, this);
	}
	CWaitCursor wait;
/*
......don't use tmpnam since this just generate a unique tmpnam, but it could
......be in top directory (such as c:) but have problems since
......since a lot of machine don't allowed to create a new file by default, 
......so I changed to use _tempnam (which will generate a unique name too)
......to have a tempname in local Pted directory
*/
//	tmpnam(tempfile);
	tmp = (char*)malloc(UX_MAX_PATH*sizeof(char));
	tmp = _tempnam(Pted_localdir, "CRE");
	strcpy(tempfile, tmp);
	free(tmp);

	savtype = m_ftype;
	m_ftype = 1;
	p = strrchr(tempfile, '.');
	if (p!=NULL)
		*p = '\0';
	strcpy(punch_file, tempfile);
	strcat(punch_file, ".pu1");
	strcpy(tpr_file, tempfile);
	strcat(tempfile, ".cla");
	Ptd_rmchar(tempfile, '-');
	ProgramSaveAs(tempfile);
	m_ftype = savtype;
	FILE *pfile;	
	char tmpstr[1000], inibuf[1000], comlin[1000], tmpinx[UX_MAX_PATH];
	int num;
	inibuf[0] = '\0';
	pfile = fopen(PWINIT_FILE , "r");
	if (pfile!=NULL)
	{
		fgets(inibuf, 1000, pfile);
		fclose(pfile);
	}

	if (inibuf[0]!='\0')
	{
		strcpy(tmpstr, inibuf);
		indx = strstr(tmpstr, "-MACHINE:");
	}
	else
		indx = NULL;
	indx = strstr(tmpstr, "-MACHINE:");
	if (indx!=NULL)
		sprintf(comlin, "Pworks %s -clfile:2 -noquiet -nosimul -noident -nolist -PUNCH:%s %s ", 
					tempfile, punch_file, inibuf);
	else
	{
		strcpy(tmpstr,"0");
		if (((CPtedMainWindow*)Pted_MainDlg)->m_input_mdf[0]!='\0')
		{
			strcpy(tmpstr, ((CPtedMainWindow*)Pted_MainDlg)->m_input_mdf);
			indx = strstr(tmpstr, "PWORKS_");
			if (indx == NULL) indx = strstr(tmpstr, "pworks_");
			if (indx!=NULL)
			{
				indx += 6;
				strcpy(tmpstr, &(indx[1]));
				indx = strchr(tmpstr, '.');
				if (indx!=NULL)
					*indx = '\0';
			}
		}		
		sprintf(comlin, "Pworks %s -MACHINE:%s -clfile:2 -noquiet -nosimul -noident -nolist -PUNCH:%s %s ", 
									tempfile, tmpstr, punch_file, inibuf);
	}
	int result, len = strlen(comlin);
	csystem(comlin, &len, &result);
	if (result==-1)
	{
		sprintf(buf, "Error trying to run PWorks");
		Disp_Msg(buf, 1);
		Pted_Close_ProcessWindow(this);
		return;
	}
	strcpy(fext, ".pu1");
	indx = strstr(comlin, "-NOPR");
	if (indx!=NULL)
		print_file[0] = '\0';
	else
	{
		indx = strstr(comlin, "-PR");
		if (indx!=NULL)
		{
			strcpy(tmpinx, indx);
			indx = strchr(tmpinx, ':');
			if (indx!=NULL)
			{
				strcpy(tmpinx, &(indx[1]));
				indx = strtok(tmpinx, " \r\n\t");
				if (indx!=NULL)
					strcpy(print_file, indx);
			}
		}
		else
			strcpy(print_file, ".pr1");
	}
	if ((fp = fopen(punch_file, "r")) == NULL)
	{
		sprintf(buf, "Can't open %s\r\nPunch file is not created.", punch_file);
		Disp_Msg(buf, 1);
		punch_file[0] = '\0';
		goto printfile;
	}
	else
		fclose(fp);

	
	CPtedChildWindow* child;
	strcpy(title, m_file);
	indx = strchr(title, '.');
	if (indx==NULL)
	{
		strcpy(oname, title);
		strcat(title, fext);
	}
	else
	{
		*indx = '\0';
		strcpy(title2, title);
		if (m_convert)
		{
			indx = strrchr(title2, '_');
			if (indx!=NULL)
				*indx = '\0';
		}
		strcat(title, fext);
		strcpy(oname, title2);
	}
	pted_get_subwindow_name(oname, title, fname, fext);
	if (m_pParent==NULL)
		child = new CPtedChildWindow(Pted_MainDlg, fname, 0);
	else
		child = new CPtedChildWindow(m_pParent, fname, 0);
	child->Create(IDD_WINDOWDIALOG);
	child->ShowWindow(TRUE);
	if (m_pParent==NULL)
	{
		Pted_MainDlg->Add_Child(child);
	}
	else
		((CPtedMainWindow*)m_pParent)->Add_Child(child);

	child->LoadProgram(punch_file, 1);
	child->Reset_open();
	child->SetFtype(2);
	m_convert = 1;
	child->m_TextView->Get_TextBuffer()->SetModified(1);

printfile:;
	CPtedChildWindow* child1 = NULL;
	tmp_print[0] = '\0';
	if (print_file[0]!='\0')
	{
		if (strncmp(print_file, ".pr", 3)==0)
		{
			strcpy(fext, print_file);
			strcat(tpr_file, fext);

			strcpy(tmp_print, tempfile);
			p = strrchr(tmp_print, '.');
			if (p!=NULL)
				*p = '\0';
			strcat(tmp_print, fext);

			strcpy(title, m_file);
			indx = strchr(title, '.');
			if (indx==NULL)
			{
				strcat(title, print_file);
			}
			else
			{
				*indx = '\0';
				strcat(title, print_file);
			}
			strcpy(print_file, title);
			pted_get_subwindow_name(oname, print_file, print_file, fext);
		}
		else
			strcpy(tpr_file, print_file);
		if ((fp = fopen(tpr_file, "r")) == NULL)
		{
			sprintf(buf, "Can't open %s\r\nPrint file is not created.", tpr_file);
			Disp_Msg(buf, 1);
			tpr_file[0] = '\0';
			goto done;
		}
		else
			fclose(fp);
		if (m_pParent==NULL)
			child1 = new CPtedChildWindow(Pted_MainDlg, print_file, 0, 3);
		else
			child1 = new CPtedChildWindow(m_pParent, print_file, 0, 3);
		child1->Create(IDD_WINDOWDIALOG);
		child1->ShowWindow(TRUE);
		if (m_pParent==NULL)
		{
			Pted_MainDlg->Add_Child(child1);
		}
		else
			((CPtedMainWindow*)m_pParent)->Add_Child(child1);

		child1->LoadProgram(tpr_file, 0);
		child1->Reset_open();
		child1->SetFtype(1);
		child1->m_convert = 1;
		child1->m_TextView->Get_TextBuffer()->SetModified(1);
	}
done:;
	TRY
	{
		if (tempfile[0] != '\0')
			CFile::Remove(tempfile);
	}
	CATCH( CFileException, e )
	{
		sprintf(buf, "Can't delete %s", tempfile);
		Disp_Msg(buf, 1);
	}
	END_CATCH
	TRY
	{
		if (punch_file[0] != '\0')
			CFile::Remove(punch_file);
	}
	CATCH( CFileException, e )
	{
		sprintf(buf, "Can't delete %s", punch_file);
		Disp_Msg(buf, 1);
	}
	END_CATCH
	TRY
	{
		if (tmp_print[0]!='\0')
			CFile::Remove(tmp_print);
	}
	CATCH( CFileException, e )
	{
		sprintf(buf, "Can't delete %s", tmp_print);
		Disp_Msg(buf, 1);
	}
	END_CATCH
	Pted_Close_ProcessWindow(this);
}
