/************************************************************************
c
c   FILE NAME: testglfun.c
c
c	 CONTAINS: 
c		void load_exe_jf()
c		void execute_glcom(string)
c
c     COPYRIGHT 2004 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c     MODULE NAME AND RELEASE LEVEL
c        testglfun.c , 21.1
c     DATE AND TIME OF LAST  MODIFICATION
c        12/10/09 , 18:01:42
c
c**********************************************************************
*/
#include <stdio.h>
#include <Windows.h>
#include <wingdi.h>
#include <GL/glu.h>
#include <GL/gl.h>

FILE *GLjou_fd = NULL, *GLcode_file = NULL;
void execute_glcom(char *string);
GLenum cur_RenderMode = GL_RENDER;
static char infile[256], outfile[256];

int open_input_output()
{
	int nc;
	if (GLjou_fd == NULL)
	{
		test_get_filename("Open Input test file",
				"OpenGL test input files (*.jgl)|*.jgl|All Files (*.*)|*.*||",
				infile, &nc);
	}
	GLjou_fd= fopen(infile,"r");
	if (GLjou_fd == 0) 
	{
		MessageBox(NULL, "can't open input testing file", "Error", MB_OK);
		return -1;
	}

	if (GLcode_file == NULL)
	{
		test_get_filename("Open Output test file",
			"OpenGL test output files (*.jgl2)|*.jgl2|All Files (*.*)|*.*||",
			outfile, &nc);
	}
	GLcode_file = fopen(outfile,"w");
	if (GLcode_file == 0) 
	{
		MessageBox(NULL, "can't open output file", "Error", MB_OK);
		return -1;
	}
	return 0;
}
/**********************************************************************
**    I_FUNCTION : load_exe_jf()
**			load test file and execute openGL command
**			
**    PARAMETERS   
**       INPUT  : none
**       OUTPUT :  none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void load_exe_jf()
{
	int frd,nc;
	char string[256];
	int count = 0;

	GLjou_fd= fopen(infile,"r");
	if (GLjou_fd == 0) 
	{
		MessageBox(NULL, "can't open input testing file", "Error", MB_OK);
		return;
	}

	GLcode_file = fopen(outfile,"w");
	if (GLcode_file == 0) 
	{
		MessageBox(NULL, "can't open output file", "Error", MB_OK);
		return;
	}

	frd = 1;
	count = 0;

	fprintf(GLcode_file,"GLuint n;\n");
	fprintf(GLcode_file,"GLenum type;\n");           
	fprintf(GLcode_file,"GLuint list;\n"); 
	fprintf(GLcode_file,"GLenum mode,sfactor, dfactor;\n");    
	fprintf(GLcode_file,"GLsizei width, height;\n");           
	fprintf(GLcode_file,"GLfloat xorig,yorig, xmove, ymove; \n");         
	fprintf(GLcode_file,"GLfloat fparam, fparams[4];\n");
	fprintf(GLcode_file,"GLint iparam, iparams[4];\n");
	fprintf(GLcode_file,"GLdouble dmat[16], fmat[16];\n");
	fprintf(GLcode_file,"static GLubyte dot[] = {0x80};\n");
	fprintf(GLcode_file,"static GLubyte plus[] = {0x04, 0x00, 0x04, 0x00, 0x04,0x00, 0x04, 0x00,\n");
	fprintf(GLcode_file,"0x04, 0x00, 0xFF, 0xE0, 0x04, 0x00,0x04, 0x00,\n");
	fprintf(GLcode_file,"0x04, 0x00, 0xFF, 0xE0, 0x04, 0x00,0x04, 0x00,\n");
	fprintf(GLcode_file,"0x04, 0x00, 0x04, 0x00, 0x04, 0x00};\n");
	fprintf(GLcode_file,"static GLubyte star[] = {0x04, 0x00, 0x04, 0x00, 0x24, 0x80, 0x15, 0x00,\n");
	fprintf(GLcode_file,"0x0E, 0x00, 0xFF, 0xE0, 0x0E, 0x00, 0x15, 0x00,\n");
	fprintf(GLcode_file,"0x24, 0x80, 0x04, 0x00, 0x04, 0x00};\n");
	fprintf(GLcode_file,"static GLubyte circle[] = {0x0E, 0x00, 0x31, 0x80, 0x40, 0x40, 0x40, 0x40,\n");
	fprintf(GLcode_file,"0x80, 0x20, 0x80, 0x20, 0x80, 0x20,0x40, 0x40,\n"); 
	fprintf(GLcode_file,"0x40, 0x40, 0x31, 0x80, 0x0E, 0x00};\n");
	fprintf(GLcode_file,"static GLubyte cross[] = {0x80, 0x00, 0x20, 0x00, 0x40, 0x00, 0x40, 0x00,\n"); 
	fprintf(GLcode_file,"0x20, 0x00, 0x80, 0x00, 0x10, 0x01,0x00, 0x00, 0x08,\n");
	fprintf(GLcode_file,"0x02, 0x00, 0x00, 0x04, 0x04, 0x00,0x00, 0x02, 0x08,\n");
	fprintf(GLcode_file,"0x00, 0x00, 0x01, 0x10, 0x00, 0x00,0x00, 0xA0, 0x00,\n");
	fprintf(GLcode_file,"0x00, 0x00, 0x40, 0x00, 0x00, 0x00,0xA0, 0x00,0x00,\n");
	fprintf(GLcode_file,"0x01, 0x10, 0x00, 0x00, 0x02, 0x08,0x00, 0x00, 0x04,\n");
	fprintf(GLcode_file,"0x04, 0x00, 0x00,0x08, 0x02, 0x00, 0x00, 0x10, 0x01,\n");
	fprintf(GLcode_file,"0x00, 0x00, 0x20, 0x00, 0x80, 0x00, 0x40,0x00, 0x40,\n");
	fprintf(GLcode_file,"0x00, 0x80, 0x00, 0x20, 0x00};\n");
	fprintf(GLcode_file,"static GLubyte diamond[] = {0x04, 0x00, 0x0A ,0x00, 0x11, 0x00, 0x20, 0x80,\n");
	fprintf(GLcode_file,"0x40, 0x40, 0x80, 0x20, 0x40, 0x40,0x20, 0x80, \n");
	fprintf(GLcode_file,"0x11, 0x00, 0x0A, 0x00, 0x04,0x00};\n");
	fprintf(GLcode_file,"static GLubyte square[] = {0xFF, 0xFF, 0xE0,0x00, 0x80,0x00, 0x20,0x00,\n");
	fprintf(GLcode_file,"0x80,0x00, 0x20,0x00, 0x80, 0x00,0x20, 0x00, 0x80,\n");
	fprintf(GLcode_file,"0x00, 0x20,0x00, 0x80,0x00, 0x20,0x00, 0x80, 0x00,\n");
	fprintf(GLcode_file,"0x20, 0x00,0x80, 0x00, 0x20, 0x00, 0x80, 0x00,0x20,\n");
	fprintf(GLcode_file,"0x00, 0x80,0x00, 0x20,0x00, 0x80,0x00,0x20,0x00,\n");
	fprintf(GLcode_file,"0x80,0x00, 0x20,0x00, 0x80,0x00, 0x20,0x00, 0x80,\n");
	fprintf(GLcode_file,"0x00, 0x20,0x00,0x80,0x00, 0x20,0x00, 0x80,0x00,\n");
	fprintf(GLcode_file,"0x20,0x00, 0x80,0x00, 0x20,0x00, 0x80,0x00,0x20,\n");
	fprintf(GLcode_file,"0x00, 0xFF,0xFF, 0xE0,0x00};\n");

	while (frd==1)
	{
		if (fgets(string, 256, GLjou_fd)==NULL) 
		{
			if (feof(GLjou_fd))
				break;
			else
				goto failed;
		}
		count++;
		execute_glcom(string);
	}
	goto done;
failed:; 
   MessageBox(NULL, "fgets error!", "Error", MB_OK);
done:;
	if (GLjou_fd == NULL)
		fclose(GLjou_fd);
	if (GLcode_file == NULL)
		fclose(GLcode_file);
}
/**********************************************************************
**    I_FUNCTION : execute_glcom(string)
**			execute testing openGL string command
**			
**    PARAMETERS   
**       INPUT  : string: command to execute
**       OUTPUT :  none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void execute_glcom(string)
char *string;
{
#define LEN 100000 
	GLfloat buffer[LEN];
	char *tok;
	GLenum mode,sfactor, dfactor;   
	GLsizei width, height;          
	GLfloat xorig,yorig, xmove, ymove;         
	GLuint n;
	GLenum type;           
	GLbitfield mask;
	GLclampf red, green, blue, alpha;		
	GLclampd depth;
	GLint s;   
	GLboolean bred, bgreen,bblue, balpha;
	GLint x, y,z;         
	GLfloat fx, fy, fz;         
	GLdouble dx, dy, dz, angle;         
	GLuint list;
	GLsizei range;
	GLenum func;   
	GLboolean flag;
	GLclampd znear, zfar;
	GLenum cap;
	GLenum light, pname;   
	GLfloat fparam, fparams[4];   
	GLint iparam, iparams[4];   
	GLushort pattern;  
	GLfloat lwid;   
	GLdouble dmat[16], fmat[16];   
	GLfloat nx, ny, nz, token;   
	GLdouble left1, right1, bottom1, top1, near1, far1;   
	GLdouble d1, d2, d3, d4;
	GLint x1, y1,x2,y2, factor,base,i, ref;
	GLubyte mask2;
	GLuint mask3;
	GLenum fail,zfail, zpass, face;
	char oldstr[256];
	int mtyp;

	static GLubyte dot[] = {0x80};
	static GLubyte plus[] = {0x04, 0x00, 0x04, 0x00, 0x04,0x00, 0x04, 0x00, 
									 0x04, 0x00, 0xFF, 0xE0, 0x04, 0x00,0x04, 0x00,
									 0x04, 0x00, 0x04, 0x00, 0x04, 0x00};
	static GLubyte star[] = {0x04, 0x00, 0x04, 0x00, 0x24, 0x80, 0x15, 0x00,
									 0x0E, 0x00, 0xFF, 0xE0, 0x0E, 0x00, 0x15, 0x00,
									 0x24, 0x80, 0x04, 0x00, 0x04, 0x00};
	static GLubyte circle[] = {0x0E, 0x00, 0x31, 0x80, 0x40, 0x40, 0x40, 0x40,
							   	   0x80, 0x20, 0x80, 0x20, 0x80, 0x20,0x40, 0x40, 
										0x40, 0x40, 0x31, 0x80, 0x0E, 0x00};
	static GLubyte cross[] = {0x80, 0x00, 0x20, 0x00, 0x40, 0x00, 0x40, 0x00, 
									0x20, 0x00, 0x80, 0x00, 0x10, 0x01,0x00, 0x00, 0x08,
									0x02, 0x00, 0x00, 0x04, 0x04, 0x00,0x00, 0x02, 0x08,
									0x00, 0x00, 0x01, 0x10, 0x00, 0x00,0x00, 0xA0, 0x00,
									0x00, 0x00, 0x40, 0x00, 0x00, 0x00,0xA0, 0x00,0x00,
									0x01, 0x10, 0x00, 0x00, 0x02, 0x08,0x00, 0x00, 0x04,
								   0x04, 0x00, 0x00,0x08, 0x02, 0x00, 0x00, 0x10, 0x01,
								   0x00, 0x00, 0x20, 0x00, 0x80, 0x00, 0x40,0x00, 0x40,
									0x00, 0x80, 0x00, 0x20, 0x00};
	static GLubyte diamond[] = {0x04, 0x00, 0x0A ,0x00, 0x11, 0x00, 0x20, 0x80,
									 	0x40, 0x40, 0x80, 0x20, 0x40, 0x40,0x20, 0x80, 
										0x11, 0x00, 0x0A, 0x00, 0x04,0x00};
	static GLubyte square[] = {0xFF, 0xFF, 0xE0,0x00, 0x80,0x00, 0x20,0x00,
									 0x80,0x00, 0x20,0x00, 0x80, 0x00,0x20, 0x00, 0x80,
									 0x00, 0x20,0x00, 0x80,0x00, 0x20,0x00, 0x80, 0x00,
									 0x20, 0x00,0x80, 0x00, 0x20, 0x00, 0x80, 0x00,0x20,
									 0x00, 0x80,0x00, 0x20,0x00, 0x80,0x00,0x20,0x00,
									 0x80,0x00, 0x20,0x00, 0x80,0x00, 0x20,0x00, 0x80,
									 0x00, 0x20,0x00,0x80,0x00, 0x20,0x00, 0x80,0x00,
									 0x20,0x00, 0x80,0x00, 0x20,0x00, 0x80,0x00,0x20,
										0x00, 0xFF,0xFF, 0xE0,0x00};

	strcpy(oldstr, string);

	tok = strtok(string, "\t ,\n");
	if (tok==NULL)
		return;

	if (strcmp(tok, "glFeedbackBuffer")==0)
			return;
	if ((cur_RenderMode!=GL_RENDER) && (strcmp(tok, "glRenderMode")!=0))
		return;

	if (strcmp(tok, "glBegin")==0)
	{
		tok = strtok(NULL, "\t ,\n");
		if (tok==NULL) 
			return;
		mode = (GLenum) atoi(tok);
		glBegin (mode);
		fprintf(GLcode_file, "\tglBegin (%d);\n", mode);
		return;
	}
	else if (strcmp(tok, "glEnd")==0)
	{
		glEnd();
		fprintf(GLcode_file, "\tglEnd ();\n");
		return;
	}
	else if (strcmp(tok, "glBitmap")==0)
	{
		tok = strtok(NULL, "\t ,\n");
		if (tok==NULL) goto failed;
		width = (GLsizei)atoi(tok);
		fprintf(GLcode_file,"\twidth = (GLsizei)%d;\n", width);
		tok = strtok(NULL, "\t ,\n");
		if (tok==NULL) goto failed;
		height = (GLsizei)atoi(tok);
		fprintf(GLcode_file,"\theight = (GLsizei)%d;\n", height);

		tok = strtok(NULL, "\t ,\n");
		if (tok==NULL) goto failed;
		xorig = (GLfloat)atof(tok);
		fprintf(GLcode_file,"\txorig = (GLfloat)%f;\n", xorig);
		tok = strtok(NULL, "\t ,\n");
		if (tok==NULL) goto failed;
		yorig = (GLfloat)atof(tok);
		fprintf(GLcode_file,"\tyorig = (GLfloat)%f;\n", yorig);

		tok = strtok(NULL, "\t ,\n");
		if (tok==NULL) goto failed;
		xmove = (GLfloat)atof(tok);
		fprintf(GLcode_file,"\txmove = (GLfloat)%f;\n", xmove);
		tok = strtok(NULL, "\t ,\n");
		if (tok==NULL) goto failed;
		ymove = (GLfloat)atof(tok);
		fprintf(GLcode_file,"\tymove = (GLfloat)%f;\n", ymove);
/*
.....added marker type
*/
		tok = strtok(NULL, "\t ,\n");
		if (tok==NULL) goto failed;
		mtyp = atoi (tok);
		if (mtyp==0)
		{
			glBitmap(width, height, xorig, yorig, xmove, ymove, dot);		
			fprintf(GLcode_file,"\tglBitmap(width, height, xorig, yorig, xmove, ymove, dot);\n");
		}
		else if (mtyp==1)
		{
			glBitmap(width, height, xorig, yorig, xmove, ymove, plus);		
			fprintf(GLcode_file,"\tglBitmap(width, height, xorig, yorig, xmove, ymove, plus);\n");
		}
		else if (mtyp==2)
		{
			glBitmap(width, height, xorig, yorig, xmove, ymove, star);		
			fprintf(GLcode_file,"\tglBitmap(width, height, xorig, yorig, xmove, ymove, star);\n");
		}
		else if (mtyp==3)
		{
			glBitmap(width, height, xorig, yorig, xmove, ymove, circle);		
			fprintf(GLcode_file,"\tglBitmap(width, height, xorig, yorig, xmove, ymove, circle);\n");
		}
		else if (mtyp==4)
		{
			glBitmap(width, height, xorig, yorig, xmove, ymove, cross);		
			fprintf(GLcode_file,"\tglBitmap(width, height, xorig, yorig, xmove, ymove, cross);\n");
		}
		else if (mtyp==6)
		{
			glBitmap(width, height, xorig, yorig, xmove, ymove, diamond);		
			fprintf(GLcode_file,"\tglBitmap(width, height, xorig, yorig, xmove, ymove, diamond);\n");
		}
		else if (mtyp==7)
		{
			glBitmap(width, height, xorig, yorig, xmove, ymove, square);		
			fprintf(GLcode_file,"\tglBitmap(width, height, xorig, yorig, xmove, ymove, square);\n");
		}
		else 
		{
			glBitmap(width, height, xorig, yorig, xmove, ymove, dot);		
			fprintf(GLcode_file,"\tglBitmap(width, height, xorig, yorig, xmove, ymove, dot);\n");
		}
		return;
	}
	else if (strcmp(tok, "glBlendFunc")==0)
	{
		tok = strtok(NULL, "\t ,\n");
		if (tok==NULL) goto failed;
		sfactor = (GLenum)atoi(tok);
		tok = strtok(NULL, "\t ,\n");
		if (tok==NULL) goto failed;
		dfactor = (GLenum)atoi(tok);
		glBlendFunc(sfactor, dfactor);
		fprintf(GLcode_file,"\tdfactor = (GLenum)%d;\n", dfactor);
		fprintf(GLcode_file,"\tsfactor = (GLenum)%d;\n", sfactor);
		fprintf(GLcode_file,"\tglBlendFunc(sfactor, dfactor);\n");
		return;
	}
	else if (strcmp(tok, "glCallList")==0)
	{
		tok = strtok(NULL, "\t ,\n");
		if (tok==NULL) goto failed;
		list = (GLuint)atoi(tok);
		glCallList(list);
		fprintf(GLcode_file,"\tlist = (GLuint)%d;\n", list);
		fprintf(GLcode_file,"\tglCallList(list);\n");
		return;
	}
	else if (strcmp(tok, "glCallLists")==0)
	{
		tok = strtok(NULL, "\t ,\n");
		if (tok==NULL) goto failed;
		n = (GLuint)atoi(tok);
		tok = strtok(NULL, "\t ,\n");
		if (tok==NULL) goto failed;
		type = (GLenum)atoi(tok);
		tok = strtok(NULL, "\t ,\n");
		if (tok==NULL) goto failed;
		glCallLists(n,type,tok);
		fprintf(GLcode_file,"\tn = (GLuint)%d;\n", n);
		fprintf(GLcode_file,"\ttype = (GLenum)%d;\n", type);
		fprintf(GLcode_file,"\tglCallLists(n,type,\"%s\");\n", tok);
		return;
	}
	else if (strcmp(tok, "glClear")==0)
	{
		tok = strtok(NULL, "\t ,\n");
		if (tok==NULL) goto failed;
		mask = (GLbitfield)atoi(tok);
		glClear(mask);
		fprintf(GLcode_file,"\tglClear(%d);\n", mask);
		return;
	}
	else if (strcmp(tok, "glClearColor")==0)
	{
		tok = strtok(NULL, "\t ,\n");
		if (tok==NULL) goto failed;
		red = (GLclampf)atof(tok);
		tok = strtok(NULL, "\t ,\n");
		if (tok==NULL) goto failed;
		green = (GLclampf)atof(tok);
		tok = strtok(NULL, "\t ,\n");
		if (tok==NULL) goto failed;
		blue = (GLclampf)atof(tok);
		tok = strtok(NULL, "\t ,\n");
		if (tok==NULL) goto failed;
		alpha = (GLclampf)atof(tok);
		glClearColor(red, green, blue, alpha);
		fprintf(GLcode_file,"\tglClearColor(%f,%f,%f,%f);\n", red, green, blue, alpha);
		return;
	}
	else if (strcmp(tok, "glClearDepth")==0)
	{
		tok = strtok(NULL, "\t ,\n");
		if (tok==NULL) goto failed;
		depth = (GLclampd)atof(tok);
		glClearDepth(depth);
		fprintf(GLcode_file,"\tglClearDepth(%f);\n", depth);
		return;
	}
	else if (strcmp(tok, "glClearStencil")==0)
	{
		tok = strtok(NULL, "\t ,\n");
		if (tok==NULL) goto failed;
		s = (GLint)atoi(tok);
		glClearStencil(s);
		fprintf(GLcode_file,"\tglClearStencil(%d);\n", s);
		return;
	}
	else if (strcmp(tok, "glColor3f")==0)
	{
		tok = strtok(NULL, "\t ,\n");
		if (tok==NULL) goto failed;
		red = (GLfloat)atof(tok);
		tok = strtok(NULL, "\t ,\n");
		if (tok==NULL) goto failed;
		green = (GLfloat)atof(tok);
		tok = strtok(NULL, "\t ,\n");
		if (tok==NULL) goto failed;
		blue = (GLfloat)atof(tok);
		glColor3f(red, green, blue);
		fprintf(GLcode_file,"\tglColor3f((GLfloat)%f,(GLfloat)%f,(GLfloat)%f);\n", red, green, blue);
		return;
	}
	else if (strcmp(tok, "glColorMask")==0)
	{
		tok = strtok(NULL, "\t ,\n");
		if (tok==NULL) goto failed;
		bred = (GLboolean)atoi(tok);
		tok = strtok(NULL, "\t ,\n");
		if (tok==NULL) goto failed;
		bgreen = (GLboolean)atoi(tok);
		tok = strtok(NULL, "\t ,\n");
		if (tok==NULL) goto failed;
		bblue = (GLboolean)atoi(tok);
		tok = strtok(NULL, "\t ,\n");
		if (tok==NULL) goto failed;
		balpha = (GLboolean)atoi(tok);
		glColorMask(bred, bgreen, bblue, balpha) ;
		fprintf(GLcode_file,"\tglColorMask(%d,%d,%d,%d);\n", red, green, blue, alpha);
		return;
	}
	else if (strcmp(tok, "glCopyPixels")==0)
	{
		tok = strtok(NULL, "\t ,\n");
		if (tok==NULL) goto failed;
		x = (GLint)atoi(tok);
		tok = strtok(NULL, "\t ,\n");
		if (tok==NULL) goto failed;
		y = (GLint)atoi(tok);
		tok = strtok(NULL, "\t ,\n");
		if (tok==NULL) goto failed;
		width = (GLsizei)atoi(tok);
		tok = strtok(NULL, "\t ,\n");
		if (tok==NULL) goto failed;
		height = (GLsizei)atoi(tok);
		tok = strtok(NULL, "\t ,\n");
		if (tok==NULL) goto failed;
		type = (GLenum)atoi(tok);
		glCopyPixels(x,y,width,height, type);
		fprintf(GLcode_file,"\tglCopyPixels(%d,%d,%d,%d,%d);\n", x,y,width,height, type);
		return;
	}
	else if (strcmp(tok, "glDeleteLists")==0)
	{
		tok = strtok(NULL, "\t ,\n");
		if (tok==NULL) goto failed;
		list = (GLint)atoi(tok);
		tok = strtok(NULL, "\t ,\n");
		if (tok==NULL) goto failed;
		range = (GLsizei)atoi(tok);
		glDeleteLists(list,  range);
		fprintf(GLcode_file,"\tglDeleteLists(%d,%d);\n", list, range);
		return;
	}
	else if (strcmp(tok, "glDepthFunc")==0)
	{
		tok = strtok(NULL, "\t ,\n");
		if (tok==NULL) goto failed;
		func = (GLenum)atoi(tok);
		glDepthFunc(func);
		fprintf(GLcode_file,"\tglDepthFunc(%d);\n", func);
		return;
	}
	else if (strcmp(tok, "glDepthMask")==0)
	{
		tok = strtok(NULL, "\t ,\n");
		if (tok==NULL) goto failed;
		flag = (GLboolean)atoi(tok);
		glDepthMask(flag);
		fprintf(GLcode_file,"\tglDepthMask(%d);\n", flag);
		return;
	}
	else if (strcmp(tok, "glDepthRange")==0)
	{
		tok = strtok(NULL, "\t ,\n");
		if (tok==NULL) goto failed;
		znear = (GLclampd)atof(tok);
		tok = strtok(NULL, "\t ,\n");
		if (tok==NULL) goto failed;
		zfar = (GLclampd)atof(tok);
		glDepthRange(znear,zfar);
		fprintf(GLcode_file,"\tglDepthRange(%f,%f);\n", znear,zfar);
		return;
	}
	else if (strcmp(tok, "glEnable")==0)
	{
		tok = strtok(NULL, "\t ,\n");
		if (tok==NULL) goto failed;
		cap = (GLenum)atoi(tok);
		glEnable(cap);
		fprintf(GLcode_file,"\tglEnable(%d);\n", cap);
		return;
	}
	else if (strcmp(tok, "glDisable")==0)
	{
		tok = strtok(NULL, "\t ,\n");
		if (tok==NULL) goto failed;
		cap = (GLenum)atoi(tok);
		glDisable(cap);
		fprintf(GLcode_file,"\tglDisable(%d);\n", cap);
		return;
	}
	else if (strcmp(tok, "glDrawBuffer")==0)
	{
		tok = strtok(NULL, "\t ,\n");
		if (tok==NULL) goto failed;
		mode = (GLenum)atoi(tok);
		glDrawBuffer(mode);
		fprintf(GLcode_file,"\tglDrawBuffer(%d);\n", mode);
		return;
	}
	else if (strcmp(tok, "glDrawPixels")==0)
	{
/////////
//	fprintf(GLjou_fd,"glDrawPixels\t%d,%d,%d,%d\n", width, height, format,  type);
		return;
	}
	else if (strcmp(tok, "glNewList")==0)
	{
		tok = strtok(NULL, "\t ,\n");
		if (tok==NULL) goto failed;
		list = (GLint)atoi(tok);
		tok = strtok(NULL, "\t ,\n");
		if (tok==NULL) goto failed;
		mode = (GLenum)atoi(tok);
		glNewList(list, mode);
		fprintf(GLcode_file,"\tglNewList(%d,%d);\n", list, mode);
		return;
	}
	else if (strcmp(tok, "glEndList")==0)
	{
		glEndList();
		fprintf(GLcode_file,"\tglEndList();\n");
		return;
	}
	else if (strcmp(tok, "glFeedbackBuffer")==0)
	{
//Yurong
		tok = strtok(NULL, "\t ,\n");
		if (tok==NULL) goto failed;
		range = (GLsizei)atoi(tok);
		tok = strtok(NULL, "\t ,\n");
		if (tok==NULL) goto failed;
		type = (GLenum)atoi(tok);
		glFeedbackBuffer(range, type,  buffer);
		fprintf(GLjou_fd,"glFeedbackBuffer\t%d,%d, buffer\n", range, type);
//////
////	fprintf(GLjou_fd,"glFeedbackBuffer\t%d,%d\n", size, type);
/////   glFeedbackBuffer_(size, type,  buffer)
		return;
	}
	else if (strcmp(tok, "glFinish")==0)
	{
		glFinish();  
		fprintf(GLcode_file,"\tglFinish();\n");
		return;
	}
	else if (strcmp(tok, "glFlush")==0)
	{
		glFlush();  
		fprintf(GLcode_file,"\tglFlush();\n");
		return;
	}
	else if (strcmp(tok, "glFrontFace")==0)
	{
		tok = strtok(NULL, "\t ,\n");
		if (tok==NULL) goto failed;
		mode = (GLenum)atoi(tok);
		glFrontFace(mode);
		fprintf(GLcode_file,"\tglFrontFace(%d);\n", mode);
		return;
	}
	else if (strcmp(tok, "glGenLists")==0)
	{
///////	glGenLists_(range)
///////	fprintf(GLjou_fd,"glGenLists\t%d\n", range);
		return;
	}
	else if (strcmp(tok, "glGetDoublev")==0)
	{
//////	fprintf(GLjou_fd,"glGetDoublev\t%d\n", pname);
		return;
	}
	else if (strcmp(tok, "glGetDoublev")==0)
	{
//////	fprintf(GLjou_fd,"glGetIntegerv\t%d\n", pname);
		return;
	}  
	else if (strcmp(tok, "glIsEnabled")==0)
	{
//////	fprintf(GLjou_fd,"glIsEnabled\t%d\n", cap);
		return;
	}  
	else if (strcmp(tok, "glIsList")==0)
	{
//////	fprintf(GLjou_fd,"glIsList\t%d\n", list);
		return;
	}  
	else if (strcmp(tok, "glLightf")==0)
	{
		tok = strtok(NULL, "\t ,\n");
		if (tok==NULL) goto failed;
		light = (GLenum)atoi(tok);
		tok = strtok(NULL, "\t ,\n");
		if (tok==NULL) goto failed;
		pname = (GLenum)atoi(tok);
		tok = strtok(NULL, "\t ,\n");
		if (tok==NULL) goto failed;
		fparam = (GLfloat)atof(tok);
		glLightf(light,pname, fparam);
		fprintf(GLcode_file,"\tglLightf(%d,%d,%f);\n", light,pname, fparam);
		return;
	}
	else if (strcmp(tok, "glLightfv")==0)
	{
		tok = strtok(NULL, "\t ,\n");
		if (tok==NULL) goto failed;
		light = (GLenum)atoi(tok);
		tok = strtok(NULL, "\t ,\n");
		if (tok==NULL) goto failed;
		pname = (GLenum)atoi(tok);

		tok = strtok(NULL, "\t ,\n");
		if (tok==NULL) goto failed;
		fparams[0] = (GLfloat)atof(tok);
		fprintf(GLcode_file,"\tfparams[0] = (GLfloat)%f;\n",  fparams[0]);
		if ((pname==GL_SPOT_DIRECTION) || (pname==GL_AMBIENT) || (pname==GL_DIFFUSE)
			|| (pname==GL_SPECULAR) || (pname==GL_POSITION))
		{
			tok = strtok(NULL, "\t ,\n");
			if (tok==NULL) goto failed;
			fparams[1] = (GLfloat)atof(tok);
			fprintf(GLcode_file,"\tfparams[1] = (GLfloat)%f;\n",  fparams[1]);
			tok = strtok(NULL, "\t ,\n");
			if (tok==NULL) goto failed;
			fparams[2] = (GLfloat)atof(tok);
			fprintf(GLcode_file,"\tfparams[2] = (GLfloat)%f;\n",  fparams[2]);
		}
		if ((pname==GL_AMBIENT) || (pname==GL_DIFFUSE)
			|| (pname==GL_SPECULAR) || (pname==GL_POSITION))
		{	
			tok = strtok(NULL, "\t ,\n");
			if (tok==NULL) goto failed;
			fparams[3] = (GLfloat)atof(tok);
			fprintf(GLcode_file,"\tfparams[3] = (GLfloat)%f;\n",  fparams[3]);
		}
		glLightfv(light,pname, fparams);
		fprintf(GLcode_file,"\tglLightfv(%d,%d,fparams);\n", light,pname);
		return;
	}
	else if (strcmp(tok, "glLightModeli")==0)
	{
		tok = strtok(NULL, "\t ,\n");
		if (tok==NULL) goto failed;
		pname = (GLenum)atoi(tok);
		tok = strtok(NULL, "\t ,\n");
		if (tok==NULL) goto failed;
		iparam = (GLenum)atoi(tok);
		glLightModeli(pname, iparam);
		fprintf(GLcode_file,"\tglLightModeli(%d,%d);\n", pname, iparam);
		return;
	}
 	else if (strcmp(tok, "glLineStipple")==0)
	{
		tok = strtok(NULL, "\t ,\n");
		if (tok==NULL) goto failed;
		factor = (GLint)atoi(tok);
		tok = strtok(NULL, "\t ,\n");
		if (tok==NULL) goto failed;
		pattern = (GLushort)atoi(tok);
		glLineStipple(factor, pattern);
		fprintf(GLcode_file,"\tglLineStipple(%d,%d);\n", factor, pattern);
		return;
	}
 	else if (strcmp(tok, "glLineWidth")==0)
	{
		tok = strtok(NULL, "\t ,\n");
		if (tok==NULL) goto failed;
		lwid = (GLfloat)atof(tok);
		glLineWidth(lwid);
		fprintf(GLcode_file,"\tglLineWidth(%f);\n", width);
		return;
	}
 	else if (strcmp(tok, "glListBase")==0)
	{
		tok = strtok(NULL, "\t ,\n");
		if (tok==NULL) goto failed;
		base = (GLuint)atoi(tok);
		glListBase(base);
		fprintf(GLcode_file,"\tglListBase(%d);\n", base);
		return;
	}
 	else if (strcmp(tok, "glLoadIdentity")==0)
	{
		glLoadIdentity();
		fprintf(GLcode_file,"\tglLoadIdentity();\n");
		return;
	}
	else if (strcmp(tok, "glLoadMatrixd")==0)
	{
		for (i=0; i<16;i++)
		{
			tok = strtok(NULL, "\t ,\n");
			if (tok==NULL) goto failed;
			dmat[i] = (GLdouble)atof(tok);
			fprintf(GLcode_file,"\tdmat[%d] = %f;\n",  i, dmat[i]);
		}
		glLoadMatrixd(dmat);
		fprintf(GLcode_file,"\tglLoadMatrixd(dmat);\n");
		return;
	}
	else if (strcmp(tok, "glMateriali")==0)
	{
		tok = strtok(NULL, "\t ,\n");
		if (tok==NULL) goto failed;
		face = (GLenum)atoi(tok);
		tok = strtok(NULL, "\t ,\n");
		if (tok==NULL) goto failed;
		pname = (GLenum)atoi(tok);
		tok = strtok(NULL, "\t ,\n");
		if (tok==NULL) goto failed;
		iparam = (GLint)atoi(tok);
		glMateriali(face, pname, iparam);
		fprintf(GLcode_file,"\tglMateriali(%d,%d,%d);\n", face, pname, iparam);
		return;
	}
	else if (strcmp(tok, "glMaterialfv")==0)
	{
		tok = strtok(NULL, "\t ,\n");
		if (tok==NULL) goto failed;
		face = (GLenum)atoi(tok);
		tok = strtok(NULL, "\t ,\n");
		if (tok==NULL) goto failed;
		pname = (GLenum)atoi(tok);
		tok = strtok(NULL, "\t ,\n");
		if (tok==NULL) goto failed;
		fparams[0] = (GLfloat)atof(tok);
		fprintf(GLcode_file,"\tfparams[0] = %f;\n",  fparams[0]);
		if ((pname==GL_COLOR_INDEXES) || (pname==GL_AMBIENT) || (pname==GL_DIFFUSE)
			|| (pname==GL_SPECULAR) || (pname==GL_EMISSION)
			|| (pname==GL_AMBIENT_AND_DIFFUSE ))
		{
			tok = strtok(NULL, "\t ,\n");
			if (tok==NULL) goto failed;
			fparams[1] = (GLfloat)atof(tok);
			fprintf(GLcode_file,"\tfparams[1] = (GLfloat)%f;\n",  fparams[1]);
			tok = strtok(NULL, "\t ,\n");
			if (tok==NULL) goto failed;
			fparams[2] = (GLfloat)atof(tok);
			fprintf(GLcode_file,"\tfparams[2] = (GLfloat)%f;\n",  fparams[2]);
		}
		if ((pname==GL_AMBIENT) || (pname==GL_DIFFUSE)
			|| (pname==GL_SPECULAR) || (pname==GL_EMISSION)
			|| (pname==GL_AMBIENT_AND_DIFFUSE ))
		{
			tok = strtok(NULL, "\t ,\n");
			if (tok==NULL) goto failed;
			fparams[3] = (GLfloat)atof(tok);
			fprintf(GLcode_file,"\tfparams[3] = (GLfloat)%f;\n",  fparams[3]);
		}
		glMaterialfv(face, pname, fparams);
		fprintf(GLcode_file,"\tglMaterialfv(%d,%d,fparams);\n", face, pname);
		return;
	}
	else if (strcmp(tok, "glMatrixMode")==0)
	{
		tok = strtok(NULL, "\t ,\n");
		if (tok==NULL) goto failed;
		mode = (GLenum)atoi(tok);
		glMatrixMode(mode);
		fprintf(GLcode_file,"\tglMatrixMode(%d);\n", mode);
		return;
	}
	else if (strcmp(tok, "glMultMatrixd")==0)
	{
		for (i=0; i<16;i++)
		{
			tok = strtok(NULL, "\t ,\n");
			if (tok==NULL) goto failed;
			dmat[i] = (GLdouble)atof(tok);
			fprintf(GLcode_file,"\tdmat[%d] = %f;\n",  i, dmat[i]);
		}
		glMultMatrixd(dmat);
		fprintf(GLcode_file,"\tglMultMatrixd(dmat);\n");
		return;
	}
	else if (strcmp(tok, "glNormal3f")==0)
	{
		tok = strtok(NULL, "\t ,\n");
		if (tok==NULL) goto failed;
		nx = (GLfloat)atof(tok);
		tok = strtok(NULL, "\t ,\n");
		if (tok==NULL) goto failed;
		ny = (GLfloat)atof(tok);
		tok = strtok(NULL, "\t ,\n");
		if (tok==NULL) goto failed;
		nz = (GLfloat)atof(tok);
		glNormal3f(nx,ny,nz);
		fprintf(GLcode_file,"\tglNormal3f((GLfloat)%f,(GLfloat)%f,(GLfloat)%f);\n", nx, ny, nz);
		return;
	}
	else if (strcmp(tok, "glOrtho")==0)
	{
		tok = strtok(NULL, "\t ,\n");
		if (tok==NULL) goto failed;
		left1 = (GLdouble)atof(tok);
		tok = strtok(NULL, "\t ,\n");
		if (tok==NULL) goto failed;
		right1 = (GLdouble)atof(tok);
		tok = strtok(NULL, "\t ,\n");
		if (tok==NULL) goto failed;
		bottom1 = (GLdouble)atof(tok);
		tok = strtok(NULL, "\t ,\n");
		if (tok==NULL) goto failed;
		top1 = (GLdouble)atof(tok);
		tok = strtok(NULL, "\t ,\n");
		if (tok==NULL) goto failed;
		near1 = (GLdouble)atof(tok);
		tok = strtok(NULL, "\t ,\n");
		if (tok==NULL) goto failed;
		far1 = (GLdouble)atof(tok);
		glOrtho(left1, right1, bottom1, top1, near1, far1);
		fprintf(GLcode_file,"\tglOrtho(%f,%f,%f,%f,%f,%f);\n", 
					left1, right1, bottom1, top1, near1, far1);
		return;
	}
	else if (strcmp(tok, "glPixelStorei")==0)
	{
		tok = strtok(NULL, "\t ,\n");
		if (tok==NULL) goto failed;
		pname = (GLenum)atoi(tok);
		tok = strtok(NULL, "\t ,\n");
		if (tok==NULL) goto failed;
		iparam = (GLenum)atoi(tok);
		glPixelStorei(pname, iparam);
		fprintf(GLcode_file,"\tglPixelStorei(%d,%d);\n", pname, iparam);
		return;
	}
	else if (strcmp(tok, "glPixelTransferf")==0)
	{
		tok = strtok(NULL, "\t ,\n");
		if (tok==NULL) goto failed;
		pname = (GLenum)atoi(tok);
		tok = strtok(NULL, "\t ,\n");
		if (tok==NULL) goto failed;
		fparam = (GLfloat)atof(tok);
		glPixelTransferf(pname, fparam);
		fprintf(GLcode_file,"\tglPixelTransferf(%d,%d);\n", pname, fparam);
		return;
	}
	else if (strcmp(tok, "glPolygonMode")==0)
	{
		tok = strtok(NULL, "\t ,\n");
		if (tok==NULL) goto failed;
		face = (GLenum)atoi(tok);
		tok = strtok(NULL, "\t ,\n");
		if (tok==NULL) goto failed;
		mode = (GLenum)atoi(tok);
		glPolygonMode(face, mode);
		fprintf(GLcode_file,"\tglPolygonMode(%d,%d);\n", face, mode);
		return;
	}
	else if (strcmp(tok, "glPushAttrib")==0)
	{
		tok = strtok(NULL, "\t ,\n");
		if (tok==NULL) goto failed;
		mask = (GLbitfield)atoi(tok);
		glPushAttrib(mask);
		fprintf(GLcode_file,"\tglPopAttrib();\n");
		return;
	}
	else if (strcmp(tok, "glPopAttrib")==0)
	{
		glPopAttrib();
		fprintf(GLcode_file,"\tglPopAttrib();\n");
		return;
	}
	else if (strcmp(tok, "glPushMatrix")==0)
	{
		glPushMatrix();
		fprintf(GLcode_file,"\tglPushMatrix();\n");
		return;
	}
	else if (strcmp(tok, "glPopMatrix")==0)
	{
		glPopMatrix();
		fprintf(GLcode_file,"\tglPopMatrix();\n");
		return;
	}
	else if (strcmp(tok, "glRasterPos2f")==0)
	{
		tok = strtok(NULL, "\t ,\n");
		if (tok==NULL) goto failed;
		fx = (GLfloat)atof(tok);
		tok = strtok(NULL, "\t ,\n");
		if (tok==NULL) goto failed;
		fy = (GLfloat)atof(tok);
		glRasterPos2f(fx,fy);
		fprintf(GLcode_file,"\tglRasterPos2f(%f,%f);\n", fx,fy);
		return;
	}
	else if (strcmp(tok, "glRasterPos2i")==0)
	{
		tok = strtok(NULL, "\t ,\n");
		if (tok==NULL) goto failed;
		x = (GLint)atoi(tok);
		tok = strtok(NULL, "\t ,\n");
		if (tok==NULL) goto failed;
		y = (GLint)atoi(tok);
		glRasterPos2i(x,y);
		fprintf(GLcode_file,"\tglRasterPos2i(%d,%d);\n", x,y);
		return;
	}
	else if (strcmp(tok, "glRasterPos3f")==0)
	{
		tok = strtok(NULL, "\t ,\n");
		if (tok==NULL) goto failed;
		fx = (GLfloat)atof(tok);
		tok = strtok(NULL, "\t ,\n");
		if (tok==NULL) goto failed;
		fy = (GLfloat)atof(tok);
		tok = strtok(NULL, "\t ,\n");
		if (tok==NULL) goto failed;
		fz = (GLfloat)atof(tok);
		glRasterPos3f(fx,fy,fz);
		fprintf(GLcode_file,"\tglRasterPos3f((GLfloat)%f,(GLfloat)%f,(GLfloat)%f);\n", fx,fy,fz);
		return;
	}
	else if (strcmp(tok, "glRasterPos3i")==0)
	{
		tok = strtok(NULL, "\t ,\n");
		if (tok==NULL) goto failed;
		x = (GLint)atoi(tok);
		tok = strtok(NULL, "\t ,\n");
		if (tok==NULL) goto failed;
		y = (GLint)atoi(tok);
		tok = strtok(NULL, "\t ,\n");
		if (tok==NULL) goto failed;
		z = (GLint)atoi(tok);
		glRasterPos3i(x,y,z);
		fprintf(GLcode_file,"\tglRasterPos3i(%d,%d,%d);\n", x,y,z);
		return;
	}
	else if (strcmp(tok, "glReadBuffer")==0)
	{
		tok = strtok(NULL, "\t ,\n");
		if (tok==NULL) goto failed;
		mode = (GLenum)atoi(tok);
		glReadBuffer(mode);
		fprintf(GLcode_file,"\tglReadBuffer(%d);\n", mode);
		return;
	}
	else if (strcmp(tok, "glReadPixels")==0)
	{
//////glReadPixels_(x,y,width, height,format,type,pixels)
///// fprintf(GLjou_fd,"glReadPixels\t%d,%d,%d,%d,%d,%d\n", 
/////		x,y,width, height,format,type);
		return;
	}
	else if (strcmp(tok, "glRecti")==0)
	{
		tok = strtok(NULL, "\t ,\n");
		if (tok==NULL) goto failed;
		x1 = (GLint)atoi(tok);
		tok = strtok(NULL, "\t ,\n");
		if (tok==NULL) goto failed;
		y1 = (GLint)atoi(tok);
		tok = strtok(NULL, "\t ,\n");
		if (tok==NULL) goto failed;
		x2 = (GLint)atoi(tok);
		tok = strtok(NULL, "\t ,\n");
		if (tok==NULL) goto failed;
		y2 = (GLint)atoi(tok);
		glRecti(x1,y1,x2,y2);
		fprintf(GLcode_file,"\tglRecti(%d,%d,%d,%d);\n", x1, y1,x2,y2);
		return;
	}
	else if (strcmp(tok, "glRenderMode")==0)
	{
//yurong
		tok = strtok(NULL, "\t ,\n");
		if (tok==NULL) goto failed;
		mode = (GLenum)atoi(tok);
		if (mode==GL_RENDER)
		{
			glRenderMode(mode);
			fprintf(GLjou_fd,"glRenderMode\t%d\n", mode); 
		}
		cur_RenderMode = mode;
/////	GLint glRenderMode_(mode)
/////fprintf(GLjou_fd,"glRenderMode\t%d\n", mode); 
		return;
	}
	else if (strcmp(tok, "glRotated")==0)
	{
		tok = strtok(NULL, "\t ,\n");
		if (tok==NULL) goto failed;
		angle = (GLdouble)atof(tok);
		tok = strtok(NULL, "\t ,\n");
		if (tok==NULL) goto failed;
		dx = (GLdouble)atof(tok);
		tok = strtok(NULL, "\t ,\n");
		if (tok==NULL) goto failed;
		dy = (GLdouble)atof(tok);
		tok = strtok(NULL, "\t ,\n");
		if (tok==NULL) goto failed;
		dz = (GLdouble)atof(tok);
		glRotated(angle, x,y,z);
		fprintf(GLcode_file,"\tglRotated(%f,%f,%f,%f)\n", angle, x,y,z);
		return;
	}
	else if (strcmp(tok, "glScaled")==0)
	{
		tok = strtok(NULL, "\t ,\n");
		if (tok==NULL) goto failed;
		dx = (GLdouble)atof(tok);
		tok = strtok(NULL, "\t ,\n");
		if (tok==NULL) goto failed;
		dy = (GLdouble)atof(tok);
		tok = strtok(NULL, "\t ,\n");
		if (tok==NULL) goto failed;
		dz = (GLdouble)atof(tok);
		glScaled(x,y,z);
		fprintf(GLcode_file,"\tglScaled(%f,%f,%f);\n", x,y,z);
		return;
	}
	else if (strcmp(tok, "glScissor")==0)
	{
		tok = strtok(NULL, "\t ,\n");
		if (tok==NULL) goto failed;
		x = (GLint)atoi(tok);
		tok = strtok(NULL, "\t ,\n");
		if (tok==NULL) goto failed;
		y = (GLint)atoi(tok);
		tok = strtok(NULL, "\t ,\n");
		if (tok==NULL) goto failed;
		width = (GLint)atoi(tok);
		tok = strtok(NULL, "\t ,\n");
		if (tok==NULL) goto failed;
		height = (GLint)atoi(tok);
		glScissor(x,y,width,height);
		fprintf(GLcode_file,"\tglScissor(%d,%d,%d,%d);\n", x,y,width,height);
		return;
	}
	else if (strcmp(tok, "glShadeModel")==0)
	{
		tok = strtok(NULL, "\t ,\n");
		if (tok==NULL) goto failed;
		mode = (GLenum)atoi(tok);
		glShadeModel(mode);
		fprintf(GLcode_file,"\tglShadeModel(%d);\n", mode);
		return;
	}
	else if (strcmp(tok, "glStencilFunc")==0)
	{
		tok = strtok(NULL, "\t ,\n");
		if (tok==NULL) goto failed;
		func = (GLenum)atoi(tok);
		tok = strtok(NULL, "\t ,\n");
		if (tok==NULL) goto failed;
		ref = (GLint)atoi(tok);
		tok = strtok(NULL, "\t ,\n");
		if (tok==NULL) goto failed;
		mask3 = (GLuint)atoi(tok);
		glStencilFunc(func, ref, mask3);
		fprintf(GLcode_file,"\tglStencilFunc(%d,%d,%d);\n", func, ref, mask);
		return;
	}
	else if (strcmp(tok, "glStencilMask")==0)
	{
		tok = strtok(NULL, "\t ,\n");
		if (tok==NULL) goto failed;
		mask3 = (GLuint)atoi(tok);
		glStencilMask(mask3);
		fprintf(GLcode_file,"\tglStencilMask(%d)\n", mask);
		return;
	}
	else if (strcmp(tok, "glStencilOp")==0)
	{
		tok = strtok(NULL, "\t ,\n");
		if (tok==NULL) goto failed;
		fail = (GLenum)atoi(tok);
		tok = strtok(NULL, "\t ,\n");
		if (tok==NULL) goto failed;
		zfail = (GLenum)atoi(tok);
		tok = strtok(NULL, "\t ,\n");
		if (tok==NULL) goto failed;
		zpass = (GLenum)atoi(tok);
		glStencilOp(fail,zfail, zpass);
		fprintf(GLcode_file,"\tglStencilOp(%d,%d,%d)\n", fail,zfail, zpass);
		return;
	}
	else if (strcmp(tok, "glTranslatef")==0)
	{
		tok = strtok(NULL, "\t ,\n");
		if (tok==NULL) goto failed;
		fx = (GLfloat)atof(tok);
		tok = strtok(NULL, "\t ,\n");
		if (tok==NULL) goto failed;
		fy = (GLfloat)atof(tok);
		tok = strtok(NULL, "\t ,\n");
		if (tok==NULL) goto failed;
		fz = (GLfloat)atof(tok);
		glTranslatef(fx,fy,fz);
		fprintf(GLcode_file,"\tglTranslatef(%f,%f,%f);\n", fx,fy,fz);
		return;
	}
	else if (strcmp(tok, "glVertex2f")==0)
	{
		tok = strtok(NULL, "\t ,\n");
		if (tok==NULL) goto failed;
		fx = (GLfloat)atof(tok);
		tok = strtok(NULL, "\t ,\n");
		if (tok==NULL) goto failed;
		fy = (GLfloat)atof(tok);
		glVertex2f(fx,fy);
		fprintf(GLcode_file,"\tglVertex2f(%f,%f);\n", fx,fy);
		return;
	}
	else if (strcmp(tok, "glVertex2i")==0)
	{
		tok = strtok(NULL, "\t ,\n");
		if (tok==NULL) goto failed;
		x = (GLint)atoi(tok);
		tok = strtok(NULL, "\t ,\n");
		if (tok==NULL) goto failed;
		y = (GLint)atoi(tok);
		glVertex2i(x,y);
		fprintf(GLcode_file,"\tglVertex2i(%d,%d);\n", x,y);
		return;
	}
	else if (strcmp(tok, "glVertex3f")==0)
	{
		tok = strtok(NULL, "\t ,\n");
		if (tok==NULL) goto failed;
		fx = (GLfloat)atof(tok);
		tok = strtok(NULL, "\t ,\n");
		if (tok==NULL) goto failed;
		fy = (GLfloat)atof(tok);
		tok = strtok(NULL, "\t ,\n");
		if (tok==NULL) goto failed;
		fz = (GLfloat)atof(tok);
		glVertex3f(fx,fy,fz);
		fprintf(GLcode_file,"\tglVertex3f((GLfloat)%f,(GLfloat)%f,(GLfloat)%f);\n", fx,fy,fz);
		return;
	}
	else if (strcmp(tok, "glVertex3i")==0)
	{
		tok = strtok(NULL, "\t ,\n");
		if (tok==NULL) goto failed;
		x = (GLint)atoi(tok);
		tok = strtok(NULL, "\t ,\n");
		if (tok==NULL) goto failed;
		y = (GLint)atoi(tok);
		tok = strtok(NULL, "\t ,\n");
		if (tok==NULL) goto failed;
		z = (GLint)atoi(tok);
		glVertex3i(x,y,z);
		fprintf(GLcode_file,"\tglVertex3i(%d,%d,%d);\n", x,y,z);
		return;
	}
	else if (strcmp(tok, "glViewport")==0)
	{
		tok = strtok(NULL, "\t ,\n");
		if (tok==NULL) goto failed;
		x = (GLint)atoi(tok);
		tok = strtok(NULL, "\t ,\n");
		if (tok==NULL) goto failed;
		y = (GLint)atoi(tok);
		tok = strtok(NULL, "\t ,\n");
		if (tok==NULL) goto failed;
		width = (GLsizei)atoi(tok);
		tok = strtok(NULL, "\t ,\n");
		if (tok==NULL) goto failed;
		height = (GLsizei)atoi(tok);
		glViewport(x,y,width,height);
		fprintf(GLcode_file,"\tglViewport(%d,%d,%d,%d);\n", x,y,width,height);
		return;
	}
	else if (strcmp(tok, "gluPickMatrix")==0)
	{
///////
//Yurong
		tok = strtok(NULL, "\t ,\n");
		if (tok==NULL) goto failed;
		d1 = (GLdouble)atof(tok);
		tok = strtok(NULL, "\t ,\n");
		if (tok==NULL) goto failed;
		d2 = (GLdouble)atof(tok);
		tok = strtok(NULL, "\t ,\n");
		if (tok==NULL) goto failed;
		d3 = (GLdouble)atof(tok);
		tok = strtok(NULL, "\t ,\n");
		if (tok==NULL) goto failed;
		d4 = (GLdouble)atof(tok);
		tok = strtok(NULL, "\t ,\n");
		if (tok==NULL) goto failed;
		iparams[0] = atoi(tok);
		tok = strtok(NULL, "\t ,\n");
		if (tok==NULL) goto failed;
		iparams[1] = atoi(tok);
		tok = strtok(NULL, "\t ,\n");
		if (tok==NULL) goto failed;
		iparams[2] = atoi(tok);
		tok = strtok(NULL, "\t ,\n");
		if (tok==NULL) goto failed;
		iparams[3] = atoi(tok);
		gluPickMatrix(x,y,width,height,iparams);
		fprintf(GLcode_file,"gluPickMatrix\t%f,%f,%f,%f,d,%d,%d,%d\n", 
			x,y,width,height,iparams[0],iparams[1],iparams[2],iparams[3]);
		return;
	}
	else if (strcmp(tok, "glPassThrough")==0)
	{
		tok = strtok(NULL, "\t ,\n");
		if (tok==NULL) goto failed;
		token = (GLfloat)atof(tok);
		glPassThrough(token);
		fprintf(GLcode_file,"\tglPassThrough(%f);\n", token);
		return;
	}
	else if (strcmp(tok, "SwapBuffers")==0)
	{
		SwapBuffers(wglGetCurrentDC());
		fprintf(GLcode_file, "\tSwapBuffers(wglGetCurrentDC());\n");
		return;
	}
	goto done;
failed:;
	MessageBox(NULL, string, "Error", MB_OK);
done:;
}



