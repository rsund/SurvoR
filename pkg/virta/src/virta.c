#include "sqlite3ext.h"
SQLITE_EXTENSION_INIT1

#include <stdio.h>
#include <string.h>


/***************************************************************************
  Code of test_ functions posted to sqlite-users@sqlite.org list by
    Clemens Ladisch on Sat, 22 Sep 2012 00:12
****************************************************************************/
static int test_CreateConnect(sqlite3 *db, void *pAux, int argc, const char *const argv[], sqlite3_vtab **ppVTab, char **pzErr)
{
        int err;

        *ppVTab = sqlite3_malloc(sizeof(struct sqlite3_vtab));
        if (!*ppVTab)
                return SQLITE_NOMEM;
        memset(*ppVTab, 0, sizeof(**ppVTab));

        err = sqlite3_declare_vtab(db, "CREATE TABLE x(ID INTEGER, Name TEXT)");
        if (err != SQLITE_OK) {
                sqlite3_free(*ppVTab);
                *ppVTab = NULL;
                return err;
        }

        return SQLITE_OK;
}

static int test_BestIndex(sqlite3_vtab *pVTab, sqlite3_index_info *index_info)
{
        index_info->idxNum = 0;
        index_info->idxStr = "";
        index_info->estimatedCost = 1000000;
        return SQLITE_OK;
}

static int test_DisconnectDestroy(sqlite3_vtab *pVTab)
{
        sqlite3_free(pVTab);
        return SQLITE_OK;
}

struct test_cursor {
        sqlite3_vtab_cursor base;
        int row;
};

static int test_Open(sqlite3_vtab *pVTab, sqlite3_vtab_cursor **ppCursor)
{
        struct test_cursor *cursor;

        cursor = sqlite3_malloc(sizeof(struct test_cursor));
        if (!cursor)
                return SQLITE_NOMEM;
        memset(cursor, 0, sizeof(*cursor));

        *ppCursor = &cursor->base;

        return SQLITE_OK;
}

static int test_Close(sqlite3_vtab_cursor *pCursor)
{
        sqlite3_free(pCursor);
        return SQLITE_OK;
}

static int test_Eof(sqlite3_vtab_cursor *pCursor)
{
        struct test_cursor *cursor = (void *)pCursor;

        return cursor->row >= 5;
}

static int test_Filter(sqlite3_vtab_cursor *pCursor, int idxNum, const char *idxStr, int argc, sqlite3_value **argv)
{
        struct test_cursor *cursor = (void *)pCursor;

        cursor->row = 0;
        return SQLITE_OK;
}

static int test_Next(sqlite3_vtab_cursor *pCursor)
{
        struct test_cursor *cursor = (void *)pCursor;

        cursor->row++;
        return SQLITE_OK;
}

static int test_Column(sqlite3_vtab_cursor *pCursor, sqlite3_context *context, int N)
{
        struct test_cursor *cursor = (void *)pCursor;
        char str[16];

        switch (N) {
        case 0:
                sqlite3_result_int(context, 42 + cursor->row);
                break;
        case 1:
                sprintf(str, "row%d", cursor->row);
                sqlite3_result_text(context, str, -1, SQLITE_TRANSIENT);
                break;
        }
        return SQLITE_OK;
}

static int test_Rowid(sqlite3_vtab_cursor *pCursor, sqlite_int64 *pRowid)
{
        struct test_cursor *cursor = (void *)pCursor;

        *pRowid = cursor->row;
        return SQLITE_OK;
}

static int test_Rename(sqlite3_vtab* pVTab, const char *zNew)
{
        return SQLITE_OK;
}

static const struct sqlite3_module test_module = { 
		.iVersion = 1, 
		.xCreate = test_CreateConnect, 
		.xConnect = test_CreateConnect, 
		.xBestIndex = test_BestIndex, 
		.xDisconnect = test_DisconnectDestroy, 
		.xDestroy = test_DisconnectDestroy, 
		.xOpen = test_Open, 
		.xClose = test_Close, 
		.xFilter = test_Filter, 
		.xNext = test_Next, 
		.xEof = test_Eof, 
		.xColumn = test_Column, 
		.xRowid = test_Rowid, 
		.xRename = test_Rename, 
};


#include "muste.h"
#include "survo.h"
#include "survodat.h"
#include <R.h>
#include <R_ext/Rdynload.h>

static char xbuf[LLENGTH];
static char ybuf[LLENGTH];
// static SURVO_DATA svodata;

typedef struct
	{
    sqlite3_vtab vtab;
    SURVO_DATA *svodata;
	} svo_vtab;

typedef struct {
        sqlite3_vtab_cursor base;
        long row;
} svo_cursor;


int(*data_open3)(char *,SURVO_DATA *,int,int,int,int);
void(*data_close)(SURVO_DATA *);
int(*muste_iconv)(char *,char *,char *);
int(*fi_load)(SURVO_DATA_FILE *,long,int,double *);
int(*fi_alpha_load)(SURVO_DATA_FILE *,long,int,char *);
void*(*muste_malloc)(size_t);
int(*muste_free)(void *);

static void svo_data_close(SURVO_DATA *svo)
	{
	data_close(svo);
//	free(svo);
	}

static SURVO_DATA *svo_data_open(char *file,int p1,int p2,int p3,int kirjoitus)
	{
	int i;

    SURVO_DATA *svo = muste_malloc(sizeof(SURVO_DATA));
	if (svo==NULL) return NULL;
//	memset(svo,0,sizeof(SURVO_DATA));
	
	i=data_open3(file,svo,p1,p2,p3,kirjoitus);
	if (i<0) 
		{ 
//		muste_free(svo);
		// svo_data_close(svo);
		return NULL;
		}		

    return svo;
	}	
		


static int svo_connect(sqlite3 *db, void *pAux, int argc, const char *const argv[], sqlite3_vtab **ppVTab, char **pzErr)
	{
    (void) pAux;    /* Unused */
	int i,k,vi,nvar;
	char ch;
	char *dot,*para;
	SURVO_DATA *svodata;
	svo_vtab *vtab;


    if ( argc < 4 )
    	{
        *pzErr = sqlite3_mprintf( "No input file!" );
        return SQLITE_ERROR;
    	}

//	Rprintf( "Target table is '%s' on '%s' database.\n", argv[2], argv[1] );
	
	
 
    strncpy(xbuf,argv[3],LLENGTH);
    para=xbuf+strlen(xbuf)-1;
    while (*para==' ' || *para=='"') *para--=EOS;
    para=xbuf;
    while (*para==' ' || *para=='"') para++;
//	Rprintf( "Using '%s' as input file.\n", para );
    svodata=svo_data_open(para,0,1,1,0);
	if (svodata==NULL)
		{
		*pzErr = sqlite3_mprintf( "Error opening input file '%s'!", para );
        return SQLITE_ERROR;
		}

	nvar=svodata->m;
	if (nvar==0)
    	{	
        svo_data_close(svodata);
        *pzErr = sqlite3_mprintf( "Invalid or empty SVO file!" );
        return SQLITE_ERROR;
    	}

    vtab=(svo_vtab *)sqlite3_malloc(sizeof(svo_vtab));
    if (vtab==NULL)
    	{
        svo_data_close(svodata);
        return SQLITE_NOMEM;;
    	}
    memset(vtab,0,sizeof(svo_vtab));
    	
    vtab->svodata=svodata;

	strcpy(ybuf,"CREATE TABLE x( ");
	for(i = 0; i < nvar; i++)
		{
		if (i>0) strcat(ybuf,", ");	
		vi=i; // vi=d.v[i];

		do { dot=strchr(svodata->varname[vi],'.');
		if (dot!=NULL) *dot='_';
		} while (dot!=NULL);
		do { dot=strchr(svodata->varname[vi],'~');
		if (dot!=NULL) *dot='_';
		} while (dot!=NULL);
	    ch=*(svodata->varname[vi]);
	    if (ch>='0' && ch<='9') snprintf(xbuf,svodata->d2.l-1,"_%s",svodata->varname[vi]); 		
	    else snprintf(xbuf,svodata->d2.l-1,"%s",svodata->varname[vi]);
	    muste_iconv(xbuf,"UTF-8","CP850");
	    xbuf[8]=EOS; k=7;
	    while (xbuf[k]==' ' && k>0) xbuf[k--]=EOS;		

		strcat(ybuf,xbuf);

		switch (svodata->vartype[vi][0])
			{
			case '4':
			case '8':
				strcat(ybuf," REAL");
				break;
			case '1':
			case '2':
				strcat(ybuf," INT");
				break;
			default:
				strcat(ybuf," TEXT");
				break;
			}
		
		}
	strcat(ybuf," )");

// Rprintf("\ndeclaration: %s",ybuf);

    i = sqlite3_declare_vtab( db, ybuf );
    if ( i != SQLITE_OK )
    {
        *pzErr = sqlite3_mprintf( "declare_vtab: SQL error: '%s'!", ybuf );
        svo_data_close(svodata);
        sqlite3_free(vtab);
        return SQLITE_ERROR;
    }

    *ppVTab = &vtab->vtab;
    *pzErr  = NULL;	

    return SQLITE_OK;
	}

static int svo_disconnect(sqlite3_vtab *pVTab)
	{
	svo_vtab *tab=(svo_vtab *)pVTab;
	svo_data_close(tab->svodata);
	sqlite3_free(tab);
    return SQLITE_OK;
	}

static int svo_opencursor(sqlite3_vtab *pVTab, sqlite3_vtab_cursor **ppCursor)
	{
	svo_cursor *cursor;

	cursor = sqlite3_malloc(sizeof(svo_cursor));
	if (cursor==NULL) return SQLITE_NOMEM;
	memset(cursor, 0, sizeof(svo_cursor));

	*ppCursor=&cursor->base;

	return SQLITE_OK;
	}

static int svo_closecursor(sqlite3_vtab_cursor *pCursor)
	{
	sqlite3_free(pCursor);
	return SQLITE_OK;
	}


static int svo_eof(sqlite3_vtab_cursor *pCursor)
	{
    svo_cursor *cur = (svo_cursor *) pCursor;
    svo_vtab   *tab = (svo_vtab *) cur->base.pVtab;
    return cur->row > tab->svodata->d2.n;
	}


static int svo_filter(sqlite3_vtab_cursor *pCursor, int idxNum, const char *idxStr, int argc, sqlite3_value **argv)
	{
    (void) idxNum;  /* Unused */
    (void) idxStr;  /* Unused */
    (void) argc;    /* Unused */
    (void) argv;    /* Unused */

    svo_cursor *cur = (svo_cursor *) pCursor;
//    svo_vtab   *tab = (svo_vtab *) cur->base.pVtab;
	cur->row=1;
    return SQLITE_OK;
	}

static int svo_nextrow(sqlite3_vtab_cursor *pCursor)
	{
    svo_cursor *cur = (svo_cursor *) pCursor;
	cur->row++;
    return SQLITE_OK;
	}

static int svo_readcolumn(sqlite3_vtab_cursor *pCursor, sqlite3_context *context, int N)
	{
	int j,vi;
	double a;
    svo_cursor *cur = (svo_cursor *) pCursor;
    svo_vtab   *tab = (svo_vtab *) cur->base.pVtab;

	j=cur->row;
	vi=N; // d.v[i];
	a=0;        	
            	
	switch (tab->svodata->vartype[vi][0])
		{
		case '4':
		case '8': 				
		fi_load(&tab->svodata->d2,j,vi,&a);
		if (a==MISSING8) sqlite3_result_null(context);             
		else sqlite3_result_double(context,a);
		break;
		case '1':
		case '2':
		fi_load(&tab->svodata->d2,j,vi,&a);
		if (a==MISSING8) sqlite3_result_null(context);		    	
		else sqlite3_result_int(context,(int)a);
		break;
		default:
		fi_alpha_load(&tab->svodata->d2,j,vi,xbuf);
		muste_iconv(xbuf,"UTF-8","CP850");
		sqlite3_result_text(context, xbuf, -1, SQLITE_TRANSIENT);		    	
		break;
		}		
        return SQLITE_OK;
	}

static int svo_rowid(sqlite3_vtab_cursor *pCursor, sqlite_int64 *pRowid)
	{
    svo_cursor *cur = (svo_cursor *) pCursor;
	*pRowid=cur->row;
    return SQLITE_OK;
	}

static const struct sqlite3_module svo_module = { 
		.iVersion = 1, 
		.xCreate = svo_connect, 
		.xConnect = svo_connect, 
		.xBestIndex = test_BestIndex, 
		.xDisconnect = svo_disconnect, 
		.xDestroy = svo_disconnect, 
		.xOpen = svo_opencursor, 
		.xClose = svo_closecursor, 
		.xFilter = svo_filter, 
		.xNext = svo_nextrow, 
		.xEof = svo_eof, 
		.xColumn = svo_readcolumn, 
		.xRowid = svo_rowid, 
		.xRename = test_Rename, 
};



/****************************************************************************/
/*
 * This is the function called by SQLite3 when loading the modules.
 */

int sqlite3_extension_init( sqlite3 *db, char **pzErrMsg,
                            const sqlite3_api_routines *pApi )
{
    (void)pzErrMsg; /* unused */

	data_open3=(int(*)(char *,SURVO_DATA *,int,int,int,int))R_GetCCallable("muste", "data_open3");
//	data_open3=(int(*)(char *,SURVO_DATA *,int,int,int,int))R_FindSymbol("data_open3","muste",NULL);
	data_close=(void(*)(SURVO_DATA *))R_GetCCallable("muste", "data_close");
	muste_iconv=(int(*)(char *,char *,char *))R_GetCCallable("muste", "muste_iconv");
	fi_load=(int(*)(SURVO_DATA_FILE *,long,int,double *))R_GetCCallable("muste", "fi_load");
	fi_alpha_load=(int(*)(SURVO_DATA_FILE *,long,int,char *))R_GetCCallable("muste", "fi_alpha_load");
	muste_malloc=(void*(*)(size_t))R_GetCCallable("muste", "muste_malloc");
	muste_free=(int(*)(void *))R_GetCCallable("muste", "muste_free");

    SQLITE_EXTENSION_INIT2( pApi );

    sqlite3_create_module( db, "TEST", &test_module, NULL );
    sqlite3_create_module( db, "SVO", &svo_module, NULL );

    return SQLITE_OK;
}

/****************************************************************************/
