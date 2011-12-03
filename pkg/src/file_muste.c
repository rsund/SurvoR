#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>
#include <stdio.h>
#include "survolib.h"

#define LLENGTH 10010
#define EOS '\0'

extern int muste_iconv();

static SURVO_DATA d;
static char buf[LLENGTH];
static int prind;

SEXP muste_survodata2r(char *name,int muste_internal)
    {
    int i,j,k,all,nvar,nobs,actsar,vi;
    double a;
    extern int r,r1,prind;
    extern char *active_data;
    SEXP df, types, names, tmp, tmp2;
    
    prind=0;
    i=data_open3(name,&d,0,1,1,0); if (i<0) return(R_NilValue);
    if (d.type!=2) { data_close(&d); return(R_NilValue); } // Exit if not survo data file
    nvar=d.m; // Number of variables
    nobs=d.d2.n;
    all=1;
    
    if (muste_internal)
    	{
    	i=sp_init(r1+r-1); if (i<0) { data_close(&d); return(R_NilValue); }
    	i=spfind("ALL");
    	if (i>=0)
        	{ if (atoi(spb[i])) { all=1; muste_internal=0; } }
    	else { mask(&d); nvar=d.m_act; all=0; } 
        i=hae_apu("prind",buf); if (i) prind=atoi(buf);
        if ((i=spfind("PRIND"))>=0) prind=atoi(spb[i]);    	
    	}	

    PROTECT(df = allocVector(VECSXP, nvar)); // Make the data frame

	if (muste_internal)
		{
		i=conditions(&d); if (i<0) { data_close(&d); return(R_NilValue); }
	    for(j=d.l1,k=0; j<=d.l2; j++)
	    	{
            if (unsuitable(&d,j)) continue;
            k++;
            }
        nobs=k;    
		}

    snprintf(buf,LLENGTH,"Survo data file %s: record=%d bytes, M1=%d L=%d  M=%d N=%d",
                         active_data,d.d2.len,d.d2.m1,d.d2.l,nvar,nobs);

    PROTECT(tmp = allocVector(STRSXP, 1));
    SET_STRING_ELT(tmp, 0, mkChar(buf));
    setAttrib(df, install("status.info"), tmp); // Info from FILE STATUS as an attribute
    UNPROTECT(1); // tmp
        
    PROTECT(tmp = allocVector(STRSXP, d.d2.textn));    
    for (i=0; i<d.d2.textn; ++i)
        {
        snprintf(buf,d.d2.textlen-1,"%s",d.d2.fitext[i]);
        muste_iconv(buf,"","CP850");
        SET_STRING_ELT(tmp, i, mkChar(buf));
        }
    setAttrib(df, install("status.description"), tmp); // Description lines as an attribute vector
    UNPROTECT(1); // tmp

    PROTECT(types = allocVector(INTSXP, nvar));
    PROTECT(tmp2 = allocVector(STRSXP, nvar)); 
    PROTECT(names = allocVector(STRSXP, nvar)); 
    PROTECT(tmp = allocVector(STRSXP, nvar)); 
  
    actsar=d.d2.extra-4; // Activation columnns
    
	for(i = 0; i < nvar; i++)
		{
		vi=d.v[i];
		if (!all) if (d.vartype[vi][1]=='-') continue;
	    INTEGER(types)[i] = d.varlen[vi];
	    SET_STRING_ELT(names, i, mkChar(buf));
	    
	    snprintf(buf,actsar,"%s",d.vartype[vi]);
	    muste_iconv(buf,"","CP850");
	    SET_STRING_ELT(tmp2, i, mkChar(buf));	    
	    
	    snprintf(buf,d.d2.l-1,"%s",d.varname[vi]);
	    muste_iconv(buf,"","CP850");
	    SET_STRING_ELT(tmp, i, mkChar(buf));
	    buf[8]=EOS; k=7;
	    while (buf[k]==' ' && k>0) buf[k--]=EOS;
	    SET_STRING_ELT(names, i, mkChar(buf));
	    }

    setAttrib(df, install("status.varname"), tmp); // Long names as an attribute vector
    UNPROTECT(1); // tmp	    	    
	setAttrib(df, R_NamesSymbol, names);
    UNPROTECT(1); // names 

    setAttrib(df, install("status.vartype"), tmp2); // Variable types as an attribute vector
    UNPROTECT(1); // tmp2

	for(i=0; i<nvar; i++)
		{
		vi=d.v[i];
		if (d.vartype[vi][0]=='4' || d.vartype[vi][0]=='8')
    		{
    		SET_VECTOR_ELT(df, i, allocVector(REALSXP, nobs));
    		}
   		else if (d.vartype[vi][0]=='1' || d.vartype[vi][0]=='2')
   			{
   			SET_VECTOR_ELT(df, i, allocVector(INTSXP, nobs));
   			}
   		else
   			{
   			SET_VECTOR_ELT(df, i, allocVector(STRSXP, nobs));
   			}
   		}	
				
	for(j=d.l1,k=0; j<=d.l2; j++)
    	{
    	if (muste_internal)
        	{
            if (unsuitable(&d,j)) continue;
            if (prind) { sprintf(buf,"%d ",j); sur_print(buf); }
          	}

	    for(i=0; i<nvar; i++)
			{
			vi=d.v[i];        	
            	
			switch (d.vartype[vi][0])
				{
				case '4':
				case '8': 				
                fi_load(&d.d2,j,vi,&a);
                if (a==MISSING8) a=NA_REAL;             
		    	REAL(VECTOR_ELT(df,i))[k] = a;
		    	break;
		    	case '1':
		    	case '2':
                fi_load(&d.d2,j,vi,&a);
                if (a==MISSING8) a=NA_INTEGER;		    	
		    	INTEGER(VECTOR_ELT(df,i))[k] = (int)a;
		    	break;
				default:
				fi_alpha_load(&d.d2,j,vi,buf);
	    		muste_iconv(buf,"","CP850");		    	
		    	SET_STRING_ELT(VECTOR_ELT(df,i), k, mkChar(buf));
		    	break;
				}				
	    	}
	    	k++;
		}
		
    setAttrib(df, install("status.varlen"), types); // Variable lengths as an attribute vector 
    UNPROTECT(1); // types
    
    UNPROTECT(1); // df
    data_close(&d);
    
    return(df);	    
    } 

SEXP R_LoadSurvoData(SEXP name)
	{
	return(muste_survodata2r((char *)CHAR(STRING_ELT(name,0)),0));
	}

#include <string.h>
#include <errno.h>

SEXP do_readSurvo(SEXP fname)
{
    SEXP result;
    FILE *fp;

    if (!isValidString(fname))
	error("first argument must be a file name\n");

    fp = fopen(R_ExpandFileName(CHAR(STRING_ELT(fname,0))), "rb");
    if (!fp)
	error("unable to open file: '%s'", strerror(errno));
    fclose(fp);
    
    result = R_LoadSurvoData(fname);

    return result;
}


SEXP R_LoadSurvoData2(SEXP name)
	{
	return(muste_survodata2r((char *)CHAR(STRING_ELT(name,0)),1));
	}

int muste_set_R_survodata(char *dest,char *sour)
	{
	sprintf(buf,"%s <- .Call(\"R_LoadSurvoData2\",\"%s\",PACKAGE=\"muste\")",dest,sour);
	muste_evalr(buf);
	sprintf(buf,"attributes(%s) <- .muste.svoattributes(%s)",dest,dest);
	muste_evalr(buf);
	return(1);
	}

