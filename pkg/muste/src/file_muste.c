#include "muste.h"
#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>
#include <stdio.h>
#include "survo.h"
#include "survoext.h"
#include "survolib.h"

#define UTF8_MASK (1<<3)
#define IS_UTF8(x) (LEVELS(x) & UTF8_MASK)

#define LLENGTH 10010
#define EOS '\0'

extern int muste_iconv();

static SURVO_DATA d;
static char buf[LLENGTH];
static char buf2[LLENGTH];
static int prind;

SEXP muste_survodata2r(char *name,int muste_internal)
    {
    char tfname[]="<Temp>/_S2RCONV.SVO";
    int i,j,k,all,nvar,nobs,actsar,vi,othertype;
    double a;
    extern int r,r1,prind;
    extern char *active_data;
    extern int arguc;
	extern char *arguv[];
    SEXP df, types, names, tmp, tmp2;
    
    prind=0; othertype=0;
    i=data_open3(name,&d,0,1,1,0); if (i<0) return(R_NilValue);
    if (d.type!=2) 
    	{     	
    	data_close(&d);

		if (muste_internal && (d.type==1 || d.type==3)) // RS 26.11.2012
			{
			// FILE COPY <source_data> TO NEW <destination_file>
			edread(buf2,r1+r-1);
			sprintf(buf,"FILE COPY %s TO NEW %s",name,tfname);
			edwrite(space,r1+r-1,1);
    		edwrite(buf,r1+r-1,1);
    		strcpy(buf,tfname);	
    		sur_delete1(buf);
     		muste_dump();   		
			muste_file_copy(arguc,arguv);
			muste_restore_dump();
			edwrite(space,r1+r-1,1);
    		edwrite(buf2,r1+r-1,0);
    		muste_dump();
    		muste_restore_dump();
			s_init();
			i=data_open3(tfname,&d,0,1,1,0); if (i<0) return(R_NilValue);
			othertype=1;
			}
		else // Exit if not supported survo data file
			{
			sprintf(buf,"\nFIXME: Data type %d not supported yet! Convert to .SVO first.",d.type);
			muste_fixme(buf);
			return(R_NilValue);
			}
    	} 
    nvar=d.m; // Number of variables
    nobs=d.d2.n;
    all=1;
    
    if (muste_internal)
    	{
    	i=sp_init(r1+r-1); if (i<0) { data_close(&d); return(R_NilValue); }
    	i=spfind("ALL");
    	if (i>=0)
        	{ if (atoi(spb[i])) { all=1; muste_internal=0; } }
    	else 
    	    { 
    	    i=mask(&d); if (i<0) return(R_NilValue); 
    	    nvar=d.m_act; 
    	    all=0; 
    	    } 
        i=hae_apu("prind",buf); if (i) prind=atoi(buf);
        if ((i=spfind("PRIND"))>=0) prind=atoi(spb[i]);    	
    	}	

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

	if (othertype==1)
		{
    	snprintf(buf,LLENGTH,"Survo edit data %s: record=%d bytes, M1=%d L=%d  M=%d N=%d",
    	    	name,d.d2.len,d.d2.m1,d.d2.l,nvar,nobs);
		}
	else
		{
		snprintf(buf,LLENGTH,"Survo data file %s: record=%d bytes, M1=%d L=%d  M=%d N=%d",
                active_data,d.d2.len,d.d2.m1,d.d2.l,nvar,nobs);
		}
		

Rprintf("\n%s",buf);

    PROTECT(df = allocVector(VECSXP, nvar)); // Make the data frame

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
    extern char sbuf[];
    extern int dsp;

    if (!isValidString(fname))
	error("first argument must be a file name\n");

    fp = fopen(R_ExpandFileName(CHAR(STRING_ELT(fname,0))), "rb");
    if (!fp)
	error("unable to open file: '%s'", strerror(errno));
    fclose(fp);

	sprintf(sbuf,"unknown error");    
    dsp=1; // Global variable disabling error messages
    result = R_LoadSurvoData(fname);
    dsp=0;
    if (result==R_NilValue) error("%s",sbuf);
    
    return result;
}


SEXP R_LoadSurvoData2(SEXP name)
	{
	return(muste_survodata2r((char *)CHAR(STRING_ELT(name,0)),1));
	}

//void muste_set_R_survodata(char *dest,char *sour)
void muste_Survo2R(char *dest,char *sour)
	{
	sprintf(buf,"%s <- .Call(\"R_LoadSurvoData2\",\"%s\",PACKAGE=\"muste\")",dest,sour);
	muste_evalr(buf);
	sprintf(buf,"attributes(%s) <- muste:::.muste.svoattributes(%s)",dest,dest);
	muste_evalr(buf);
	return;
	}





static void tilanpuute(int muste_internal)
	{
	if (muste_internal) { sur_print("\nNot enough memory!"); WAIT; }
	}

int muste_r2survodata(char *sname, int muste_internal, SEXP df, char *rname)
        {
        int i,j,j2,k=0,nvar,nobs,charlen,min,max;
        int filen,fim1,fim,fil,fiextra,fitextn,fitextlen;
        char **fitext;
        char *privi[1];
        char xx[LLENGTH], *xosa[2];
        int max_varlen,namelength;
    	SEXP names,enc;
        char sana[16];
        int ep4;
        char *vartype, *p;
        char **pvartype;
        int *varlen;
        char **varname;
        char *jakso, *nimet;
        SURVO_DATA d2;
        double tulos;

//    if ((sizeof(double) != 8) | (sizeof(int) != 4) | (sizeof(float) != 4))
 //     error("cannot yet read write .svo on this platform");

    if (!inherits(df,"data.frame"))
    	{
	    if (muste_internal) { sur_print("\nData to be saved must be in a data frame"); WAIT; return(-1); }
	    else error("data to be saved must be in a data frame");
		}

        max_varlen=64;
    	nvar = length(df);   // How many variables in R data frame 
    	nobs=length(VECTOR_ELT(df, 0)); // How many observations in R data frame

// Rprintf("\nnvar: %d, nobs: %d",nvar,nobs);

		if (muste_internal)
			{		
			i=sp_init(r1+r-1); if (i<0) return(-1);
        	i=spfind("VARLEN"); if (i>=0) max_varlen=atoi(spb[i]);
//        	sprintf(sbuf,"\nSince Survo data file %s does not exist,",sname); sur_print(sbuf);
//        	sur_print("\ncreating a new one...");

        	ep4=EP4; i=hae_apu("ep4",sana); if (i) ep4=atoi(sana);
        	i=spfind("MAXFIELDS");
        	if (i>=0) ep4=atoi(spb[i]);
//        	ep41=ep4+1;
    	
        	if (nvar>ep4)
            	{
            	sprintf(sbuf,"\nToo many variables in R data frame %s! (max=%d)",rname,ep4);
            	sur_print(sbuf);
            	sprintf(sbuf,"\nUse the MAXFIELDS=<#_of_fields> specification!");
            	sur_print(sbuf); WAIT; return(-1);
            	}
            }
            
        ep4=nvar+1;    
        namelength=max_varlen;

// Rprintf("\nep4: %d, namelength: %d",ep4,namelength);
            
        vartype=(char *)muste_malloc(ep4*9);
        if (vartype==NULL) { tilanpuute(muste_internal); return(-1); }
        pvartype=(char **)muste_malloc(ep4*sizeof(char **));
        if (pvartype==NULL) { tilanpuute(muste_internal); return(-1); }
        varlen=(int *)muste_malloc(ep4*sizeof(int));
        if (varlen==NULL) { tilanpuute(muste_internal); return(-1); }
        varname=(char **)muste_malloc(ep4*sizeof(char *));
        if (varname==NULL) { tilanpuute(muste_internal); return(-1); }  
        jakso=(char *)muste_malloc(127*LLENGTH);
        if (jakso==NULL) { tilanpuute(muste_internal); return(-1); }    
        nimet=(char *)muste_malloc(ep4*namelength);
        if (nimet==NULL) { tilanpuute(muste_internal); return(-1); }


    PROTECT(names = getAttrib(df, R_NamesSymbol));
	for(i = 0; i < nvar; i++)
	  {
	  p=nimet+i*namelength;
	  enc=STRING_ELT(names, i);
//	  strncpy(p, CHAR(STRING_ELT(names, i)), namelength);
	  strncpy(p, CHAR(enc), namelength);	  
	  if (IS_UTF8(enc)) muste_iconv(p,"CP850","UTF-8");
      else muste_iconv(p,"CP850","");	  
	  varname[i]=p;
	  }


	for(i = 0; i < nvar; i++)
	  {
      strncpy(vartype+i*9,space,8); vartype[i*9+8]=EOS;
      vartype[i*9+1]='A';

// Rprintf("\nvarname[%d]: %s, type: %d", i,varname[i], TYPEOF(VECTOR_ELT(df, i)));
  
	  switch(TYPEOF(VECTOR_ELT(df, i)))
	    {
	    case LGLSXP:
		vartype[i*9+0]='1'; varlen[i]=1;
		break;
	    case INTSXP:
		min=max=0;
		for(j = 0;j < nobs; j++)
			{
//		    k = INTEGER_POINTER(VECTOR_ELT(df, i))[j];
		    if (INTEGER(VECTOR_ELT(df, i))[j]==NA_INTEGER) k=0;
		    else k = INTEGER(VECTOR_ELT(df, i))[j];
		    if (k > max) max=k;
		    if (k < min) min=k;		    
			}
		if (min>0 && max<256) { vartype[i*9+0]='1'; varlen[i]=1; }
		else if (min>-32000 && max<32000) { vartype[i*9+0]='2';	varlen[i]=2; }
		else { vartype[i*9+0]='4'; varlen[i]=4;	} 
		break;
	    case REALSXP:
		vartype[i*9+0]='8'; varlen[i]=8;
		break;
	    case STRSXP:

		charlen = 1;
		for(j = 0;j < nobs; j++)
			{
     	    enc=STRING_ELT(VECTOR_ELT(df, i), j);	// RS 11.2.2013
			strncpy(jakso,CHAR(enc),120*LLENGTH);			
			if (IS_UTF8(enc)) muste_iconv(jakso,"CP850","UTF-8");
    		else muste_iconv(jakso,"CP850","");
		    k = strlen(jakso);
//		    k = strlen(CHAR(STRING_ELT(VECTOR_ELT(df, i),j)));
		    if (k > charlen) charlen = k;
			}
		vartype[i*9+0]='S'; varlen[i]=charlen;	    
            	
        if (muste_internal && varlen[i]>max_varlen)
            {
            sprintf(sbuf,"\nThe length of the field %s (%d) is more than %d.",
                                varname[i],varlen[i],max_varlen);
            sur_print(sbuf);
            sprintf(sbuf,"\nYou may increase the limit by specification VARLEN=%d",
                                varlen[i]);
            sur_print(sbuf);
            sur_print("\nHowever, field lengths greater than 64 should be avoided.");
            WAIT; return(-1);
            }		
		break;
	    default:
        if (muste_internal) 
        	{
        	sur_print("\nUnknown data type for variable %s!", varname[i]);
        	WAIT; return(-1);
        	}
		else error("unknown data type");
		break;
	    }
	}      

        filen=0;
        for (i=0; i<nvar; i++) 
        	{
        	pvartype[i]=vartype+i*9;
        	filen+=varlen[i];
        	
// Rprintf("\nvartype[%d]:%c, varlen[%d]: %d",i,vartype[i*9+0],i,varlen[i]);        	
        	}

		fim=nvar;
        filen+=filen/4+20;
        fim1=fim+fim/4+4;

		if (muste_internal)
		{
        i=spfind("NEWSPACE");  /* 23.10.1994 */
        if (i>=0)
            {
            strcpy(xx,spb[i]); i=split(xx,xosa,2);
            if (i<2)
                {
                sur_print("\nError in NEWSPACE! Usage NEWSPACE=<extra_space>,<#_of_extra_fields>");
                WAIT; return(-1);
                }
            filen+=atoi(xosa[0]);
            fim1=fim+atoi(xosa[1]);
            }
		}
		
        fil=64;
        fiextra=12;
        fitextn=1;
        fitextlen=c2;
        strcpy(jakso," Copied from R data frame "); strcat(jakso,rname); privi[0]=jakso;
        fitext=privi;
 

// Rprintf("\nfilen: %d, fim1: %d, fim: %d, fil: %d",filen,fim1,fim,fil);

        i=fi_create(sname,filen,fim1,fim,0L,fil,fiextra,fitextn,fitextlen,
                    fitext,varname,varlen,pvartype);
		UNPROTECT(1); // names
        if (i<0) return(-1);
        
        data_open(sname,&d2);

        if (muste_internal && d2.type!=2)
            {
            sprintf(sbuf,"\nDestination %s must be a data file!",word[3]);
            sur_print(sbuf); WAIT; data_close(&d2); return(-1);
            }

	j2=d2.n;

    /** The Data **/
    for(i=0; i<nobs; i++)
    	{
    	++j2;
        if (j2>d2.n) d2.n=j2;   // fi_save vaatii j2<=d2.n 
        if (d2.m>nvar) fi_miss_obs(&d2.d2,j2);	    	
		for(j=0;j<nvar;j++)
			{		
			
	    	switch (TYPEOF(VECTOR_ELT(df, j)))
	    		{
	    		case LGLSXP:
//		OutDataByteBinary(LOGICAL(VECTOR_ELT(df,j))[i], fp);
				if (LOGICAL(VECTOR_ELT(df,j))[i]==NA_LOGICAL) fi_miss_save(&d2.d2,j2,j);
				else
				{
				tulos=(double)LOGICAL(VECTOR_ELT(df,j))[i];
                fi_save(&d2.d2,j2,j,&tulos);
				}
				break;
	    		case INTSXP:
//		OutIntegerBinary(INTEGER(VECTOR_ELT(df,j))[i], fp, 0);
                if (INTEGER(VECTOR_ELT(df,j))[i]==NA_INTEGER) fi_miss_save(&d2.d2,j2,j);
				else
				{
                tulos=(double)INTEGER(VECTOR_ELT(df,j))[i];
                fi_save(&d2.d2,j2,j,&tulos);
                }
				break;
	    		case REALSXP:
//		OutDoubleBinary(REAL(VECTOR_ELT(df,j))[i], fp, 0);
                if (ISNA(REAL(VECTOR_ELT(df,j))[i])) fi_miss_save(&d2.d2,j2,j);
				else
				{                
                tulos=(double)REAL(VECTOR_ELT(df,j))[i];
                fi_save(&d2.d2,j2,j,&tulos);
                }
				break;
	    		case STRSXP:
      	    	enc=STRING_ELT(VECTOR_ELT(df, j), i);	
//				strncpy(jakso,CHAR(STRING_ELT(VECTOR_ELT(df, j), i)),d2.varlen[j]);
				strncpy(jakso,CHAR(enc),3*d2.varlen[j]);  // RS 11.2.2013 ADD 3*			
				if (IS_UTF8(enc)) muste_iconv(jakso,"CP850","UTF-8");
      			else muste_iconv(jakso,"CP850","");
                for (k=strlen(jakso); k<d2.varlen[j]; ++k) jakso[k]=' ';				
                fi_alpha_save(&d2.d2,j2,j,jakso);	
				break;
	    		default:
	    		muste_fixme("\nFIXME: Unknown variable type while transforming R data frame to Survo data!");
				error("this should not happen.");
				break;
	    		}
			}
    	}

        fi_rewind(&d2.d2);
        fi_puts(&d2.d2,(char *)&j2,4,22L); // RS 28.1.2013 (char *)

        data_close(&d2);
        
		muste_free(nimet);
		muste_free(jakso);
		muste_free(varname);
		muste_free(varlen);
		muste_free(pvartype);
		muste_free(vartype);
        
        return(1);
        }

SEXP R_SaveSurvoData(SEXP df,SEXP name,SEXP rdfname)
	{
//  	SEXP df=R_NilValue;

//	df = findVar(install(CHAR(STRING_ELT(rdfname, 0))), R_GlobalEnv);
	
	muste_r2survodata((char *)CHAR(STRING_ELT(name,0)),0,df,(char *)CHAR(STRING_ELT(rdfname, 0)));
	return(df);
	}

SEXP R_SaveSurvoData2(SEXP rdfname, SEXP name)
	{
  	SEXP df=R_NilValue;

	df = findVar(install(CHAR(STRING_ELT(rdfname, 0))), R_GlobalEnv);
	
	muste_r2survodata((char *)CHAR(STRING_ELT(name,0)),1,df,(char *)CHAR(STRING_ELT(rdfname, 0)));
	return(df);
	}

void muste_R2Survo(char *dest,char *sour)
	{
	sprintf(buf,".Call(\"R_SaveSurvoData2\",\"%s\",\"%s\",PACKAGE=\"muste\")",sour,dest);
	muste_evalr(buf);
	return;
	}


SEXP do_writeSurvo(SEXP dataf,SEXP svofile,SEXP dfname)
{
//    SEXP fname, df, leveltable;
//    FILE *fp;
//    int version;
    extern int dsp;

    if (!inherits(dataf,"data.frame"))
	error("data to be saved must be in a data frame");

    if (!isValidString(svofile))
	error("second argument must be a file name\n");


    dsp=1; // Global variable disabling error messages
    R_SaveSurvoData(dataf,svofile,dfname);
    dsp=0;
  
    return R_NilValue;


/*
    if ((sizeof(double) != 8) | (sizeof(int) != 4) | (sizeof(float) != 4))
      error("cannot yet read write .svo on this platform");


    if (!isValidString(fname = CADR(call)))
	error("first argument must be a file name\n");


    fp = fopen(R_ExpandFileName(CHAR(STRING_ELT(fname,0))), "wb");
    if (!fp) error("unable to open file for writing: '%s'", strerror(errno));
    fclose(fp);

    df = CADDR(call);
    if (!inherits(df,"data.frame"))
	error("data to be saved must be in a data frame");

    R_SaveSurvoData(fp,df,version,leveltable);

    return R_NilValue;
*/   
}


