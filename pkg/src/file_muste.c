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

Rprintf("\n%s",buf);

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
    dsp=1; // Glabal variable disabling error messages
    result = R_LoadSurvoData(fname);
    dsp=0;
    if (result==R_NilValue) error("%s",sbuf);
    
    return result;
}


SEXP R_LoadSurvoData2(SEXP name)
	{
	return(muste_survodata2r((char *)CHAR(STRING_ELT(name,0)),1));
	}

void muste_set_R_survodata(char *dest,char *sour)
	{
	sprintf(buf,"%s <- .Call(\"R_LoadSurvoData2\",\"%s\",PACKAGE=\"muste\")",dest,sour);
	muste_evalr(buf);
	sprintf(buf,"attributes(%s) <- muste:::.muste.svoattributes(%s)",dest,dest);
	muste_evalr(buf);
	return;
	}

#if 0

static void tilanpuute()
	{
	if (muste_internal) { sur_print("\nNot enough memory!"); WAIT; }
	}

int muste_r2survodata(char *rname, char *sname, int muste_internal, SEXP df)
        {
        int i,j,k=0,l,nvar,nobs,charlen,txtlen,len,min,max;
        int filen,fim1,fim,fil,fiextra,fitextn,fitextlen;
        char **fitext;
        char *privi[1];
        char xx[LLENGTH], *xosa[2];
        int max_varlen;
    	SEXP names,types,theselabels,orig_names;
        char sana[16];
        int ep4,ep41;


        max_varlen=64;
    	nvar = length(df);   // How many variables in R data frame 
    	nobs=length(VECTOR_ELT(df, 0)); // How many observations in R data frame

		if (muste_internal)
			{		
			i=sp_init(r1+r-1); if (i<0) return(-1);
        	i=spfind("VARLEN"); if (i>=0) max_varlen=atoi(spb[i]);
        	sprintf(sbuf,"\nSince Survo data file %s does not exist,",sname); sur_print(sbuf);
        	sur_print("\ncreating a new one...");

        	ep4=EP4; i=hae_apu("ep4",sana); if (i) ep4=atoi(sana);
        	i=spfind("MAXFIELDS");
        	if (i>=0) ep4=atoi(spb[i]);
        	ep41=ep4+1;
    	
        	if (nvar>ep4)
            	{
            	sprintf(sbuf,"\nToo many variables in R data frame! (max=%d)",ep4);
            	sur_print(sbuf);
            	sprintf(sbuf,"\nUse the MAXFIELDS=<#_of_fields> specification!");
            	sur_print(sbuf); WAIT; return(-1);
            	}
            }
            
        ep=nvar+1;    
            
        vartype=(char *)muste_malloc(ep4*9);
        if (vartype==NULL) { tilanpuute(); return(-1); }
        pvartype=(char **)muste_malloc(ep4*sizeof(char **));
        if (pvartype==NULL) { tilanpuute(); return(-1); }
        varlen=(int *)muste_malloc(ep4*sizeof(int));
        if (varlen==NULL) { tilanpuute(); return(-1); }
    

	for(i = 0; i < nvar; i++)
	  {
      strncpy(vartype+i*9,space,8); vartype[i*9+8]=EOS;
      vartype[i*9+1]='A';
  
	  switch(TYPEOF(VECTOR_ELT(df, i)))
	    {
	    case LGLSXP:
		vartype[i*9+0]='1'; varlen[i]=1;
		break;
	    case INTSXP:
		min=max=0;
		for(j = 0;j < nobs; j++)
			{
		    k = INTEGER(VECTOR_ELT(df, i),j);
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
		charlen = 0;
		for(j = 0;j < nobs; j++){
		    k = strlen(CHAR(STRING_ELT(VECTOR_ELT(df, i),j)));
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


/*        
        i=tutki_textdata(); if (i<0) return(-1);

nvar: kuinka monta muuttujaa
tyyppi[nvar]: 2=string
kok[nvar]: muuttujan pituus
des[nvar]: kuinka monta desimaalia
neg[nvar]: onko negatiivinen

tarvitaan myös
vartype, jonka pituus 8*nvar
pvartype[nvar], pointterit vartypen oikeisiin kohtiin
varlen[nvar], jossa muuttujan koko

*/        


        filen=0;
        for (i=0; i<nvar; ++i) 
        	{
        	pvartype[i]=vartype+i*9;
        	filen+=varlen[i];
        	}


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
        i=fi_create(sname,filen,fim1,fim,0L,fil,fiextra,fitextn,fitextlen,
                    fitext,varname,varlen,pvartype);
        if (i<0) return(-1);
        data_open(sname,&d2);
        return(1);
        }





void R_SaveStataData(FILE *fp, SEXP df, int version, SEXP leveltable)
{
    int i,j,k=0,l,nvar,nobs,charlen,txtlen,len;
    char datalabel[81]="Written by R.              ", timestamp[18], aname[33];
    char format9g[50]="%9.0g", strformat[50]="";
    SEXP names,types,theselabels,orig_names;

    int namelength = 8;
    int fmtlist_len = 12;

    if (version >= 7) namelength=32;
    if (version >= 10) fmtlist_len = 49;

    /* names are 32 characters in version 7 */

    /** first write the header **/
    if (version == 6)
	OutByteBinary((char) VERSION_6, fp);            /* release */
    else if (version == 7)
	OutByteBinary((char) VERSION_7, fp);
    else if (version == 8)  /* and also 9, mapped in R code */
	OutByteBinary((char) VERSION_8, fp);
    else if (version == 10) /* see comment above */
	OutByteBinary((char) VERSION_114, fp);
    OutByteBinary((char) CN_TYPE_NATIVE, fp);
    OutByteBinary(1, fp);            /* filetype */
    OutByteBinary(0, fp);            /* padding */

    nvar = length(df);
    OutShortIntBinary(nvar, fp);
    nobs=length(VECTOR_ELT(df, 0));
    OutIntegerBinary(nobs, fp, 1);  /* number of cases */
    OutStringBinary(datalabel, fp, 81);   /* data label - zero terminated string */
    /* FIXME: use a real time */
    for(i = 0; i < 18; i++) timestamp[i] = 0;
    OutStringBinary(timestamp,fp,18);   /* file creation time - zero terminated string */



    /** write variable descriptors **/

    /** types **/
    /* FIXME: writes everything as double or integer to save effort*/
    /*  we should honor the "Csingle" attribute and also write logicals as
	byte rather than long */

    PROTECT(types = allocVector(INTSXP,nvar));
    if (version <= 7) {
	for(i = 0;i < nvar; i++){
	    switch(TYPEOF(VECTOR_ELT(df, i))){
	    case LGLSXP:
		OutByteBinary(STATA_BYTE, fp);
		break;
	    case INTSXP:
		OutByteBinary(STATA_INT, fp);
		break;
	    case REALSXP:
		OutByteBinary(STATA_DOUBLE, fp);
		break;
	    case STRSXP:
		/* NB: there is a 244 byte limit on strings */
		charlen = 0;
		for(j = 0; j < nobs; j++){
		    k = strlen(CHAR(STRING_ELT(VECTOR_ELT(df, i), j)));
		    if (k > charlen) charlen = k;
		}
		if(charlen > 244)
		    warning("character strings of >244 bytes in column %d will be truncated", i+1);
		charlen =  (charlen < 244) ? charlen : 244;
		OutByteBinary((unsigned char)(charlen+STATA_STRINGOFFSET), fp);
		INTEGER(types)[i] = charlen;
		break;
	    default:
		error(_("unknown data type"));
		break;
	    }
	}
    } else { /* version 8, 10 */
	for(i = 0; i < nvar; i++){
	    switch(TYPEOF(VECTOR_ELT(df, i))){
	    case LGLSXP:
		OutByteBinary(STATA_SE_BYTE,fp);
		break;
	    case INTSXP:
		OutByteBinary(STATA_SE_INT,fp);
		break;
	    case REALSXP:
		OutByteBinary(STATA_SE_DOUBLE,fp);
		break;
	    case STRSXP:
		/* NB: there is a 244 byte limit on strings */
		charlen = 0;
		for(j = 0;j < nobs; j++){
		    k = strlen(CHAR(STRING_ELT(VECTOR_ELT(df, i),j)));
		    if (k > charlen) charlen = k;
		}
		if(charlen > 244)
		    warning("character strings of >244 bytes in column %d will be truncated", i+1);
		charlen =  (charlen < 244) ? charlen : 244;
		OutByteBinary((unsigned char)(charlen+STATA_SE_STRINGOFFSET), fp);
		INTEGER(types)[i] = charlen;
		break;
	    default:
		error(_("unknown data type"));
		break;
	    }
	}
    }
    /** names truncated to 8 (or 32 for v>=7) characters**/

    PROTECT(names = getAttrib(df, R_NamesSymbol));
    for (i = 0; i < nvar;i ++){
	strncpy(aname, CHAR(STRING_ELT(names, i)), namelength);
	OutStringBinary(nameMangleOut(aname, namelength), fp, namelength);
	OutByteBinary(0, fp);
    }



    /** sortlist -- not relevant **/
    for (i = 0; i < 2*(nvar+1); i++) OutByteBinary(0, fp);

    /** format list: arbitrarily write numbers as %9g format
	but strings need accurate types */
    for (i = 0; i < nvar; i++) {
	if (TYPEOF(VECTOR_ELT(df,i)) == STRSXP){
	    /* string types are at most 244 characters
	       so we can't get a buffer overflow in sprintf **/
	    sprintf(strformat,"%%%ds",INTEGER(types)[i]);
	    OutStringBinary(strformat, fp, fmtlist_len);
	} else {
	    OutStringBinary(format9g, fp, fmtlist_len);
	}
    }

    /** value labels.  These are stored as the names of label formats,
	which are themselves stored later in the file.
	The label format has the same name as the variable. **/


    for(i = 0; i < nvar; i++) {
	if (VECTOR_ELT(leveltable, i) == R_NilValue){ /* no label */
	    for(j = 0; j < namelength+1; j++) OutByteBinary(0, fp);
	} else {                                   /* label */
	    strncpy(aname, CHAR(STRING_ELT(names, i)), namelength);
	    OutStringBinary(nameMangleOut(aname, namelength), fp, namelength);
	    OutByteBinary(0, fp);
	}
    }


    /** Variable Labels -- full R name of column**/
    /** FIXME: this is now just the same abbreviated name **/

    PROTECT(orig_names = getAttrib(df,install("orig.names")));
    for(i = 0; i < nvar; i++) {
	strncpy(datalabel,CHAR(STRING_ELT(orig_names,i)),81);
	datalabel[80] = (char) 0;
	OutStringBinary(datalabel, fp, 81);
    }
    UNPROTECT(1);


    /** variable 'characteristics' -- not relevant**/
    OutByteBinary(0, fp);
    OutByteBinary(0, fp);
    OutByteBinary(0, fp);
    if (version >= 7) { /*longer in version 7. This is wrong in the manual*/
	OutByteBinary(0, fp);
	OutByteBinary(0, fp);
    }


    /** The Data **/
    for(i = 0; i < nobs; i++){
	for(j = 0;j < nvar; j++){
	    switch (TYPEOF(VECTOR_ELT(df, j))) {
	    case LGLSXP:
		OutDataByteBinary(LOGICAL(VECTOR_ELT(df,j))[i], fp);
		break;
	    case INTSXP:
		OutIntegerBinary(INTEGER(VECTOR_ELT(df,j))[i], fp, 0);
		break;
	    case REALSXP:
		OutDoubleBinary(REAL(VECTOR_ELT(df,j))[i], fp, 0);
		break;
	    case STRSXP:
		/* Up to 244 bytes should be written, zero-padded */
		k = length(STRING_ELT(VECTOR_ELT(df, j), i));
		if(k > 244) k = 244;
		OutStringBinary(CHAR(STRING_ELT(VECTOR_ELT(df, j), i)), fp, k);
		for(l = INTEGER(types)[j]-k; l > 0; l--) OutByteBinary(0, fp);
		break;
	    default:
		error(_("this should not happen."));
		break;
	    }
	}
    }

    /** value labels: pp92-94 of 'Programming' manual in v7.0 **/

    for(i = 0;i < nvar; i++){
	if (VECTOR_ELT(leveltable, i) == R_NilValue)
	    continue; /* no labels */
	else {
	    theselabels = VECTOR_ELT(leveltable, i);
	    len = 4*2*(length(theselabels)+1);
	    txtlen = 0;
	    for (j = 0; j < length(theselabels); j++)
		txtlen += strlen(CHAR(STRING_ELT(theselabels, j))) + 1;
	    len += txtlen;
	    OutIntegerBinary(len, fp, 0); /* length of table */
	    strncpy(aname, CHAR(STRING_ELT(names, i)), namelength);
	    OutStringBinary(nameMangleOut(aname, namelength), fp, namelength);
	    OutByteBinary(0, fp); /* label format name */
	    OutByteBinary(0, fp); OutByteBinary(0, fp); OutByteBinary(0, fp); /*padding*/
	    OutIntegerBinary(length(theselabels), fp, 0);
	    OutIntegerBinary(txtlen, fp, 0);
	    /* offsets */
	    len = 0;
	    for (j = 0; j < length(theselabels); j++){
		OutIntegerBinary(len, fp, 0);
		len += strlen(CHAR(STRING_ELT(theselabels,j))) + 1;
	    }
	    /* values: just 1,2,3,...*/
	    for (j = 0; j < length(theselabels); j++)
		OutIntegerBinary(j+1, fp, 0);
	    /* the actual labels */
	    for(j = 0; j < length(theselabels); j++){
		len = strlen(CHAR(STRING_ELT(theselabels, j)));
		OutStringBinary(CHAR(STRING_ELT(theselabels,j)), fp, len);
		OutByteBinary(0, fp);
		txtlen -= len+1;
		if (txtlen < 0) error(_("this should happen: overrun"));
	    }
	    if (txtlen > 0) error(_("this should happen: underrun"));
	}
    }
    UNPROTECT(2); /* names,types */
}

SEXP do_writeStata(SEXP call)
{
    SEXP fname, df, leveltable;
    FILE *fp;
    int version;

    if ((sizeof(double) != 8) | (sizeof(int) != 4) | (sizeof(float) != 4))
      error(_("cannot yet read write .dta on this platform"));


    if (!isValidString(fname = CADR(call)))
	error(_("first argument must be a file name\n"));


    fp = fopen(R_ExpandFileName(CHAR(STRING_ELT(fname,0))), "wb");
    if (!fp) error(_("unable to open file for writing: '%s'"), strerror(errno));

    df = CADDR(call);
    if (!inherits(df,"data.frame"))
	error(_("data to be saved must be in a data frame"));

    version = INTEGER(coerceVector(CADDDR(call), INTSXP))[0];
    /* 9 is mapped to 8 in R code */
    if ((version < 6) || (version > 10))
	error(_("can only write version 6-10 formats"));
    leveltable = CAD4R(call);

    R_SaveStataData(fp,df,version,leveltable);
    fclose(fp);
    return R_NilValue;
}
#endif
