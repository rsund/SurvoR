#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>
#include <stdio.h>
#include "survolib.h"

#define LLENGTH 10010
#define EOS '\0'

extern int muste_iconv();

static SURVO_DATA d;

SEXP muste_survodata2r(char *name,int muste_internal)
    {
    int i,k,all,nvar,nobs,actsar;
    char buf[LLENGTH];
    extern int r,r1;
    extern char *active_data;
    SEXP df, types, names, tmp;
    
    i=data_open3(name,&d,0,1,1,0); if (i<0) return(R_NilValue);
    if (d.type!=2) return(R_NilValue); // Exit if not survo data file
    nvar=d.m; // Number of variables
    nobs=d.d2.n;
    all=1;
    
    if (muste_internal)
    	{
    	i=sp_init(r1+r-1); if (i<0) return(R_NilValue);
    	i=spfind("ALL");
    	if (i>=0)
        	{ if (atoi(spb[i])) all=1; }
    	else { mask(&d); nvar=d.m_act; all=0; }    
    	}

    PROTECT(df = allocVector(VECSXP, nvar)); // Make the data frame

//    actsar=d.d2.extra-4; // Activation column
    snprintf(buf,LLENGTH,"Survo data file %s: record=%d bytes, M1=%d L=%d  M=%d N=%d",
                         active_data,d.d2.len,d.d2.m1,d.d2.l,nvar,nobs);


    PROTECT(tmp = allocVector(STRSXP, 1));
    SET_STRING_ELT(tmp, 0, mkChar(buf));
    setAttrib(df, install("statusinfo"), tmp); // Info from FILE STATUS as an attribute
    UNPROTECT(1); // tmp
        
    PROTECT(tmp = allocVector(STRSXP, d.d2.textn));    
    for (i=0; i<d.d2.textn; ++i)
        {
        snprintf(buf,d.d2.textlen-1,"%s",d.d2.fitext[i]);
        muste_iconv(buf,"","CP850");
        SET_STRING_ELT(tmp, i, mkChar(buf));
        }
    setAttrib(df, install("description"), tmp); // Description lines as an attribute vector
    UNPROTECT(1); // tmp


    /** read variable descriptors **/

    /** types **/

    PROTECT(types = allocVector(INTSXP, nvar));
    PROTECT(names = allocVector(STRSXP, nvar));
    PROTECT(tmp = allocVector(STRSXP, nvar));    
    
	for(i = 0; i < nvar; i++)
		{
		if (!all) if (d.vartype[i][1]=='-') continue;
	    INTEGER(types)[i] = d.varlen[i];
	    SET_STRING_ELT(names, i, mkChar(buf));
	    snprintf(buf,d.d2.l-1,"%s",d.varname[i]);
	    muste_iconv(buf,"","CP850");
	    SET_STRING_ELT(tmp, i, mkChar(buf));
	    buf[8]=EOS; k=7;
	    while (buf[k]==' ' && k>0) buf[k--]=EOS;
	    SET_STRING_ELT(names, i, mkChar(buf));
	    
	    if (d.vartype[i][0]=='4' || d.vartype[i][0]=='8')
	    	{
	    	SET_VECTOR_ELT(df, i, allocVector(REALSXP, nobs));
	    	}
	    else if (d.vartype[i][0]=='1' || d.vartype[i][0]=='2')
	    	{
	    	SET_VECTOR_ELT(df, i, allocVector(INTSXP, nobs));
	    	}
	    else
	    	{
	    	SET_VECTOR_ELT(df, i, allocVector(STRSXP, nobs));
	    	}
	    }

    setAttrib(df, install("longnames"), tmp); // Long names as an attribute vector
    UNPROTECT(1); // tmp	    	    
	setAttrib(df, R_NamesSymbol, names);
    UNPROTECT(1); // names    
    
    UNPROTECT(1); /* types */
	    
    UNPROTECT(1); /* df */
    data_close(&d);
    return(df);	    
    } 


SEXP R_LoadMusteData(SEXP koe)
	{
	return(muste_survodata2r("LKOE.SVO",1));
	}

/*
static void status()
        {
        int i,k;
        char x[LLENGTH];
        int all;

        if (g<3)
            {
            sur_print("\nUsage:");
            sur_print("\nFILE STATUS <Survo_data_file>");
            WAIT; return;
            }
        strcpy(x,word[2]);
        subst_survo_path(x);
        i=data_open3(x,&d,0,1,1,0); if (i<0) return;
        if (d.type!=2) return;
        m=d.m;
        i=sp_init(r1+r-1); if (i<0) return;
        all=0;
        i=spfind("ALL");
        if (i>=0)
            { if (atoi(spb[i])) all=1; }
        else mask(&d);

        i=spfind("GETVAR"); // 20.10.2001
        if (i>=0) { getvar(i); return; }

        actsar=3;
        if (g>3) actsar=atoi(word[3])+1;
        if (actsar>d.d2.extra-4) actsar=d.d2.extra-4;

        tulosrivi=r1+r;
        if (g>4) { tulosrivi=edline2(word[4],1,1); if (tulosrivi==0) return; }
        for (i=0; i<d.d2.textn; ++i)
            { k=tulosta(d.d2.fitext[i]); if (k<0) return; }
        k=tulosta("FIELDS: (active)"); if (k<0) return;
        for (i=0; i<m; ++i)
            {
            char type[LLENGTH];
            char *p;

            if (!all) if (d.vartype[i][1]=='-') continue;
            strcpy(type,d.vartype[i]);
            if (*type=='1' || *type=='2' || *type=='4' || *type=='8')
                *type='N';
            while ((p=strchr(type,' '))!=NULL) *p='_';
            sprintf(x,"%4d %.*s %3d %s",
                  i+1,actsar,type,d.varlen[i],d.varname[i]);
            k=tulosta(x); if (k<0) return;
            }
        k=tulosta("END"); if (k<0) return;
                                                               // RS CHA N=%ld -> N=%d
        sprintf(x,"Survo data file %s: record=%d bytes, M1=%d L=%d  M=%d N=%d",
                         active_data,d.d2.len,d.d2.m1,d.d2.l,m,d.d2.n);
        tulosta(x);
        data_close(&d);
        }

void muste_file(int argc,char *argv[])
        {
        int i;

        if (argc==1) return;


         
        s_init(argv[1]);
        argv1=argv[1];

        if (muste_strcmpi(word[1],"STATUS")==0)
            { status(); s_end(argv[1]); return; }

        }

*/

#if 0
SEXP R_LoadStataData(FILE *fp)
{
    int i, j = 0, nvar, nobs, charlen, version, swapends, 
	varnamelength, nlabels, totlen, res;
    unsigned char abyte;
    /* timestamp is used for timestamp and for variable formats */
    char datalabel[81], timestamp[50], aname[33];
    char stringbuffer[245], *txt;
    SEXP df, names, tmp, varlabels, types, row_names;
    SEXP levels, labels, labeltable, sversion;
    int *off;
    int fmtlist_len = 12;


    /** first read the header **/

    abyte = RawByteBinary(fp, 1);   /* release version */
    version = 0;			/* -Wall */
    varnamelength = 0;		/* -Wall */
    labeltable = R_NilValue;	/* -Wall */
    switch (abyte) {
    case VERSION_5:
	version = 5;
	varnamelength = 8;
	break;
    case VERSION_6:
	version = 6;
	varnamelength = 8;
	break;
    case VERSION_7:
	version = 7;
	varnamelength = 32;
	break;
    case VERSION_7SE:
	version = -7;
	varnamelength = 32;
	break;
    case VERSION_8:
	version = -8;  /* version 8 automatically uses SE format */
	varnamelength = 32;
	break;
    case VERSION_114:
	version = -10;
	varnamelength = 32;
	fmtlist_len = 49;
    case VERSION_115:
	/* Stata say the formats are identical,
	   but _115 allows business dates */
	version = -12;
	varnamelength = 32;
	fmtlist_len = 49;
	break;
    default:
	error(_("not a Stata version 5-12 .dta file"));
    }
    stata_endian = (int) RawByteBinary(fp, 1);     /* byte ordering */
    swapends = stata_endian != CN_TYPE_NATIVE;

    RawByteBinary(fp, 1);            /* filetype -- junk */
    RawByteBinary(fp, 1);            /* padding */
    nvar = (InShortIntBinary(fp, 1, swapends)); /* number of variables */
    nobs = (InIntegerBinary(fp, 1, swapends));  /* number of cases */
    /* data label - zero terminated string */
    switch (abs(version)) {
    case 5:
	InStringBinary(fp, 32, datalabel);
	break;
    case 6:
    case 7:
    case 8:
    case 10:
    case 12:
	InStringBinary(fp, 81, datalabel);
	break;
    }
    /* file creation time - zero terminated string */
    InStringBinary(fp, 18, timestamp);

    /** make the data frame **/

    PROTECT(df = allocVector(VECSXP, nvar));

    /** and now stick the labels on it **/

    PROTECT(tmp = allocVector(STRSXP, 1));
    SET_STRING_ELT(tmp, 0, mkChar(datalabel));
    setAttrib(df, install("datalabel"), tmp);
    UNPROTECT(1);

    PROTECT(tmp = allocVector(STRSXP, 1));
    SET_STRING_ELT(tmp, 0, mkChar(timestamp));
    setAttrib(df, install("time.stamp"), tmp);
    UNPROTECT(1);


    /** read variable descriptors **/

    /** types **/

    PROTECT(types = allocVector(INTSXP, nvar));
    if (version > 0){
	for(i = 0; i < nvar; i++){
	    abyte = RawByteBinary(fp, 1);
	    INTEGER(types)[i] = abyte;
	    switch (abyte) {
	    case STATA_FLOAT:
	    case STATA_DOUBLE:
		SET_VECTOR_ELT(df, i, allocVector(REALSXP, nobs));
		break;
	    case STATA_INT:
	    case STATA_SHORTINT:
	    case STATA_BYTE:
		SET_VECTOR_ELT(df, i, allocVector(INTSXP, nobs));
		break;
	    default:
		if (abyte < STATA_STRINGOFFSET)
		    error(_("unknown data type"));
		SET_VECTOR_ELT(df, i, allocVector(STRSXP, nobs));
		break;
	    }
	}
    } else {
	for(i = 0; i < nvar; i++){
	    abyte = RawByteBinary(fp, 1);
	    INTEGER(types)[i] = abyte;
	    switch (abyte) {
	    case STATA_SE_FLOAT:
	    case STATA_SE_DOUBLE:
		SET_VECTOR_ELT(df, i, allocVector(REALSXP, nobs));
		break;
	    case STATA_SE_INT:
	    case STATA_SE_SHORTINT:
	    case STATA_SE_BYTE:
		SET_VECTOR_ELT(df, i, allocVector(INTSXP, nobs));
		break;
	    default:
		if (abyte > 244)
		    error(_("unknown data type"));
		SET_VECTOR_ELT(df, i, allocVector(STRSXP, nobs));
		break;
	    }
	}
    }

    /** names **/

    PROTECT(names = allocVector(STRSXP, nvar));
    for (i = 0; i < nvar; i++) {
	InStringBinary(fp, varnamelength+1, aname);
	SET_STRING_ELT(names, i, mkChar(nameMangle(aname, varnamelength+1)));
    }
    setAttrib(df, R_NamesSymbol, names);
    UNPROTECT(1);

    /** sortlist -- not relevant **/

    for (i = 0; i < 2*(nvar+1); i++) RawByteBinary(fp, 1);

    /** format list
	passed back to R as attributes.
	Used to identify date variables.
    **/

    PROTECT(tmp = allocVector(STRSXP, nvar));
    for (i = 0; i < nvar; i++) {
	InStringBinary(fp, fmtlist_len, timestamp);
	SET_STRING_ELT(tmp, i, mkChar(timestamp));
    }
    setAttrib(df, install("formats"), tmp);
    UNPROTECT(1);
    setAttrib(df, install("types"), types);


    /** value labels.  These are stored as the names of label formats,
	which are themselves stored later in the file. **/

    PROTECT(tmp = allocVector(STRSXP, nvar));
    for(i = 0; i < nvar; i++) {
	InStringBinary(fp, varnamelength+1, aname);
	SET_STRING_ELT(tmp ,i, mkChar(aname));
    }
    setAttrib(df,install("val.labels"), tmp);
    UNPROTECT(1); /*tmp*/

    /** Variable Labels **/

    PROTECT(varlabels=allocVector(STRSXP,nvar));

    switch(abs(version)){
    case 5:
	for(i = 0; i < nvar; i++) {
	    InStringBinary(fp, 32, datalabel);
	    SET_STRING_ELT(varlabels, i, mkChar(datalabel));
	}
	break;
    case 6:
    case 7:
    case 8:
    case 10:
    case 12:
	for(i = 0; i < nvar; i++) {
	    InStringBinary(fp, 81, datalabel);
	    SET_STRING_ELT(varlabels, i, mkChar(datalabel));
	}
    }
    setAttrib(df, install("var.labels"), varlabels);

    UNPROTECT(1);

    /** variable 'characteristics'  -- not yet implemented **/

    while(RawByteBinary(fp, 1)) {
	if (abs(version) >= 7) /* manual is wrong here */
	    charlen = (InIntegerBinary(fp, 1, swapends));
	else
	    charlen = (InShortIntBinary(fp, 1, swapends));
	for (i = 0; i < charlen; i++) InByteBinary(fp, 1);
    }
    if (abs(version) >= 7)
	charlen = (InIntegerBinary(fp, 1, swapends));
    else
	charlen = (InShortIntBinary(fp, 1, swapends));
    if (charlen != 0)
	error(_("something strange in the file\n (Type 0 characteristic of nonzero length)"));


    /** The Data **/

    if (version > 0) { /* not Stata/SE */
	for(i = 0; i < nobs; i++){
	    for(j = 0; j < nvar; j++){
		switch (INTEGER(types)[j]) {
		case STATA_FLOAT:
		    REAL(VECTOR_ELT(df,j))[i] = InFloatBinary(fp, 0, swapends);
		    break;
		case STATA_DOUBLE:
		    REAL(VECTOR_ELT(df,j))[i] = InDoubleBinary(fp, 0, swapends);
		    break;
		case STATA_INT:
		    INTEGER(VECTOR_ELT(df,j))[i] = InIntegerBinary(fp, 0, swapends);
		    break;
		case STATA_SHORTINT:
		    INTEGER(VECTOR_ELT(df,j))[i] = InShortIntBinary(fp, 0, swapends);
		    break;
		case STATA_BYTE:
		    INTEGER(VECTOR_ELT(df,j))[i] = (int) InByteBinary(fp, 0);
		    break;
		default:
		    charlen = INTEGER(types)[j] - STATA_STRINGOFFSET;
		    if(charlen > 244) {
			warning("invalid character string length -- truncating to 244 bytes");
			charlen = 244;
		    }
		    InStringBinary(fp, charlen, stringbuffer);
		    stringbuffer[charlen] = 0;
		    SET_STRING_ELT(VECTOR_ELT(df, j), i, mkChar(stringbuffer));
		    break;
		}
	    }
	}
    }  else {
	for(i = 0; i < nobs; i++){
	    for(j = 0;j < nvar; j++){
		switch (INTEGER(types)[j]) {
		case STATA_SE_FLOAT:
		    REAL(VECTOR_ELT(df,j))[i] = InFloatBinary(fp, 0, swapends);
		    break;
		case STATA_SE_DOUBLE:
		    REAL(VECTOR_ELT(df,j))[i] = InDoubleBinary(fp, 0, swapends);
		    break;
		case STATA_SE_INT:
		    INTEGER(VECTOR_ELT(df,j))[i] = InIntegerBinary(fp, 0, swapends);
		    break;
		case STATA_SE_SHORTINT:
		    INTEGER(VECTOR_ELT(df,j))[i] = InShortIntBinary(fp, 0, swapends);
		    break;
		case STATA_SE_BYTE:
		    INTEGER(VECTOR_ELT(df,j))[i] = (int) InByteBinary(fp, 0);
		    break;
		default:
		    charlen = INTEGER(types)[j]-STATA_SE_STRINGOFFSET;
		    if(charlen > 244) {
			warning("invalid character string length -- truncating to 244 bytes");
			charlen = 244;
		    }
		    InStringBinary(fp, charlen, stringbuffer);
		    stringbuffer[charlen] = 0;
		    SET_STRING_ELT(VECTOR_ELT(df,j), i, mkChar(stringbuffer));
		    break;
		}
	    }
	}
    }


    /** value labels **/
    if (abs(version) > 5) {
	/* There may be up to nvar value labels, but possibly 0 */
	PROTECT(labeltable = allocVector(VECSXP, nvar));
	PROTECT(tmp = allocVector(STRSXP, nvar));
	for(j = 0; j < nvar; j++) {
	    /* first int not needed, use fread directly to trigger EOF */
	    res = fread((int *) aname, sizeof(int), 1, fp);
	    if (feof(fp)) break;
	    if (res != 1) warning(_("a binary read error occurred"));
	    InStringBinary(fp, varnamelength+1, aname);
	    SET_STRING_ELT(tmp, j, mkChar(aname));
	    RawByteBinary(fp, 1); RawByteBinary(fp, 1); RawByteBinary(fp, 1); /*padding*/
	    nlabels = InIntegerBinary(fp, 1, swapends);
	    totlen = InIntegerBinary(fp, 1, swapends);
	    off =  Calloc((size_t) nlabels, int);
	    PROTECT(levels = allocVector(REALSXP, nlabels));
	    PROTECT(labels = allocVector(STRSXP, nlabels));
	    for(i = 0; i < nlabels; i++)
		off[i] = InIntegerBinary(fp, 1, swapends);
	    for(i = 0; i < nlabels; i++)
		REAL(levels)[i] = (double) InIntegerBinary(fp, 0, swapends);
	    txt =  Calloc((size_t) totlen, char);
	    InStringBinary(fp, totlen, txt);
	    for(i = 0; i < nlabels; i++)
		SET_STRING_ELT(labels, i, mkChar(txt+off[i]));
	    namesgets(levels, labels);
	    SET_VECTOR_ELT(labeltable, j, levels);
	    Free(off);
	    Free(txt);
	    UNPROTECT(2);/* levels, labels */
	}
	namesgets(labeltable, tmp);
	UNPROTECT(1); /*tmp*/
	if(j > 0 && j < nvar) {
	    labeltable = lengthgets(labeltable, j);
	    UNPROTECT(1);
	    PROTECT(labeltable);
	}
    }

    /** tidy up **/

    PROTECT(row_names = allocVector(STRSXP, nobs));
    for (i = 0; i < nobs; i++) {
	sprintf(datalabel, "%d", i+1);
	SET_STRING_ELT(row_names,i,mkChar(datalabel));
    }
    setAttrib(df, R_RowNamesSymbol, row_names);
    UNPROTECT(1);

    PROTECT(sversion = allocVector(INTSXP,1));
    INTEGER(sversion)[0] = (version == -7)? version : abs(version);
    setAttrib(df, install("version"), sversion);
    UNPROTECT(1);

    if (abs(version) > 5) {
	if(j > 0) setAttrib(df, install("label.table"), labeltable);
	UNPROTECT(1); /*labeltable*/;
    }
    UNPROTECT(2); /* types, df */
    return(df);
}
#endif
