#include "muste.h"
/* dat.c 17.7.85/SM (23.9.1991)

*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "survo.h"
#include "survoext.h"
#include "survolib.h"

#define TYPELEN 4
#define ERC 128
#define EQ '\176'

int dsp=0; /* dsp=1: ei sur_print-virheilmoituksia (GPLOT) */
int survo_ferror=0;
static FILE *survo_data;

int subst_survo_path(char *s)
    {

	subst_survo_path_in_editor(s);    
/*    
    char x[LLENGTH];
    char *p;
    int i;
    extern char *survo_path;
    extern char *muste_startpath;
	extern char muste_Rpath[LNAME]; // RS ADD 27.9.2012
	extern char muste_homedir[LNAME]; // RS ADD 20.10.2012    

	p=NULL;
    while (strchr(s,'<')!=NULL) // RS ADD 27.9.2012
        {
        p=strstr(s,"<Temp>");
        if (p==NULL) p=strstr(s,"<TEMP>");
        if (p==NULL) break;
        *p=EOS;
        strcpy(x,s);
        strcat(x,etmpd);
        i=strlen(x); x[i-1]=EOS;  // RS CHA oli x[i-3]
        strcat(x,p+6);
        strcpy(s,x);
        }

	if (p==NULL) 
    while (strchr(s,'<')!=NULL)
        {
        p=strstr(s,"<Sys>");
        if (p==NULL) p=strstr(s,"<SYS>");
        if (p==NULL) break;
        *p=EOS;
        strcpy(x,s);
        strcat(x,survo_path);strcat(x,"SYS/");
        i=strlen(x); x[i-1]=EOS;
        strcat(x,p+5);
        strcpy(s,x);
        }

    if (p==NULL) 
    while (strchr(s,'<')!=NULL)
        {
        p=strstr(s,"<Survo>");
        if (p==NULL) p=strstr(s,"<SURVO>");
        if (p==NULL) break;
        *p=EOS;
        strcpy(x,s);
        strcat(x,survo_path);
        i=strlen(x); x[i-1]=EOS;  // RS CHA oli x[i-3]
        strcat(x,p+7);
        strcpy(s,x);
        }
        
 if (p==NULL) // RS ADD 27.9.2012
    while (strchr(s,'<')!=NULL)
        {
        p=strstr(s,"<R>");
        if (p==NULL) break;
        *p=EOS;
        strcpy(x,s);
        strcat(x,muste_Rpath);
        i=strlen(x); x[i-1]=EOS;  // RS CHA oli x[i-3]
        strcat(x,p+3);
        strcpy(s,x);
        }       

    if (p==NULL) // RS ADD 20.10.2012
    while (strchr(s,'<')!=NULL)
        {
        p=strstr(s,"<Home>");
        if (p==NULL) break;
        *p=EOS;
        strcpy(x,s);
        strcat(x,muste_homedir);
        i=strlen(x); x[i-1]=EOS;
        strcat(x,p+6);
        strcpy(s,x);
        }        

    if (p==NULL) // RS ADD 20.10.2012    
    while (strchr(s,'~')!=NULL)
        {
        if (*s!='~') break;
        p=s; *p=EOS;
        strcpy(x,s);
        strcat(x,muste_homedir);
        i=strlen(x); x[i-1]=EOS;
        strcat(x,p+1);
        strcpy(s,x);
        } 

    if (p==NULL) // RS ADD 20.10.2012
    while (strchr(s,'<')!=NULL)
        {
        p=strstr(s,"<Start>");
        if (p==NULL) break;
        *p=EOS;
        strcpy(x,s);
        strcat(x,muste_startpath);
        i=strlen(x); x[i-1]=EOS;
        strcat(x,p+7);
        strcpy(s,x);
        } 
*/        
    return(1);
    }

int tilavajaus(SURVO_DATA_FILE *s)
        {
        sprintf(sbuf,"Not enough memory!");
        muste_fclose((*s).survo_data);
        if (dsp) return(-1);
        sur_print("\n"); sur_print(sbuf);
        WAIT; return(-1);
        }

void fi_close(SURVO_DATA_FILE *s)
        {
        muste_fclose((*s).survo_data);
//        if ((*s).fitext!=NULL) { 
        muste_free((*s).fitext); (*s).fitext=NULL;
//        if ((*s).varname!=NULL) { 
        muste_free((*s).varname); (*s).varname=NULL;
//        if ((*s).varpos!=NULL) { 
        muste_free((*s).varpos); (*s).varpos=NULL; 
//        if ((*s).varlen!=NULL) { 
        muste_free((*s).varlen); (*s).varlen=NULL; 
//        if ((*s).vartype!=NULL) { 
        muste_free((*s).vartype); (*s).vartype=NULL;
//        if ((*s).obs!=NULL) { 
        muste_free((*s).obs); (*s).obs=NULL;
        }

void fi_rewind(SURVO_DATA_FILE *s)
        {
        rewind((*s).survo_data); (*s).point=0L; (*s).mode=0;
        }

void fi_puts(SURVO_DATA_FILE *s, char *jakso, muste_int64 pit, muste_int64 paikka)
        {
        int i;
        muste_int64 ero=paikka-(muste_int64)(*s).point;


//Rprintf("\npaikka: %ld",paikka);
        if (ero || (*s).mode!=1)
            {
            muste_fseek((*s).survo_data,(muste_int64)paikka,0);
            }

        for (i=0; i<pit; ++i)
            {
            putc((unsigned char)jakso[i],(*s).survo_data);   // RS CHA (int) -> (unsigned char)
            }

        if (ferror((*s).survo_data)) survo_ferror=1;

//if (survo_ferror) Rprintf("\nFerror");        
        
        (*s).point=paikka+pit;  // RS 23.5.2012 CHECK (int)paikka works!
        (*s).mode=1;
        }


void fi_gets(SURVO_DATA_FILE *s, char *jakso, muste_int64 pit, muste_int64 paikka) // RS CHA int pit
        {
        muste_int64 i; // RS CHA int i
        muste_int64 ero;
        muste_int64 max;
        unsigned char apu;
    
		max=(muste_int64)((*s).data+(muste_int64)((*s).n-1)*(muste_int64)((*s).len));       

        if ((*s).point>max) 
        	{
//        	muste_fseek((*s).survo_data,0, SEEK_END);
        	(*s).point=(muste_int64)muste_ftell((*s).survo_data);	
        	}		

        ero=(muste_int64)(paikka-(muste_int64)(*s).point);

// Rprintf("\ngets paikka: %d, max: %d, pit: %d,point: %d",(int)paikka,(int)max,(int)pit,(int)(*s).point); 			



        if (ero || (*s).mode!=2)
        	{
//            muste_fseek((*s).survo_data,(long)paikka,SEEK_SET);  // RS CHA 0 -> SEEK_SET

// Rprintf("\ngets paikka: %d, spoint: %d, pit: %d",(int)paikka,(int)(paikka+pit),(int)pit); 			
 			
 			muste_fseek((*s).survo_data,(muste_int64)ero,SEEK_CUR);
// Rprintf(" ero: %d, todpaikka: %d",(int)ero,muste_ftell((*s).survo_data));  			
 						
/*          fseek((*s).survo_data,ero,SEEK_CUR);  */
			}
			
			

// Rprintf("\nfi_gets, from: %d :",(int)muste_ftell((*s).survo_data));
        for (i=0; i<pit; ++i) 
{
apu=(unsigned char)getc((*s).survo_data); 
// jakso[i]=(unsigned char)getc((*s).survo_data); // (unsigned char)getc((*s).survo_data); 
//Rprintf("%c,",(unsigned char)apu);
jakso[i]=apu;
}

           

  /*    fread(jakso,pit,1,(*s).survo_data);  */

        if (ferror((*s).survo_data)) survo_ferror=1;
//        (*s).point=(int)muste_ftell((*s).survo_data); // RS 
		ero=paikka+pit;
        if (ero>max) 
        	{
//        	muste_fseek((*s).survo_data,0, SEEK_END);
        	(*s).point=(muste_int64)muste_ftell((*s).survo_data);	
        	}
        else (*s).point=(muste_int64)ero;   // ((long)paikka+(long)pit); // RS CHA (long) -> (int)  
        
//Rprintf(" newpoint: %d",(*s).point);        
        (*s).mode=2;
        jakso[pit]=EOS; /* ylittää 1:llä aik. varatun tilan 3.3.1996 */
       
// Rprintf("\n%s",jakso);       
            
        }

void fi_miss_save(SURVO_DATA_FILE *s,long j,int i)
        {
        double x;

        switch ((*s).vartype[i][0])
            {
          case '1': x=(double)MISSING1; fi_save(s,j,i,&x); break;
          case '2': x=(double)MISSING2; fi_save(s,j,i,&x); break;
          case '4': x=(double)MISSING4; fi_save(s,j,i,&x); break;
          case '8': x=(double)MISSING8; fi_save(s,j,i,&x); break;
          case 'S': fi_save(s,j,i,space); break;
            }
        }

void fi_init_save(SURVO_DATA_FILE *s,long j,int i,char *initstring) // RS 22.11.2012
        {
        double x;
        
        switch ((*s).vartype[i][0])
            {
            case 'S': 
				strcpy(sbuf,initstring);
				strncat(sbuf,space,LLENGTH);
				fi_save(s,j,i,sbuf); break;
			default:
				x=(double)atoi(initstring);	
          		fi_save(s,j,i,&x); break;          
            }
        }

void fi_miss_obs(SURVO_DATA_FILE *s,long j)
        {
        int i;

        for (i=0; i<(*s).m; ++i) fi_miss_save(s,j,i);
        }

int fi_increase_n(SURVO_DATA_FILE *s,long n_new_cases)
        {
        long l,ln;

        ln=s->n+1;
        fi_rewind(s);
        s->n+=n_new_cases;
        fi_puts(s,(char *)&s->n,sizeof(int),22);  // RS CHA 64-BIT sizeof(long) -> sizeof(int) 22L -> 22

        for (l=ln; l<=s->n; ++l)
            fi_miss_obs(s,l);
        if (ferror(s->survo_data)) return(-1);
        return(1);
        }



int fi_find2(char *nimi, SURVO_DATA_FILE *s, char *pathname, int kirjoitus)
        {
        strcpy(pathname,nimi);
/* RS FIXME: Levytunnus ei ehkä porttautuvaa koodia */
        if (!muste_is_path(nimi)) // RS CHA if ((strchr(nimi,':')==NULL) && (nimi[0]!='/') && (nimi[0]!='~') && (nimi[0]!='\\') && (nimi[0]!='<') && (nimi[0]!='.')) // RS ADD unix path FIXME
            { strcpy(pathname,edisk); strcat(pathname,nimi); }
        muste_append_path(pathname,".SVO"); // RS CHA if (strchr(pathname+strlen(pathname)-4,'.')==NULL) strcat(pathname,".SVO");
            
        if (pathname[strlen(pathname)-1]=='.') pathname[strlen(pathname)-1]=EOS; // RS ADD
            
/*
        if (sur_file_time_check(pathname)==-2) return(-2);
*/

        if (kirjoitus)
            (*s).survo_data=muste_fopen(pathname,"r+b");
        else
            (*s).survo_data=muste_fopen(pathname,"rb");
            
//		if ((*s).survo_data==NULL && kirjoitus) (*s).survo_data=muste_fopen(pathname,"w+b");   // RS ADD         
        if ((*s).survo_data==NULL) return(-1);
        return(1);
        }

int fi_find(char *nimi, SURVO_DATA_FILE *s, char *pathname)
        {
        return(fi_find2(nimi,s,pathname,1));
        }

int fi_to_write(char *nimi, SURVO_DATA_FILE *s)
        {
        int i;
        char pathname[LLENGTH];
        char jakso[LLENGTH];

        muste_fclose((*s).survo_data);
        i=fi_find2(nimi,s,pathname,1);
        if (i<0)
            {
            sprintf(jakso,"Cannot write to data %s",pathname);
            if (etu==2) { sprintf(tut_info,"___@1@FILE OPEN@%s@",jakso); return(-1); }
            sprintf(sbuf,"\n%s",jakso); sur_print(sbuf);
            WAIT; return(-1);
            }
        return(1);
        }

int fi_var_save(SURVO_DATA_FILE *s, int i, char *vartype, int varlen, char *varname)
        {
/*        unsigned */ char jakso[LLENGTH];
        int h,k;

        if (i>(*s).m1-1)
            {
            sprintf(sbuf,"\nMax.# of fields in current file is %d",(*s).m1); sur_print(sbuf);
            sprintf(sbuf,"\nNot space for field %.8s",varname); sur_print(sbuf);
            sprintf(sbuf,"\nIncrease capacity by FILE EXPAND <data_file>"); sur_print(sbuf);
            WAIT; return(-1);
            }
        if (i>(*s).m-1)
            {
            i=(*s).m;
            (*s).varpos[i]=(*s).varpos[i-1]+(*s).varlen[i-1];
            (*s).varlen[i]=varlen;
            if ((*s).varpos[i]+varlen>(*s).len)
                {
                sur_print("\nNo space in current file for new field:");
                sprintf(sbuf," %.8s",varname); sur_print(sbuf);
                sprintf(sbuf,"\nIncrease record length %d by FILE EXPAND <data_file>.",
                        (*s).len); sur_print(sbuf);
                WAIT; return(-1);
                }
            strcpy(jakso,varname); jakso[8]=EOS;
            k=strlen(jakso); for (h=k; h<8; ++h) jakso[h]=' ';
            for (h=0; h<(*s).m; ++h)
                {
                if (strncmp(jakso,(*s).varname[h],8)==0)
                    {
                    sprintf(sbuf,"\nFieldname %.8s already in use!",(*s).varname[h]);
                        sur_print(sbuf);
                    WAIT; return(-1);
                    }
                }
            ++(*s).m;
            fi_rewind(s);
            fi_puts(s,(char *)&((*s).m),2,20L);
            }
        *(int *)jakso=(*s).varpos[i];
        *(int *)(jakso+2)=(*s).varlen[i];
        for (h=0; h<(*s).extra-4; ++h) { jakso[h+4]=' ';
                                         (*s).vartype[i][h]=' '; }
        for (h=0; h<(*s).extra-4; ++h)
            {
            if (vartype[h]==EOS) break;
            jakso[h+4]=vartype[h]; (*s).vartype[i][h]=vartype[h];
            }

        (*s).vartype[i][(*s).extra-4]=EOS;
        for (h=0; h<(*s).l; ++h) jakso[h+(*s).extra]=' ';
        for (h=0; h<(*s).l; ++h)
            {
            if (varname[h]==EOS) break;
            jakso[h+(*s).extra]=varname[h];
            }

        fi_rewind(s);
        fi_puts(s,jakso,(*s).l+(*s).extra,
                     (long)((*s).var+(long)i*((long)(*s).l+(long)(*s).extra)));
        return(1);
        }

int fi_value_to_string(SURVO_DATA_FILE *s,int i,double x,char *sana)
        {
        int varlen;

        varlen=s->varlen[i];
        if (fabs(x)>MISSING8/1000.0)
            {
            strncpy(sana,space,varlen);
            }
        else
            {
            int tarkkuus=varlen;

            if (tarkkuus==1) fconv(x,"",sana);
            else
                {
                if (tarkkuus>accuracy+2) tarkkuus=accuracy+2;
                fnconv(x,tarkkuus,sana);
                }
            if (strlen(sana)>varlen) strncpy(sana,space,varlen);
            }
        return(1);
        }


void fi_save(SURVO_DATA_FILE *s,
long j,         /* havainnon nro 1,2,3,... */
int i,          /* muuttuja 0,1,2,... */
char *sana     /* talletettava tieto */
)
        {
 /*       unsigned */ char jakso[8];
        int pit=4;
        char *p;

       if (j<1L) { sur_print("Survo saving error!"); /* getch(); */ return; }
        p=jakso;
        switch ((*s).vartype[i][0])
            {
          case '1':
                    if (*(double *)sana<0.0 || *(double *)sana>255.0)
                        *jakso=(unsigned char)MISSING1;
                    else
                        *jakso=(unsigned char)(*(double *)sana);
                    pit=1;
                    break;
          case '2':
                    if (*(double *)sana<-32768.0 || *(double *)sana>32767.0)
                        *(short *)jakso=MISSING2;
                    else
                        *(short *)jakso=*(double *)sana;
                    pit=2;
                    break;
          case '4': *(float *)jakso=*(double *)sana;
                    pit=4;
                    break;
          case '8': p=sana; pit=8; break;

          case 'S': p=sana; pit=(*s).varlen[i]; break;
            }
//Rprintf("\nfi_save: i=%d pit=%d j=%ld len=%d data=%ld pos=%d",
//               i,pit,j,(*s).len,(*s).data,(*s).varpos[i]); // getch();
//    fi_rewind(s); 
  fi_puts(s,p,(long)pit,(long)((*s).data+(j-1L)*(long)(*s).len+(long)(*s).varpos[i]));
    // RS CHA (long)
  
        }




int not_float(unsigned char *s)
        {
        if (s[3]==255) return(1);
        return(0);
        }

int not_double(unsigned char *s)
        {
        if (s[6]>=224 && s[7]==255) return(1);
        return(0);
        }

void fi_alpha_load(
SURVO_DATA_FILE *s,
long j,         /* havainnon nro 1,2,3,... */
int i,          /* muuttuja 0,1,2,... */
char *jakso     /* luettava tieto */
)
        {

// Rprintf("\nfialo: %d",((*s).varpos[i]));
        
        fi_gets(s,jakso,(long)(*s).varlen[i],
              // RS CHA (long)
                 (long)((*s).data+(j-1L)*(long)(*s).len+(long)(*s).varpos[i]));

// RS 64-bit kokeilu     sprintf(sbuf,"%u",(unsigned int)((*s).data+(j-1L)*(long)(*s).len+(long)(*s).varpos[i]));
//        fi_gets(s,jakso,(*s).varlen[i],atoi(sbuf));


/* RS Character encoding kokeilu
   int luuppi=0;
   while (jakso[luuppi]!='\0') { 
       if ((unsigned char)jakso[luuppi]>127) jakso[luuppi]='?'; 
       luuppi++;
   }
*/
/*
#include <R_ext/Riconv.h>

    void *cd = NULL;
    size_t  i_len, o_len, status;
    char buf[256];
    const char *i_buf;
    char *o_buf;

    strcpy(buf,jakso);
    i_buf=(char *)buf;
    o_buf=(char *)jakso;
    cd = Riconv_open("LATIN1", "CP850");
    o_len = i_len = strlen(i_buf);
    status = Riconv(cd, &i_buf, (size_t *)&i_len, &o_buf, (size_t *)&o_len);

    Riconv_close(cd);
*    
********************************/

        }


int fi_load(   /* numeerinen */
SURVO_DATA_FILE *s,
long j,         /* havainnon nro 1,2,3,... */
int i,          /* muuttuja 0,1,2,... */
double *px      /* luettava tieto */
)
        {
/*        unsigned */ char jakso[LLENGTH];

        fi_gets(s,jakso,(long)(*s).varlen[i],
              // RS CHA (long)
                 (long)((*s).data+(j-1L)*(long)(*s).len+(long)(*s).varpos[i]));
                
// RS 64 bit kokeilu           sprintf(sbuf,(long)((*s).data+(j-1L)*(long)(*s).len+(long)(*s).varpos[i])); 
//           fi_gets(s,jakso,(*s).varlen[i],atoi(sbuf));
                      
               
        switch ((*s).vartype[i][0])
            {
          case '1': *px=(double)((unsigned char)*jakso);
                    if (*px==MISSING1) *px=MISSING8;
                    return(1);
          case '2': *px=(double)(*((short *)jakso));
                    if (*px==MISSING2) *px=MISSING8;
                    return(1);
          case '4': if (not_float((unsigned char *)jakso)) { *px=MISSING8; return(1); }
                    *px=(double)(*((float *)jakso));
                    if (*px>MISSING4/10.0) *px=MISSING8;
                    return(1);
          case '8': if (not_double((unsigned char *)jakso)) { *px=MISSING8; return(1); }
                    *px=*((double *)jakso);
                    return(1);
          case 'S': if (strncmp(jakso,space,(unsigned int)(*s).varlen[i])!=0)
                        *px=atof(jakso);
                    else
                        *px=MISSING8;
                    return(1);
            }
        return(1);
        }



int fi_open3(
char *nimi,
SURVO_DATA_FILE *s,
int laaja,        /* 1= m1 muuuttujaa 0= m muuttujaa */
int kokonimet,    /* 1= koko nimi  0=vain kutsumanimet (8 merkkiä) */
int tekstitieto,  /* 1= tekstiosa luetaan 0=ei tekstiosaa */
int kirjoitus     /* 1= kirjoitus sallittu 0=ei sallittu */
)
        {
        int h;
        long i;  // RS CHA int i
        char *p;
/* char *q; */
        char pathname[LLENGTH];
/*        unsigned */ char alku[LLENGTH];
/*        unsigned */ char jakso[LLENGTH];
        unsigned int tekstiosa,pteksti,nimet,pnimet,tyypit,ptyypit;
        int m;
        long l; // RS CHA int l
        char name[LLENGTH];
        long li;
        int muste_posextra; // RS ADD 23.5.2012

        strcpy(name,nimi);
        if (*name=='*') { if (name[1]==EOS) strcpy(name,active_data); }          
        i=fi_find2(name,s,pathname,kirjoitus);
        if (i<0)
            {
            sprintf(jakso,"Cannot open Survo data file %s!",pathname);
            if (etu==2) { sprintf(tut_info,"___@1@FILE OPEN@%s@",jakso); return(-1); }
            PR_EINV; sprintf(sbuf,"%s",jakso);
            if (dsp) return(-1);
            sur_print("\n"); sur_print(sbuf);
            WAIT; return(-1);
            }

        strcpy(active_data,name);
        (*s).point=0L; (*s).mode=0;
        fi_gets(s,alku,64,0L);
        if (strncmp(alku,"SURVO 84C DATA",14)!=0
        	&& strncmp(alku,"MUSTE DATA",10)!=0) // RS ADD 13.7.2012
            {
            sprintf(jakso,"%s is not a Survo data file!",pathname);
            if (etu==2) { sprintf(tut_info,"___@2@FILE OPEN@%s@",jakso); return(-1); }
            PR_EINV; sprintf(sbuf,"%s",jakso);
            if (dsp) return(-1);
            sur_print("\n"); sur_print(sbuf);
            WAIT; return(-1);
            }
        (*s).len=*(short *)(alku+16);        
        muste_posextra=0; // RS ADD 23.5.2012
        
		if ((*s).len<0 || (*s).len>32750) // RS ADD 23.5.2012 
			{
			(*s).len=*(int *)(alku+46); 
			muste_posextra=4;
			}   
			  
// Rprintf("\nlen: %d",(*s).len);        
        (*s).m1=*(short *)(alku+18);
        (*s).m=*(short *)(alku+20);
        (*s).n=*(long *)(alku+22);
        (*s).l=*(short *)(alku+26);
        (*s).extra=*(short *)(alku+28);
        (*s).textn=*(short *)(alku+30);
        (*s).textlen=*(short *)(alku+32);
        (*s).text=*(long *)(alku+34);
        (*s).var=*(long *)(alku+38);
        (*s).data=*(long *)(alku+42);

        if (laaja) m=(*s).m1; else m=(*s).m;
        if (kokonimet) l=(*s).l; else l=8;
        tekstiosa=(*s).textn*((*s).textlen+1);
        pteksti=(*s).textn*sizeof(char *);

        li=(long)m*(long)(l+1)+(long)sizeof(char *)*(long)m;

        nimet=m*(l+1);                      /* nimitekstit */
        pnimet=sizeof(char *)*m;            /* nimiosoittimet */

        tyypit=((*s).extra-4+1)*m;
        ptyypit=sizeof(char *)*m;


        if (!tekstitieto) (*s).fitext=NULL;
        else
            {

            (*s).fitext=(char **)muste_malloc((unsigned int)(pteksti+tekstiosa+1));
            if ((*s).fitext==NULL) { tilavajaus(s); return(-1); }
            p=(char *)(*s).fitext; p+=pteksti;
            for (i=0; i<(*s).textn; ++i)
                {
                (*s).fitext[i]=p;
                fi_rewind(s); 
                
// Rprintf("\nop31 pit: %u,paikka: %u",(long)(*s).textlen,(long)((long)(*s).text+(long)i*(long)(*s).textlen));
                
                
                fi_gets(s,jakso,(long)(*s).textlen,(long)((long)(*s).text+(long)i*(long)(*s).textlen));                 
                          // RS (long)
// RS 64-bit kokeilu                sprintf(sbuf,"%u",(unsigned int)((*s).text+i*((*s).textlen)));
//                fi_gets(s,jakso,(*s).textlen,atoi(sbuf));


/* RS Character encoding kokeilu

   int luuppi=0;
   while (jakso[luuppi]!='\0') { 
       if ((unsigned char)jakso[luuppi]>127) jakso[luuppi]='?'; 
       luuppi++;
   }
*********************************/

                for (h=0; h<(*s).textlen; ++h) *p++=jakso[h];
                *p++=EOS;
                }

            }             
        (*s).varname=(char **)muste_malloc((size_t)(pnimet+nimet));
        if ((*s).varname==NULL) { tilavajaus(s); return(-1); }
        p=(char *)(*s).varname;
        p+=pnimet;
        if (laaja)
            {
            for (i=(*s).m; i<(*s).m1; ++i) (*s).varname[i]=p+(long)i*(long)(l+1);
            }
        for (i=0; i<(*s).m; ++i)
            {
            (*s).varname[i]=p;
      fi_rewind(s); 
// Rprintf("\nop32 npit: %u,paikka: %u",(long)l,(long)((long)(*s).var+(long)i*((long)(*s).l+(long)(*s).extra)+(long)(*s).extra));

      fi_gets(s,jakso,(long)l,(long)((long)(*s).var+(long)i*((long)(*s).l+(long)(*s).extra)+(long)(*s).extra));
                // RS (long)
// RS 64-bit kokeilu                sprintf(sbuf,"%u",(unsigned int)((*s).var+(long)i*((long)(*s).l+(long)(*s).extra)+(long)(*s).extra));
//                fi_gets(s,jakso,l,atoi(sbuf));


/* RS Character encoding kokeilu 
   int luuppi=0;
   while (jakso[luuppi]!='\0') { 
       if ((unsigned char)jakso[luuppi]>127) jakso[luuppi]='?'; 
       luuppi++;
   }
**********************************/


            for (h=0; h<l; ++h) *p++=jakso[h];
            *p++=EOS;

// Rprintf("varname: %s\n",(*s).varname[i]); // RS

            }                       
        (*s).varpos=(int *)muste_malloc(m*sizeof(int));  // RS CHA sizeof(short)
        if ((*s).varpos==NULL) { tilavajaus(s); return(-1); }
        (*s).varlen=(short *)muste_malloc(m*sizeof(int)); // RS CHA sizeof(short)
        if ((*s).varlen==NULL) { tilavajaus(s); return(-1); }
        for (i=0; i<(*s).m; ++i)
            {
            fi_rewind(s);
// Rprintf("\nop33 npit: %u,paikka: %u",(long)4,(long)((long)(*s).var+(long)i*((long)(*s).l+(long)(*s).extra)));

            fi_gets(s,jakso,(long)4,(long)((long)(*s).var+(long)i*((long)(*s).l+(long)(*s).extra)));
                      // RS (long)         (long)
// RS 64-bit kokeilu                sprintf(sbuf,"%u",(unsigned int)((*s).var+(long)i*((long)(*s).l+(long)(*s).extra)));
//                fi_gets(s,jakso,4,atoi(sbuf));


            (*s).varpos[i]=*(short *)jakso; // RS int
            (*s).varlen[i]=*(int *)(jakso+2); // RS int
            } 
        (*s).vartype=(char **)muste_malloc(ptyypit+tyypit);
        if ((*s).vartype==NULL) { tilavajaus(s); return(-1); }
        p=(char *)(*s).vartype; p+=ptyypit;
        if (laaja)
            {
         for (i=(*s).m; i<(*s).m1; ++i) (*s).vartype[i]=p+(long)i*((long)(*s).extra-4L+1L);
            }           
        for (i=0; i<(*s).m; ++i)
            {
            (*s).vartype[i]=p;

        fi_rewind(s);
        
// Rprintf("\nop34 npit: %u,paikka: %u",(long)((*s).extra-4),(long)((long)(*s).var+(long)i*((long)(*s).l+(long)(*s).extra)+4L));
        
        fi_gets(s,jakso,(long)((*s).extra-4),(long)((long)(*s).var+(long)i*((long)(*s).l+(long)(*s).extra)+4L));
                  // RS (long)                      (long)

// RS 64-bit kokeilu        sprintf(sbuf,"%u",(unsigned int)((*s).var+(long)i*((long)(*s).l+(long)(*s).extra)+4L));
//        fi_gets(s,jakso,(*s).extra-4,atoi(sbuf));



            for (h=0; h<(*s).extra-4-muste_posextra; ++h) *p++=jakso[h]; // RS ADD 23.5.2012 muste_posextra
            *p++=EOS;
//            if (muste_posextra && (*s).varpos[i]>32750) Rprintf("\npos %d: %d",i,*(int *)(jakso+h));             
            if (muste_posextra && (*s).varpos[i]>32750) (*s).varpos[i]=(unsigned int)(*(int *)(jakso+h)); // RS ADD 23.5.2012       
            }
        (*s).obs=muste_malloc((unsigned int)((*s).len+1)); /* fi_gets() tarvitsee +1  3.3.1996 */
        if ((*s).obs==NULL) { tilavajaus(s); return(-1); }        
        return(1);
        }

int fi_open2(
char *nimi,
SURVO_DATA_FILE *s,
int laaja,        /* 1= m1 muuuttujaa 0= m muuttujaa */
int kokonimet,    /* 1= koko nimi  0=vain kutsumanimet (8 merkkiä) */
int tekstitieto   /* 1= tekstiosa luetaan 0=ei tekstiosaa */
)
        {
        return(fi_open3(nimi,s,laaja,kokonimet,tekstitieto,1));
        }

int fi_open(char *nimi, SURVO_DATA_FILE *s)
        {
        return(fi_open3(nimi,s,0,0,0,1));
        }

void fi_alpha_save(
SURVO_DATA_FILE *s,
long j,         /* havainnon nro 1,2,3,... */
int i,          /* muuttuja 0,1,2,... */
char *jakso     /* luettava tieto */
)
        {
        
//Rprintf("\npaikka: %d, pos: %d, jakso: %s",((*s).data+(j-1L)*(long)(*s).len+(long)(*s).varpos[i]),(*s).varpos[i],jakso);        
        fi_puts(s,jakso,(*s).varlen[i],
                 (long)((*s).data+(j-1L)*(long)(*s).len+(long)(*s).varpos[i]));
        }


static int cre_del(char *pathname)
        {
  /*    char x[LLENGTH];   */

sur_delete1(pathname);  // RS CHA        remove(pathname);
/*      sprintf(x,"DEL %s",pathname);
        system(x);
*/
        return(1);
        }


static int talleta(char *jono,int pit,long paikka)
        {
        int i;

        muste_fseek(survo_data,paikka,0);
        for (i=0; i<pit; ++i)
           {
           putc((int)jono[i],survo_data);
           }
        if (ferror(survo_data))
            {
            muste_fclose(survo_data);
            sur_print("\nCannot save in data file!");
            WAIT; return(-1);
            }
        return(1);
        }

int fi_create(char *filename,int filen,int fim1,int fim,long fin,int fil,int fiextra,
int fitextn, int fitextlen, char *fitext[],char *varname[],int varlen[],char *vartype[])
        {
        int i,h;
        char pathname[LNAME],tempname[LNAME];
        char jakso[LLENGTH]; // RS REM unsigned
        long osfitext,osfivar,osfidata;
        int pos;
        char varname0[9];
        int varname_error=0;
        int muste_posextra;  // RS ADD 23.5.2012 
/*************************
                    Rprintf("\nfilename=%s filen=%d fim1=%d fim=%d fin=%ld fil=%d",
                              filename,filen,fim1,fim,fin,fil);
                    Rprintf("\nfitextn=%d fitextlen=%d",fitextn,fitextlen);

                    for (i=0; i<fitextn; ++i) Rprintf("\n%.64s",fitext[i]);
                    getch();
                    for (i=0; i<fim; ++i) Rprintf("\n%d %s",i+1,varname[i]);
                    Rprintf("\nvarlen:");
                    for (i=0; i<fim; ++i)
                        Rprintf("\n%d %d %.3s",i+1,varlen[i],vartype[i]);

                    getch();
*****************/
/*
        li=(long)fim1*((long)(fil+1)+(long)sizeof(char *));
        if (li>65535L)
            {
            sur_print("\nToo long names of fields or too many fields!");
            sprintf(sbuf,"\nnamelength nl=%d  # of fields m1=%d  Restriction: m1*(nl+%d+1)<65536.",
                                   fil,fim1,sizeof(char *));
            sur_print(sbuf); WAIT; return(-1);
            }
*/
        strcpy(pathname,filename);
// RS FIXME pathname
        if (!muste_is_path(pathname)) // RS CHA if (strchr(pathname,':')==NULL && *pathname!='/' && *pathname!='.' && *pathname!='<' && *pathname!='~' && *pathname!='\\' )
            { strcpy(pathname,edisk); strcat(pathname,filename); }
            
        muste_append_path(pathname,".SVO"); // RS CHA if (strchr(pathname+strlen(pathname)-4,'.')==NULL) strcat(pathname,".SVO");
        

        if (sur_find_file(pathname)) // RS CHA survo_data!=NULL)
            {
            if (erun==0 && etu==0)
            	{
// RS REM            survo_data=muste_fopen(pathname,"rb");
// RS REM            muste_fclose(survo_data);

                sprintf(sbuf,"\nFile %s already exists!",pathname); sur_print(sbuf);
                sur_print("\nOverwrite (Y/N)? ");
                i=0;
                while (i<31 || i>256) i=sur_getch(); // RS CHA
                if (i!='Y' && i!='y') return(-1);
                }
            snprintf(tempname,LNAME,"%sMUSTEFC.SVO",etmpd); // RS ADD    
            sur_copy_file(pathname,tempname); // RS ADD 
            sur_delete(pathname); // 1.1.2009
        	}
		survo_data=NULL; // RS ADD 16.10.2012
        survo_data=muste_fopen(pathname,"wb");
        if (survo_data==NULL)
            {
            sprintf(sbuf,"\nNo access to file %s",pathname); sur_print(sbuf);
            WAIT; return(-1);
            }
        if (fil<8)
            {
            sur_print("\nAt least 8 bytes must be reserved for each field name!");
            WAIT; return(-1);
            }

        if (fitextlen>250) fitextlen=250;
//Rprintf("\nfilen: %d",filen);


        osfitext=64L;
        osfivar=(long)(osfitext+(long)fitextn*(long)fitextlen);
        for (i=0; i<LLENGTH; i++) jakso[i]=0; // RS ADD 16.10.2012
        strcpy(jakso,"SURVO 84C DATA");
        muste_posextra=0; // RS ADD 23.5.2012
        if (filen>32750) // RS ADD 23.5.2012
        	{
        	strcpy(jakso,"MUSTE DATA    "); // RS ADD 13.7.2012 
        	*(short *)(jakso+16)=32751;
        	muste_posextra=4;
        	fiextra+=4;   
        	}     
        else *(short *)(jakso+16)=filen;
        
        osfidata=(long)(osfivar+(long)fim1*((long)fil+(long)fiextra));
        
        *(short *)(jakso+18)=fim1;
        *(short *)(jakso+20)=fim;
        *(long *)(jakso+22)=fin;
        *(short *)(jakso+26)=fil;
        *(short *)(jakso+28)=fiextra;
        *(short *)(jakso+30)=fitextn;
        *(short *)(jakso+32)=fitextlen;
        *(long *)(jakso+34)=osfitext;
        *(long *)(jakso+38)=osfivar;
        *(long *)(jakso+42)=osfidata;
        *(int *)(jakso+46)=filen; // RS ADD 23.5.2012
        for (i=50; i<64; ++i) jakso[i]=' '; // RS CHA 23.5.2012 46->50

        i=talleta(jakso,64,0); if (i<0) return(-1);

        for (i=0; i<fitextn; ++i)
            {
            if (strlen(fitext[i])>249) fitext[i][249]=EOS;
            strcpy(jakso,fitext[i]);
            for (h=strlen(fitext[i]); h<fitextlen; ++h) jakso[h]=' ';
            h=talleta(jakso,fitextlen,(long)(osfitext+(long)i*(long)fitextlen));
            if (h<0) return(-1);
            }
        h=0; for (i=0; i<fim; ++i) h+=varlen[i];
        if (h>filen)
            {
            sprintf(sbuf,"\nRecord length %d too small (%d required, at least)",
                        filen,h); sur_print(sbuf);
            WAIT; muste_fclose(survo_data); cre_del(pathname); return(-1);
            }

        pos=0;
        for (i=0; i<fim; ++i)
            {
            for (h=0; h<i; ++h)
                {
                if (strncmp(varname[i],varname[h],8)==0)
                    {
                    if (strncmp(varname[i],varname0,8)!=0)
                        {
                        sprintf(sbuf,"\nField name %.8s appears at least twice!",varname[i]);
                             sur_print(sbuf);
                        *varname0=EOS; strncpy(varname0,varname[i],8);
                        varname_error=1;
                        }
                    }
                }

            if (pos>32750) // RS ADD 23.5.2012
            	{
            	*(short *)jakso=32751;
            	}
            else *(short *)jakso=pos;
            *(short *)(jakso+2)=varlen[i];
            for (h=0; h<fiextra-4; ++h) jakso[h+4]=' ';
            for (h=0; h<fiextra-4-muste_posextra; ++h) // RS ADD 23.5.2012 muste_posextra
                {
                if (vartype[i][h]==EOS) break;
                jakso[h+4]=vartype[i][h];
                }
            if (muste_posextra) *(int *)(jakso+fiextra-muste_posextra)=pos; // RS ADD 23.5.2012
            pos+=varlen[i];            
            for (h=0; h<fil; ++h) jakso[h+fiextra]=' ';
            for (h=0; h<fil; ++h)
                {
                if (varname[i][h]==EOS) break;
                jakso[h+fiextra]=varname[i][h];
                }
            h=talleta(jakso,fil+fiextra,(long)(osfivar+(long)i*((long)fil+(long)fiextra)));
            if (h<0) return(-1);
            }

        if (varname_error)
            {
            sur_print("\n***********************************************");
            sur_print("\n Survo data file will be created, but");
            sur_print("\n use FILE STATUS/UPDATE for field name editing!");
            sur_print("\n***********************************************");
            WAIT;
            sur_print("\n");
            }

        if (fin>0L)
            {
            int disp;
            long il;
            char *rec;

            disp=1;
            sur_print("\nSaving 0's as default values...");
            rec=muste_malloc(filen);
            if (rec==NULL)
                {
                sur_print("\nNot space enough!");
                WAIT; return(-1);
                }
            for (i=0; i<filen; ++i) rec[i]='\0';
            for (il=0L; il<fin; ++il)
                {
                h=talleta(rec,filen,(long)(osfidata+(long)il*(long)filen));
                if (h<0) return(-1);
//  21.11.01    if (disp) { sprintf(sbuf," %ld",il); sur_print(sbuf); }
//              if (kbhit()) { disp=1-disp; getch(); }
                }
            muste_free(rec);
            }

        muste_fclose(survo_data);
        return(1);
        }

int data_to_write(char *name,SURVO_DATA *d)
        {
        if (d->type!=2) return(1);
        return(fi_to_write(name,&(d->d2)));
        }

int create_newvar1(SURVO_DATA *d,char *name,char type,int len,char act) // 29.8.2000
        {
        char vartype[LLENGTH];
        int i;
        int slen;

        if (d->type!=2)
            {
            sprintf(sbuf,"\nNew variable %s cannot be created!",name); sur_print(sbuf);
            WAIT; return(-1);
            }
        strncpy(vartype,space,d->d2.extra-4);
        *vartype=type;
        vartype[1]=act; vartype[2]='-';
        if (type=='S') slen=len; else slen=type-'0';
        i=fi_var_save(&d->d2,d->d2.m,vartype,slen,name);
        if (i<0) return(-1);
        d->m=d->d2.m;
        i=d->m-1;
        strncpy(d->varname[i],space,8);
        strncpy(d->varname[i],name,8);
        return(i); /* index of newvar */
        }


int create_newvar(SURVO_DATA *d,char *name,char type,int len)
/* len;  used in 'S' type only */
    {
    return(create_newvar1(d,name,type,len,'A'));
    }

int update_varname(SURVO_DATA *d,int i,char *s)
/* i; # of variable */
/* s new comment */
        {
        char x[LLENGTH];
        int k,len,h;

        if (d->type!=2) return(1);
        len=d->d2.l; if (len<10) return(1);
        fi_rewind(&(d->d2));
        fi_gets(&(d->d2),x,d->d2.l,
          (long)(d->d2.var+(long)i*((long)len+(long)d->d2.extra)+(long)d->d2.extra));
        x[len]=EOS;

        k=8; while (x[k]==' ' && k<len) ++k;
        if (k==len || x[k]==EQ)
            {
            if (x[k]==EQ) for (h=k; h<len; ++h) x[h]=' ';
            x[9]=EQ;
            h=0; while (h<strlen(s) && h+10<len) { x[h+10]=s[h]; ++h; }
        fi_rewind(&(d->d2));
        fi_puts(&(d->d2),x,d->d2.l,
          (long)(d->d2.var+(long)i*((long)len+(long)d->d2.extra)+(long)d->d2.extra));
            }
        return(1);
        }

void rem_update(SURVO_DATA *d,char *key,char *text)
        {
        int i,len;
        char *p,*q;
        char x[LLENGTH];

        p=NULL;
        if (d->d2.textn==0) return;
        for (i=0; i<d->d2.textn; ++i)
            { p=strstr(d->d2.fitext[i],key); if (p!=NULL) break; }
        if (p==NULL)
            {
            len=strlen(key)+strlen(text)+2;
            for (i=0; i<len; ++i) x[i]=' '; x[i]=EOS;
            for (i=d->d2.textn-1; i>=0; --i)
                {
                p=strstr(d->d2.fitext[i],x);
                if (p==NULL || p-d->d2.fitext[i]>c3-1) continue;
                strcpy(x,key); strcat(x,text);
                ++p; q=x;
                while (*q) *p++=*q++;
                break;
                }
            if (i<0) return;
            }
        else
            {
            p+=strlen(key);
            q=text;
            while (*p && *q) *p++=*q++;
            while (*p && *p!=' ') *p++=' ';
            }


        for (i=0; i<d->d2.textn; ++i)
            {
            fi_rewind(&d->d2);
            fi_puts(&d->d2,d->d2.fitext[i],d->d2.textlen,
               (long)(d->d2.text+(long)i*(long)(d->d2.textlen)));
            }

        }


int tilavirhe()
        {
        sprintf(sbuf,"Not enough memory!");
        if (dsp) return(-1);
        sur_print("\n"); sur_print(sbuf);
        WAIT; return(-1);
        }


int ma_close(SURVO_DATA_MATRIX *s)
        {
//        if(s->pma != NULL) { 
        muste_free(s->pma); s->pma=NULL;
//        } // RS ADD NULL Check
        return(1);
        }

int ma_missing(char *p)
        {
        int len,puuttuu;
        char *q;

        /* puuttuva esim. "   " tai " - " tai "---" tai "123-"  */

        len=strlen(p); puuttuu=0;
        if (strncmp(p,space,(unsigned int)len)==0) puuttuu=1;
        else
            {
            q=strchr(p,'-');
            if (q!=NULL)
                {
                if (*(q+1)==' ' || *(q+1)=='-' || q-p==len-1) puuttuu=1;
                }
            }
        return(puuttuu);
        }

int ma_load(SURVO_DATA_MATRIX *s, int j, int i, double *px, 
            int alpha) /* 1=alpha 0=num */
        {
        int k;
        char *sana[EP4];
        char *p,*r;
/*
        char *q;
        int len;
        int puuttuu;
*/

        if (s->mask!=NULL)
            {
            r=z+(s->l1+j-2)*ed1+s->varpos[i];
            if ( (s->varpos[i]>1 && *(r-1)!=' ') || *(r+s->varlen[i])!=' ')
                {
                sprintf(sbuf,"\nIncorrect mask for data on edit line %d!",
                              s->l1+j-1);
                if (dsp) return(-1);
                sur_print(sbuf);
                WAIT; return(-1);
                }
            strncpy(s->obs,r,(unsigned int)s->varlen[i]);
            s->obs[s->varlen[i]]=EOS;
            p=s->obs;
            }
        else
            {
            edread(s->obs,s->l1+j-1);
            k=split(s->obs+1,sana,i+1);
            if (k<i+1)
                {
                sprintf(sbuf,"\nInvalid data line %d",s->l1+j-1); sur_print(sbuf);
                if (dsp) return(-1);
                WAIT; return(-1);
                }
            p=sana[i];
            }

        if (alpha) { strcpy((char *)px,p); return(1); }

        if (ma_missing(p)) *px=MISSING8; else *px=atof(p);
        return(1);
        }



int ma_open(char *name,SURVO_DATA_MATRIX *s,int drivi)
/* int drivi;    DATA line   0=unknown  */
        {
        char datadef[LLENGTH]; char *dsana[6]; int gdat;
        int i,h,m,tila;
        int l1,l2,lx,lmask;
        char rivi[LLENGTH], *xname[EP4];
        char *p, *nimitila, *tyypit, *masktila;
        char x[LLENGTH];
        char *q;
        int labtila;

        if (!drivi)
            {
            drivi=wfind("DATA",name,1);
            if (drivi<0)
                {
                sprintf(sbuf,"\nDATA %s not found in the edit field",name); sur_print(sbuf);
                WAIT; return(-1);
                }
            }
        edread(datadef,drivi);
        gdat=split(datadef+1,dsana,6);
        if (gdat>2 && gdat<5)
            {
            sur_print("\nIncomplete DATA definition!");
            WAIT;
            return(-1);
            }
        if (gdat==2)    /* vain DATA <nimi> */
            {
            lx=drivi+1; l1=lx+1;
            l2=l1;
            while (l2<r2+1)
                {
                edread(rivi,l2); if (strncmp(rivi+1,space,(unsigned int)c2)==0) break;
                ++l2;
                }
            --l2;
            }
        else
            {
            l1=edline2(dsana[2],1,1); if (l1==0) return(-1);
            l2=edline2(dsana[3],1,1); if (l2==0) return(-1);
            lx=edline2(dsana[4],1,1); if (lx==0) return(-1);
            }
        s->l1=l1; s->n=l2-l1+1;
        edread(rivi,lx); m=split(rivi+1,xname,EP4);
        if (m==0)
            {
            sprintf(sbuf,"\nLabels missing on line %d!",lx);  /* 10.4.93 */
            if (dsp) return(-1);
            sur_print(sbuf); WAIT; return(0);
            }
/*
        tilat:
        nimitila    m*10              28.5.90 ennen 2*LLENGTH
        **varname   m*sizeof(char *)
        tyypit      m*(TYPELEN+1)
        **vartype   m*sizeof(char *)
        *varlen     m*sizeof(int)
        *varpos     m*sizeof(int)
        masktila    LLENGTH
        **mask      m*sizeof(char *)
        *obs        LLENGTH
*/
        labtila=10*m; if (labtila<2*LLENGTH) labtila=2*LLENGTH; /* 8.3.1991 */
/*      tila=10*m+2*LLENGTH+3*m*sizeof(char *)+2*m*sizeof(int)+(TYPELEN+1)*m; */
        tila=labtila+2*LLENGTH+3*m*sizeof(char *)+2*m*sizeof(int)+(TYPELEN+1)*m;
           /* 8.3.1991 */

        s->pma=muste_malloc((unsigned int)tila);
        if (s->pma==NULL)
            {
            sur_print("\nNot enough memory (SURVO_DATA_MATRIX)!");
            WAIT; return(-1);
            }
        p=s->pma;
  /*    nimitila=p; p+=10*m;     28.5.90  ennen 2*LLENGTH */
        nimitila=p; p+=labtila;
        s->varname=(char **)p; p+=m*sizeof(char *);
        tyypit=p; p+=(TYPELEN+1)*m;
        s->vartype=(char **)p; p+=m*sizeof(char *);
        for (i=0; i<m; ++i) s->vartype[i]=tyypit+(TYPELEN+1)*i;
        s->varlen=(short *)p; p+=m*sizeof(short);
        s->varpos=(short *)p; p+=m*sizeof(short);
        masktila=p; p+=LLENGTH;
        s->mask=(char **)p; p+=m*sizeof(char *);
        s->obs=p; p+=LLENGTH;

/*      Rprintf("\ntila=%d kytetty=%d",tila,p-s->pma);          */
        edread(x,lx); split(x+1,s->varname,m);
        p=nimitila;

        for (i=0; i<m; ++i)      /* 20.10.90 */
            {
            q=s->varname[i];
            s->varname[i]=p;
            h=0;
   /*       while (*q && h<8) { *p++=*q++; ++h; }       h<8 20.10.90 */
            while (*q) *p++=*q++;   /* 8.3.1991 */
            while (p-s->varname[i]<9) *p++=' ';
            *p++=EOS;
            }

        s->m=m;
        for (i=0; i<m; ++i)
            {
            for (h=0; h<TYPELEN; ++h)
                s->vartype[i][h]=' ';
            s->vartype[i][TYPELEN]=EOS;
            s->vartype[i][0]='S';
            s->vartype[i][1]='A';
            }
        if (gdat<6)
            {
            s->mask=NULL;
            s->varpos=NULL;
            s->varlen=NULL;
            }
        else
            {
            lmask=edline2(dsana[5],1,1); if (lmask==0) return(-1);
            edread(masktila,lmask);
            i=split(masktila+1,s->mask,m);
            if (i<m)
                {
                sprintf(sbuf,"\nIncorrect mask line for DATA %s",name); sur_print(sbuf);
                if (dsp) return(-1);
                WAIT; return(-1);
                }

            for (i=0; i<m; ++i)
                {
                s->varpos[i]=s->mask[i]-masktila;
                s->varlen[i]=strlen(s->mask[i]);
                s->vartype[i][1]=s->mask[i][0];
                }
            }
        *active_data=EOS;
        return(1);
        }


int madata_open(char *name,SURVO_DATA *d,int drivi)
/* int drivi;    DATA line   0=unknown */
        {
        int i,k;

        d->type=1;
        i=ma_open(name,&(d->d1),drivi); if (i<0) return(-1);
        d->m=d->d1.m;
        d->n=(int)d->d1.n; // RS CHA (long) -> (int)
        d->l1=1; d->l2=d->n;
/*      if (d->pspace!=NULL) muste_free(d->pspace);   6.6.86 */
        d->pspace=muste_malloc(d->m*sizeof(int));
        if (d->pspace==NULL) { tilavirhe(); return(-1); }

        d->v=(short *)d->pspace;
        k=0;

        for (i=0; i<d->m; ++i)
            if (d->d1.vartype[i][1]!='-') d->v[k++]=i;
        d->m_act=k;
        d->typelen=4;
        d->varname=d->d1.varname;
        d->varlen=d->d1.varlen;
        d->vartype=d->d1.vartype;
        d->varpos=d->d1.varpos;
        return(1);
        }

int not_suitable_matfile(char *y)
        {
        sprintf(sbuf,"\n%s as a symmetric matrix is not a accepted as a data matrix!",y);
        if (dsp) return(1);
        sur_print(sbuf); WAIT;
        return(1);
        }


int matr_open(char *name, SURVO_DATA *d)
        {
        int i,k,h,m,n;
        char *p,*q;
        unsigned int tila;
        char x[LLENGTH],y[LLENGTH];
        char *sana[7];
        int mname,mrl,mcl;
        char type[TYPELEN+1];
        char ch;

        strcpy(y,name);
        if (!muste_is_path(y)) // RS CHA if (strchr(y,':')==NULL) 
        { strcpy(y,edisk); strcat(y,name); }
        d->d2.survo_data=muste_fopen(y,"rb");
        if (d->d2.survo_data==NULL)
            {
            sprintf(sbuf,"\nMatrix file %s not found!",y);
            if (dsp) return(-1);
            sur_print(sbuf); WAIT; return(-1);
            }

        for (i=0; i<ERC; ++i) x[i]=(char)getc(d->d2.survo_data);
        x[ERC]=EOS;

        if (strncmp(x,"MATRIX84D",9)!=0)
            {
            not_suitable_matfile(y);
            if (dsp) return(-1); /* 10.3.2005 */
            WAIT; return(-1);
            }
        i=split(x,sana,7);
        if (atoi(sana[6])!=0) /* type */
            {
            if (atoi(sana[1])>2) /* 10.3.2005 */
                {
                not_suitable_matfile(y);
                if (dsp) return(-1); /* 10.3.2005 */
                WAIT; return(-1);
                }
            }
        n=atoi(sana[1]); m=atoi(sana[2])+1;
        mname=atoi(sana[3]); mrl=atoi(sana[4]); mcl=atoi(sana[5]);
        d->d2.var=(long)((mname+1)*ERC);
        d->d2.data=d->d2.var+mcl*(m-1);
        if (mrl!=8)
            {
            not_suitable_matfile(y); WAIT; return(-1);
            }

        muste_fseek(d->d2.survo_data,(long)(d->d2.var),0);

        d->type=4;
        d->m=m;
        d->n=(long)n;
        d->m_act=m-1;
        d->l1=1L; d->l2=d->n;
        d->typelen=TYPELEN;
/*
        char **varname; m*sizeof(char *);  +m*(8+1) (pspace)
        char **vartype; m*sizeof(char *);  +m*(TYPELEN+1) (pspace)
        int *v;   m*sizeof(int);
        int *varlen; m*sizeof(int);
*/
        tila=m*(8+1)+m*(TYPELEN+1)
             +2*m*sizeof(char *)+2*m*sizeof(int);
        d->pspace=muste_malloc(tila);
        if (d->pspace==NULL) { tilavirhe(); return(-1); }

        p=d->pspace;
        q=p+m*(8+1);
        d->varname=(char **)q;
        for (i=0; i<m; ++i)
            {
            d->varname[i]=(char *)p;
            if (i==0)
                {
                strcpy(p,"CASE    "); p+=9;
                }
            else
                {
                h=0;
                for (k=0; k<mrl; ++k)
                    {
                    ch=(char)getc(d->d2.survo_data);
                    if (ch!=' ') { *p++=ch; ++h; }
                    }
                for ( ; h<mrl; ++h) *p++=' ';
                *p++=EOS;
                }
            }
        p=q+m*sizeof(char *);
        q=p+m*(TYPELEN+1);
        d->vartype=(char **)q;
        strcpy(type,"8A- ");
        for (i=0; i<m; ++i)
            {
            d->vartype[i]=(char *)p;
            for (k=0; k<TYPELEN; ++k) *p++=type[k];
            *p++=EOS;
            }
        d->vartype[0][0]='S'; d->vartype[0][3]='-';
        p=q+m*sizeof(char *);
        d->varlen=(short *)p;
        p+=m*sizeof(short);
        d->v=(short *)p;
        d->varlen[0]=8;
        for (i=1; i<m; ++i) { d->varlen[i]=8; d->v[i-1]=i; }

/*
printf("\ntila=%u %u",tila,p-d->pspace+m*sizeof(int)); Rprintf("\n");
for (i=0; i<m; ++i) Rprintf(" %s",d->varname[i]); Rprintf("\n");
for (i=0; i<m; ++i) Rprintf(" %s",d->vartype[i]); Rprintf("\n");
for (i=0; i<m; ++i) Rprintf(" %d",d->varlen[i]); Rprintf("\n");
for (i=0; i<d->m_act; ++i) Rprintf(" %d",d->v[i]); Rprintf("\n");
getch();
*/
        *active_data=EOS;
        return(1);
        }

int matr_load(SURVO_DATA *d, int j, int i, double *px)
        {
        register int h;
        char *p;
        char s[9];

        muste_fseek(d->d2.survo_data,(long)(d->d2.data+8L*((long)(j-1)*(long)d->m+(long)i)),0);
                                                           /* longit lisätty 23.12.88 */
        p=(char *)px;
        for (h=0; h<sizeof(double); ++h) *p++=(char)getc(d->d2.survo_data);
        if (i==0)
            {
            strncpy(s,(char *)px,8); s[8]=EOS; *px=atof(s);
            }
        return(1);
        }

int matr_alpha_load(SURVO_DATA *d,int j,int i,char *s)
        {
        char *p;
        register int h;

        if (i!=0) { sur_print("\nOnly CASE to be used as string variable!"); WAIT; return(-1); }
        muste_fseek(d->d2.survo_data,(long)(d->d2.data+8*(j-1)*d->m),0);
        p=s;
        for (h=0; h<8; ++h) *p++=(char)getc(d->d2.survo_data);
        *p++=EOS;
        return(1);
        }

int matr_close(SURVO_DATA *d)
        {
        muste_fclose(d->d2.survo_data);
/*      muste_free(d->pspace);  */
        return(1);
        }

static FILE *MAT; /* -4.1..1997 defined as local! */

int matrix_nospace()
        {
        sur_print("\nNot enough space for matrices");
        WAIT; return(1);
        }

int matrix_space(
double **A,  /* matriisitila (alkuosoite) (malloc) */
int m,      /* rivien lkm */
int n,      /* sar. lkm   */
char **rlab, /* rivien otsikot (malloc) */
char **clab, /* sar. otsikot   (malloc) */
int mcr,    /* riviotsikoiden pituus */
int mcl     /* sarakeotsikoiden pituus */
)
        {
/*      if ( (long)m*n*sizeof(double)>MAXTILA )
                { matrix_nospace(); return(-1); }
*/
#if 0
        if (*A!=NULL) muste_free(*A);
        *A=(double *)muste_malloc(m*n*sizeof(double));
        if (*A==NULL) { matrix_nospace(); return(-1); }
                               /*  Rprintf("\nmat-tila varattu! %d",m*n); */
        if (rlab!=NULL)
            {
            if (*rlab!=NULL) muste_free(*rlab);
            *rlab=(char *)muste_malloc((unsigned int)(m*mcr+1));
            if (*rlab==NULL) { matrix_nospace(); return(-1); }
                               /* Rprintf("\nrlab-tila varattu! %d %d",m,mcr); */
            }
        if (clab!=NULL)
            {
            if (*clab!=NULL) muste_free(*clab);
            *clab=(char *)muste_malloc((unsigned int)(n*mcl+1));
            if (*clab==NULL) { matrix_nospace(); return(-1); }
                               /* Rprintf("\nclab-tila varattu! %d %d",n,mcl); */
            }
#endif

        *A=(double *)muste_realloc(*A,m*n*sizeof(double));
        if (*A==NULL) { matrix_nospace(); return(-1); }
        *rlab=(char *)muste_realloc(*rlab,m*mcr+1);
        if (*rlab==NULL) { matrix_nospace(); return(-1); }
        *clab=(char *)muste_realloc(*clab,n*mcl+1);
        if (*clab==NULL) { matrix_nospace(); return(-1); }


/* Rprintf("\nAosoite=%lu",*A);
   Rprintf("\n&viim.matriisialkio=%lu",&((*A)[m*n-1]));
   Rprintf("\nclabosoite=%lu",*clab);
   getch();
*/
        return(1);
        }

int matrix_name(char *matfile, char *matr)
        {
        int i;

        *matfile=EOS;
        if (!muste_is_path(matr)) // RS CHA if ((strchr(matr,':')==NULL) && *matr!='<' && *matr!='.' && *matr!='~' && *matr!='\\' && *matr!='/')
        strcpy(matfile,edisk);
        
        strcat(matfile,matr);
        i=strlen(matr)-4; if (i<0) i=1;
        if (strchr(matr+i,'.')==NULL) strcat(matfile,".MAT");
        return(1);
        }



int matrix_load(
char *matr,  /* matriisin nimi */
double **A,  /* matriisitila (alkuosoite) (muste_malloc) */
int *rdim,   /* rivien lkm */
int *cdim,   /* sar. lkm   */
char **rlab, /* rivien otsikot (muste_malloc) */
char **clab, /* sar. otsikot   (muste_malloc) */
int *lr,     /* riviotsikon pituus */
int *lc,     /* sar.otsikon pituus */
int  *type,  /* tyyppi */
char *expr   /* lauseke (sis.nimi) max ERC */
)
        {
        char matfile[LNAME];
        char x[ERC+1], *osa[10];
        int i,j,j1,j2;  /* i1,i2, */
        int mname,mc,mcl,mrl; /* mr, mat; */
        int m,n;
        char *p;
        double *a;
        register int h;
        char *pl;

        pl=NULL;  
        i=matrix_name(matfile,matr);

        MAT=muste_fopen(matfile,"rb");
        if (MAT==NULL)
            {
            PR_EBLD;
            sprintf(sbuf,"\nMatrix file %s not found!",matfile); sur_print(sbuf);
            WAIT; PR_ENRM; return(-1);
            }
        for (i=0; i<ERC; ++i) x[i]=(char)getc(MAT); x[ERC]=EOS;
        i=split(x,osa,10);
        if (strncmp(osa[0],"MATRIX84",8)!=0)
            {
            PR_EBLD;
            sprintf(sbuf,"\n%s is not a matrix file!",matfile); sur_print(sbuf);
            WAIT; PR_ENRM; muste_fclose(MAT); return(-1);
            }
/*    for (i=0; i<10; ++i) Rprintf("\n%s",osa[i]);  getch(); */
        *rdim=atoi(osa[1]); *cdim=atoi(osa[2]);
        mname=atoi(osa[3]);
            *lr=atoi(osa[4]); *lc=atoi(osa[5]); *type=atoi(osa[6]);
            ++mname; mc=mname+1;

        m=*rdim; n=*cdim;
        mrl=*lr; mcl=*lc;
        i=matrix_space(A,m,n,rlab,clab,mrl,mcl); if (i<0) return(-1);
        a=*A;

        if (*type==20)
            { for (i=0; i<m; ++i) for (j=0; j<n; ++j) a[i+m*j]=0; }

        muste_fseek(MAT,(long)((mname-1)*ERC),0);
        for (i=0; i<ERC; ++i) expr[i]=(char)getc(MAT); expr[ERC-1]=EOS;
        p=strchr(expr,' '); if (p-expr<ERC) *p=EOS;

        i=muste_fseek(MAT,(long)((mc-1)*ERC),0);
        if (clab!=NULL)
            for (i=0; i<n*mcl; ++i) (*clab)[i]=(char)getc(MAT);
        else
            for (i=0; i<n*mcl; ++i) getc(MAT);
            {
            if (rlab!=NULL) pl=*rlab;
            for (i=0; i<m; ++i)
                {
                if (rlab!=NULL)
                    for (j=0; j<mrl; ++j) { *pl=(char)getc(MAT); ++pl; }
                else
                    for (j=0; j<mrl; ++j) getc(MAT);
                j1=0; j2=n-1;
                if (*type)
                    {
                    j2=i;
                    if (*type==20) j1=i;
                    }
                for (j=j1; j<=j2; ++j)
                    {
                    p=(char *)&a[i+m*j];
                    for (h=0; h<sizeof(double); ++h) *(p+h)=getc(MAT);
                    }
                }
            if (*type==10)
                {
                for (i=0; i<m; ++i) for (j=0; j<=i; ++j)
                    a[j+m*i]=a[i+m*j];
                }
            }

        muste_fclose(MAT);
        return(1);
        }

extern int muste_fclose2();
int mat_load(
char *matr,  /* matriisin nimi */
double **A,  /* matriisitila (alkuosoite) (muste_malloc) */
int *rdim,   /* rivien lkm */
int *cdim,   /* sar. lkm   */
char **rlab, /* rivien otsikot (muste_malloc) */
char **clab, /* sar. otsikot   (muste_malloc) */
int *lr,     /* riviotsikon pituus */
int *lc      /* sar.otsikon pituus */
)
        {
        char matfile[LNAME];
        char x[ERC+1], *osa[10];
        int i,j,j1,j2;
        int mname,mc,mcl,mrl; /* mr, mat; */
        int m,n;
        char *p;
        double *a;
        int h;
        char *pl;
        int type;

/*        i=matname(matfile,matr,1);  */
        i=matrix_name(matfile,matr);

        MAT=muste_fopen2(matfile,"rb");
        if (MAT==NULL)
            {
            PR_EBLD;
            sprintf(sbuf,"\nMatrix file %s not found!",matfile); sur_print(sbuf);
            WAIT; PR_ENRM; return(-1);
            }
/*
        if (MAT==NULL)
            {
            errno=0;
            i=matname(matfile,matr,2);
            MAT=muste_fopen(matfile,"rb");
            if (MAT==NULL)
                {
                sprintf(sbuf,"\nMatrix file %s not found!",matr);
                sur_print(sbuf); WAIT; PR_ENRM; return(-1);
                }
            }
*/
        for (i=0; i<ERC; ++i) x[i]=(char)getc(MAT); x[ERC]=EOS;
        i=split(x,osa,10);
        if (strncmp(osa[0],"MATRIX84",8)!=0)
            {
            sprintf(sbuf,"\n%s is not a matrix file!",matfile);
            sur_print(sbuf); WAIT; PR_ENRM; muste_fclose2(MAT); return(-1);
            }

        *rdim=atoi(osa[1]); *cdim=atoi(osa[2]);
        mname=atoi(osa[3]);

        *lr=atoi(osa[4]); *lc=atoi(osa[5]); type=atoi(osa[6]);
        ++mname; mc=mname+1;

        m=*rdim; n=*cdim;
        mrl=*lr; mcl=*lc;

/*        i=mat_varaa_tila(A,m,n,rlab,clab,mrl,mcl); if (i<0) return(-1); */
        i=matrix_space(A,m,n,rlab,clab,mrl,mcl); if (i<0) { muste_fclose2(MAT); return(-1); }
        a=*A;
        if (type==20)
            { for (i=0; i<m; ++i) for (j=0; j<n; ++j) a[i+m*j]=0; }

        i=muste_fseek(MAT,(long)((mc-1)*ERC),0);

        for (i=0; i<n*mcl; ++i) (*clab)[i]=(char)getc(MAT);

        pl=*rlab;
        for (i=0; i<m; ++i)
            {
            for (j=0; j<mrl; ++j) { *pl=(char)getc(MAT); ++pl; }
            j1=0; j2=n-1;
            if (type)
                {
                j2=i;
                if (type==20) j1=i;
                }
            for (j=j1; j<=j2; ++j)
                {
                p=(char *)&a[i+m*j];
                for (h=0; h<sizeof(double); ++h) *(p+h)=getc(MAT);
                }
            }
        if (type==10)
            {
            for (i=0; i<m; ++i) for (j=0; j<=i; ++j)
                a[j+m*i]=a[i+m*j];
            }

        muste_fclose2(MAT);
        return(1);
        }

char *matrix_label(char *lablist,int l,int len,int i,char *label)
        {
        int k=0;
        int h=0;

        while (lablist[i*l+h]==' ' && h<l) ++h;

        while (h<l && k<len) { label[k]=lablist[i*l+h]; ++k; ++h; }
        label[k]=EOS;
        while (label[k-1]==' ' && k>0) label[--k]=EOS;
        return(label);
        }


int matrix_print(double *A,int m,int n,char *rlab,char *clab,int lr,int lc,
int m2,int n2,int *mv,int *nv,char *form,int width,int editline,
char *outfile,char *header)

// double *A;
// int m,n;
// char *rlab,*clab;
// int lr,lc;
// int m2,n2;        /* # of selected rows/cols */
// int *mv,*nv;      /* lists of selected rows/cols */
// char *form;       /* format for single element 123.12 or %5.5g  */
// int width;        /* entire printing width */
// int editline;
// char *outfile;
// char *header;

        {
        int i,j,k,h,ii,jj;
        int len;
        int nblock;
        int i1,i2,j1,j2;
        char x[LLENGTH];
        char label[LLENGTH];
        char *matrix_label();

        output_open(outfile);
        output_line(header,outfile,editline); if (editline) ++editline;

        if (*form=='%') len=sprintf(x,form,fabs(A[0]))+1;
        else len=strlen(form);

        nblock=(width-lr-1+1)/(len+1);
        if (nblock>n2) nblock=n2;

        i1=0; i2=m2-1;
        j1=0; j2=nblock-1;

        while (1)
            {
            k=sprintf(x,"%.*s",lr+1,space);
            for (j=j1; j<=j2; ++j)
                {
                if (nv==NULL) jj=j; else jj=nv[j];
                k+=sprintf(x+k,"%*.*s ",len,len,matrix_label(clab,lc,len,jj,label));
                }
            output_line(x,outfile,editline); if (editline) ++editline;
            for (i=i1; i<=i2; ++i)
                {
                if (mv==NULL) ii=i; else ii=mv[i];
                k=0; while (k<lr) { label[k]=rlab[ii*lr+k]; ++k; } label[lr]=EOS;
                k=sprintf(x,"%*.*s ",lr,lr,label);
                for (j=j1; j<=j2; ++j)
                    {
                    if (nv==NULL) jj=j; else jj=nv[j];
                    h=fconv(A[jj*m+ii],form,label);
                    if (h<0) { strncpy(label,space,len); label[len-1]='-'; }
                    k+=sprintf(x+k,"%*.*s ",len,len,label);
                    }
                    output_line(x,outfile,editline); if (editline) ++editline;
                }
            strcpy(x," ");
            output_line(x,outfile,editline); if (editline) ++editline;
            if (j2==n2-1) break;
            j1=j2+1;
            j2+=nblock; if (j2>=n2) j2=n2-1;
            }

        output_close(outfile);
        return(editline);
        }





int sample_open(char *name, SURVO_DATA *d, int drivi)
        {
        int i,ii,j,k,m,n,nn,h;
        char x[LLENGTH];
        char *p,*q;
        char y[LLENGTH];
        char *sana[EP4];
        char x2[LLENGTH];
        char *s2[EP4];
        int alku;
        unsigned int tila;
        double *px;
        char type[TYPELEN+1];

        edread(x,drivi);
        p=strchr(x,':');
        if (*(p+1)==' ')
            {
            m=1;
            strcpy(y,name); sana[0]=y;
            alku=p-x+1;
            }
        else if (*(p+1)=='(')
            {
            i=split(p+2,sana,EP4);
            m=1; k=1;
            while (m-1<i)
                {
                k=strlen(sana[m-1]);
                if (sana[m-1][k-1]==')') { sana[m-1][k-1]=EOS; break; }
                ++m;
                }
            alku=sana[m-1]-x+k;
            }
        else
            {
            sprintf(sbuf,"\nError in DATA on line %d!",drivi); sur_print(sbuf);
            WAIT; return(-1);
            }
/*
  Rprintf("\nm=%d",m);
  for (i=0; i<m; ++i) Rprintf(" %s",sana[i]);
  Rprintf("\nalku=%d",alku);
*/
        k=alku; j=drivi; n=0; h=0;
        while (1)
            {
            edread(x2,j);
            if (j>drivi && (p=strchr(x2+1,':'))!=NULL) k=p-x2+1;
            i=split(x2+k,s2,EP4);
            if (!i) { if (!n && j<r2) { ++j; k=1; continue; } else break; }
            for (ii=0; ii<i; ++ii)
                { if (muste_strcmpi(s2[ii],"END")==0) { h=1; break; } ++n; }
            if (h || j==r2) break;
            ++j; k=1;
            }
        if (m*(n/m)!=n)
            {
            sprintf(sbuf,"\n# of data values %d not divisible by %d",n,m);
                sur_print(sbuf);
            WAIT; return(-1);
            }
        n/=m;
        d->type=3;
        d->m=m;
        d->n=(int)n; // RS CHA (long) -> (int)
        d->m_act=m;
        d->l1=1; d->l2=d->n;
        d->typelen=TYPELEN;
/*
        char **varname; m*sizeof(char *);  +m*(8+1) (pspace)
        char **vartype; m*sizeof(char *);  +m*(TYPELEN+1) (pspace)
        int *v;   m*sizeof(int);
        int *varlen; m*sizeof(int);
*/
        tila=m*n*sizeof(double)+m*(8+1)+m*(TYPELEN+1)
             +2*m*sizeof(char *)+2*m*sizeof(int);
        d->pspace=muste_malloc(tila);
        if (d->pspace==NULL) { tilavirhe(); return(-1); }

        k=alku; j=drivi; px=(double *)d->pspace; h=0; nn=0;
        while (1)
            {
            edread(x2,j);
            if (j>drivi && (p=strchr(x2+1,':'))!=NULL) k=p-x2+1;
            i=split(x2+k,s2,EP4);
            if (!i) { if (!nn && j<r2) { ++j; k=1; continue; } else break; }
            for (ii=0; ii<i; ++ii)
                {
                q=s2[ii];
                if (muste_strcmpi(q,"END")==0) { h=1; break; }
                if (*q=='-' && *(q+1)==EOS) *px=MISSING8;
                else *px=atof(q);
                ++px; ++nn;
                }
            if (h || j==r2) break;
            ++j; k=1;
            }

        p=d->pspace+m*n*sizeof(double);
        q=p+m*(8+1);
        d->varname=(char **)q;
        for (i=0; i<m; ++i)
            {
            d->varname[i]=(char *)p;
            for (k=0; k<8; ++k)
                {
                if (k<strlen(sana[i])) *p++=sana[i][k];
                else *p++=' ';
                }
            *p++=EOS;
            }
        p=q+m*sizeof(char *);
        q=p+m*(TYPELEN+1);
        d->vartype=(char **)q;
        strcpy(type,"8A- ");  /* TYPELEN=4 */
        for (i=0; i<m; ++i)
            {
            d->vartype[i]=(char *)p;
            for (k=0; k<TYPELEN; ++k) *p++=type[k];
            *p++=EOS;
            }
        p=q+m*sizeof(char *);

        d->varlen=(short *)p;
        p+=m*sizeof(short);
        d->v=(short *)p;

        for (i=0; i<m; ++i) { d->varlen[i]=8; d->v[i]=i; }
/*
printf("\ntila=%u %u",tila,p-d->pspace+m*sizeof(int)); Rprintf("\n");
px=(double *)d->pspace;
for (i=0; i<m*n; ++i) Rprintf(" %g",px[i]); Rprintf("\n");
for (i=0; i<m; ++i) Rprintf(" %s",d->varname[i]); Rprintf("\n");
for (i=0; i<m; ++i) Rprintf(" %s",d->vartype[i]); Rprintf("\n");
for (i=0; i<m; ++i) Rprintf(" %d",d->varlen[i]); Rprintf("\n");
for (i=0; i<m; ++i) Rprintf(" %d",d->v[i]); Rprintf("\n");
getch();
*/
        *active_data=EOS;
        return(1);
        }

int sa_load(SURVO_DATA *d,int j,int i,double *px)
        {
        double *py;

        py=(double *)d->pspace;
        *px=py[(j-1)*d->m+i];
        return(1);
        }


int fidata_open2(char *name,SURVO_DATA *d,int p1,int p2,int p3,int kirjoitus)
        {
        int i,k;

        d->type=2;       
        i=fi_open3(name,&(d->d2),p1,p2,p3,kirjoitus); if (i<0) return(-1); 
        d->m=d->d2.m;
        d->n=d->d2.n;
/*      if (d->pspace!=NULL) muste_free(d->pspace);  6.6.86 */
        d->pspace=NULL; // RS ADD

        d->pspace=muste_malloc(d->m*sizeof(int)); // RS CHA was sizeof(short)
        if (d->pspace==NULL) { tilavirhe(); return(-1); }

        d->v=(short *)d->pspace;
        k=0;
        for (i=0; i<d->d2.m; ++i)
            if (d->d2.vartype[i][1]!='-') d->v[k++]=i;

        d->m_act=k;
        d->typelen=d->d2.extra-4;
        d->varname=d->d2.varname;
        d->varlen=d->d2.varlen;
        d->vartype=d->d2.vartype;
        d->varpos=d->d2.varpos;
        return(1);
        }

int fidata_open(char *name,SURVO_DATA *d,int p1,int p2,int p3)
        {
        return(fidata_open2(name,d,p1,p2,p3,1));
        }

long datol(char *s,long last)
        {
        long j;

        if (strncmp(muste_strupr(s),"END",3)==0)
            {
            j=last;
            if (strlen(s)>4) j+=atol(s+3);
            return(j);
            }
        return(atol(s));
        }


int data_open3(char *nimi, SURVO_DATA *d, int p1, int p2, int p3, int kirjoitus)
        {
        char name[LLENGTH];
        char datadef[LLENGTH]; char *dsana[6]; int gdat;
        int i;
        int drivi;
        char ch,*pch,*pch2;


        strcpy(name,nimi);
        if (*name=='*') { if (name[1]==EOS) strcpy(name,active_data); }
// RS REM        muste_strupr(name);  // case sensitive paths in linux!!!
        if (strstr(name,".M")!=NULL)
            { i=matr_open(name,d); return(i); }

        pch=z+ed1*ed2;
        ch=*pch; *pch=EOS;
        pch2=strstr(z,"DATA "); *pch=ch;
        if (pch2==NULL) drivi=r2+1;
    else
        {
        drivi=1;
        while (drivi<=r2)
            {
            edread(datadef,drivi);
            i=split(datadef+1,dsana,2);
            if (i>1 && strcmp("DATA",dsana[0])==0)
                {
				 if (strncmp(name,dsana[1],strlen(name))==0) // RS REM strupr  if (strncmp(name,muste_strupr(dsana[1]),strlen(name))==0)
                    {
                    if (strlen(name)==strlen(dsana[1])) break;
                    if (dsana[1][strlen(name)]==':')
                        { i=sample_open(nimi,d,drivi); return(i); }
       /* ennen 26.8.87 { i=sample_open(name,d,drivi); return(i); }  */
                    }
                }
            ++drivi;
            }
        }        
        if (drivi>r2)
            {
            i=fidata_open2(name,d,p1,p2,p3,kirjoitus);
            d->l1=1L; d->l2=d->d2.n;
            return(i);
            }
        edread(datadef,drivi);
        gdat=split(datadef+1,dsana,6);
        if (gdat<4)
            {
            i=madata_open(name,d,drivi);
            return(i);
            }
        if (strcmp(dsana[2],"IN")==0)
            {
            i=fidata_open2(dsana[3],d,p1,p2,p3,kirjoitus);
            if (i<0) return(-1);
            d->l1=1L; d->l2=d->d2.n;
            if (gdat<5) return(1);
            d->l1=datol(dsana[4],d->l2);
            if (d->l1<1L) d->l1=1L;
            if (gdat<6) return(1);
            d->l2=datol(dsana[5],d->l2);
            if (d->l2<d->l1 || d->l2>d->d2.n) d->l2=d->d2.n;
            return(1);
            }
        i=madata_open(name,d,drivi);
        return(i);
        }

int data_open2(char *nimi, SURVO_DATA *d, int p1, int p2, int p3)
        {
        return(data_open3(nimi,d,p1,p2,p3,1));
        }

int data_open(char *name, SURVO_DATA *d)
        {
        return(data_open2(name,d,0,0,0));
        }

int data_read_open(char *name, SURVO_DATA *d)
        {
        return(data_open3(name,d,0,0,0,0));
        }


void data_close(SURVO_DATA *d)
        {
/*      sel_free(); */ /* 14.5.90 */
// RS REM        if (d->pspace!=NULL) { 
        muste_free(d->pspace); d->pspace=NULL;
/* Rprintf("\npspace=%lu",d->pspace); getch();  */
        if (d->type==2) { fi_close(&(d->d2)); return; }
        if (d->type==1) { ma_close(&(d->d1)); return; }
        if (d->type==3) return;
        if (d->type==4) { matr_close(d); return; }
        }

void ma_save(SURVO_DATA_MATRIX *s, int j, int i, char *sana)
        {
        int ii;
        unsigned int jj,k;
/*        int len; */

        if (j>ed2 || j<1 || s->mask==NULL) return;
                            /* 4.9.89 */

        jj=k=(s->l1+j-2)*ed1+s->varpos[i];
        for (ii=0; ii<s->varlen[i]; ++ii) z[jj++]=' ';
        jj=k;
        for (ii=0; ii<s->varlen[i]; ++ii) { if (sana[ii]==EOS) break; z[jj++]=sana[ii]; }
        }

int data_save(SURVO_DATA *d, long j, int i, double x)
        {
        char sana[LLENGTH];
        char sana2[LLENGTH];
        char type;
        int varlen;
        int k;

        if (d->type==2)
            {
            if (j>d->n || j<1L) return(-1);
            if (d->vartype[i][2]=='P')
                {
                sprintf(sbuf,"Field %.8s is protected!",d->varname[i]);
                if (etu==2)
                    {
                    sprintf(tut_info,"___@4@DATA SAVE@%s@",sbuf); return(-1);
                    }
                sur_print("\n"); sur_print(sbuf);
                WAIT; return(-1);
                }

            type=d->d2.vartype[i][0];
            if (type!='S')
                {
                if (fabs(x)>MISSING8/1000.0)
                    switch(type)
                        {
                      case '1': x=(double)MISSING1; break;
                      case '2': x=(double)MISSING2; break;
                      case '4': x=(double)MISSING4; break;
                      case '8': x=(double)MISSING8; break;
                        }
                fi_save(&(d->d2),j,i,(char *)&x);
                return(1);
                }
            varlen=d->d2.varlen[i];
            if (fabs(x)>MISSING8/1000.0)
                {
                strncpy(sana,space,(unsigned int)varlen);
                }
            else
                {
                int tarkkuus=varlen;

                if (tarkkuus==1) fconv(x,"",sana); /* 9.3.1994 */
                else
                    {
                    if (tarkkuus>accuracy+2) tarkkuus=accuracy+2;
                    fnconv(x,tarkkuus,sana);
                    }
                if (strlen(sana)>varlen) strncpy(sana,space,(unsigned int)varlen);
                }
            fi_save(&(d->d2),j,i,sana);
            return(1);
            }

        if (d->type>=3) { sur_print("\nCannot write data!"); WAIT; return(-1); }

        /* d->type=1 */
        if (d->d1.mask==NULL)
            {
            if (etu==2)
                {
                strcpy(tut_info,"___@5@DATA SAVE@%s@Cannot write to the data table!");
                return(-1);
                }
            sur_print("\nCannot write to the data table!");
            sur_print("\nMask line in DATA <name>,L1,L2,<label line>,<mask line>");
            sur_print(" missing!");
            WAIT; return(-1);
            }

        varlen=d->varlen[i];
        strncpy(sana,space,(unsigned int)varlen); sana[varlen-1]='-';
        if (fabs(x)<MISSING8/1000.0)
            {
            k=fconv(x,d->d1.mask[i],sana2);
            if (k>=0) strcpy(sana,sana2);
            }
        ma_save(&(d->d1),(int)j,i,sana);
        return(1);
        }




int data_load(SURVO_DATA *d, long j, int i, double *px)
        {
        if (d->type==2) { fi_load(&(d->d2),j,i,px); return(1); }
        if (d->type==3) { sa_load(d,(int)j,i,px); return(1); }
        if (d->type==4) { matr_load(d,(int)j,i,px); return(1); }
        return (ma_load(&(d->d1),(int)j,i,px,0));
        }

/* RS kuten varfind, vain max nimen pituus lisätty */
int varfindlong(SURVO_DATA *d, char *nimi, int maxlen)
        {
        int len;
        int i;
        char *p; // RS ADD
        char nimi2[LLENGTH]; // RS ADD

        if (*nimi=='#')
            {
            i=atoi(nimi+1)-1;
            if (i<0 || i>d->m-1)
                { sprintf(sbuf,"\nIllegal var %s",nimi); sur_print(sbuf); WAIT; return(-1); }
            return(i);
            }

        len=strlen(nimi);
        p=strchr(nimi,' '); if (p!=NULL) len=p-nimi; // RS ADD
        if (len>maxlen) len=maxlen;
        strncpy(nimi2,nimi,len); nimi2[len]=EOS; // RS ADD   
        for (i=0; i<d->m; ++i)
            {
//            if ( strncmp(nimi,d->varname[i],(unsigned int)len)==0 &&
//Rprintf("\nlen: %d\nnimi   : %s\nnimi2  : %s\nvarname: %s",len,nimi,nimi2,d->varname[i]);     
			if ( strncmp(nimi2,d->varname[i],(unsigned int)len)==0 &&
                ( (d->varname[i][len]==' ' || d->varname[i][len]==EOS ) || len==maxlen ) )
            return(i);
            }

        sprintf(sbuf,"Variable %s not found!",nimi2);
        if (etu==2)
            {
            sprintf(tut_info,"___@3@VARFIND@%s@",sbuf);
            return(-1);
            }
        if (dsp) return(-1);
        sur_print("\n"); sur_print(sbuf);
        WAIT;
        return(-1);
        }

/* kuten varfind, vain ilm.valinta lisätty */
int varfind2(SURVO_DATA *d, char *nimi, int virheilm)
        {
        int len;
        int i;

        if (*nimi=='#')
            {
            i=atoi(nimi+1)-1;
            if (i<0 || i>d->m-1)
                {
                if (virheilm)
                    { sprintf(sbuf,"\nIllegal var %s",nimi); sur_print(sbuf); WAIT; }
                return(-1);
                }
            return(i);
            }
        len=strlen(nimi); if (len>8) len=8;
        for (i=0; i<d->m; ++i)
            {
            if ( strncmp(nimi,d->varname[i],(unsigned int)len)==0 &&
                ( (d->varname[i][len]==' ' || d->varname[i][len]==EOS ) || len==8 ) )
            return(i);
            }
        if (virheilm)
            {
            sprintf(sbuf,"Variable %.8s not found!",nimi); sur_print(sbuf);
            if (etu==2)
                {
                sprintf(tut_info,"___@3@VARFIND@%s@",sbuf); return(-1);
                }
            sur_print("\n"); sur_print(sbuf);
            WAIT;
            }
        return(-1);
        }

int varfind(SURVO_DATA *d, char *nimi)
        {
        int len;
        int i;

        if (*nimi=='#')
            {
            i=atoi(nimi+1)-1;
            if (i<0 || i>d->m-1)
                { sprintf(sbuf,"\nIllegal var %s",nimi); sur_print(sbuf); WAIT; return(-1); }
            return(i);
            }
        len=strlen(nimi); if (len>8) len=8;
        for (i=0; i<d->m; ++i)
            {
            if ( strncmp(nimi,d->varname[i],(unsigned int)len)==0 &&
                ( (d->varname[i][len]==' ' || d->varname[i][len]==EOS ) || len==8 ) )
            return(i);
            }

        sprintf(sbuf,"Variable %.8s not found!",nimi);
        if (etu==2)
            {
            sprintf(tut_info,"___@3@VARFIND@%s@",sbuf);
            return(-1);
            }
        if (dsp) return(-1);
        sur_print("\n"); sur_print(sbuf);
        WAIT;
        return(-1);
        }

int data_alpha_load(SURVO_DATA *d, long j, int i, char *sana)
        {
        if (d->type==2) { fi_alpha_load(&(d->d2),j,i,sana); return(1); }
        if (d->type==3) { sur_print("\ndata_alpha_load not in samples!"); return(-1); }
        if (d->type==4) { matr_alpha_load(d,(int)j,i,sana); return(1); }
        return (ma_load(&(d->d1),(int)j,i,(double *)sana,1));
        }

int data_alpha_save(SURVO_DATA *d,long j,int i,char *x)
        {
        char sana[LLENGTH];
        char type;
        int varlen;

        if (d->type==2)
            {
            if (j>d->n || j<1L) return(-1);
            if (d->vartype[i][2]=='P')
                {
                sprintf(sbuf,"Field %.8s is protected!",d->varname[i]);
                if (etu==2)
                    {
                    sprintf(tut_info,"___@4@DATA SAVE@%s@",sbuf); return(-1);
                    }
                sur_print("\n"); sur_print(sbuf);
                WAIT; return(-1);
                }

            type=d->d2.vartype[i][0];
            if (type!='S')
                {
                sprintf(sbuf,"Field %.8s is not a string field!",d->varname[i]);
                if (etu==2)
                    {
                    sprintf(tut_info,"___@4@DATA SAVE@%s@",sbuf); return(-1);
                    }
                sur_print("\n"); sur_print(sbuf);
                WAIT; return(-1);
                }
            varlen=d->d2.varlen[i];
            strncpy(sana,space,varlen);
            strncpy(sana,x,varlen);
            fi_alpha_save(&(d->d2),j,i,sana);
            return(1);
            }

        if (d->type>=3) { sur_print("\nCannot write data!"); WAIT; return(-1); }

        /* d->type=1 */
        if (d->d1.mask==NULL)
            {
            if (etu==2)
                {
                strcpy(tut_info,"___@5@DATA SAVE@%s@Cannot write to the data table!");
                return(-1);
                }
            sur_print("\nCannot write to the data table!");
            sur_print("\nMask line in DATA <name>,L1,L2,<label line>,<mask line>");
            sur_print(" missing!");
            WAIT; return(-1);
            }
        ma_save(&(d->d1),(int)j,i,x);
        return(1);
        }


int right_par_missing()
        {
        sprintf(sbuf,"')' missing in SELECT!");
        if (dsp) return(-1);
        sur_print("\n"); sur_print(sbuf);
        WAIT;
        return(-1);
        }


/*
  INPUT: t=(A+B*(C+D)+E)+F  RETURN: pointer to )+F
                                 *ppk is pointer to +B*(C+D)+E)+F
                                      (to `weakest' point in the expression)
*/
char *end_par(char *t, char **ppk, int u)
/* u=1: search for )    u=0: search for the term end */
        {
        int n,i;
        char *s;

        n=u; s=t; i=0; *ppk=t;
        while (*(s+1))
            {
            ++s;
            if (*s=='(') { ++n; continue; }
            if (*s==')') { --n; if (u && n==0) break; if (!u && n<0) return(s-1); }
            if (!u)
                {
                if (n==0 && (*s=='*' || *s=='+')) return(s-1);
                }
            if (n==1)
                {
                if (*s=='+') { i=2; *ppk=s; }
                else if (*s=='*' && i==0) { i=1; *ppk=s; }
                }
            }
        if (u && n) { right_par_missing(); s=NULL; }
        return(s);
        }

/*
    Returns the end of the first multiplicative or additive term in s
*/
char *end_term(char *s)
        {
        char *pk;
/*
        char *p;
        int i;
*/
        if (*s=='(') return(end_par(s,&pk,1));
        return(end_par(s,&pk,0));
        }

/*
    Returns the end of product s
*/
char *end_prod(char *s)
        {
        char *p;

        p=s;
        while (1)
            {
            p=end_term(p); if (p==NULL) return(p);
            if (*(p+1)!='*') break;
            p+=2;
            }
        return(p);
        }

/*
   Multiplication (A+B+C)*D*E=A*D*E+B*D*E+C*D*E
                 pt    qt p q
   Result connected to s
*/
int bool_mult(char *s,char *pt,char *qt,char *p,char *q)
        {
        char *p2;
        char x[LLENGTH];
        char y[LLENGTH];
        int i;

/*  Rprintf("\nBool_mult");
    Rprintf("\ns=%s\npt=%s\nqt=%s\np=%s\nq=%s\n",s,pt,qt,p,q); getch(); */

        *x=EOS; strncat(x,p,(unsigned int)(q-p)+1);

        ++pt; i=0;
        while (pt<qt)
            {
            p2=end_prod(pt); if (p2==NULL) return(-1);
            if (p2>qt-1) p2=qt-1;
            *y=EOS; strncat(y,pt,(unsigned int)(p2-pt)+1);
            if (i) strcat(s,"+");
            strcat(s,y); strcat(s,"*"); strcat(s,x);
            pt=p2+2; ++i;
            }
        return(1);
        }

/*
    INPUT: s=...!(A+B) pt=!(A+B)  OUTPUT: s=...(!A*!B)
    INPUT: s=...!(A*B) pt=!(A+B)  OUTPUT: s=...(!A+!B)

*/
char *bool_neg(char *s, char *pt)
        {
        char *q;
        char *pk;
        char m[2];

        m[1]=EOS;
        q=end_par(pt,&pk,1); if (q==NULL) return(q);
        switch (*pk)
            {
          case '(':
            if (*(pt+1)=='!')  /* poista !! */
                strncat(s,pt+2,(unsigned int)(q-pt)-2);
            else
                {
                strcat(s,"!"); strncat(s,pt+1,(unsigned int)(q-pt)-1);
                }
            break;


          case '+': *m='*'; break;
          case '*': *m='+'; break;
            }
        if (*pk!='(')
            {
            strcat(s,"!("); strncat(s,pt+1,(unsigned int)(pk-pt)-1);
            strcat(s,")"); strcat(s,m);
            strcat(s,"!("); strncat(s,pk+1,(unsigned int)(q-pk)-1);
            strcat(s,")");
            }
        return(q+1);
        }




/*
   Converts Boolean expression s to form A1*B1*... + A2*B2*... + ...
*/
int bool_norm(char *s)
        {
        char t[3*LLENGTH];
        char *ps,*pt;
        char *p,*q,*p2;
        char *pk;
        int i,k;

        while (1)
            {
            ps=strchr(s,'(');
            if (ps==NULL) break;
            strcpy(t,s);
            pt=strchr(t,'(');
            if (pt==t || *(pt-1)=='+')
                {
                *ps=EOS;
                p=end_par(pt,&pk,1);
                if (p==NULL) return(-1);
                if (*(p+1)!='*' || *pk=='*')
                    {
                    strncat(s,pt+1,(unsigned int)(p-pt-1));
                    strcat(s,p+1);
                    }
                else
                    {
                    q=end_prod(p+2); if (q==NULL) return(-1);
                    k=bool_mult(s,pt,p,p+2,q); if (k<0) return(-1);
                    strcat(s,q+1);
                    }
                }
            else if (*(pt-1)=='!')
                {
                if ((int)(pt-t)>1) *(ps-1)=EOS; else *s=EOS;
                strcat(s,"(");
                q=bool_neg(s,pt); if (q==NULL) return(-1);
                strcat(s,")");
                strcat(s,q);
                }
            else  /* *(pt-1)='*' */
                {
                q=pt-2;
                while (q>t) { if (*q=='+') { ++q; break; } --q; }
                *(s+(q-t))=EOS;
                p2=end_term(pt); if (p2==NULL) return(-1);
                k=0; if (*(p2+1)=='*') { k=1; strcat(s,"("); }
                i=bool_mult(s,pt,p2,q,pt-2); if (i<0) return(-1);
                if (k) strcat(s,")");
                strcat(s,p2+1);
                }
            }
        return(1);
        }

int sel_virhe(char *s)
        {
        sprintf(sbuf,"Error in %s specification!",s);
        if (etu==2)
            {
            sprintf(tut_info,"___@10@CONDITIONS@%s@",sbuf);
            return(-1);
            }
        if (dsp) return(-1);
        sur_print("\n"); sur_print(sbuf); WAIT;
        return(-1);
        }


int n_select;
int *sel_var;
char *sel_type; /* 0=ind 1=cases */
char *sel_rel;  /* * tai + */
double *sel_lower,*sel_upper;
char **sel_cases;
char **sel_lastcase;
char *sel_neg;
char cases_space; /* 2.1.2003 */

/*  *-cases */
int n_cases_wild=0; /* 8.1.2003 */
char cases_wild;
/* ?-cases */
int n_cases_wild2=0; /* 9.1.2003 */
char cases_wild2;

static int muste_var_nro=0;

int find_cond(SURVO_DATA *d, char *nimi, int nro)
        {
        int i,k;
        char x[LLENGTH],x2[LLENGTH],*sana[3],*condvar[256];
        char *p,*q,*r,condtype, condneg, condrel;
        char *nimi2;
        double a;
        int samecond, samecondloop; // RS ADD
        char checksamecond;

		condrel=condtype=0;
		r=NULL;
		nro+=muste_var_nro; // RS ADD
		
        nimi2=nimi; sel_neg[nro]=' ';
        if (*nimi2=='!') { ++nimi2; sel_neg[nro]='!'; }
        condneg=sel_neg[nro];

        i=spfind(nimi2);
        if (i<0) return(-1);

        strcpy(x2,spb[i]); // RS CHA
        p=x2; samecond=1; checksamecond=FALSE; // RS ADD

// Samecond support on for SELECT
        
        while (*p) 
        	{
        	if (*p==':' || *p==',') { r=p+1; condtype=*p; *p=EOS; break; }
        	if ((*p=='*' || *p=='+') && strcmp(nimi,"IND")!=0 && strcmp(nimi,"CASES")!=0) 
        		{
        		++samecond; 
        		condrel=*p; *p=' ';
        		if (checksamecond)
        			if (checksamecond!=condrel) return(-2);
        		checksamecond=condrel;		
        		} 
        	++p; 
        	}
		k=split(x2,condvar,256);

		if (condneg=='!')
			{
			if (condrel=='*') condrel='+';
			else if (condrel=='+') condrel='*';
			}
		
        for (samecondloop=0; samecondloop<samecond; samecondloop++)
        {
        sprintf(x,"%s%c%s",condvar[samecondloop],condtype,r);
	
		if (samecondloop>0) 
			{ 
			nro++; muste_var_nro++;
			sel_neg[nro]=condneg;
			sel_rel[nro]=condrel;
			}

        k=split(x,sana,3);
        if (k==0) return(-2);

        p=strchr(sana[0],':');    
        if (p==NULL) /* IND-tyyppinen */
            {
            sel_type[nro]='0';
            if (muste_strcmpi(sana[0],"ORDER")==0) sel_var[nro]=-1;
            else
                {
                sel_var[nro]=varfind(d,sana[0]); if(sel_var[nro]<0) return(-2);
                }

            if (strcmp(nimi,"CASES")==0)
                {
                sprintf(sbuf,"CASES should not be used with ind-type (,) conditions!\nUse IND or SELECT instead.");
                if (etu==2)
                    {
                    sprintf(tut_info,"___@11@CONDITIONS@%s@",sbuf); return(-2); // RS CHA exit(1) -> return(-2)
                    }
                sur_print("\n"); sur_print(sbuf); return(-2);
                }
                
            sel_lower[nro]=sel_upper[nro]=1.0;
            if (k>1)
                {               
                if (strcmp(sana[1],"!MISSING")==0) /* 3.1.2003 */
                    { sel_lower[nro]=-1e300; sel_upper[nro]=1e300; }
                else
                  {
                  if (strcmp(sana[1],"MISSING")==0) a=MISSING8; /* 31.12.2002 */
                  else if (strcmp(sana[1],"MIN")==0) a=-1e300; 
                  else a=atof(sana[1]);
                  sel_lower[nro]=sel_upper[nro]=a;
                  if (k>2) 
                  	{
                  	if (strcmp(sana[2],"MAX")==0) sel_upper[nro]=1e300; // RS ADD
                  	else sel_upper[nro]=atof(sana[2]); 
                  	}
                  }
                }
            }

        else  /* CASES-tyyppinen */
            {
            sel_type[nro]='1';
            *p=EOS;
            sel_cases[nro]=spb[i]+(r-x2); //  RS CHA spb[i]+(p-x+1); Pointer must be for global var           
            p=q=sel_cases[nro];            
            while (*p)
                {
                if (*p==',') q=p+1;
                ++p;
                }
            sel_lastcase[nro]=q;            
            if (muste_strcmpi(sana[0],"ORDER")==0) sel_var[nro]=-1; // RS ADD
            else  { sel_var[nro]=varfind(d,sana[0]); if (sel_var[nro]<0) return(-2); }
			
			if (sel_var[nro]>=0) // RS ADD
            if (d->vartype[sel_var[nro]][0]!='S' && strcmp(nimi,"CASES")==0)
                {
                sprintf(sbuf,"Variable %s not a string!",sana[0]);
                if (etu==2)
                    {
                    sprintf(tut_info,"___@11@CONDITIONS@%s@",sbuf); return(-2); // RS CHA exit(1) -> return(-2)
                    }
                sur_print("\n"); sur_print(sbuf); return(-2);
                }

            if (strcmp(nimi,"IND")==0)
                {
                sprintf(sbuf,"IND should not be used with cases-type (:) conditions!\nUse CASES or SELECT instead.");
                if (etu==2)
                    {
                    sprintf(tut_info,"___@11@CONDITIONS@%s@",sbuf); return(-2); // RS CHA exit(1) -> return(-2)
                    }
                sur_print("\n"); sur_print(sbuf); return(-2);
                }

            if (cases_space!=EOS) /* 2.1.2003 */
                {
                p=sel_cases[nro];
                while (*p) { if (*p==cases_space) *p=' '; ++p; }
                }
    /* Rprintf("\nsel_cases[nro]=%s|",sel_cases[nro]); getch(); */

            } /* CASES */
			} // RS ADD samecondloop end

//		muste_var_nro=nro; // RS ADD
	
        return(1);
        }

int conditions(SURVO_DATA *d)
        {
        int i,k;
        char x[3*LLENGTH];
        char s[LLENGTH];
        char *p,*q;
        int n_select_space;
/*        char siirtop[16];  */
        n_select=k=0;
        i=spfind("IND"); if (i>=0) ++k;
        i=spfind("CASES"); if (i>=0) ++k;
        i=spfind("SELECT");
        if (i>=0)
            {
            strcpy(x,spb[i]);
            
            if (strchr(x,',')!=NULL || strchr(x,':')!=NULL) // RS ADD
              {
              strcpy(x,"SELECT");
              }

            
            if (strchr(x,'(')!=NULL)
                {
                bool_norm(x);
/*              strcpy(s,survo_path); strcat(s,"BOOLPAR.EXE");
                sprintf(siirtop,"%p",&x);
                i=spawnl(P_WAIT,s,s,siirtop,NULL);
                if (i<0)
                    {
                    sel_virhe("SELECT (not enough memory)");
                    return(-1);
                    }
*/
                if (*x==EOS)
                    {
                    sel_virhe("SELECT"); return(-1);
                    }
                }
            p=x;
            while (*p) { if (*p=='*' || *p=='+') ++n_select; ++p; }
            ++n_select;

            }
        cases_space=EOS; /* 2.1.2003 */
        i=spfind("CASES_SPACE");
        if (i>=0) cases_space=*spb[i];

        cases_wild='\377'; /* 8.1.2003 */
        i=spfind("CASES_WILD*");
        if (i>=0) { cases_wild=*spb[i]; n_cases_wild=1; }

        cases_wild2='\377'; /* 9.1.2003 */
        i=spfind("CASES_WILD?");
        if (i>=0) { cases_wild2=*spb[i]; n_cases_wild2=1; }


        if (n_select==0 && k==0) return(1);
        n_select+=2;  /* aina tilat 0 ja 1 IND ja CASES */
        n_select_space=n_select*d->m; // RS ADD space for max variable conditions 

        sel_var=(int *)muste_malloc(n_select_space*sizeof(int));
        if (sel_var==NULL) { tilavirhe(); return(-1); }
        sel_type=muste_malloc((unsigned int)n_select_space);
        if (sel_type==NULL) { tilavirhe(); return(-1); }
        sel_rel=muste_malloc((unsigned int)n_select_space);
        if (sel_rel==NULL) { tilavirhe(); return(-1); }
        sel_lower=(double *)muste_malloc(n_select_space*sizeof(double));
        if (sel_lower==NULL) { tilavirhe(); return(-1); }
        sel_upper=(double *)muste_malloc(n_select_space*sizeof(double));
        if (sel_upper==NULL) { tilavirhe(); return(-1); }
        sel_cases=(char **)muste_malloc(n_select_space*sizeof(char **));
        if (sel_cases==NULL) { tilavirhe(); return(-1); }
        sel_lastcase=(char **)muste_malloc(n_select_space*sizeof(char **));
        if (sel_lastcase==NULL) { tilavirhe(); return(-1); }
        sel_neg=muste_malloc((unsigned int)n_select_space);
        if (sel_neg==NULL) { tilavirhe(); return(-1); }

		muste_var_nro=0; // RS ADD
        sel_var[0]=sel_var[1]=-2; sel_neg[0]=sel_neg[1]=' ';
        i=find_cond(d,"IND",0);
        if (i==-2) { sel_virhe("IND"); return(-1); }
        i=find_cond(d,"CASES",1);
        if (i==-2) { sel_virhe("CASES"); return(-1); }
        if (n_select==2) return(1);
        p=x; sel_rel[2]='*';       
        for (k=2; k<n_select; ++k)
            {
            q=p;
            while (*q && *q!='*' && *q!='+') ++q;
            if (*q) sel_rel[muste_var_nro+k+1]=*q;  // RS CHA [k+1]
            i=q-p; strncpy(s,p,(unsigned int)i); s[i]=EOS; p=q+1;
            i=find_cond(d,s,k);
            if (i<0) { sel_virhe(s); return(-1); }
                /* i==-2 -22.4.1992 */
            }

		n_select+=muste_var_nro; // RS ADD
/*		
Rprintf("\nn_select: %d, muste_var_nro: %d",n_select,muste_var_nro);
		for (i=0; i<(n_select); i++)
			{
			Rprintf("\n%d:%c|%c|%c|%d|",i,sel_type[i],sel_neg[i],sel_rel[i],sel_var[i]);
			}
*/
        return(1);
        }


int t_neg(int i,int nro)
        {
        if (sel_neg[nro]=='!') return(1-i);
        else return(i);
        }

int unsuit(SURVO_DATA *d, long l, int nro)
        {
        int len;
        double x;
        char *p;
        char sana[LLENGTH];
        char *q,*q1,*q2;
        char sbuf[LLENGTH]; /* varmuuden vuoksi lokaalisena */
        char sana2[LLENGTH];
        char *term[256]; // RS ADD
        int i;

        if (sel_type[nro]=='0')
            {
            if (sel_var[nro]==-1) x=l;  /* ORDER */
            else data_load(d,l,sel_var[nro],&x);
            if (x<sel_lower[nro] || x>sel_upper[nro]) return(t_neg(1,nro));
            return(t_neg(0,nro));
            }
        p=sel_cases[nro];

		i=FALSE; // RS ADD
        if (sel_var[nro]==-1) { x=l; i=TRUE; } // RS ADD - ORDER CASES
        else if (d->vartype[sel_var[nro]][0]!='S') // RS ADD - numeric CASES
            {
            data_load(d,l,sel_var[nro],&x);       
        	i=TRUE;
        	}
        if (i)
        	{ 	
            strcpy(sbuf,sel_cases[nro]);
            len=split(sbuf,term,256);
            for (i=0; i<len; i++) if (atoi(term[i])==(int)x) return(t_neg(0,nro));
            return(t_neg(1,nro));
            }
		else 
		{
        data_alpha_load(d,l,sel_var[nro],sana+1); *sana=',';
        len=strlen(sana); while(sana[len-1]==' ') sana[--len]=EOS;
        if (strcmp(sel_lastcase[nro],sana+1)==0) return(t_neg(0,nro));
        sana[len]=','; ++len; sana[len]=EOS;
        if (n_cases_wild || n_cases_wild2) /* 9.1.2003 */
            {
            if (strchr(p,cases_wild)!=NULL || strchr(p,cases_wild2)!=NULL)
                {
/*              Rprintf("\np=%s|",p);
              Rprintf("\nsana+1=%s|",sana+1); getch();
*/
                while (1)
                    {
                    q=strchr(p,',');
                    if (q!=NULL)
                        {
                        *sbuf=EOS; strncat(sbuf,p,(unsigned int)(q-p));
                        }
                    else strcpy(sbuf,p);
/*           Rprintf("\nsbuf=%s|",sbuf); getch(); */
                    q1=strchr(sbuf,cases_wild);
                    if (q1==NULL)
                      {
                      sana[len-1]=EOS;

                      q2=strchr(sbuf,cases_wild2);
                      if (q2!=NULL)
                          {
                          strcpy(sana2,sana);
                          while (q2!=NULL)
                              {
                              i=q2-sbuf;
                              if (i>strlen(sana2)-2) break;
                              sana2[i+1]=cases_wild2;
                              q2=strchr(sbuf+i+1,cases_wild2);
                              }
/* Rprintf("\n?: sbuf=%s| sana2+1=%s|",sbuf,sana2+1); getch(); */
                          if (strcmp(sbuf,sana2+1)==0) return(t_neg(0,nro));
                          else { if (q==NULL) break; p=q+1; continue; }
                          }

                      if (strcmp(sbuf,sana+1)==0) return(t_neg(0,nro));
                      }
/* -9.1.2003
                    else if (strncmp(sbuf,sana+1,(int)(q1-sbuf))==0)
                                                return(t_neg(0,nro));
***/
                    else
                        {

                        q2=strchr(sbuf,cases_wild2);
                        if (q2==NULL)
                             {
                             if (strncmp(sbuf,sana+1,(unsigned int)(q1-sbuf))==0)
                                                return(t_neg(0,nro));
                             }
                        else
                            {
  /*                        Rprintf("\n* ja ? yht'aikaa!"); getch(); */

                            strcpy(sana2,sana);
                            sana2[(int)(q1-sbuf)+1]=EOS;
                            *q1=EOS;
                            while (q2!=NULL)
                                {
                                i=q2-sbuf;
                                if (i>strlen(sana2)-2) break;
                                sana2[i+1]=cases_wild2;
                                q2=strchr(sbuf+i+1,cases_wild2);
                                }
/* Rprintf("\n?: sbuf=%s| sana2+1=%s|",sbuf,sana2+1); getch(); */
                            if (strcmp(sbuf,sana2+1)==0) return(t_neg(0,nro));
                            else { if (q==NULL) break; p=q+1; continue; }
                            }
                        }


                    if (q==NULL) break;
                    p=q+1;

                    }
                return(t_neg(1,nro));
                }
            }
        if (strncmp(p,sana+1,(unsigned int)(len-1))==0) return(t_neg(0,nro));
        if (strstr(p,sana)!=NULL) return(t_neg(0,nro));
        return(t_neg(1,nro));
        }
        }

int unsuitable(SURVO_DATA *d, long l)
        {
/*        int i, */
        int k,h;
        extern int muste_expand;        

		if (muste_expand) return(0); // RS ADD
        if (n_select==0) return(0);
        if (sel_var[0]>-2 && unsuit(d,l,0)) return(1);
        if (sel_var[1]>-2 && unsuit(d,l,1)) return(1);

        if (n_select==2) return(0);
        h=0; k=2;
        while (k<n_select)
            {
            if (h && sel_rel[k]=='+') return(0);
            if (unsuit(d,l,k))
                {
                ++k;
                while (k<n_select && sel_rel[k]=='*') ++k;
                if (k==n_select) return(1);
                h=0; continue;
                }
            ++k; h=1;
            }
        return(0);
        }



void scales(SURVO_DATA *d)      /* removes '-' scale variables */
        {
        int i,j;

        j=0;
        for (i=0; i<d->m_act; ++i)
            if (d->vartype[d->v[i]][3]!='-')
                d->v[j++]=d->v[i];
            else
                d->vartype[d->v[i]][1]='-';
        d->m_act=j;
        }

int scale_ok(SURVO_DATA *d, int i, char *scale)
        {
        char itype=d->vartype[i][3];

        if (itype=='-') return(0);
        if (scale_check==0) return(1);

        if (strchr(scale,itype)==NULL) return(0);
        return(1);
        }

int activated(SURVO_DATA *d, char merkki)
        {
        int i;
        for (i=0; i<d->m; ++i) if (d->vartype[i][1]==merkki) return(i);
        return(-1);
        }

/* dat5.c 9.2.1987/SM (26.7.1989)

   mask_sort  sorts the active field indices d->v[] in alphabetic order
              of their masks.
              In ties the original order is preserved.
   -1 is returned, if not enough space.
*/
int mask_sort(SURVO_DATA *d)
        {
        int i,k;
        int m=d->m_act;
        short *v;
        char **type;
        unsigned char t0;
        int n0,v0;
        char ind;
        int h;

static unsigned char *t;
static int *nro;

        t=muste_malloc((unsigned int)(m+1));
        if (t==NULL)
            { sur_print("\nNot enough memory!"); WAIT; return(-1); }
        nro=(int *)muste_malloc((m+1)*sizeof(int));
        if (nro==NULL)
            { sur_print("\nNot enough memory!"); WAIT; return(-1); }
        v=d->v;
        type=d->vartype;
        for (i=0; i<m; ++i) { nro[i]=i; t[i]=type[v[i]][1]; }

        h=m;
        while (h>1)
            {
            h/=2;
            while (1)
                {
                ind='1';
                for (k=0; k<m-h; ++k)
                    {
                    if (t[k]>t[k+h] || (t[k]==t[k+h] && nro[k]>nro[k+h]))
                        {
                        t0=t[k]; t[k]=t[k+h]; t[k+h]=t0;
                        n0=nro[k]; nro[k]=nro[k+h]; nro[k+h]=n0;
                        v0=v[k]; v[k]=v[k+h]; v[k+h]=v0;

                        ind='0';
                        }
                    }
                if (ind=='1') break;
                }
            }

        muste_free(nro); muste_free(t);
        return(1);
        }


int mask(SURVO_DATA *d)
        {
        int i,k,h;
        int masknro;
        char maskset[LLENGTH];
        char *p,*q;
        char *sana[EP4];
        char act;
        int k2; /* 9.11.2007 */
        extern int muste_expand;

		if (muste_expand) return(1); // RS ADD
        i=spfind("VAR");
        if (i<0) i=spfind("VARS");
        if (i>=0) // RS ADD !muste_expand
            {
            strcpy(maskset,spb[i]);
            k=split(maskset,sana,EP4);
                                                 /* 3.8.2005 */
            if (k>0 && strcmp(sana[0],"ALL")==0) /* && varfind2(d,sana[0],0)<0) */
                {
                for (i=0; i<d->m; ++i) d->vartype[i][1]='A';
                for (i=1; i<k; ++i)
                    {
                    p=sana[i]; if (*p=='-') ++p;
                    h=varfind(d,p); if (h<0) return(-1);
                    d->vartype[h][1]='-';
                    }
                d->m_act=k=d->m-k+1;
                h=0; for (i=0; i<d->m; ++i) if (d->vartype[i][1]=='A') { d->v[h]=i; ++h; }
                return(1);
                }
            for (i=0; i<d->m; ++i) d->vartype[i][1]='-';
            k2=0;
            for (i=0; i<k; ++i)
                {
                p=strchr(sana[i],'(');
                if (p!=NULL) { act=*(p+1); *p=EOS; } else act='A';
/* Rprintf("\nsana1=%s|",sana[i]); getch();
 No error message for varibles given as [name]  9.11.2007
*/
                if (*sana[i]=='[')
                    {
                    p=strchr(sana[i],']'); if (p!=NULL) *p=EOS;
/* Rprintf("\nsana2=%s|",sana[i]+1); getch();  */
                    h=varfind2(d,sana[i]+1,0);
                    if (h<0) continue;
                    }
                else
                    {
                    h=varfind(d,sana[i]); if (h<0) return(-1);
                    }
                d->v[k2]=h; d->vartype[h][1]=act;
                ++k2;
                }
            d->m_act=k2;
            return(1);
            }
        i=spfind("MASK"); if (i<0) return(1);
        *maskset=EOS;
        if (*spb[i]=='#')
            {
            masknro=atoi(spb[i]+1);
            p=strchr(spb[i],'(');
            if (p!=NULL)
                {
                q=strchr(p,')');
                if (q==NULL)
                    {
                    sprintf(sbuf,") missing in MASK!");
                    if (dsp) return(-1);
                    sur_print("\n"); sur_print(sbuf);
                    WAIT; return(-1);
                    }
                strncpy(maskset,p+1,(unsigned int)(q-p-1)); maskset[q-p-1]=EOS;
                }
            if (d->type!=2 || ( masknro<0 || masknro>d->typelen) )
                {
                sprintf(sbuf,"Illegal MASK #");
                if (dsp) return(-1);
                sur_print("\n"); sur_print(sbuf);
                WAIT; return(-1);
                }
            for (k=0; k<d->m; ++k)
                {
                d->vartype[k][1]=d->vartype[k][masknro];
                if (*maskset && strchr(maskset,d->vartype[k][1])==NULL )
                    d->vartype[k][1]='-';
                }
            }
        else
            {
            for (k=0; k<d->m; ++k) d->vartype[k][1]='-';
            h=strlen(spb[i]); if (h>d->m) h=d->m;  /* 6.7.87 */
            for (k=0; k<h; ++k)
                d->vartype[k][1]=spb[i][k];
            }
        d->m_act=0;
        for (k=0; k<d->m; ++k)
            if (d->vartype[k][1]!='-') d->v[d->m_act++]=k;
        return(1);
        }

int type_mat(double *A,int m,int n)
        {
        int i,j;
        int type=20;

        if (m!=n) return(0);
        for (i=0; i<m; ++i) for (j=0; j<i; ++j)
            {
            if (A[i+m*j]) type=10;
            if (A[i+m*j]!=A[j+m*i]) { type=0; i=j=m; }
            }
        return(type);
        }

int mat_name(char *matfile, char *matr)
        {
/*        int i; */

        *matfile=EOS;       
        if (!muste_is_path(matr)) // RS CHA if ((strchr(matr,':')==NULL) && (matr[0]!='/') && (matr[0]!='.') && matr[0]!='<' && (matr[0]!='~') && matr[0]!='\\')  // RS ADD unix path FIXME? 
          strcpy(matfile,edisk);
        strcat(matfile,matr);
        muste_append_path(matfile,".MAT"); // RS CHA if (strchr(matr,'.')==NULL) strcat(matfile,".MAT");
        return(1);
        }

static FILE *MAT; /* -3.1.1997 defined as local! */

int matrix_save0(
char *matr,   /* matriisin nimi */
double *A,    /* matriisitila */
int m,        /* rivien lkm */
int n,        /* sar. lkm   */
char *rlab,   /* rivien otsikot */
char *clab,   /* sar. otsikot  */
int mrl,      /* riviotsikon pituus */
int mcl,      /* sar.otsikon pituus */
int type,     /* tyyppi =-1,jos tuntematon */
char *expr,   /* lauseke (sis.nimi) max ERC */
int nrem,     /* kommenttirivien lkm */
int remrivi,  /* kommenttien alku */
char *ptext  /* Jos !=NULL, osoitin nrem*ERC-mittaiseen tekstiin */
)
        {
        char matfile[LNAME];
        char x[ERC+1];
        int i,j,j1,j2;
        int mname;
        register int h;
        char *p;
        char rivi[LLENGTH];
        char *nimi;
        char *pl;
        int min_m;

        nimi=expr;
        i=strlen(matr);
        if (matr[i-1]=='!') { matr[i-1]=EOS; nimi=matr; }
        mat_name(matfile,matr);
        MAT=muste_fopen(matfile,"wb");
        if (MAT==NULL)
            {
            sprintf(sbuf,"\nCannot open file %s !",matfile); sur_print(sbuf);
            WAIT; return(-1);
            }

        if (nrem<0) nrem=0;
        mname=nrem+1;

        if (type==-1) type=type_mat(A,m,n);

        sprintf(x,"MATRIX84D %d %d %d %d %d %d",
                           m,n,mname,mrl,mcl,type);
        for (i=strlen(x); i<ERC; ++i) x[i]=' ';
        for (i=0; i<ERC; ++i) putc((int)x[i],MAT);

        if (nrem)
            {
            if (ptext!=NULL)
                for (i=0; i<nrem*ERC; ++i)
                    putc((int)ptext[i],MAT);
            else
                for (j=0; j<nrem; ++j)
                    {
                    edread(rivi,remrivi+j);
                    min_m=ERC; if (c2<ERC) min_m=c2;
                    for (i=0; i<min_m; ++i) putc((int)rivi[i+1],MAT);
                    for (i=c2; i<ERC; ++i) putc((int)' ',MAT);
                    }
            }

        j=strlen(expr);
        for (i=0; i<j; ++i) putc((int)nimi[i],MAT);
        for (i=j; i<ERC; ++i) putc((int)' ',MAT);
        for (i=0; i<n*mcl; ++i) putc((int)clab[i],MAT);

        pl=rlab;
        for (i=0; i<m; ++i)
            {
            for (j=0; j<mrl; ++j) { putc((int)*pl,MAT); ++pl; }
            j1=0; j2=n-1;
            if (type)
                {
                j2=i;
                if (type==20) j1=i;
                }
            for (j=j1; j<=j2; ++j)
                {
                p=(char *)&A[i+m*j];
                for (h=0; h<sizeof(double); ++h) putc((int)(*(p+h)),MAT);
                }
            }

        i=1;
        if (ferror(MAT))
            {
            sprintf(sbuf,"\nCannot save matrix %s !",matfile); sur_print(sbuf);
            WAIT; i=-1;
            }
        muste_fclose(MAT);
        return(i);
        }

int matrix_save(
char *matr,   /* matriisin nimi */
double *A,    /* matriisitila */
int m,        /* rivien lkm */
int n,        /* sar. lkm   */
char *rlab,   /* rivien otsikot */
char *clab,   /* sar. otsikot  */
int mrl,      /* riviotsikon pituus */
int mcl,      /* sar.otsikon pituus */
int type,     /* tyyppi =-1,jos tuntematon */
char *expr,   /* lauseke (sis.nimi) max ERC */
int nrem,     /* kommenttirivien lkm */
int remrivi  /* kommenttien alku */
)
    {
    int i;

    i=matrix_save0(matr,A,m,n,rlab,clab,mrl,mcl,type,expr,
                  nrem,remrivi,NULL);
    return(i);
    }

int sel_free()
        {
        if (n_select==0) return(1);

        muste_free(sel_var);
        muste_free(sel_type);
        muste_free(sel_rel);
        muste_free(sel_lower);
        muste_free(sel_upper);
        muste_free(sel_cases);
        muste_free(sel_lastcase);
        muste_free(sel_neg);
        n_select=0;
        return(1);
        }       
