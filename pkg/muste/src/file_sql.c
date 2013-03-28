#include "muste.h"
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "survo.h"
#include "survoext.h"
#include "survolib.h"


static int remarks()
        {
        init_remarks();
        rem_pr("Usage:");
        rem_pr("FILE SQL <query> TO <new_data_file>");
        rem_pr("performs SQL <query> and returns results to a new data file.");
        rem_pr(" ");
        rem_pr("The QUERY list defined in the edit field contains standard");
        rem_pr("SQL query in which SVO-files are referenced as");
        rem_pr("MUSTE.<path_to_file>");
        rem_pr(" ");
        rem_pr("QUERY EXAMPLE");
        rem_pr(" select Kunta,Lääni from MUSTE.<Survo>/D/KUNNAT");
        rem_pr("END");
        rem_pr(" ");
        rem_pr("Database to be used should be given with specification");
        rem_pr("DATABASE=<path_to_database_file>, otherwise memory is used.");
        wait_remarks(2);
        return(1); 
        }

void muste_file_sql(int argc,char *argv[]) // RS 12.2.2013
        {
        int i;
        char obuf[LLENGTH];
        char muisti[]=":memory:";
        char *ofile,*qbuf,*loppu,*db;

        if (argc==1) return;
		if (!muste_requirepackage("virta")) return;        
        s_init(argv[1]);
        if (g<3) { remarks(); return; }
        qbuf=NULL;
 		qbuf=(char *)malloc(10*LLENGTH);
		if (qbuf==NULL) { sur_print("\nOut of memory ERROR!"); WAIT; return; }     
        
        strcpy(qbuf,"virta.sql('");
        ofile=obuf; *ofile=EOS;
        if (g>3) if (muste_strcmpi(word[3],"TO")==0) 
        	{ 
        	if (g>4)
        		{
				if (muste_strcmpi(word[4],"NEW")==0)
					{
					strcpy(ofile,word[5]);
					if (strchr(ofile,'.')==NULL) strcat(ofile,".SVO");
					sur_delete(ofile);			
					}
				else
					{
					strcpy(ofile,word[4]);
					if (strchr(ofile,'.')==NULL) strcat(ofile,".SVO");
					}
        		}	
        	}
        if (strchr(word[2],' ')!=NULL) 	// Query from command line
        	{
        	strcat(qbuf,word[2]);
        	}
        else							// Query from QUERY list
        	{
        	i=wfind("QUERY",word[2],1); 
			if (i<0) { strcat(word[2],":"); i=wfind("QUERY",word[2],1); } // RS 22.3.2013
			if (i<0)
				{
				sprintf(sbuf,"\nQuery %s not found!",word[2]);
				sur_print(sbuf); WAIT; free(qbuf); return;
				}        	
 
        	while (i<r2)
            	{
				++i;
				edread(sbuf,i);
				if (strncmp(sbuf,"*END ",5)==0) break;
				loppu=strstr(sbuf," / ");
				if (loppu==NULL) loppu=sbuf+c2;
				while (loppu>sbuf && *loppu==' ') *loppu--=EOS;	
				strcat(qbuf,sbuf+1);
				strcat(qbuf,"\n");
        		}				
        	}
    
        i=spec_init(r1+r-1); if (i<0) { free(qbuf); return; }
        db=muisti;
        i=spfind("DATABASE");
        if (i>=0) 
        	{
        	strcpy(sbuf,spb[i]);
        	muste_expand_path(sbuf);
        	if (strchr(sbuf,'.')==NULL) strcat(sbuf,".db");
        	db=sbuf;
        	}

		strcat(qbuf,"','");
		strcat(qbuf,ofile);
		strcat(qbuf,"','");
		strcat(qbuf,db);
		strcat(qbuf,"')");
		
		muste_iconv(qbuf,"","CP850");

		do { loppu=strchr(qbuf,'\\');
		if (loppu!=NULL) *loppu='/';
		} while (loppu!=NULL);

		i=muste_evalr(qbuf);
		if (i<0)
			{
			sur_print("\nSQL Error!"); WAIT;
			}
		free(qbuf);
        return;
        
        }
