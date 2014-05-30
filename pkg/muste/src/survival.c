#include "muste.h"

#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include "survo.h"
#include "survoext.h"
#include "survolib.h"

static int b,f=0,aikavaleja=0,p,results_line,koko=0,cens=0,censor=0;
static int n=0,strata=0,predictors=0,solmu=0,
	nodes=0,numero=0,terminal=0,memnum,nro=1,suitable,eikay,parasmuut,puumalli=-1,lifetable=-1,
	solmuja=0,MinObs,treshold=1,sub,isopuu=0;
static double interval=1.0,MaxAika=0.0,MaxMax=0.0,summa,summa3,bestval,step,splitpoint,fixed=4.0,
	Testisuure=0.0,treesum=0.0,katkaisu=0.0,ylaK=0.0,medi=0.0,alaK=0.0,keski=0.0;
static char merkkis;

static char CLASS[LLENGTH],INTERVAL[LLENGTH],METHOD[LLENGTH],NODES[LLENGTH],PENALTY[LLENGTH],
	variable[20],TESTI[LLENGTH],nimi[LLENGTH],line[LLENGTH],OBSLIMIT[LLENGTH]; 

static SURVO_DATA d;
static SURVO_DATA e;

#define TYHJA 100000.0;
#define TYHJA2 -100000.0;

static int *predind,*c_failed,*c_censored,*c_n_enter,*sensuroitu,*c_indeksi,*Pc_failed,*Pc_left,
	*Pnimittaja,*TerNod,*Codes;
static double *c_lower,*c_upper,*c_eff_size,*c_cpf,*c_cps,
	*c_cpf_error,*c_survival,*c_failure,*c_surv_error,
	*c_surv_lcl,*c_surv_ucl,*c_pdf,*c_pdf_error,
	*c_hazard,*c_haz_error,*c_median,*c_med_error,
	*c_duration,*c_aika,*c_class,*x,*Stat,*Tres,
	*Pc_survival,*Pc_failure,*Pc_st_error,*Pc_lower_cl,*Pc_upper_cl; 

static int **m_events,**m_size,**m_censored;
static double **m_taulukko;
static char **NIMET;

static long double *tapahtumat;
static long double *riskiryh,*sensuurit,*paino,*mV2,*mD,*mU,*vector,*apumat,*mU2;
static long double **mV;
 
static char *vartype, **pvartype;         
static int *varlen;
static char **varname, *vartila;

typedef struct tnode {
  double Score, Split, Prune; 
  char Var[20];
  long int *data;
  int datasize,paraskoko,Num,censsize,parascens,solmuja,Code;
  struct tnode *parent, *lchild, *rchild;
} tnode;

static tnode *puu;

static int n_enter(int ed_failed,int ed_censored,int ed_n_enter);
static double eff_size(int n_enter,int censored);
static double cpf(int failed,double eff_size);
static double cps(double cpf);
static double cpf_error(double cpf,double eff_size);
static double survival(double ed_cps,double ed_survival);
static double failure(double survival);
static double surv_error(double survival,double cpf,double eff_size);
static double surv_lcl(double survival,double surv_error);
static double surv_ucl(double survival,double surv_error);
static double pdf(double survival,double cpf);
static double apu_pdf_error(double cps,double eff_size);
static double pdf_error(double pdf,double cps,double eff_size,double summa3);
static double hazard(double cpf);
static double haz_error(double hazard,double eff_size,double cpf);
static double median(double lower,double lower2,double survival,double survival2,double survival3);
static double med_error(double survival,double pdf,double eff_size);
static int failed(int censored,int ed_failed);
static double Psurvival(double ed_survival,int left,int ed_left,int sensuroituja);
static double Pfailure(double survival);
static double st_error(double survival,int events,int left);
static double lower_cl(double survival,double st_error);
static double upper_cl(double survival,double st_error);
static void qsort_mtaulukko(int left, int right, tnode *node);
static void qsort_aika(int left, int right, tnode *node);
static int swap_mtaulukko(int i, int j, tnode *node);
static int not_enough_memory();
static int print_line(char *line);
static double paras_arvo(double x,double y);
static int Laske_aikavaleja();
static void space_allocation();
static void vapauta();
static int Lue_Data();
static int Pilko();
static int Laske();
static int Tulosta();
static int Luo_uusi();
static int Talletus();
static int Plaske();
static int Ptulosta();
static int Pluo_uusi();
static int Ptalletus();
static int Muodosta();
static long double Testi(int tunnus);
static int Tulos();
static int add(tnode *node);
static int pruning(tnode *node);
static int Pilko2(int muut,tnode *node);
static int Muodosta2(tnode *node);
static int intra();
static void prune(tnode *node);
static int Kurvit(tnode *node);
static int Tulos2(tnode *node);
static int Tulos3(tnode *node);
static int uusiT(tnode *node);
static int talletaT();																	

//------------------------------------------------------------------------------------------

static int n_enter(int ed_failed,int ed_censored,int ed_n_enter)	// Elinajantaulun sarakkeiden laskemiseen
{		                                                	// kaytettavat apufunktiot
	return(ed_n_enter-ed_failed-ed_censored);
}

static double eff_size(int n_enter,int censored)
{
	return(n_enter-(double)censored/2);
}

static double cpf(int failed,double eff_size)
{
	return(failed/eff_size);
}

static double cps(double cpf)
{
	return(1.0-cpf);
}

static double cpf_error(double cpf,double eff_size)
{
	return (double)sqrt(cpf*(1-cpf)/eff_size);
}

static double survival(double ed_cps,double ed_survival)
{
	return(ed_survival*ed_cps);
}

static double failure(double survival)
{
	return(1-survival);
}

static double surv_error(double survival,double cpf,double eff_size)
{
	summa=summa+cpf/(eff_size*(1-cpf));
	return survival*sqrt(summa);
}

static double surv_lcl(double survival,double surv_error)
{
	return survival-1.96*surv_error;
}

static double surv_ucl(double survival,double surv_error)
{
	return survival+1.96*surv_error;
}

static double pdf(double survival,double cpf)
{
	return(survival*cpf/interval);
}

static double apu_pdf_error(double cps,double eff_size)
{
	return (1-cps)/(eff_size*cps);
}

static double pdf_error(double pdf,double cps,double eff_size,double summa3)
{	
	return pdf*sqrt(summa3+cps/(eff_size*(1-cps)));
}

static double hazard(double cpf)
{
	return 2*cpf/(interval*(1+(1-cpf)));
}

static double haz_error(double hazard,double eff_size,double cpf)
{
	return hazard*sqrt((1-(interval*hazard/2)
		*(interval*hazard/2))/(eff_size*cpf));
}

static double median(double lower,double lower2,double survival,double survival2,double survival3)
{
	return lower2-lower+interval
		*(survival2-survival/2)/(survival2-survival3);
}

static double med_error(double survival,double pdf,double eff_size)
{
	return survival/(2*pdf*sqrt(eff_size));
}

//------------------------------------------------------------------------------------------

static int failed(int censored,int ed_failed)				// Tulorajaestimaattorin sarakkeiden laskemiseen
{					// k‰ytett‰v‰t apufunktiot
	if (censored!=1) 
		return ed_failed;
	else
		return ed_failed+1;
}

static double Psurvival(double ed_survival,int left,int ed_left,int sensuroituja)
{  
	return ed_survival*left/(ed_left-sensuroituja);
}

static double Pfailure(double survival)
{
	return (1-survival);
}

static double st_error(double survival,int events,int left)
{
	summa=summa+(double)events/(left*(left-events));
	return survival*sqrt(summa);
}

static double lower_cl(double survival,double st_error)
{
	return survival-1.96*st_error;
}

static double upper_cl(double survival,double st_error)
{
	return survival+1.96*st_error;
}

//------------------------------------------------------------------------------------------


static void qsort_mtaulukko(int left, int right, tnode *node){		// J‰rjest‰‰ taulukon havainnot
															// ensisijaisesti annetun
	int i, last;											// luokittelumuuttujan arvojen mukaan
															
	if (left>=right)
		return;
	swap_mtaulukko(left, (left+right)/2, node);
	last=left;
	for (i=left+1; i<=right; i++)
      if  ((c_class[i]< c_class[left]) 
          || ((c_class[i]==c_class[left]) && (c_duration[i]<c_duration[left]))
		  ||  (c_class[i]==c_class[left] && c_duration[i]==c_duration[left] && 
			   sensuroitu[i]==1))
		  swap_mtaulukko(++last, i, node);
	swap_mtaulukko(left, last, node);
	qsort_mtaulukko(left, last-1, node);
	qsort_mtaulukko(last+1, right, node);
}


static void qsort_aika(int left, int right, tnode *node){		// J‰rjest‰‰ taulukon havainnot 
														// ensisijaisesti ajan mukaan
	int i, last;										
	
	if (left>=right)
		return;
	swap_mtaulukko(left, (left+right)/2, node);
	last=left;
	for (i=left+1; i<=right; i++)
      if  ((c_duration[i]<c_duration[left])
		  || (c_duration[i]==c_duration[left] && 
			   sensuroitu[i]==1))
		  swap_mtaulukko(++last, i, node);
	swap_mtaulukko(left, last, node);
	qsort_aika(left, last-1, node);
	qsort_aika(last+1, right, node);
}


static int swap_mtaulukko(int i, int j, tnode *node){		// J‰rjestysalgoritmien k‰ytt‰m‰
													// apufunktio
	double tdur,tclass;		
	int tsens,tidx, tdata;
  
	tdur=c_duration[i];
	tsens=sensuroitu[i];
	tclass=c_class[i];
	tidx=c_indeksi[i];
	tdata=node->data[i];

	c_duration[i]=c_duration[j];
	sensuroitu[i]=sensuroitu[j];
	c_class[i]=c_class[j];
	c_indeksi[i]=c_indeksi[j];
	node->data[i]=node->data[j];

	c_duration[j]=tdur;
	sensuroitu[j]=tsens;
	c_class[j]=tclass;
	c_indeksi[j]=tidx;
	node->data[j]=tdata;

return 0;
}

//------------------------------------------------------------------------------------------

static int not_enough_memory(){					// Apufunktio tilanvarausten tarkistuksiin
	sur_print("Not enough memory!");WAIT;
return 0;
}

static int print_line(char *line)						// Tulostuksessa k‰ytett‰v‰ apufunktio
{	
	output_line(line,eout,results_line);	
	if (results_line) ++results_line;
	return 0;
}

static double paras_arvo(double x,double y)						// Laskee parhaat parametrit kuvien piirt‰miselle// (bestval ja step!)
        {
        double z;
        int merkki=1;
        char a[22],b[22];
        int i,j,k,h;
        char u,v;

        if (x>y) { z=x; x=y; y=z; }
        if (x<=0.0 && y>=0.0) return(0.0);
        if (x==y) return(x);
        if (x<0) { merkki=-1; z=x; x=-y; y=-z; }
		
        sprintf(a,"%21.10f",x); a[21]='\0';
        sprintf(b,"%21.10f",y); b[21]='\0';
        i=0; while (a[i]==' ') a[i++]='0';
        i=0; while (b[i]==' ') b[i++]='0';

        i=0; k=0; while (a[i]==b[i] && i<22) { ++i; if (a[i]!='0') ++k; }
        h=0;
        for (j=i+1; j<21; ++j) { if (b[j]!='.') b[j]='0';
                                 if (a[j]!='.' && a[j]!='0') ++h;
                               }
        u=a[i]; v=b[i];
        if (h>0) ++u;
        if (u==v) ;
        else if (u=='0' && k==0) b[i]='1';
        else if (u=='0') b[i]='0';
        else if (u<'6' && v>'4') b[i]='5';
        else if (u<'3' && v<='3') b[i]='2';
        else if (u<'5' && v<'5') b[i]='4';
        else if (u=='6' && v=='7') b[i]='6';
        else b[i]='8';

        return(merkki*atof(b)); 
}

static int Laske_aikavaleja(){					// Tarvitaan elinajantaulun tilanvarauksiin
									
	double aika;
	int j,i;
	long l;

	j=varfind(&d,word[2]);
	if (j==-1)
		return(-1);
			
	for(l=d.l1; l<=d.l2; ++l){
		if (unsuitable(&d,l)) continue;
		data_load(&d,l,j,&aika);
		if (aika>MaxMax)
			MaxMax=aika;
	}

	if (puumalli!=1 && lifetable==1){
		i=spfind("INTERVAL");
		if (i>=0){
			strcpy(INTERVAL,spb[i]);
			interval=atof(INTERVAL);
			if (interval<=0){
				output_open(eout);
				sprintf(line,"The interval must be positive");
				print_line(line);WAIT;
				output_close(eout);				
				return(-1);
			}
		}
		aikavaleja=(int)((MaxMax/interval)+1);
	}
return 0;
}

//------------------------------------------------------------------------------------------

static void space_allocation(){									// Tilanvarausfunktio

	int i,row,apu,apu2,kaksi,viisi;

	apu=d.n+1;
	apu2=d.m;
	kaksi=2;
	viisi=12;

	predind=(int *)muste_malloc(apu2*sizeof(int));
	c_censored=(int *)muste_malloc(apu*sizeof(int));
	sensuroitu=(int *)muste_malloc(apu*sizeof(int));
	c_duration=(double *)muste_malloc(apu*sizeof(double));
	c_aika=(double *)muste_malloc(apu*sizeof(double));
	c_class=(double *)muste_malloc(apu*sizeof(double));
	x=(double *)muste_malloc(viisi*sizeof(double));
	m_size=muste_malloc(apu*sizeof(int *));			// T‰ytyy olla varattu tilaa,
	for (row=0;row<apu;row++){					// koska kutsutaan Pilko()-funktiossa
		m_size[row]=muste_malloc(viisi*sizeof(int));    
	}
    c_indeksi=(int *)muste_malloc(apu*sizeof(int));

	if (puumalli==1){
		Stat=(double *)muste_malloc(apu*sizeof(double));
		TerNod=(int *)muste_malloc(apu*sizeof(int));
		Tres=(double *)muste_malloc(apu*sizeof(double));
		Codes=(int *)muste_malloc(apu*sizeof(int));
		Pc_left=(int *)muste_malloc(apu*sizeof(int));
		Pc_survival=(double *)muste_malloc(apu*sizeof(double));
		NIMET=muste_malloc(apu2*sizeof(char *));
		for (row=0;row<apu2;row++){
			NIMET[row]=muste_malloc(20*sizeof(char));
		}
		m_taulukko=muste_malloc(apu2*sizeof(int *));
		for (row=0;row<apu2;row++){
			m_taulukko[row]=muste_malloc(apu*sizeof(double));
		}
		m_events=muste_malloc(apu*sizeof(int *));
		m_size=muste_malloc(apu*sizeof(int *));
		m_censored=muste_malloc(apu*sizeof(int *));
		for (row=0;row<apu;row++){
			m_events[row]=muste_malloc(kaksi*sizeof(int));
			m_size[row]=muste_malloc(kaksi*sizeof(int));
			m_censored[row]=muste_malloc(kaksi*sizeof(int));
		}
	}
	else{
		m_taulukko=muste_malloc(3*sizeof(int *));
		for (row=0;row<3;row++){
			m_taulukko[row]=muste_malloc(apu*sizeof(double));
		}
		if (lifetable==1){
			c_censored=(int *)muste_malloc(aikavaleja*sizeof(int));
			c_failed=(int *)muste_malloc(aikavaleja*sizeof(int));
			c_n_enter=(int *)muste_malloc(aikavaleja*sizeof(int));
			c_lower=(double *)muste_malloc(aikavaleja*sizeof(double));
			c_upper=(double *)muste_malloc(aikavaleja*sizeof(double));
			c_eff_size=(double *)muste_malloc(aikavaleja*sizeof(double));
			c_cpf=(double *)muste_malloc(aikavaleja*sizeof(double));
			c_cps=(double *)muste_malloc(aikavaleja*sizeof(double));
			c_cpf_error=(double *)muste_malloc(aikavaleja*sizeof(double));
			c_survival=(double *)muste_malloc(aikavaleja*sizeof(double));
			c_failure=(double *)muste_malloc(aikavaleja*sizeof(double));
			c_surv_error=(double *)muste_malloc(aikavaleja*sizeof(double));
			c_surv_lcl=(double *)muste_malloc(aikavaleja*sizeof(double));
			c_surv_ucl=(double *)muste_malloc(aikavaleja*sizeof(double));
			c_pdf=(double *)muste_malloc(aikavaleja*sizeof(double));
			c_pdf_error=(double *)muste_malloc(aikavaleja*sizeof(double));
			c_hazard=(double *)muste_malloc(aikavaleja*sizeof(double));
			c_haz_error=(double *)muste_malloc(aikavaleja*sizeof(double));
			c_median=(double *)muste_malloc(aikavaleja*sizeof(double));
			c_med_error=(double *)muste_malloc(aikavaleja*sizeof(double));
		}	
		else{
			Pc_failed=(int *)muste_malloc(apu*sizeof(int));
			Pc_left=(int *)muste_malloc(apu*sizeof(int));
			Pnimittaja=(int *)muste_malloc(apu*sizeof(int)); 
			Pc_survival=(double *)muste_malloc(apu*sizeof(double));
			Pc_failure=(double *)muste_malloc(apu*sizeof(double));
			Pc_st_error=(double *)muste_malloc(apu*sizeof(double));
			Pc_lower_cl=(double *)muste_malloc(apu*sizeof(double));
			Pc_upper_cl=(double *)muste_malloc(apu*sizeof(double));
		}
	}
	i=varfind2(&d,CLASS,0);
	if (i>=0){
		m_events=muste_malloc(apu*sizeof(int *));
		m_censored=muste_malloc(apu*sizeof(int *));
		for (row=0;row<apu;row++){
			m_events[row]=muste_malloc(viisi*sizeof(int));
			m_censored[row]=muste_malloc(viisi*sizeof(int));
		}
	}
}


static void vapauta(){									// Tilan vapautus funktio

	int i,row,apu,apu2;
	
	apu=d.n+1;
	apu2=d.m;

	muste_free(predind);
	muste_free(c_censored);
	muste_free(sensuroitu);
	muste_free(c_duration);
	muste_free(c_aika);
	muste_free(c_class);
	muste_free(x);
	for (row=0;row<apu;row++){
		muste_free(m_size[row]);   
	}
	muste_free(m_size);			
	muste_free(c_indeksi);

	if (puumalli==1){
		muste_free(Stat);
		muste_free(TerNod);
		muste_free(Tres);
		muste_free(Codes);
		muste_free(Pc_left);
		muste_free(Pc_survival);
		for (row=0; row<apu2; row++){
			muste_free(NIMET[row]);
		}
		muste_free(NIMET);
		for (row=0;row<apu2;row++){
			muste_free(m_taulukko[row]);
		}
		muste_free(m_taulukko);
		for (row=0;row<apu;row++){
			muste_free(m_events[row]);
			muste_free(m_censored[row]);
		}
		muste_free(m_events);
		muste_free(m_censored);
	}
	else{
		for (row=0;row<3;row++){
			muste_free(m_taulukko[row]);
		}
		muste_free(m_taulukko);
		if (lifetable==1){
			muste_free(c_failed);
			muste_free(c_n_enter);
			muste_free(c_lower);
			muste_free(c_upper);
			muste_free(c_eff_size);
			muste_free(c_cpf);
			muste_free(c_cps);
			muste_free(c_cpf_error);
			muste_free(c_survival);
			muste_free(c_failure);
			muste_free(c_surv_error);
			muste_free(c_surv_lcl);
			muste_free(c_surv_ucl);
			muste_free(c_pdf);
			muste_free(c_pdf_error);
			muste_free(c_hazard);
			muste_free(c_haz_error);
			muste_free(c_median);
			muste_free(c_med_error);
		}	
		else{
			muste_free(Pc_failed);
			muste_free(Pc_left);
			muste_free(Pnimittaja);
			muste_free(Pc_survival);
			muste_free(Pc_failure);
			muste_free(Pc_st_error);
			muste_free(Pc_lower_cl);
			muste_free(Pc_upper_cl);
		}
	}
	i=varfind2(&d,CLASS,0);
	if (i>=0){
		for (row=0;row<apu;row++){
			muste_free(m_events[row]);
			muste_free(m_censored[row]);
		}
		muste_free(m_events);
		muste_free(m_censored);
	}			
}

static int Lue_Data(){							// Lukee aineistosta aikamuuttujan, sensurointimuuttujan ja
									// mahdollisen luokittelumuuttujan matriisiin m_taulukko.
	int h,i,j,k,m;					// Puumallin tapauksessa lukee luokittelumuuttujiksi,
	long l;							// muuttuja, jotka on merkitty X:ll‰.
	char ch;

	suitable=0;
	eikay=0;

    j=varfind(&d,word[2]);			// etsit‰‰n aikamuuttujan indeksi
	if (j==-1)		
		return(-1);
	
	k=varfind(&d,word[3]);			// etsit‰‰n sensurointimuuttujan indeksi
	if (k==-1)		
		return(-1);

	if (puumalli==1){
		i=mask(&d); 
		if (i>=0){
			for (m=0; m<d.m_act; m++){		// etsit‰‰n puumallin selitt‰vien muuttujien indeksit
				ch=d.vartype[d.v[m]][1];
				if (ch=='X'){
					if (d.v[m]==j || d.v[m]==k){
						output_open(eout);
						sprintf(line,"Time-variable or censored-variable can not be explanatory variables!");
						print_line(line);WAIT;
						output_close(eout);
						return(-1);
					}
					strcpy(NIMET[predictors],d.varname[d.v[m]]);
					predind[predictors]=m;
					predictors++;
				}
			}
			if (predictors==0){
				output_open(eout);
				sprintf(line,"Mark the explanatory variables with X in the MASK-operation");
				print_line(line);WAIT;
				output_close(eout);
				return(-1);
			}
		}
	}
	else{ 		
		if (puumalli==0){
			h=varfind(&d,CLASS);			// etsit‰‰n mahdollisen luokittelumuuttujan indeksi
			if (h==-1)
				return(-1);
		}
		else
			h=-1;
	}
	
	for(l=d.l1; l<=d.l2; ++l){
		if (unsuitable(&d,l)){
			eikay++;				// lasketaan havainnot jotka rajattu pois
			continue;
		}
		data_load(&d,l,j,&m_taulukko[0][l-eikay]);
		if (m_taulukko[0][l-eikay]<0){
			output_open(eout);
			sprintf(line,"Negative values for time variable are not allowed!");
			print_line(line);WAIT;
			output_close(eout);
			return(-1);
		}
		data_load(&d,l,k,&m_taulukko[1][l-eikay]);			
		if (puumalli==1)
			for (m=0; m<predictors; m++)
				data_load(&d,l,d.v[predind[m]],&m_taulukko[m+2][l-eikay]);
		else if (h==-1)
				m_taulukko[2][l-eikay]=0.0;
			else
				data_load(&d,l,h,&m_taulukko[2][l-eikay]);						
	}
																															
suitable=d.n-eikay;
return 1;
}


static int Pilko(){							// Pilkkoo datan luokittelumuuttujan arvojen mukaan									// ja tekee estimaattorin kullekin luokalle erikseen.

	MaxAika=0.0;

	output_open(eout);	
	sprintf(line,"Survival analysis for %s",word[1]);
	print_line(line);
	
	if (lifetable==1){
		sprintf(line,"Method = Life Table");
		print_line(line);
	}
	else{
		sprintf(line,"Method = Product Limit");
		print_line(line);
	}

	sprintf(line,"\n");
	print_line(line);
	sprintf(line,"\n");
	print_line(line);
	output_close(eout);

	for (b=1; b<=suitable; b++){
		if (b==suitable || c_class[b+1]!=c_class[b]){
			if (strata==12){
				output_open(eout);
				sprintf(line,"There can only be up to 12 different classes");
				print_line(line);
				sprintf(line,"in the classificatory variable");
				print_line(line);WAIT;
				output_close(eout);
				return(-1);
			}
			koko++;
			m_size[0][strata]=koko-n;
			x[strata]=c_class[b];
			MaxAika=c_duration[koko];
			bestval=paras_arvo(MaxAika,MaxAika+2*sqrt(MaxAika));
			step=paras_arvo(bestval/5,bestval/9);
			if (lifetable==1){
				Laske();
				Tulosta();
			}
			else{
				Plaske();
				Ptulosta();
			}
			strata++;
			n=koko;
			}
			else
				koko++;
	}
return 0;
}


static int Laske(){									//laskee elinajantaulun sarakkeet

	int i,j;
	double summa2=0.0;	
	censor=0;

	f=(int)((MaxAika/interval)+1);

	for (i=0; i<aikavaleja; i++){
		c_censored[i]=0;
		c_failed[i]=0;
	}

	for (i=n+1; i<=koko; i++){
		if (sensuroitu[i]!=1){
			if (c_duration[i]==0.0){
				c_censored[0]++;
				censor++;
			}
			else{
				c_censored[(int)(c_duration[i]/interval)]++;
				censor++;
			}
		}
		else if (c_duration[i]==0.0) 
				c_failed[0]++;
			else
				c_failed[(int)(c_duration[i]/interval)]++;
	}	
  
	for (i=0; i<f; i++){
		if (i==0)
		  c_lower[i]=0;
		else
		  c_lower[i]=c_lower[i-1]+interval;
	}

	for (i=0; i<f; i++){
		if (i==0){
			c_upper[i]=interval;
			continue;
		}
		if (i==f-1){
			c_upper[i]=-1.0; 
			continue;
		}
		c_upper[i]=c_upper[i-1]+interval;
	}

	for (i=0; i<f; i++){
		if (i==0)
			c_n_enter[i]=koko-n;
		else
			c_n_enter[i]=n_enter(c_failed[i-1],c_censored[i-1],c_n_enter[i-1]);
	}
  
	for (i=0; i<f; i++){   
		c_eff_size[i]=eff_size(c_n_enter[i],c_censored[i]);
	}

	for (i=0; i<f; i++){
		c_cpf[i]=cpf(c_failed[i],c_eff_size[i]);
	}

	for (i=0; i<f; i++){
		c_cps[i]=cps(c_cpf[i]);	
	}

	for (i=0; i<f; i++){
		c_cpf_error[i]=cpf_error(c_cpf[i],c_eff_size[i]); 
	}

	for (i=0; i<f; i++){
		if (i==0) 
			c_survival[i]=1.0;
		else
			c_survival[i]=survival(c_cps[i-1],c_survival[i-1]);
	}

	for (i=0; i<f; i++){
		c_failure[i]=failure(c_survival[i]);
	}

	for (i=0; i<f; i++){
		if (i==0)
			c_surv_error[i]=0.0;
		else
			c_surv_error[i]=surv_error(c_survival[i],c_cpf[i-1],c_eff_size[i-1]);
	}

	for (i=0; i<f; i++){
		c_surv_lcl[i]=surv_lcl(c_survival[i],c_surv_error[i]);
		if (c_surv_lcl[i]<0.0)
			c_surv_lcl[i]=0.0;
	}

	for (i=0; i<f; i++){
		c_surv_ucl[i]=surv_ucl(c_survival[i],c_surv_error[i]);
		if (c_surv_ucl[i]>1.0)
			c_surv_ucl[i]=1.0;
	}

	for (i=0; i<f; i++){
		if (i==f-1)
			c_pdf[i]=-1.0; 
		else
			c_pdf[i]=pdf(c_survival[i],c_cpf[i]);
	}

	for (i=0; i<f; i++){
		if (c_pdf[i]<=0.0){
			c_pdf_error[i]=-1.0; 
			continue;
		}
		for (j=0; j<i; j++){
			summa2=summa2+apu_pdf_error(c_cps[j],c_eff_size[j]);
		}
		c_pdf_error[i]=pdf_error(c_pdf[i],c_cps[i],c_eff_size[i],summa2);
		summa2=0.0;
	}

	for (i=0; i<f; i++){
		if (i==f-1)
			c_hazard[i]=-1.0; 
		else
			c_hazard[i]=hazard(c_cpf[i]);
	}

	for (i=0; i<f; i++){
		if (c_hazard[i]<=0.0){
			c_haz_error[i]=0.0; 
			continue;
		}	
		c_haz_error[i]=haz_error(c_hazard[i],c_eff_size[i],c_cpf[i]);
	}	

	for (i=0; i<f; i++){
		if (i==f-1)
			c_median[i]=-1.0;
		else{
			for (j=0; j<f; j++){
				if ((c_survival[i]/2)>=c_survival[j]){
					c_median[i]=median(c_lower[i],c_lower[j-1],c_survival[i],
								c_survival[j-1],c_survival[j]);
				break;
				}
				else
					c_median[i]=-1.0;
			}
		}
	}
				
	for (i=0; i<f; i++){
		if (c_median[i]<=0.0)
			c_med_error[i]=-1.0;
		else{
			for (j=0; j<f; j++){
				if ((c_survival[i]/2)>=c_survival[j]){
					c_med_error[i]=med_error(c_survival[i],c_pdf[j-1],
									c_eff_size[i]);
				break;
				}
			}
		}
	}

return 0;
}


static int Tulosta(){									// Tulostaa kuvaruudulle elinajantaulun
	
	int i,liikaa=20;

	output_open(eout);
	i=Luo_uusi();
	if (i<0) return -1;
	Talletus();
	sprintf(line,"Class=%.0f  N=%d  Events=%d  Censored=%d  MaxDur=%.1f",c_class[b],koko-n,koko-n-censor,censor,MaxAika);
	print_line(line);
	sprintf(line, "\n");
	print_line(line);
	i=spfind("LIMIT");
	if (i>=0)
		liikaa=atoi(spb[i]);
	if(aikavaleja<=liikaa){
		sprintf(line,"Lower Upper Fail Cens Enter  Size   CPF  Survival   PDF   Hazard Median ");
		print_line(line);
		for (i=0; i<f; i++){
			sprintf(line,"%4.0f %5.0f %4d %4d %5d %6.1f %6.3f %7.3f %7.3f %6.3f %7.2f\n", 
			c_lower[i],c_upper[i],c_failed[i],c_censored[i],c_n_enter[i],
			c_eff_size[i],c_cpf[i],c_survival[i],c_pdf[i],c_hazard[i],
			c_median[i]);
			print_line(line);
		}
	}
	else{	
		if(strata==0){
			sprintf(line,"Because there is over %d time intervals, the lifetable will not be",liikaa); 
			print_line(line);		 
			sprintf(line,"printed on the screen, but only to Survo data file LTc%.0f!",c_class[b]);
			print_line(line);
			sprintf(line,"\n"); 
			print_line(line);
			sprintf(line,"You can reduce the number of intervals with specification,"); 
			print_line(line);
			sprintf(line,"INTERVAL=(wanted length of intervals)"); 
			print_line(line);
			sprintf(line,"If you want more intervals to be shown on the screen,"); 
			print_line(line);
			sprintf(line,"use specification LIMIT=(number of allowed intervals)"); 
			print_line(line);WAIT;
		}
	}
	sprintf(line, "\n");
	print_line(line);
	sprintf(line, "FILE SHOW LTc%.0f",c_class[b]);
	print_line(line);
	sprintf(line, "\n");
	print_line(line);
	sprintf(line, "......................................................................");
	print_line(line);
	sprintf(line, "HEADER=Survival_curve_for_class_%.0f",c_class[b]);
	print_line(line);
	sprintf(line, "GPLOT LTc%.0f,Lower,Surv  / OUTFILE=LTc%.0f",c_class[b],c_class[b]);
	print_line(line);
	sprintf(line, "LINE=3 YSCALE=0(0.2)1 XSCALE=0(%d)%d YLABEL=Survival XLABEL=Duration",(int)step,(int)bestval);
	print_line(line);
	sprintf(line, "......................................................................");
	print_line(line);
	sprintf(line, "HEADER=Survival_curve_for_class_%.0f",c_class[b]);
	print_line(line);
	sprintf(line, "GPLOT LTc%.0f,Lower,SurvLCL,SurvUCL  / INFILE=LTc%.0f",c_class[b],c_class[b]);
	print_line(line);
	sprintf(line, "LINE=[YELLOW],3 YSCALE=0(0.2)1 XSCALE=0(%d)%d YLABEL=Survival XLABEL=Duration",(int)step,(int)bestval);
	print_line(line);
	sprintf(line, "......................................................................");
	print_line(line);
	sprintf(line, "\n");
	print_line(line);
	sprintf(line, "......................................................................");
	print_line(line);
	sprintf(line, "HEADER=Hazard_curve_for_class_%.0f",c_class[b]);
	print_line(line);
	sprintf(line, "GPLOT LTc%.0f,Lower,Hazard",c_class[b]);
	print_line(line);
	sprintf(line, "LINE=3 XSCALE=0(%d)%d YLABEL=Hazard XLABEL=Duration",(int)step,(int)bestval);
	print_line(line);
	sprintf(line, "......................................................................");
	print_line(line);
	sprintf(line, "\n");
	print_line(line);
	sprintf(line, "\n");
	print_line(line);
	output_close(eout);
	return 0;
}


static int Luo_uusi(){								// Luo uuden Survo-tiedoston elinajantaululle
        
        int i;
        int Plen,Pm1,Pm,Pf,Pextra,Ptextn,Ptextlen;
        char **Ptext;
        char *privi[1];
        
        Pm=20;
        vartype=muste_malloc(Pm*9);
        if (vartype==NULL) { not_enough_memory(); return(-1); }
        pvartype=(char **)muste_malloc(Pm*sizeof(char *));
        if (pvartype==NULL) { not_enough_memory(); return(-1); }
        varlen=(int *)muste_malloc(Pm*sizeof(int));
        if (varlen==NULL) { not_enough_memory(); return(-1); }
        varname=(char **)muste_malloc(Pm*sizeof(char *));
        if (varname==NULL) { not_enough_memory(); return(-1); }
        vartila=muste_malloc(Pm*9);
        if (vartila==NULL) { not_enough_memory(); return(-1); }
		
        for (i=0; i<Pm; ++i) pvartype[i]=vartype+i*9;
		for (i=0; i<Pm; ++i) varname[i]=vartila+i*9;
		varname[0]="Lower";    varlen[0]=8;  pvartype[0]="8A";
		varname[1]="Upper";    varlen[1]=8;  pvartype[1]="8A";
		varname[2]="Events";   varlen[2]=8;  pvartype[2]="8A";
		varname[3]="Cens";     varlen[3]=8;  pvartype[3]="8A";
		varname[4]="Enter";    varlen[4]=8;  pvartype[4]="8A"; 
		varname[5]="Size";     varlen[5]=8;  pvartype[5]="8A";
		varname[6]="CPF";      varlen[6]=8;  pvartype[6]="8A";
		varname[7]="CPS";      varlen[7]=8;  pvartype[7]="8A";
		varname[8]="CPFse";    varlen[8]=8;  pvartype[8]="8A";
  		varname[9]="Surv";     varlen[9]=8;  pvartype[9]="8A";
		varname[10]="Fail";    varlen[10]=8; pvartype[10]="8A";
		varname[11]="Survse";  varlen[11]=8; pvartype[11]="8A";
		varname[12]="SurvLCL"; varlen[12]=8; pvartype[12]="8A";
		varname[13]="SurvUCL"; varlen[13]=8; pvartype[13]="8A"; 
		varname[14]="PDF";     varlen[14]=8; pvartype[14]="8A";
		varname[15]="PDFse";   varlen[15]=8; pvartype[15]="8A";
		varname[16]="Hazard";  varlen[16]=8; pvartype[16]="8A";
		varname[17]="Hazse";   varlen[17]=8; pvartype[17]="8A";
		varname[18]="Median";  varlen[18]=8; pvartype[18]="8A";
		varname[19]="Medse";   varlen[19]=8; pvartype[19]="8A";
		Plen=0;
        for (i=0; i<Pm; ++i) Plen+=varlen[i];
        Plen=Plen+10;
        Pm1=Pm+4;
        Pf=64;
        Pextra=12;
        Ptextn=0;
        Ptextlen=0;
        Ptext=privi;
        sprintf(nimi,"%sLTc%.0f.SVO",edisk,c_class[b]);
		sur_delete(nimi);
		sprintf(nimi,"LTc%.0f",c_class[b]);
		i=fi_create(nimi,Plen,Pm1,Pm,f,Pf,Pextra,Ptextn,Ptextlen,
                    Ptext,varname,varlen,pvartype);
        if (i<0) return(-1);
        muste_free(vartype); muste_free(pvartype); muste_free(varlen); muste_free(varname); muste_free(vartila);
return(1);
}


static int Talletus(){								// Tallettaa elinajantaulun sarakkeet
											// uuteen tiedostoon
long l;
	
	data_open(nimi,&e);
	
	for (l=e.l1; l<=e.l2; l++){
		data_save(&e,l,e.v[0],c_lower[l-1]);
		data_save(&e,l,e.v[1],c_upper[l-1]);
		data_save(&e,l,e.v[2],(double)c_failed[l-1]);
		data_save(&e,l,e.v[3],(double)c_censored[l-1]);
		data_save(&e,l,e.v[4],(double)c_n_enter[l-1]);
		data_save(&e,l,e.v[5],c_eff_size[l-1]);
		data_save(&e,l,e.v[6],c_cpf[l-1]);
		data_save(&e,l,e.v[7],c_cps[l-1]);
		data_save(&e,l,e.v[8],c_cpf_error[l-1]);
		data_save(&e,l,e.v[9],c_survival[l-1]);
		data_save(&e,l,e.v[10],c_failure[l-1]);
		data_save(&e,l,e.v[11],c_surv_error[l-1]);
		data_save(&e,l,e.v[12],c_surv_lcl[l-1]);
		data_save(&e,l,e.v[13],c_surv_ucl[l-1]);
		data_save(&e,l,e.v[14],c_pdf[l-1]);
		data_save(&e,l,e.v[15],c_pdf_error[l-1]);
		data_save(&e,l,e.v[16],c_hazard[l-1]);
		data_save(&e,l,e.v[17],c_haz_error[l-1]);
		data_save(&e,l,e.v[18],c_median[l-1]);
		data_save(&e,l,e.v[19],c_med_error[l-1]);	
		}
    data_close(&e);
return(1);
}

//---------------------------------------------------------------------------

static int Plaske(){								// Laskee tulorajaestimaattorin sarakkeet

	int i,laskuri=0,mem_left=koko-n,mem_left2=koko-n,
	mem_failed=0,counter=0;

	censor=0;
	keski=0.0;

	for (i=1; i<=suitable; i++){
		c_censored[i]=sensuroitu[i];
		c_aika[i]=c_duration[i];
	}

	c_aika[n]=0.0;
	c_censored[n]=0;

	for (i=n+1; i<=koko; i++){
		if (c_censored[i]!=1.0)
			censor++;
	}

	for (i=n; i<=koko; i++){
		if (i==n)
			Pc_failed[i]=0;
		else
			Pc_failed[i]=failed(c_censored[i],Pc_failed[i-1]);		
	}

	for (i=n; i<=koko; i++){
		Pc_left[i]=koko-i;
	}

	for (i=n; i<=koko; i++){
		if (i==n){
		    Pc_survival[i]=1.0;
			continue;
		}
		if (c_censored[i]!=1){
			Pc_survival[i]=Pc_survival[i-1];
			laskuri++;
		}
		else if (i!=koko && c_aika[i+1]==c_aika[i] && c_censored[i+1]==1)
				Pc_survival[i]=Pc_survival[i-1];
			else{
				Pc_survival[i]=Psurvival(Pc_survival[i-1],Pc_left[i],mem_left,laskuri);
				mem_left=Pc_left[i];
				laskuri=0;
			}
	}
  
	for (i=n; i<=koko; i++) {   
		Pc_failure[i]=Pfailure(Pc_survival[i]);
	}

	for (i=n; i<=koko; i++){
		if (i==n || i==koko){
		    Pc_st_error[i]=0.0;
			continue;
		}
		if (c_censored[i]!=1){
			Pc_st_error[i]=Pc_st_error[i-1];
			counter++;
		}
		else if (c_aika[i+1]==c_aika[i] && c_censored[i+1]==1)
				Pc_st_error[i]=Pc_st_error[i-1];
			else{
				Pc_st_error[i]=st_error(Pc_survival[i],Pc_failed[i]-mem_failed,mem_left2-counter);
				mem_left2=Pc_left[i];
				mem_failed=Pc_failed[i];		
				counter=0;
			}
	}

	for (i=n; i<=koko; i++){
		Pc_lower_cl[i]=lower_cl(Pc_survival[i],Pc_st_error[i]);
		if (Pc_lower_cl[i]<0.0)
			Pc_lower_cl[i]=0.0;
	}

	for (i=n; i<=koko; i++){
		Pc_upper_cl[i]=upper_cl(Pc_survival[i],Pc_st_error[i]);
		if (Pc_upper_cl[i]>1.0)
			Pc_upper_cl[i]=1.0;
	}

	for (i=n+1; i<=koko; i++){
		keski=keski+Pc_survival[i-1]*(c_aika[i]-c_aika[i-1]);
	}
	
return 0;
}


static int Ptulosta(){								// Tulostaa kuvaruudulle tulorajaestimaattorin
	
	int i;

	output_open(eout);
	
	i=Pluo_uusi();
	if (i<0) return -1;
	Ptalletus();
	sprintf(line,"Class=%.0f  N=%d  Events=%d  Censored=%d  MaxDur=%.1f  Mean=%.2f",c_class[b],koko-n,koko-n-censor,censor,c_duration[koko],keski);
	print_line(line);
	sprintf(line, "\n");
	print_line(line);
	sprintf(line, "......................................................................");
	print_line(line);
	sprintf(line, "HEADER=Survival_curve_for_class_%.0f",c_class[b]);
	print_line(line);
	sprintf(line, "GPLOT PLc%.0f,Dur,Surv  /  INFILE=PL  OUTFILE=PL",c_class[b]);
	print_line(line);
	sprintf(line, "LINE=3,1,%.0f YSCALE=0(0.2)1 XSCALE=0(%d)%d YLABEL=Survival XLABEL=Duration",c_class[b],(int)step,(int)bestval);
	print_line(line);
	sprintf(line, "......................................................................");
	print_line(line);
	sprintf(line, "\n");
	print_line(line);
	sprintf(line, "FILE SHOW PLc%.0f",c_class[b]);
	print_line(line);
	sprintf(line, "\n");
	print_line(line);
	output_close(eout);

return 0;
}


static int Pluo_uusi(){						// Luo uuden Survo-tiedoston tulorajaestimaattorille       
        
		int i;
        int Plen,Pm1,Pm,Pf,Pextra,Ptextn,Ptextlen;
        char **Ptext;
        char *privi[1];
        
        Pm=9;
        vartype=muste_malloc(Pm*9);
        if (vartype==NULL) { not_enough_memory(); return(-1); }
        pvartype=(char **)muste_malloc(Pm*sizeof(char *));
        if (pvartype==NULL) { not_enough_memory(); return(-1); }
        varlen=(int *)muste_malloc(Pm*sizeof(int));
        if (varlen==NULL) { not_enough_memory(); return(-1); }
        varname=(char **)muste_malloc(Pm*sizeof(char *));
        if (varname==NULL) { not_enough_memory(); return(-1); }
        vartila=muste_malloc(Pm*9);
        if (vartila==NULL) { not_enough_memory(); return(-1); }
		
        for (i=0; i<Pm; ++i) pvartype[i]=vartype+i*9;
		for (i=0; i<Pm; ++i) varname[i]=vartila+i*9;
		varname[0]="Dur";  varlen[0]=8; pvartype[0]="8A";
		varname[1]="Cens"; varlen[1]=1; pvartype[1]="1A";
		varname[2]="Surv"; varlen[2]=8; pvartype[2]="8A";
		varname[3]="Fail"; varlen[3]=8; pvartype[3]="8A";
		varname[4]="s.e";  varlen[4]=8; pvartype[4]="8A"; 
		varname[5]="lcl";  varlen[5]=8; pvartype[5]="8A";
		varname[6]="ucl";  varlen[6]=8; pvartype[6]="8A";
		varname[7]="Even"; varlen[7]=8; pvartype[7]="8A";
		varname[8]="Left"; varlen[8]=8; pvartype[8]="8A";
  		Plen=0;
        for (i=0; i<Pm; ++i) Plen+=varlen[i];
        Plen=Plen+10;
        Pm1=Pm+4;
        Pf=64;
        Pextra=12;
        Ptextn=0;
        Ptextlen=0;
        Ptext=privi;
        sprintf(nimi,"%sPLc%.0f.SVO",edisk,c_class[b]);
		sur_delete(nimi);
		sprintf(nimi,"PLc%.0f",c_class[b]);
		i=fi_create(nimi,Plen,Pm1,Pm,koko-n+1,Pf,Pextra,Ptextn,Ptextlen,
                    Ptext,varname,varlen,pvartype);
        if (i<0) return(-1);
        muste_free(vartype); muste_free(pvartype); muste_free(varlen); muste_free(varname); muste_free(vartila);

return(1);
}


static int Ptalletus(){							// Tallettaa tulorajaestimaattorin sarakkeet 
											// uuteen Survo-tiedostoon								
	long l;
	
	data_open(nimi,&e);
	data_save(&e,1,e.v[0],c_aika[n]);
	data_save(&e,1,e.v[1],(double)c_censored[n]);
	data_save(&e,1,e.v[2],Pc_survival[n]);
	data_save(&e,1,e.v[3],Pc_failure[n]);
	data_save(&e,1,e.v[4],Pc_st_error[n]);
	data_save(&e,1,e.v[5],Pc_lower_cl[n]);
	data_save(&e,1,e.v[6],Pc_upper_cl[n]);
	data_save(&e,1,e.v[7],(double)Pc_failed[n]);
	data_save(&e,1,e.v[8],(double)Pc_left[n]);
	for (l=e.l1+1; l<=e.l2; l++){
		data_save(&e,l,e.v[0],c_aika[l+n-1]);
		data_save(&e,l,e.v[1],(double)c_censored[l+n-1]);
		data_save(&e,l,e.v[2],Pc_survival[l+n-1]);
		data_save(&e,l,e.v[3],Pc_failure[l+n-1]);
		data_save(&e,l,e.v[4],Pc_st_error[l+n-1]);
		data_save(&e,l,e.v[5],Pc_lower_cl[l+n-1]);
		data_save(&e,l,e.v[6],Pc_upper_cl[l+n-1]);
		data_save(&e,l,e.v[7],(double)Pc_failed[l+n-1]);
		data_save(&e,l,e.v[8],(double)Pc_left[l+n-1]);
	}
    data_close(&e);

return(1);
}

//------------------------------------------------------------------------------------------

static int Muodosta(){							// Muodostaa testisuureiden laskemiseen tarvittavat
										// matriisit. 
	int i,j;
	p=0;

	for (i=0; i<suitable; i++){
		for (j=0; j<strata; j++){
			m_events[i][j]=0;
			m_censored[i][j]=0;
			m_size[i+1][j]=0;
		}
	}

	for (i=1; i<=suitable; i++){
		if (sensuroitu[i]!=1){
			for (j=0; j<strata; j++){
				if (c_class[i]==x[j])
					m_censored[p][j]++;
			}
		}
		else{
			if (i==1){
				for (j=0; j<strata; j++){
					if (c_class[i]==x[j])
						m_events[p][j]++;
				}
				continue;
			}
			if (c_duration[i]==c_duration[i-1]){
				for (j=0; j<strata; j++){
					if (c_class[i]==x[j])
						m_events[p][j]++;
				}
			}
			if (c_duration[i]>c_duration[i-1]){
				p++;
				for (j=0; j<strata; j++){
					m_size[p][j]=m_size[p-1][j]-m_events[p-1][j]-m_censored[p-1][j];
				}
				for (j=0; j<strata; j++){
					if (c_class[i]==x[j])
						m_events[p][j]++;
				}
			}
		}
	}
	
return 0;
}


static long double Testi(int tunnus){					// Laskee eloonj‰‰mifunktioiden eroja 
												// mittaavat testisuureet.
	int i,j,k,l,row;
	long double sum=0.0,kerroin=0.0,suurin=0.0,eps=1e-16,tol=(1e-300)/eps;

	tapahtumat=(long double *)muste_malloc((p+1)*sizeof(long double));
	riskiryh=(long double *)muste_malloc((p+1)*sizeof(long double));
	sensuurit=(long double *)muste_malloc((p+1)*sizeof(long double));
	paino=(long double *)muste_malloc((p+1)*sizeof(long double));
	vector=(long double *)muste_malloc((strata)*sizeof(long double));
	mV=muste_malloc((strata)*sizeof(long double *));
	for (row=0;row<strata;row++){
		mV[row]=muste_malloc((strata)*sizeof(long double));
	}
	mV2=(long double *)muste_malloc((strata*strata)*sizeof(long double));
	mD=(long double *)muste_malloc((strata)*sizeof(long double));
	mU=(long double *)muste_malloc((strata*strata)*sizeof(long double));
	mU2 =(long double *)muste_malloc((strata*strata)*sizeof(long double));
	apumat=(long double *)muste_malloc((strata*strata)*sizeof(long double));
	
	
	for (i=0; i<=p; i++){
		for (j=0; j<strata; j++){
			sum=sum+m_events[i][j];
		}
		tapahtumat[i]=sum;
		sum=0.0;
	}

	for (i=0; i<=p; i++){
		for (j=0; j<strata; j++){
			sum=sum+m_size[i][j];
		}
		riskiryh[i]=sum;
		sum=0.0;
	}

	for (i=0; i<=p; i++){
		sensuurit[i]=(riskiryh[i]-(long double)tapahtumat[i]);
	}

	for (i=0; i<=p; i++){
		if (tunnus==1)
			paino[i]=1;
		if (tunnus==2)
			paino[i]=riskiryh[i];
		if (tunnus==3)
			paino[i]=sqrt(riskiryh[i]);
	}

	for (j=0; j<strata; j++){
		for (i=0; i<=p; i++){
			sum=sum+paino[i]*(m_events[i][j]-m_size[i][j]*tapahtumat[i]/riskiryh[i]); 
		}
		vector[j]=sum;
		sum=0.0;
	}

	for (j=0; j<strata; j++){
		for (l=0; l<strata; l++){
			if (l==j)
				kerroin=1.0;
			for (i=0; i<=p; i++){
				if (riskiryh[i]<=1)
					continue;
				sum=sum+paino[i]*paino[i]*tapahtumat[i]*sensuurit[i]
					*(riskiryh[i]*m_size[i][l]*kerroin-m_size[i][j]*m_size[i][l])
					/(riskiryh[i]*riskiryh[i]*(riskiryh[i]-1));
			}
			mV[j][l]=sum;
			sum=0.0;
			kerroin=0;
		}
	}

	for (j=0; j<strata; j++){
		for (l=0; l<strata; l++){
			mV2[j+strata*l]=mV[j][l];
			if (mV[j][l]>suurin)
				suurin=mV[j][l];
		}
	}
	
	for (j=0; j<strata*strata; j++){
		mV2[j]=mV2[j]/suurin;
	}

	mat_svd(mV2,mD,mU,strata,strata,eps,tol);

	for (i=0; i<strata; i++){
		mD[i]=mD[i]*suurin;
	}

	for (i=0; i<strata; i++){
		if (mD[i]<0.00000001)
			mD[i]=0;
		else
			mD[i]=1/mD[i];
	}		

	mat_transp(mU2,mU,strata,strata);
	
	for (l=0; l<strata; l++){
		for (j=0; j<strata; j++){
			apumat[j+strata*l]=mD[l]*mV2[j+strata*l];
		}
	}

	for (l=0; l<strata; l++){
		for (j=0; j<strata; j++){
			for (k=0; k<strata; k++){
				sum=sum+apumat[j+strata*k]*mU2[k+strata*l];
			}
			mV2[j+strata*l]=sum;
			sum=0.0;
		}
	}

	for (l=0; l<strata; l++){
		for (j=0; j<strata; j++){
			sum=sum+vector[j]*mV2[j+strata*l];
		}
		apumat[l]=sum;
		sum=0.0;
	}

	for (l=0; l<strata; l++){
		sum=sum+apumat[l]*vector[l];
	}

	muste_free(tapahtumat),muste_free(riskiryh),muste_free(sensuurit),muste_free(paino),muste_free(mV2),muste_free(mD),muste_free(mU),muste_free(vector),muste_free(apumat),muste_free(mU2); 
	for (row=0;row<strata;row++){
		muste_free(mV[row]);
	}
	muste_free(mV);

return sum;
}


static int Tulos(){								// Tulostaa testisuureiden arvot
	
	extern double muste_cdf_chi2();
	double test=0.0,df=0.0,parvo=0.0,error=1e-15;

	output_open(eout);

	df=(strata-1)*(strata-1);
	sprintf(line, "   Test      Chi-Square   DF   Probability");
	print_line(line);
	test=Testi(1);
	parvo=1-muste_cdf_chi2(test,df,error);
	sprintf(line, "Log-Rank:      %.4f      %.0f      %.4f",test,df,parvo);
	print_line(line);
	test=Testi(2);
	parvo=1-muste_cdf_chi2(test,df,error);
	sprintf(line, "Wilcoxon:      %.4f      %.0f      %.4f",test,df,parvo);
	print_line(line); 
	test=Testi(3);
	parvo=1-muste_cdf_chi2(test,df,error);
	sprintf(line, "Tarone-Ware:   %.4f      %.0f      %.4f",test,df,parvo);
	print_line(line);

	output_close(eout);
	
return 0;	
}

//---------------------------------------------------------------------------

static int add(tnode *node){				// Funktio, joka etsii solmun parhaan katkaisupisteen,
									// tallettaa sit‰ vastaavat tiedot solmuun ja
									// luo puulle uudet haarat ja kutsuu j‰lleen itse‰‰n.
  int i,j;
  tnode *newn;

	if (node->datasize - node->censsize<=1)		
	  return 0;				// Jos solmussa on vain yksi tapahtuma,	ei sit‰ voida en‰‰ jakaa
  
	if (node->datasize>=MinObs*2){
		for (j=1; j<=node->datasize; ++j){
			c_duration[j]=m_taulukko[0][node->data[j]];	// Alustaa uuden solmun havainnoille
			if (m_taulukko[1][node->data[j]]!=1.0)		// laskuissa k‰ytett‰v‰t sarakkeet	
				sensuroitu[j]=0;
			else
				sensuroitu[j]=1;
			c_indeksi[j]=node->data[j];
		}

		for (i=0; i<predictors; i++){			// K‰yd‰‰n kaikki selitt‰v‰t muuttujat l‰pi						 
			for (j=1; j<=node->datasize; ++j){	
				c_class[j]=m_taulukko[i+2][node->data[j]];	
			}
			qsort_mtaulukko(1, node->datasize, node);	// Ryhmitell‰‰n aina kunkin selitt‰v‰n mukaan
			splitpoint=c_class[1];						// Alustetaan ensimm‰inen katkaisupiste 
			while (splitpoint<c_class[node->datasize]){ // K‰yd‰‰n l‰pi jokainen mahdollinen katkaisupiste
				Pilko2(i+2,node);			// J‰rjestet‰‰n ajan mukaan, tehd‰‰n testit ja talletetaan jos suurin testisuure					
				qsort_mtaulukko(1, node->datasize, node);	// Ryhmitell‰‰n j‰lleen luokittelumuuttujan mukaan,
			}												// jotta voidaan etsi‰ seuraava katkaisupiste
		}
	}
	if (node->Score==-100000.0)				
		return 0;
	
	node->Prune=node->Score;
					
	for (j=1; j<=node->datasize; ++j){
			c_class[j]=m_taulukko[parasmuut][node->data[j]];
	}
	qsort_mtaulukko(1, node->datasize, node); // sortataan parhaan katkaisumuuttujan mukaan,
											  // jotta aineisto voidaan jakaa

	/* Alustetaan solmun vasemmassa haarassa oleva uusi solmu*/
	newn = (tnode *)muste_malloc(sizeof(tnode));
	newn->parent = node;
	node->lchild = newn;
	newn->data=(long int *)muste_malloc((node->paraskoko+1)*sizeof(long int));
	if((newn->data)==NULL){not_enough_memory();WAIT;}
	for (i = 1; i <= node->paraskoko; i++)
	  newn->data[i]=c_indeksi[i];
	newn->datasize = node->paraskoko;
	newn->censsize = node->parascens;
	newn->paraskoko=0;
	newn->Score=TYHJA2;
	newn->Prune=TYHJA2;
	newn->solmuja=1;
	newn->Code=-1;
	newn->lchild = NULL;
	newn->rchild = NULL;
	add(newn);
	
	node->Prune=node->Prune+treesum;
	treesum=0.0;
	node->solmuja=node->solmuja+solmuja;
	solmuja=0;

	/* Alustetaan solmun oikeassa haarassa oleva uusi solmu*/
	newn = (tnode *)muste_malloc(sizeof(tnode));
	newn->parent = node;
	node->rchild = newn;
	newn->data=(long int *)muste_malloc((node->datasize - node->paraskoko+1)*sizeof(long int));
	if((newn->data)==NULL){not_enough_memory();WAIT;}
	for (i = node->paraskoko + 1; i <= node->datasize; i++)
		newn->data[i-node->paraskoko]=c_indeksi[i];
	newn->datasize = node->datasize - node->paraskoko;
    newn->censsize = node->censsize - node->parascens;
	newn->paraskoko=0;
	newn->Score=TYHJA2;
	newn->Prune=TYHJA2;
	newn->solmuja=1;
	newn->Code=-1;
	newn->lchild = NULL;
	newn->rchild = NULL;
	add(newn);
	
	node->Prune=node->Prune+treesum;
	node->solmuja=node->solmuja+solmuja;
	
	treesum=node->Prune;			// T‰h‰n asti node->Prune on osapuun testisuureiden summa

	node->Prune=node->Prune - fixed*node->solmuja; // T‰st‰ eteenp‰in node->Prune on osapuun
													// karsimisalgoritmin arvojen maksimi
	solmu++;
	node->Code=solmu;
	
	if (node->Prune<Tres[1]){
		Tres[1]=node->Prune;	
		Codes[1]=node->Code;
	}

	solmuja=node->solmuja;

return 0;
}


static int pruning(tnode *node){				

	int i;
	
	node->solmuja=1;
	solmuja=0;
	treesum=0.0;
  		
	if (node->Code==-1)
		return 0;
	for (i=1; i<treshold; i++){
		if (node->Code==Codes[i])
			return 0;
	}
 
	node->Prune=node->Score;
	pruning(node->lchild);	
	
	node->Prune=node->Prune+treesum;
	treesum=0.0;
	node->solmuja=node->solmuja+solmuja;
	solmuja=0;
	
	pruning(node->rchild);

	node->Prune=node->Prune+treesum;
	treesum=node->Prune;
	node->solmuja=node->solmuja+solmuja;						
	node->Prune=treesum - fixed*node->solmuja; 
			
	if (node->Prune<Tres[treshold]){
		Tres[treshold]=node->Prune;
		Codes[treshold]=node->Code;
	}

	solmuja=node->solmuja;
	solmu++;

return 0;
}


static int Pilko2(int muut,tnode *node){	// Pilkkoo solmun havainnot annetun muuttujan annetun 
									// katkaisupisteen kohdalta, laskee testisuureen ja 
	koko=0,strata=2,cens=0;			// tallettaa suurinta testisuuretta vastaavat tiedot

	for (b=1; b<node->datasize; b++){
		if (sensuroitu[b]!=1)
			cens++;
		if (c_class[b+1]>splitpoint){		
			koko++;
			splitpoint=c_class[b+1];
			if ((koko>=MinObs) && (node->datasize - koko >=MinObs)){//P‰‰tesolmut jossa v‰hemm‰n
				qsort_aika(1,node->datasize,node);		//kuin MinObs havaintoa eiv‰t mahdollisia
				Muodosta2(node);
				Testisuure=Testi(nro);
				if (Testisuure>node->Score){
					node->Score=Testisuure;
					strcpy(node->Var,NIMET[muut-2]);
					node->Split=splitpoint;
					node->paraskoko=koko;
					node->parascens=cens;
					parasmuut=muut;
				}
			}
			break;
		}
		koko++;
	}
return 0;
}


static int Muodosta2(tnode *node){				// Muodostaa testisuureen laskemiseen 
										// tarvittavat matriisit (puumalli) 
	int i,j;

	for (i=0; i<node->datasize; i++){
		for (j=0; j<strata; j++){
			m_events[i][j]=0;
			m_censored[i][j]=0;
			m_size[i][j]=0;
		}
	}
	
	m_size[0][0]=koko;
	m_size[0][1]=node->datasize-koko;
	p=0;

	for (i=1; i<=node->datasize; i++){
		if (sensuroitu[i]!=1){
			if (c_class[i]<splitpoint)
				m_censored[p][0]++;
			else
				m_censored[p][1]++;
		}
		else{
			if (i==1){
				if (c_class[i]<splitpoint)
					m_events[p][0]++;
				else 
					m_events[p][1]++;
				continue;
			}
			if (c_duration[i]==c_duration[i-1]){
				if (c_class[i]<splitpoint)
					m_events[p][0]++;
				else 
					m_events[p][1]++;
			}
			if (c_duration[i]>c_duration[i-1]){
				p++;
				m_size[p][0]=m_size[p-1][0]-m_events[p-1][0]-m_censored[p-1][0];
				m_size[p][1]=m_size[p-1][1]-m_events[p-1][1]-m_censored[p-1][1];
				if (c_class[i]<splitpoint)
					m_events[p][0]++;
				else 
					m_events[p][1]++;
			}
		}
	}
	
return 0;
}


static int intra(){							// Tulostaa solmujen Split Statistic -arvot kuvaruudulle

	int i,help;

	output_open(eout);

	
	sprintf(line,"Terminal nodes   Split statistic");
	print_line(line);
	sprintf(line,"\n");
	print_line(line);
	
	help=0;
	if (treshold>16)
		help=treshold-16;


	for (i=treshold-2; i>=help; --i){		
		sprintf(line,"      %2d             %6.2f",TerNod[i],Stat[i]);
		print_line(line);
	}
	
	sprintf(line,"\n");
	print_line(line);
	sprintf(line,".............................................................................");
	print_line(line);
	sprintf(line,"You can decide the size of the tree with specification:                            ");
	print_line(line);
	sprintf(line,"            NODES=(number of the wanted terminal nodes)                          ");
	print_line(line);
	sprintf(line,"It is recommended to pick up a tree, which ""Split statistic"" -value          ");
	print_line(line);
	sprintf(line,"is relatively high and number of terminal nodes is quite small.          ");
	print_line(line);
	sprintf(line,"Trees, which value is below 0, don't include any important structure.");
	print_line(line);
	sprintf(line,"Also trees with over 10 terminal nodes are hardly useful!");
	print_line(line);

	output_close(eout);	
	
return 0;
}


static void prune(tnode *node){					// Karsii puun halutun kokoiseksi. K‰yt‰nnˆss‰ tulostaa
									// halutun kokoisen puun (eik‰ karsi puuta).
	int i,j,terminaali;
	
	terminaali=0;

	if (node->Code==-1) terminaali=1;
	for (i=1; i<=sub; i++){
		if (node->Code==Codes[i])
			terminaali=1;
	}

	if (terminaali<1){
		numero++;
		node->Num=numero;
		Tulos2(node);
		
		memnum=numero;
		merkkis='<';
		sprintf(variable,"%s",node->Var);
		katkaisu=node->Split;
		prune(node->lchild);
		
		memnum=node->Num;
		merkkis='>';
		sprintf(variable,"%s",node->Var);
		katkaisu=node->Split;
		prune(node->rchild);
	}
	else{		
		numero++;
		node->Num=numero;
		terminal++;
		for (j=1; j<=node->datasize; ++j){
			c_duration[j]=m_taulukko[0][node->data[j]];
			if (m_taulukko[1][node->data[j]]!=1.0)
				sensuroitu[j]=0;
			else
				sensuroitu[j]=1;
			c_indeksi[j]=node->data[j];
		}
		qsort_aika(1,node->datasize,node);
		Kurvit(node);				
	}
}


static int Kurvit(tnode *node){				// Luo tulorajaestimaattorin kullekin puun p‰‰tesolmulle,
										// tulostaa p‰‰tesolmun tiedot ja GPLOT-kaavion ruudulle 
	int i,laskuri=0,mem_left=node->datasize;
	ylaK=0.0;
	medi=0.0;
	alaK=0.0;
	keski=0.0;

	for (i=1; i<=node->datasize; i++){
		c_censored[i]=sensuroitu[i];
		c_aika[i]=c_duration[i];
	}

	c_aika[0]=0.0;
	c_censored[0]=0;

	for (i=0; i<=node->datasize; i++){
		Pc_left[i]=node->datasize-i;
	}
	
	for (i=0; i<=node->datasize; i++){
		if (i==0){
		    Pc_survival[i]=1.0;
			continue;
		}
	    if (c_censored[i]!=1){
			Pc_survival[i]=Pc_survival[i-1];
			laskuri++;
		}
		else if (i!=node->datasize && c_aika[i+1]==c_aika[i] && c_censored[i+1]==1)
				Pc_survival[i]=Pc_survival[i-1];
			else{
				Pc_survival[i]=Psurvival(Pc_survival[i-1],Pc_left[i],mem_left,laskuri);
				mem_left=Pc_left[i];
				laskuri=0;
			}
	}
	
	for (i=1; i<=suitable; i++){
		if (m_taulukko[0][i]>MaxAika)
			MaxAika=m_taulukko[0][i];
	}
	
	for (i=1; i<=node->datasize; i++){
		if (Pc_survival[i]<0.75 && ylaK==0.0)
			ylaK=c_aika[i];
		if (Pc_survival[i]<0.5 && medi==0.0)
			medi=c_aika[i];
		if (Pc_survival[i]<0.25 && alaK==0.0)
			alaK=c_aika[i];
		keski=keski+Pc_survival[i-1]*(c_aika[i]-c_aika[i-1]);
	}
	
	Tulos3(node);
	
	bestval=paras_arvo(MaxAika,MaxAika+2*sqrt(MaxAika));
	step=paras_arvo(bestval/5,bestval/9);
	
	output_open(eout);
	
	i=uusiT(node);
	if (i<0) return -1;
	talletaT();
	sprintf(line, "\n");
	print_line(line);
	sprintf(line,"TERMINAL %d  N=%d   MaxDur=%.1f",terminal,node->datasize,c_duration[node->datasize]);
	print_line(line);
	sprintf(line, "......................................................................");
	print_line(line);
	sprintf(line, "HEADER=Survival_curves_for_terminal_nodes");
	print_line(line);
	sprintf(line, "GPLOT T%d,Dur,Surv  / INFILE=A OUTFILE=A ",terminal);
	print_line(line);
	sprintf(line, "LINE=3,1,%d YSCALE=0(0.2)1 XSCALE=0(%d)%d YLABEL=Survival XLABEL=Duration",terminal,(int)step,(int)bestval);
	print_line(line);
	sprintf(line, "......................................................................");
	print_line(line);

	output_close(eout);
 
return 0;
}

static int Tulos2(tnode *node){				// Tulostaa kuvaruudulle puun jaetun solmun tiedot
	
	extern double muste_cdf_chi2();
	double parvo=0.0,error=1e-15;
	char yhtasuuri=' ';

	if (merkkis=='>') yhtasuuri='=';
	
	output_open(eout);

	if (numero==1){
		sprintf(line, "TREE-model for data %s:",word[1]);
		print_line(line);
	}
	sprintf(line, "\n");
	print_line(line);
	if (memnum==0){
		sprintf(line, "                      Node  %2d",numero);
		print_line(line);
	}
	else{
		sprintf(line, "%2d  (%.4s%c%c%.4f)    Node  %2d",memnum,variable,merkkis,yhtasuuri,katkaisu,numero);
		print_line(line);
	}
	sprintf(line, "                      Events         = %d",node->datasize - node->censsize);
	print_line(line);
	sprintf(line, "                      Censored       = %d",node->censsize);
	print_line(line);
	sprintf(line, "                      Variable       = %s",node->Var);
	print_line(line);
	sprintf(line, "                      Splitpoint     = %.4f",node->Split);
	print_line(line);
	sprintf(line, "                      Testscore      = %.4f",node->Score);
	print_line(line); 
	parvo=1-muste_cdf_chi2(node->Score,1.0,error);
	sprintf(line, "                      P-value        = %.4f",parvo);
	print_line(line);

	output_close(eout);
	
return 0;	
}

static int Tulos3(tnode *node){					// Tulostaa kuvaruudulle puun p‰‰tesolmun tiedot
	
	double parvo=0.0,error=1e-15;
	char yhtasuuri=' ';

	if (merkkis=='>') yhtasuuri='=';

	output_open(eout);

	sprintf(line, "\n");
	print_line(line);
	
	sprintf(line, "%2d  (%.4s%c%c%.4f)   Node  %2d",memnum,variable,merkkis,yhtasuuri,katkaisu,numero);
	print_line(line);
	sprintf(line, "                      TERMINAL %d",terminal);
	print_line(line);
	sprintf(line, "                      Events         = %d",node->datasize - node->censsize);
	print_line(line);
	sprintf(line, "                      Censored       = %d",node->censsize);
	print_line(line);
	sprintf(line, "                      Q3             = %.0f",ylaK);
	print_line(line);
	sprintf(line, "                      Median         = %.0f",medi);
	print_line(line);
	sprintf(line, "                      Q1             = %.0f",alaK);
	print_line(line); 
	sprintf(line, "                      Mean           = %.2f",keski);
	print_line(line);

	output_close(eout);	

return 0;	
}


static int uusiT(tnode *node){						// Luo uuden Survo-tiedoston puun kullekin
											// p‰‰tesolmun havaintojen tulorajaestimaattorille	
        int i;
        int Plen,Pm1,Pm,Pf,Pextra,Ptextn,Ptextlen;
        char **Ptext;
        char *privi[1];
        
        Pm=2;
        vartype=muste_malloc(Pm*9);
        if (vartype==NULL) { not_enough_memory(); return(-1); }
        pvartype=(char **)muste_malloc(Pm*sizeof(char *));
        if (pvartype==NULL) { not_enough_memory(); return(-1); }
        varlen=(int *)muste_malloc(Pm*sizeof(int));
        if (varlen==NULL) { not_enough_memory(); return(-1); }
        varname=(char **)muste_malloc(Pm*sizeof(char *));
        if (varname==NULL) { not_enough_memory(); return(-1); }
        vartila=muste_malloc(Pm*9);
        if (vartila==NULL) { not_enough_memory(); return(-1); }
		
        for (i=0; i<Pm; ++i) pvartype[i]=vartype+i*9;
		for (i=0; i<Pm; ++i) varname[i]=vartila+i*9;
		varname[0]="Dur";  varlen[0]=8; pvartype[0]="8A";
		varname[1]="Surv"; varlen[1]=8; pvartype[1]="8A";
  		Plen=0;
        for (i=0; i<Pm; ++i) Plen+=varlen[i];
        Plen=Plen+10;
        Pm1=Pm+4;
        Pf=64;
        Pextra=12;
        Ptextn=0;
        Ptextlen=0;
        Ptext=privi;
        sprintf(nimi,"%sT%d.SVO",edisk,terminal);
		sur_delete(nimi);
		i=fi_create(nimi,Plen,Pm1,Pm,node->datasize+1,Pf,Pextra,Ptextn,Ptextlen,
                    Ptext,varname,varlen,pvartype);
        if (i<0) return(-1);
        muste_free(vartype); muste_free(pvartype); muste_free(varlen); muste_free(varname); muste_free(vartila);
        return(1);
        }

static int talletaT(){							// Tallettaa GPLOT-kaaviota varten p‰‰tesolmun
        								// eloonj‰‰misfunktion uuteen Survo -tiedostoon
	long l;
	
	data_open(nimi,&e);
	data_save(&e,1,e.v[0],c_aika[0]);
	data_save(&e,1,e.v[1],Pc_survival[0]);
	for (l=e.l1+1; l<=e.l2; l++){
		data_save(&e,l,e.v[0],c_aika[l-1]);
		data_save(&e,l,e.v[1],Pc_survival[l-1]);
	}
    data_close(&e);

return(1);
}
									
																	
void muste_survival(int argc, char *argv[])
{	
  int i,j=0,k;
  char *TREE="TREE", *LT="LT";

// RS Variable init
b=f=0;
aikavaleja=0;
p=results_line=koko=0;
cens=0;
censor=0;
n=0; strata=0; predictors=0; solmu=0;
nodes=0; numero=0; terminal=0; memnum=0;
nro=1; suitable=eikay=parasmuut=0; puumalli=-1; lifetable=-1;
solmuja=0; MinObs=0; treshold=1; sub=isopuu=0;

interval=1.0; MaxAika=0.0; MaxMax=0.0; 
summa=summa3=bestval=step=splitpoint=0;
fixed=4.0;
Testisuure=0.0; treesum=0.0; katkaisu=0.0; ylaK=0.0; 
medi=0.0; alaK=0.0; keski=0.0;

merkkis=' ';

//static char CLASS[LLENGTH],INTERVAL[LLENGTH],METHOD[LLENGTH],NODES[LLENGTH],PENALTY[LLENGTH],
//	variable[20],TESTI[LLENGTH],nimi[LLENGTH],line[LLENGTH],OBSLIMIT[LLENGTH]; 

// static SURVO_DATA d;
// static SURVO_DATA e;

predind=NULL;
c_failed=NULL;
c_censored=NULL;
c_n_enter=NULL;
sensuroitu=NULL;
c_indeksi=NULL;
Pc_failed=NULL;
Pc_left=NULL;
Pnimittaja=NULL; 
TerNod=NULL;
Codes=NULL;

c_lower=NULL;
c_upper=NULL;
c_eff_size=NULL;
c_cpf=NULL;
c_cps=NULL;
c_cpf_error=NULL;
c_survival=NULL;
c_failure=NULL;
c_surv_error=NULL;
c_surv_lcl=NULL;
c_surv_ucl=NULL;
c_pdf=NULL;
c_pdf_error=NULL;
c_hazard=NULL;
c_haz_error=NULL;
c_median=NULL;
c_med_error=NULL;
c_duration=NULL;
c_aika=NULL;
c_class=NULL;
x=NULL;
Stat=NULL;
Tres=NULL;

Pc_survival=NULL; 
Pc_failure=NULL;
Pc_st_error=NULL;
Pc_lower_cl=NULL;
Pc_upper_cl=NULL; 

m_events=NULL;
m_size=NULL;
m_censored=NULL;
m_taulukko=NULL;
NIMET=NULL;

tapahtumat=NULL;
riskiryh=NULL;
sensuurit=NULL;
paino=NULL;
mV2=NULL;
mD=NULL;
mU=NULL;
vector=NULL;
apumat=NULL;
mU2=NULL;
mV=NULL;
 
vartype=NULL;
pvartype=NULL;         
varlen=NULL;
varname=NULL;
vartila=NULL;

puu=NULL;

/*  
	if (argc==1){
		printf("/nThis program can be used as a SURVO 84C module only.");
		return;
	}
*/	
	s_init(argv[1]);
	if (g<4){
		init_remarks();
		rem_pr("SURVIVAL <data>,<time-variable>,<censored_observations_variable>,<output_line>");
		rem_pr("                                                       / K.Huuhko 17.10.2002");
		rem_pr("Computes the Kaplan-Meier estimator for the data.");
		rem_pr("With the specification METHOD=LT, computes the lifetable estimator for the data.");
		rem_pr("If you use specification CLASS, you can also pick a classificatory variable.");
		rem_pr("Specification CLASS=TREE makes a survival tree for the data.");
		rem_pr("Cases can be limited by IND and CASES specifications.");
		wait_remarks (2);
		return;
	}
	results_line=0;
	if (g>4){
		results_line=edline2(word[4],1,1);
		if (results_line==0) return;
	}
	i=data_open(word[1],&d); if (i<0) return;
	i=sp_init (r1+r-1); if (i<0) return;
	i=conditions(&d); if (i<0) return; 
	
	i=spfind("CLASS");					// etsit‰‰n m‰‰re CLASS ja sen mukaan selvitet‰‰n
	if (i>=0){							// onko kyseess‰ puumalli vai luokittelumuuttuja
		puumalli=0;		
		strcpy(CLASS,spb[i]);
		i=strlen(CLASS);
		if (i<4) i=4;
		k=strncmp(CLASS,TREE,i);
		if (k==0)
			puumalli=1;
	}

	i=spfind("METHOD");			// etsit‰‰n m‰‰re METHOD, joka kertoo onko kyseess‰
	if (i>=0){					// tulorajaestimaattori vai elinajantaulu
		strcpy(line,spb[i]);
		i=strlen(line);
		if (i<2) i=2;
		k=strncmp(line,LT,i);
		if (k==0)
			lifetable=1;
		else{
			output_open(eout);
			sprintf(line,"If you want to make a lifetable for the data, use specification");
			print_line(line);
			sprintf(line,"METHOD=LT. Otherwise don't use the METHOD specification!");
			print_line(line);WAIT;
			output_close(eout);
			return;
		}
	}

	i=Laske_aikavaleja(); if (i<0) return;
	space_allocation();
	i=Lue_Data(); if (i<0) return;				// Luetaan aineisto matriisiin m_taulukko
	
	if (puumalli<1){		// jos kyseess‰ ei ole puumalli

		puu=(tnode *)muste_malloc(sizeof(tnode));	// luodaan keinotekoinen puun juuri, koska
		puu->datasize=suitable;				// k‰ytet‰‰n samoja j‰rjestysalgoritmeja
		puu->censsize=cens;					// kuin puumallissa
		puu->parent=NULL;
		puu->lchild=NULL;
		puu->rchild=NULL;
		puu->data=(long int *)muste_malloc((suitable+1)*sizeof(long int));
		
		for (i=1; i<=suitable; i++){
			puu->data[i]=i;
		}
		for (j=1; j<=puu->datasize; ++j){			// Alustetaan laskuissa k‰ytetyt taulukot
			c_duration[j]=m_taulukko[0][puu->data[j]];
			c_class[j]=m_taulukko[2][puu->data[j]];
			if (m_taulukko[1][puu->data[j]]!=1.0)
				sensuroitu[j]=0;
			else
				sensuroitu[j]=1;
			c_indeksi[j]=puu->data[j];
		}
        qsort_mtaulukko(1, puu->datasize, puu);	// Ryhmitell‰‰n data ensisijaisesti luokan mukaan
		k=Pilko(); if (k<0) return;								// Pilkotaan data kuhunkin luokkaan
		if (strata>1){							
			qsort_aika(1, puu->datasize, puu);	// J‰rjestet‰‰n data ajan mukaan
			Muodosta();					// Muodostetaan testin tarvitsemat vektorit
			Tulos();					// Suoritetaan testit ja tulostetaan
		}							
	}
	else{							// Jos m‰‰re oli TREE, luodaan puumalli
		
		for (i=1; i<=suitable; i++){		// lasketaan aineiston sensuroidut havainnot 
			if (m_taulukko[1][i]!=1.0)
				cens++;
		}
		i=spfind("TEST");			//lukee mink‰ testisuureen mukaan puu rakennetaan	
		if (i>=0){
			strcpy(TESTI,spb[i]);
			nro=atoi(TESTI);
			if (nro<1 || nro>3){
				output_open(eout);
				sprintf(line,"Test 1 indicates to Log-Rank, test 2 to Wilcoxon and test 3 to Tarone-Ware testscores");
				print_line(line);WAIT;
				output_close(eout);
				return;
			}
		}
		MinObs=1+(suitable/10);
		i=spfind("OBSLIMIT");			//lukee mink‰ testisuureen mukaan puu rakennetaan	
		if (i>=0){
			strcpy(OBSLIMIT,spb[i]);
			MinObs=atoi(OBSLIMIT);
			if (MinObs<1 || MinObs>suitable/2){
				output_open(eout);
				sprintf(line,"OBSLIMIT must be between 1 and %d",suitable/2);
				print_line(line);WAIT;
				output_close(eout);
				return;
			}
		}
		i=spfind("PENALTY");			//lukee mink‰ testisuureen mukaan puu rakennetaan	
		if (i>=0){
			strcpy(PENALTY,spb[i]);
			fixed=atof(PENALTY);
			if (fixed<2 || fixed>4){
				output_open(eout);
				sprintf(line,"Value of the penalty parameter must be between 2 and 4");
				print_line(line);WAIT;
				output_close(eout);
				return;
			}
		}
		puu=(tnode *)muste_malloc(sizeof(tnode));	//luodaan puun juuri
		puu->datasize=suitable;
		puu->censsize=cens;
		puu->paraskoko=0;
		puu->Score=TYHJA2;
		puu->Prune=TYHJA2;
		puu->solmuja=1;
		puu->Code=-1;
		puu->parent=NULL;
		puu->lchild=NULL;
		puu->rchild=NULL;
		puu->data=(long int *)muste_malloc((suitable+1)*sizeof(long int));
		for (i=1; i<=suitable; i++){
			puu->data[i]=i;
		}
		Tres[1]=TYHJA;
		add(puu);					// luodaan puu
		TerNod[0]=solmu+1;
		Stat[0]=treesum - fixed*solmuja;
		isopuu=solmu;
		solmu=0;
		while (1==1){
			treshold++;
			Tres[treshold]=TYHJA;
			pruning(puu);
			TerNod[treshold-1]=solmu+1;	// jotta joku mihin voidaan verrata
			if (solmuja==0) break;										
			Stat[treshold-1]=treesum - fixed*solmuja; // Koko puun arvo
			solmu=0;
		}
		i=spfind("NODES");			// lukee halutun puun koon
		if (i>=0){
			strcpy(NODES,spb[i]);
			nodes=atoi(NODES);
			if (nodes<2 || nodes>isopuu+1){
				output_open(eout);
				if (isopuu+1==1){
					sprintf(line,"There is no valid splitting points in the data");
					print_line(line);WAIT;
					return;
				}
				else{
					sprintf(line,"Number of terminal nodes must be between 2 and %d",isopuu+1);
					print_line(line);WAIT;
					output_close(eout);	
					return;
				}
			}
			else{
				for (sub=0; sub<treshold-1; sub++){
					if (nodes>=TerNod[sub])
						break;
				}
				prune(puu);			// karsitaan puu halutun kokoiseksi
			}
		}
		else
			intra();
	}
	vapauta();
    data_close (&d);
	s_end(argv[1]);
// WAIT;
}















