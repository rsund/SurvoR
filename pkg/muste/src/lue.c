#include "muste.h"
/* !lue.c 5.5.1989/SM (10.9.1989) (4.8.1996)
*/
#include <stdio.h>
#include <stdlib.h>
// #include <conio.h>
#include <math.h>
#include <time.h>
#include "survo.h"
#include "survoext.h"
#include "survolib.h"

#define MAXRIVI 5*LLENGTH

extern char *argv1;
static int edrivi;
static FILE *teksti;
static char nimi[LLENGTH];
static char rivi[MAXRIVI];
static char sana[LLENGTH];
static char vmerkit[]=".,:;!";
static char haittamerkit[]="()[]{}&'\"";

static int nsanat,nlauseet,nkirjaimet,nluvut,nvokaalit;
static int nmerkki[256],nmerkki2[256];
static int ns;
static int nsanat_lauseessa;
static char mv; /* v\204limerkki */
static int lause_loppu;
static int luettavuus;
static int tulosrivi;
static int mittaus;
static int sarake;
static int uusinta=0;
static int lyhenn;

static char ruotsi[]="*och*en*ett*den*det*\204r*i*p\206*fr\206n*som*f\224r*vad*han*hon*h\204r*d\204r*";
static char englanti[]="*and*a*the*is*are*am*who*what*this*that*can*to*from*in*";
static char saksa[]="*und*ist*das*ein*eine*von*vor*die*der*sind*auf*bei*wie*es*dass*aus*";
static int nruotsi,nenglanti,nsaksa;

static char luki_tiedosto[LNAME];

static double aika,aika0,tavkerroin,sanaaika,pilkkuaika,pisteaika;
static double lukuaika;

#define NTAVUTYYPIT 10
char *tavutyyppi[]={ "KO","KOK","OK","KOO","OO","O","KOOK","KOKK","OOK","OKK" };
double tavuaika[]={  1.1,  1.3, 1.2,  1.3, 1.2, 1.0, 1.6,   1.6,   1.5,  1.5 };
/* vanhat arvot   {  1.2,  1.6, 1.5,  1.6, 1.5, 1.0, 2.0,   2.0,   1.9,  1.8 }; */
static int nttyyppi[NTAVUTYYPIT];
static int nmuutavu;
static int ntsanat, ntkirjaimet;
static int ntavutyypit=NTAVUTYYPIT;
static int ntpilkut;
static double lukunopeus=0.1;
static int nmpisteet;
static int nmkerrat=1;
static char lukijan_nimi[LLENGTH];
static char lukijan2_nimi[LLENGTH];
static char mittadata[LLENGTH];
static int kokeita;

static FILE *mittatiedot;

extern int mittaus;
extern int sarake;
extern char **spb;
extern int uusinta;
extern char luki_tiedosto[];

static int uusi_aloitus();
static int kokoa_tiedot();
static int tutki_sana();
static int tutki_sana2();
static int lopeta_lause();
static int tutki_lyhenteet();
static int tutki_kieli();
static int iso_kirjain(char ch);
static int wiio();
static int tulostus();
static int tulosta_rivi(char *rivi);
static int aloita_aika();
static int nollaa_laskurit();
static int mittaa_aika(char *sana,char mv,int lause_loppu);
static int mittaa_lukuaika();
static int init_regressio();
static int regressio();
static int lue_lukija(char *nimi,char *tiedot);
static int talleta_lukija();
static int talleta_lukijarivi();
static int aseta_vokaalit();
static int tavuta(char *sana,char *tavut,char *tavut2);
static int tavuta2(char *sana,char *tavut,char *tavut2);
static int sama_merkki_kolmasti(char *sana);
static int luku_sanoina(char *luku,char *sanat);
static int desimaaliosa(char *s,char *sanat);


#define PILKKUKERROIN 0.5
#define MUU_TAVUAIKA 1.8

extern char **spb;

void muste_lue(char *argv)
        {
        int i;

        s_init(argv);

        tut_init();
        i=sp_init(r1+r-1);
        if (i<0)
            {
            sur_print("\n Tekstiss\204 on liikaa = -merkkej\204, jotka sotketaan t\204smennyksiin.");
            sur_print("\n Talleta teksti ASCII-tiedostoon (esim. SAVEP-komennolla)");
            sur_print("\n ja k\204yt\204 sitten komentoa LUE <tekstitiedoston_nimi> .");
            WAIT; return;
            }

        tulosrivi=0;
        if (g>1)
            {
            tulosrivi=edline2(word[g-1],1,0);
            if (tulosrivi) --g;
            }
        if (g==1)
            edrivi=r1+r-1;
        else
            {
            if (*word[1]=='?')
                {
                init_remarks();
  rem_pr(" LUE-komennolla voidaan mitata tekstin (\204\204neen) lukemiseen tarvittava aika");
  rem_pr(" ja laskea tekstin luettavuutta kuvaava indeksi.");
  rem_pr(" Tutkittava teksti on joko toimituskent\204ss\204 LUE-komennon alapuolella");
  rem_pr(" tai erillisess\204 tekstitiedostossa.               ");
  rem_pr(" Tekstin ollessa toimituskent\204ss\204 komento on pelkk\204 LUE       ");
  rem_pr(" ja tekstitiedoston tapauksessa LUE <tiedoston_nimi> .        ");
  rem_pr(" ");
  rem_pr(" Tulokset n\204ytet\204\204n tekstin l\204pik\204ynnin j\204lkeen v\204liaikaisesti ruudussa");
  rem_pr(" ja lukuaika tulee my\224s komentoriville.         ");
  rem_pr(" Lis\204ksi tulokset menev\204t tulostiedostoon.                    ");
  rem_pr(" ");
  rem_pr(" Jos tulokset halutaan saada toimituskentt\204\204n, annetaan       ");
  rem_pr(" viimeisen\204 parametrina ensimm\204inen tulostusrivi,             ");
  rem_pr(" siis esim. LUE A:LUENTO1.TXT,11                              ");
                wait_remarks(1);

  rem_pr(" ");
  rem_pr(" TEKSTIN LUKUAIKA:                                            ");
  rem_pr(" LUE:n antama arvio tekstin lukemiseen tarvittavasta lukuajasta");
  rem_pr(" perustuu S.Mustosen laatimaan dynaamiseen regressiomalliin.");
  rem_pr(" Mallissa lukuaika arvioidaan tekstiss\204 esiintyvien lauseiden, ");
  rem_pr(" sanojen ja tavujen lukum\204\204rien avulla.                       ");
  rem_pr(" ");
  rem_pr(" Arvioinnin voi sopeuttaa oman lukunopeuden mukaisesti antamalla");
  rem_pr(" t\204smennyksen MITTAUS=<lukijan_nimi> (Huom. nimess\204 ei v\204lily\224ntej\204).");
  rem_pr(" T\204ll\224in kuvaputkelle saadaan lukemista koskevat ohjeet       ");
  rem_pr(" ja ohjelma mittaa todellisen lukunopeuden yhden tai muutaman");
  rem_pr(" lauseen eriss\204 kunnes k\204ytt\204j\204 (lukija) keskeytt\204\204 mittauksen");
  rem_pr(" napilla ESC.                                                 ");
                wait_remarks(1);

  rem_pr(" ");
  rem_pr(" Mittauksen aikana kuvaputken alaosassa n\204ytet\204\204n paitsi odotettu lukuaika");
  rem_pr(" ja havaittu lukuaika, my\224s regressiomallin senhetkiset kertoimet.");
  rem_pr(" N\204m\204 kertoimet muuttuvat l\204ht\224arvoistaan v\204hitellen nykyisen lukijan");
  rem_pr(" lukutapaa vastaaviksi.                   ");
  rem_pr(" Kertoimien pit\204isi j\204rkeviss\204 tilanteissa olla positiivisia.  ");
  rem_pr(" Negatiivisia kertoimia voi esiinty\204, jos lukija ei pysty k\204ytt\204ytym\204\204n");
  rem_pr(" johdonmukaisesti vaan vaihtaa lukunopeutta tai -tyyli\204.  ");
  rem_pr(" ");
  rem_pr(" Mittauksen j\204lkeen saadaan koko tekstin lukuaika uuden lukutavan mukaisesti.");
  rem_pr(" Uuden lukijan tiedot tallentuvat automaattisesti aputiedostoon");
  rem_pr(" .\\SYS\\LUKIJAT.TXT ja muiden tekstien lukemiseen tarvittava aika  ");
  rem_pr(" voidaan t\204m\204n j\204lkeen mitata niiden mukaisesti k\204ytt\204m\204ll\204 ");
  rem_pr(" t\204smennyst\204 LUKIJA=<lukijan_nimi> .                          ");
                wait_remarks(1);

  rem_pr(" ");
  rem_pr(" Lukunopeuden mittauksessa virkkeitt\204in tai virkepareittain ker\204tyt tiedot");
  rem_pr(" saa my\224s talletetuksi tekstitiedostoon antamalla t\204smennyksen");
  rem_pr(" MITTADATA=<tiedoston_nimi> . Muuntamalla t\204m\204 tiedosto SURVO 84C-havainto-");
  rem_pr(" tiedostoksi on mahdollista j\204lkeenp\204in tutkia tarkemmin lukunopeuden");
  rem_pr(" riippuvuutta mitatuista tekstin ominaisuuksista.                                  ");
                wait_remarks(1);
  rem_pr(" ");
  rem_pr(" TEKSTIN LUETTAVUUS:                                          ");
  rem_pr(" LUE:n antama luettavuusarvio perustuu O.A.Wiion tutkimuksiin.");
  rem_pr(" Tekstin teoreettista vaikeusastetta mitataan vertaamalla sit\204");
  rem_pr(" 300 lehtiartikkelin kielen tilastollisiin ominaisuuksiin.    ");
  rem_pr(" Lehtityypit ovat: lastenlehdet, sanomalehdet ja tiedelehdet. ");
  rem_pr(" Tuloksena saatava vaikeusindeksi vaihtelee tyypillisesti     ");
  rem_pr(" nollasta sataan. Se lasketaan lineaarisen regressiomallin    ");
  rem_pr(" antamana ennusteena.                                         ");

                wait_remarks(2);
                return;
                }
            edrivi=0;

            strcpy(nimi,word[1]);
            if (strchr(nimi,':')==NULL) { strcpy(nimi,edisk); strcat(nimi,word[1]); }
            teksti=muste_fopen(nimi,"rt");
            if (teksti==NULL)
                {
                sprintf(sbuf,"\nTekstitedostoa %s ei l\224ydy!",nimi);
                sur_print(sbuf); WAIT; return;
                }
            }

     strcpy(luki_tiedosto,survo_path); strcat(luki_tiedosto,"SYS/LUKIJAT.TXT"); // RS \\ -> /

        *lukijan2_nimi=EOS;
        i=spfind("MITTAUS");
        if (i<0) mittaus=0; else { mittaus=1; strcpy(lukijan2_nimi,spb[i]); }

        sur_print("\n"); PR_EUDL; sarake=0;
        if (scroll_line>r3-15) scroll_line=5;

        for (i=0; i<r3-scroll_line+3; ++i) sur_print("                                                                               \n");

        i=aloita_aika(); if (i<0) return;
        kokoa_tiedot();
        wiio();
        tulostus();
        if (*lukijan2_nimi) { i=talleta_lukija(); if (i<0) return; }
        tut_end();
        s_end(argv);
        }

static int uusi_aloitus()
        {
        int i;

        nsanat=nlauseet=nkirjaimet=nluvut=nvokaalit=0L;
        for (i=0; i<256; ++i) nmerkki[i]=nmerkki2[i]=0L;
        ns=0;
        nsanat_lauseessa=0;
        lause_loppu=0;
        sarake=0;
        lyhenn=0;
        nruotsi=nenglanti=nsaksa=0L;
        nluvut=0L;
        strcpy(lukijan_nimi,lukijan2_nimi);

        if (edrivi) edrivi=r1+r-1;
        else rewind(teksti);
        mittaus=0;
        uusinta=1;
        i=aloita_aika(); if (i<0) return(1);
        sur_print("\n\n\n");
        return(1);
        }

static int kokoa_tiedot()
        {
        int i,len;
        char *p;

        ns=0;
        while (1)
            {
            if (sur_kbhit())
                {
                i=sur_getch(); if (i=='.') break;
                }
            if (edrivi)
                {
                while (1)
                    {
                    ++edrivi;
                    if (edrivi>r2) break;
                    edread(rivi,edrivi);
                    if (strchr("-&",*rivi)==NULL) break;
                    }
                if (edrivi>r2) break;
                *rivi=EOS; p=rivi+1;
                }
            else
                {
                fgets(rivi+1,MAXRIVI,teksti);
                if (feof(teksti)) break;
                len=strlen(rivi);
                if (rivi[len-1]=='\n') rivi[len-1]=EOS;
                *rivi=EOS; p=rivi+1;
                }
            while (*p && *p==' ') ++p;
            if (*p==EOS) { lopeta_lause(); continue; }  /* tyhj\204 rivi */
            len=strlen(p); while (p[len-1]==' ') p[--len]=EOS;
            --p;
            while (1)
                {
                ++p;
                ++nmerkki2[(int)(unsigned char)*p];
                if (*p==EOS) { tutki_sana(); break; }
                if (strchr(haittamerkit,*p)!=NULL)
                    {
                    if (*(p+1)) continue; else { tutki_sana(); break; }
                    }
                if (*p==' ') { tutki_sana(); continue; }
                if (*p=='-')
                    {
                    if (*(p+1)==EOS) break;
                    if (*(p-1)==' ' || *(p-1)==EOS)
                        {             /* "rivin" edess\204 aina EOS */
                        sana[ns++]=*p; sana[ns]=EOS;
                        continue;
                        }
                    tutki_sana();
                    continue;
                    }
                mv=EOS;
                if (strchr(vmerkit,*p)!=NULL)
                    {
                    mv=*p;
                    while (1)
                        {
                        if (*(p+1)==EOS) break;
                        if (strchr(vmerkit,*(p+1))==NULL) break;
                        ++p;
                        }

                    if (ns && (mv=='.' || mv=='?' || mv=='!' || mv==':'))
                        {
                        i=0;
                        if (mv=='.')
                            {
                            i=tutki_lyhenteet(); /* 1=lyhenne 0=ei */
                            if (i!=1)
                                {
                                if (*(p+1)==' ' && iso_kirjain(*(p+2)))
                                    i=0;
                                }
                            if (i==2)  /* 2=luku */
                                {
                                sana[ns++]='.'; sana[ns]=EOS;
                                mv=EOS; continue;
                                }
                            }
                        if (mv==':')
                            {
                            if (*(p+1)!=' ' && *(p+1)!=EOS)
                                {
                                sana[ns++]=':'; sana[ns]=EOS;
                                mv=EOS; continue;
                                }
                            }
                        if (i==0 && nsanat_lauseessa>0)
                            {
                            lause_loppu=1;
                            }
                        }
                    tutki_sana();
                    if (uusinta) { uusi_aloitus(); uusinta=0; break; }
                    continue;
                    }  /* if vmerkit... */
                sana[ns++]=*p; sana[ns]=EOS;
                } /* while p */

            } /* uusi rivi */
        return(1);
        }

static int tutki_sana()
        {
        int i;
        char sana2[LLENGTH];
        char *p,*q;

        i=luku_sanoina(sana,sana2);
        if (i)
            {
            ++nluvut;
            p=sana2;
            while (1)
                {
                q=strchr(p,' ');
                if (q!=NULL) *q=EOS;
                strcpy(sana,p);
                ns=strlen(sana);
                lyhenn=1;
                tutki_sana2();
                lyhenn=0;
                if (q==NULL) return(1);
                p=q+1;
                }
            }
        else tutki_sana2();
        return(1);
        }

static int tutki_sana2()
        {
        int i,k;

        if (ns==0) return(1);
        sana[ns]=EOS;
        k=mittaa_aika(sana,mv,lause_loppu);
            /* k<0: merkkijono ei tavutu, siis koodi tai esim. jjjjjjjjjj */

        if (sarake+ns>c3+6) { sur_print("\n"); sarake=0; }

        for (i=0; i<ns; ++i) ++nmerkki[(int)(unsigned char)sana[i]];

        tutki_kieli();

        if (lyhenn) PR_EIN2;
        if (k<0) PR_EINV;
        if (mv)
            {
            sprintf(sbuf," %s%c",sana,mv); sur_print(sbuf);
            sarake+=ns+2;
            }
        else
            {
            sprintf(sbuf," %s",sana); sur_print(sbuf);
            sarake+=ns+1;
            }
        if (lyhenn) { PR_EUDL; lyhenn=0; }
        if (k<0) { PR_EUDL; ns=0; if (!lause_loppu) return(1); }
        mv=EOS;
        ++nsanat_lauseessa;

        ++nsanat;
        nkirjaimet+=ns;

        if (lause_loppu) lopeta_lause();

        ns=0;
        return(1);
        }

static int lopeta_lause()
        {
        if (nsanat_lauseessa==0) return(1);
        sur_print("_\n"); lause_loppu=0;
        sarake=0;
        if (mittaus) mittaa_lukuaika();
        if (nsanat_lauseessa>1) ++nlauseet;
        nsanat_lauseessa=0;
        return(1);
        }

/* pisteelliset lyhenteet */
static char *lyhenne[]={ "mm","muun-muassa",
                  "esim","esimerkiksi",
                  "kpl","kappaletta",
                  "n","noin",
                  "ns","niin-sanottu",
                  "jne","ja-niin-edesp\204in",
                  "v","vuonna",
                  "yht","yhteens\204",
                  "ko","kysess\204 oleva",
                  "jatk","jatkuu" };

static int n_lyhenne=10;

static int tutki_lyhenteet()   /* ja luvut */
        {
        int i;

        n_lyhenne=10;
        if (strchr("+-*/.0123456789",*sana)!=NULL) return(2); /* luku */
        sana[ns]=EOS;
        if (strchr(sana,'.')!=NULL) return(2);  /* esim. O.A. */
        for (i=0; i<n_lyhenne; ++i)
            {
            if (muste_strcmpi(sana,lyhenne[2*i])==0)
                {
                strcpy(sana,lyhenne[2*i+1]);
                ns=strlen(sana); mv=EOS;
                lyhenn=1;
                return(1); /* lyhenne */
                }
            }

        if (ns<2) return(2); /* esim. O. */
        return(0); /* ei lyhenne */
        }

static int tutki_kieli()
        {
        char s[LLENGTH];

        *s='*'; strcpy(s+1,sana); s[ns+1]='*'; s[ns+2]=EOS;

        nruotsi+=(strstr(ruotsi,s)!=NULL);
        nenglanti+=(strstr(englanti,s)!=NULL);
        nsaksa+=(strstr(saksa,s)!=NULL);
        return(1);
        }

static int iso_kirjain(char ch)
        {
        if ((ch>='A' && ch<='Z') || ch=='\216' || ch=='\231' || ch=='\217')
            return(1);
        return(0);
        }

static int wiio()
        {
        if (nlauseet==0) nlauseet=1;
        if (nkirjaimet<=50 || nsanat<=10) { luettavuus=-9999; return(1); }
/*
printf("\nKirjaimia=%d sanoja=%d lauseita=%d",nkirjaimet,nsanat,nlauseet);
getch();
*/
        luettavuus=1.416*(double)nsanat/(double)nlauseet+5.147*(double)nkirjaimet/(double)nsanat
         +(-2.731*(nmerkki[74]+nmerkki[106])*100  /* j */
           +1.957*(nmerkki[79]+nmerkki[111])*100  /* o */
           -3.015*(nmerkki[80]+nmerkki[112])*100  /* p */
           +2.016*(nmerkki[83]+nmerkki[115])*100  /* s */
           +1.222*(nmerkki[84]+nmerkki[116])*100) /* t */
           /(double)nkirjaimet
           -34.54186;
        return(1);
        }

static int tulostus()
        {
        int i,j;
        char x[LLENGTH],y[LLENGTH],x2[LLENGTH];
        char wiio_vastaus[LLENGTH];
        char *p;

        PR_EINV;

        if (luettavuus==-9999)
            sprintf(y," Liian v\204h\204n teksti\204 luettavuuden selvitt\204miseen!");
        else
            {
            if ((double)nruotsi>10.0+(double)nsanat/50.0)
                {
                strcpy(y," Teksti on todenn\204k\224isesti ruotsia!");
                luettavuus=-9999;
                }
            else if ((double)nenglanti>10.0+(double)nsanat/50.0)
                {
                strcpy(y," Teksti on todenn\204k\224isesti englantia!");
                luettavuus=-9999;
                }
            else if ((double)nsaksa>10.0+(double)nsanat/50.0)
                {
                strcpy(y," Teksti on todenn\204k\224isesti saksaa!");
                luettavuus=-9999;
                }
            else if ((double)nluvut>10.0+(double)nsanat/10.0)
                {
                strcpy(y," Tekstiss\204 on liikaa lukuja!");
                luettavuus=-9999;
                }
            else
                {
                sprintf(y," Luettavuus=%d",luettavuus);
                if (luettavuus>100) strcat(y," (Teksti eritt\204in vaikeata)");
                else if (luettavuus>=68) strcat(y," (Hyvin vaikeata = tiedelehtien ylin nelj\204nnes)");
                else if (luettavuus>=60) strcat(y," (Teksti muistuttaa tiedelehti\204)");
                else if (luettavuus>=44) strcat(y," (Teksti sanomalehtikielen rajoissa)");
                else if (luettavuus>=35) strcat(y," (Teksti muistuttaa lastenlehti\204)");
                else strcat(y," (Eritt\204in helppoa = lastenlehtien alin nelj\204nnes)");
                }
            }
        strcpy(wiio_vastaus,y);

        output_open(eout);
        if (edrivi)
            {
            sprintf(x2,"rivit %d -",r1+r);
            }
        else sprintf(x2,"tekstitiedosto %s",nimi);
        sprintf(x," Tutkittu teksti: %s",x2);
        tulosta_rivi(x);

        if (nsanat==0L) nlauseet=0L;

        sprintf(x," Tekstiss\204 on %d virkett\204, %d sanaa ja %d kirjainta.",
                      nlauseet,nsanat,nkirjaimet);
        tulosta_rivi(x);


        tulosta_rivi(" ");

        if (nlauseet)
            {
            sprintf(x," Tekstin lukeminen kest\204\204 %.1f minuuttia",lukunopeus*aika/60.0);
            tulosta_rivi(x);
            if (*lukijan2_nimi) strcpy(lukijan_nimi,lukijan2_nimi);
            if (*lukijan_nimi)
                sprintf(x," lukijan ollessa %s parametrein %.2f,%.2f,%.2f .",
                                      lukijan_nimi,pisteaika,sanaaika,tavkerroin);
            else
                sprintf(x," nopeusparametreilla %.2f,%.2f,%.2f",
                                                  pisteaika,sanaaika,tavkerroin);
            tulosta_rivi(x);
            }

        tulosta_rivi(wiio_vastaus);
        if (luettavuus!=-9999)
            {
            sprintf(x," Virkkeess\204 on keskim\204\204rin %.1f sanaa (lehtikieless\204 n. 12).",
                          (double)nsanat/(double)nlauseet);
            tulosta_rivi(x);
            sprintf(x," Sanassa on keskim\204\204rin %.1f kirjainta (lehtikieless\204 7.6).",
                          (double)nkirjaimet/(double)nsanat);
            tulosta_rivi(x);
            }

        output_close(eout);
        sur_print("\n (Tietoja LUE:n k\204yt\224st\204 saa aktivoimalla komennon LUE ? )");
        if (!tulosrivi)
            {
            j=r1+r-1;
            edread(x,j);
            p=strstr(x+1," / ");
            if (p==NULL)
                {
                i=strlen(x);
                while (x[i-1]==' ') --i;
                ++i;
                }
            else i=p-x;

            if (luettavuus==-9999) strcpy(x,"?"); else sprintf(x,"%d",luettavuus);
            sprintf(y," / Lukuaika=%.1f min. Luettavuus=%s",lukunopeus*aika/60.0,x);

/*          sprintf(y," / Lukuaika=%.1f min.",lukunopeus*aika/60.0);
*/
            edwrite(space,j,i);
            edwrite(y,j,i);
            }
        if (!tulosrivi)
            {
            PR_EIN2; sur_print("\n Paina jotain nappia!");
            i=nextch("");
            }
        return(1);
        }


static int tulosta_rivi(char *rivi)
        {
        output_line(rivi,eout,tulosrivi);
        if (tulosrivi) ++tulosrivi;
        return(1);
        }

/* aika.c 6.5.1989/SM (9.6.1989) (3.8.1996)
*/

static int aloita_aika()
        {
        int i;
        char x[LLENGTH],*osa[3];

        aika=aika0=0.0;
        aseta_vokaalit();
        nollaa_laskurit();
        if (uusinta) return(1);

        if (scroll_line>r3-15) scroll_line=5;

        pisteaika=5.2;
        sanaaika=1.2;
        tavkerroin=0.8;
        *lukijan_nimi=EOS;
        i=spfind("LUKIJA");
        if (i>=0)
            {
            strcpy(x,spb[i]); i=split(x,osa,3);
            if (i==1)
                {
                strcpy(lukijan_nimi,osa[0]);
                i=lue_lukija(osa[0],x);
                if (i<0) return(-1);
                i=split(x,osa,3);
                }
            if (i<3)
                {
                sur_print("\nPuutteellinen LUKIJA. Oikea rakenne on:");
                sur_print("\nLUKIJA=<lauseen_osat>,<sanav\204lit>,<tavukerroin>");
                sur_print("\nOletusarvot LUKIJA=5.2,1.2,0.8");
                sur_print("\nvastaavat tavallista \204\204neenlukua.");
                WAIT; return(-1);
                }
            pisteaika=atof(osa[0]); sanaaika=atof(osa[1]);
            tavkerroin=atof(osa[2]);
            }
        else /* ei LUKIJA-t\204smennyst\204 */
            {
            i=lue_lukija("-",x);
            if (i>=0)
                {
                split(x,osa,3);
                pisteaika=atof(osa[0]); sanaaika=atof(osa[1]);
                tavkerroin=atof(osa[2]);
                }
            }

        pilkkuaika=PILKKUKERROIN*pisteaika;
        if (mittaus)
            {
            init_regressio();
            PR_EIN2;
            sprintf(sbuf,"\n  LUKUNOPEUDEN MITTAAMINEN ALKAA:  Lukijana %s",lukijan2_nimi);
            sur_print(sbuf);
            PR_EINV;
            sur_print("\n  Teksti\204 tulee n\204kym\204\204n ikkunassa yhden tai parin lauseen verran kerrallaan.");
            sur_print("\n  Sinun teht\204v\204si on lukea lauseet tavalla, joka");
            sur_print("\n  vastaa normaalia lukunopeuttasi ja -tyyli\204si.");
            sur_print("\n  Paina HETI nappia ENTER, kun olet lukenut viimeisen sanan!");
            sur_print("\n  Voit lopettaa nopeuden mittauksen koska tahansa napilla ESC,");
            sur_print("\n  jolloin saat tuloksena aika-arvion koko tekstille.");
            sur_print("\n");
            sur_print("\n  Paina nyt nappia ENTER saadaksesi luettavaksi ensimm\204iset lauseet!");
            i=nextch(""); PR_EUDL;
            for (i=0; i<r3-scroll_line+1; ++i) sur_print("                                                                                \n");
            }

        if (mittaus)
            {
            *mittadata=EOS;
            i=spfind("MITTADATA");
            if (i>=0)
                {
                strcpy(mittadata,spb[i]);
                if (strchr(mittadata,':')==NULL)
                    { strcpy(mittadata,edisk); strcat(mittadata,spb[i]); }
                mittatiedot=fopen(mittadata,"wt");
                fprintf(mittatiedot,"T\204m\204 tiedosto sis\204lt\204\204 lukijan %s lukunopeutta\n",lukijan2_nimi);
                fprintf(mittatiedot,"kuvaavat mittaustulokset.\n");
                fprintf(mittatiedot,"Yksi rivi vastaa yhden tai muutaman virkkeen lukemista.\n");
                fprintf(mittatiedot,"Lukuaika 'Aika' on sekunteja. Muut arvot ovat frekvenssej\204.\n");
                fprintf(mittatiedot,"Tavut on jaettu vokaali(O)-konsonantti(K)-rakenteen mukaisiin\n");
                fprintf(mittatiedot,"ryhmiin.\n");
                fprintf(mittatiedot,"Muuttuja 'Tavut' on n\204iden frekvenssien painotettu summa\n");
                fprintf(mittatiedot,"alla n\204kyvin tavukertoimin.\n");

                fprintf(mittatiedot,"\n");

                fprintf(mittatiedot,
                  "Tavukertoimet:               1.1 1.3 1.2 1.3 1.2 1 1.6  1.6  1.5 1.5\n");
                fprintf(mittatiedot,
                  "Aika Lauseet Sanat Tavut Kirj KO KOK OK  KOO OO  O KOOK KOKK OOK OKK\n");
                }
            }

        return(1);
        }

static int nollaa_laskurit()
        {
        int i;

        ntsanat=0;
        ntkirjaimet=0;
        ntpilkut=0;
        for (i=0; i<ntavutyypit; ++i) nttyyppi[i]=0;
        nmuutavu=0;
        aika0=aika;
        nmpisteet=0;
        return(1);
        }

static int mittaa_aika(char *sana,char mv,int lause_loppu)
        {
        int i,k;
        char tavut[LLENGTH],tavut2[LLENGTH];
        char *p,*q;

        k=tavuta(sana,tavut,tavut2);
        if (k<0) return(-1);
        ntkirjaimet+=strlen(sana);
        p=tavut2;
        while (*p)
            {
            q=strchr(p,'-');
            if (q!=NULL) *q=EOS;
            for (i=0; i<ntavutyypit; ++i)
                {
                if (strcmp(p,tavutyyppi[i])==0) break;
                }
            if (i<ntavutyypit) { aika+=tavkerroin*tavuaika[i]; ++nttyyppi[i]; }
            else
                {
                aika+=MUU_TAVUAIKA*tavkerroin; ++nmuutavu;
                }
            p=q+1;
            }
        if (mv==',' || mv==';') { aika+=pilkkuaika; ++ntpilkut; }
        ++ntsanat; aika+=sanaaika;
        if (lause_loppu)
            {
            aika+=pisteaika;
            }
        return(1);
        }

static int mittaa_lukuaika()
        {
        int i;
        clock_t alku,loppu;
        double odotettu_aika;

        ++nmpisteet;
        if (nmpisteet<nmkerrat) return(1);;

        nmkerrat=3-nmkerrat;
        sur_print("\n\n");
        PR_EINV;
        sur_print("********************************************************************************\n");
        sur_print("  Lue yll\204 oleva teksti ja paina ENTER!  Lopeta mittaus napilla ESC!\n");
        sur_print("********************************************************************************\n");
        sarake=0;

        alku=clock();
        odotettu_aika=lukunopeus*(aika-aika0);
        sprintf(sbuf,"    Odotettu lukemisaika on %.2f sek.",odotettu_aika); sur_print(sbuf);
        i=nextch(""); PR_EUDL;
        if (i==CODE_EXEC) { mittaus=0; uusinta=1; return(1);}

        sur_print("\n");
        loppu=clock();
        lukuaika=(double)(loppu-alku)/CLOCKS_PER_SEC; ++kokeita; // RS CHA CLK_TCK -> CLOCKS_PER_SEC
        sprintf(sbuf,"  Todellinen lukemisaika on %.2f sek.  Erotus=%.2f   N=%d",
                                      lukuaika,odotettu_aika-lukuaika,kokeita);
        sur_print(sbuf);
        regressio();
        PR_EINV;
        sur_print("\n  Saat seuraavat lauseet napilla ENTER. Mittauksen keskeytys napilla ESC.");

        i=nextch(""); PR_EUDL;
        if (i==CODE_EXEC) { mittaus=0; uusinta=1; return(1); }


        nollaa_laskurit();
        for (i=0; i<r3-scroll_line+1; ++i) sur_print("\n");
        return(1);
        }

#define M 3
static double XX[M*M],XY[M],X[M],A[M];
static double XXINV[M*M],XX2[M*M];

/*
0 pisteaika
1 sanaaika
2 tavkerroin

*/

static int init_regressio()
        {
        int i,j;

        XY[0]=pisteaika;
        XY[1]=sanaaika;
        XY[2]=tavkerroin;

        for (i=0; i<M; ++i) XY[i]*=lukunopeus*3.0;

        for (i=0; i<M; ++i)
            {
            for (j=0; j<M; ++j) XX[i+M*j]=0.0;
            XX[i+M*i]=1.0*3.0;
            }
/*
        Rprintf("\nXY: ");
        for (i=0; i<M; ++i) Rprintf("%g ",XY[i]);
        for (i=0; i<M; ++i)
            {
            Rprintf("\n"); for (j=0; j<M; ++j) Rprintf("%g ",XX[i+M*j]);
            }
getch();
*/
        return(1);
        }

static int regressio()
        {
        int i,j;
        double y,det;


        y=lukuaika;
        X[0]=nmpisteet-1+PILKKUKERROIN*ntpilkut;
        X[1]=ntsanat;

        X[2]=MUU_TAVUAIKA*nmuutavu;
        for (i=0; i<NTAVUTYYPIT; ++i) X[2]+=tavuaika[i]*nttyyppi[i];

        if (*mittadata)
            {
            fprintf(mittatiedot,"%.2f %.2f %.2f %.2f ",y,X[0],X[1],X[2]);
            fprintf(mittatiedot,"%d ",ntkirjaimet);
            for (i=0; i<NTAVUTYYPIT; ++i)
                fprintf(mittatiedot," %d",nttyyppi[i]);
            fprintf(mittatiedot,"\n");
            }

        for (i=0; i<M; ++i)
            {
            XY[i]+=y*X[i];
            for (j=0; j<=i; ++j) XX[i+M*j]+=X[i]*X[j];
            }
        for (i=0; i<M; ++i)
            for (j=0; j<=i; ++j) XX[j+M*i]=XX[i+M*j];

        for (i=0; i<M*M; ++i) XX2[i]=XX[i];
        mat_inv(XXINV,XX2,M,&det);
        mat_mlt(A,XXINV,XY,M,M,1);

        for (i=0; i<M; ++i) A[i]/=lukunopeus;
        sprintf(sbuf,"\n  Lukuparametrit: Lausev\204lit=%.2f  Sanav\204lit=%.2f Tavut=%.2f",
                         A[0],A[1],A[2]); sur_print(sbuf);
        pisteaika=A[0]; pilkkuaika=PILKKUKERROIN*pisteaika;
        sanaaika=A[1];
        tavkerroin=A[2];
        return(1);
        }

static FILE *lukijat;

static int lue_lukija(char *nimi,char *tiedot)
        {
        int i;
        char x[LLENGTH];
        char rivi[LLENGTH],*osa[2];
        int oletuslukija;

        if (strcmp(nimi,"-")==0) oletuslukija=1; else oletuslukija=0;
        lukijat=fopen(luki_tiedosto,"rt");
        if (lukijat==NULL)
            {
            if (oletuslukija) return(-1);
            sprintf(sbuf,"\nTiedosto %s puuttuu!",luki_tiedosto);
            sur_print(sbuf); WAIT; return(-1);
            }
        while (1)
            {
            fgets(x,LLENGTH-1,lukijat);
            if (feof(lukijat))
                {
                if (oletuslukija) return(-1);
                sprintf(sbuf,"\nLUKIJA=%s ei tiedostossa %s",nimi,luki_tiedosto);
                sur_print(sbuf); muste_fclose(lukijat); WAIT; return(-1);
                }
            i=strlen(x); if (x[i-1]=='\n') x[i-1]=EOS;
            strcpy(rivi,x);
            split(rivi,osa,2);
            if (*osa[0]=='/') continue;
            if (!oletuslukija && strcmp(osa[0],nimi)!=0) continue;
            strcpy(tiedot,x+(osa[1]-rivi));
            muste_fclose(lukijat);
            return(1);
            }
        }

static FILE *lukijat2;

static int talleta_lukija()
        {
        int i;
        char x[LLENGTH];
        char rivi[LLENGTH],*osa[2];
        int ei_talletettu;
        char luki2[LNAME];

        ei_talletettu=1;
        lukijat=fopen(luki_tiedosto,"rt");
        strcpy(luki2,survo_path); strcat(luki2,"SYS/SURVO.TXT"); // RS CHA \\ -> /
        lukijat2=fopen(luki2,"wt");

        if (lukijat!=NULL)
            {
            while (1)
                {
                fgets(x,LLENGTH-1,lukijat);
                if (feof(lukijat))
                    {
                    if (ei_talletettu) talleta_lukijarivi();
                    muste_fclose(lukijat); muste_fclose(lukijat2);
//                  remove(luki_tiedosto);
                    sur_delete1(luki_tiedosto);
            /*      sprintf(sbuf,"DEL %s",luki_tiedosto);
                    system(sbuf);
            */
                    break;
                    }
                i=strlen(x); if (x[i-1]=='\n') x[i-1]=EOS;
                strcpy(rivi,x);
                split(rivi,osa,2);
                if (*osa[0]=='/') continue;
                if (strcmp(osa[0],lukijan2_nimi)==0)
                    {
                    talleta_lukijarivi(); ei_talletettu=0;
                    }
                else fprintf(lukijat2,"%s\n",x);
                }
            }
        else
            { talleta_lukijarivi(); muste_fclose(lukijat2); }
//      sprintf(sbuf,"REN %s LUKIJAT.TXT",luki2);
//      system(sbuf);
        strcpy(sbuf,survo_path); strcat(sbuf,"SYS/LUKIJAT.TXT"); // RS \\ -> /
        sur_rename(luki2,sbuf);
        return(1);
        }

static int talleta_lukijarivi()
        {
        fprintf(lukijat2,"%s %.2f %.2f %.2f\n",lukijan2_nimi,pisteaika,sanaaika, tavkerroin);
        return(1);
        }

/* tav.c 6.5.1989/SM (6.5.1989)
*/

static int vokaali[256];

static int aseta_vokaalit()
        {
        int i;

        for (i=0; i<256; ++i) vokaali[i]=0;
        vokaali[(int)(unsigned char)'A']=1;
        vokaali[(int)(unsigned char)'a']=1;
        vokaali[(int)(unsigned char)'E']=1;
        vokaali[(int)(unsigned char)'e']=1;
        vokaali[(int)(unsigned char)'I']=1;
        vokaali[(int)(unsigned char)'i']=1;
        vokaali[(int)(unsigned char)'O']=1;
        vokaali[(int)(unsigned char)'o']=1;
        vokaali[(int)(unsigned char)'U']=1;
        vokaali[(int)(unsigned char)'u']=1;
        vokaali[(int)(unsigned char)'Y']=1;
        vokaali[(int)(unsigned char)'y']=1;
        vokaali[(int)(unsigned char)'\216']=1;
        vokaali[(int)(unsigned char)'\204']=1;
        vokaali[(int)(unsigned char)'\231']=1;
        vokaali[(int)(unsigned char)'\224']=1;
        vokaali[(int)(unsigned char)'\217']=1;
        vokaali[(int)(unsigned char)'\206']=1;
        return(1);
        }

static int tavuta(char *sana,char *tavut,char *tavut2)
        {
        char s[256],tav[256],tav2[256];
        char *p,*q;
        int tavuja;

        if (sama_merkki_kolmasti(sana)) return(-1);

        *tavut=EOS;
        *tavut2=EOS;
        strcpy(s,sana);
        p=s;
        while (1)
            {
            q=strchr(p,'-');
            if (q!=NULL) *q=EOS;
            tavuta2(p,tav,tav2);
            strcat(tavut,tav); strcat(tavut2,tav2);
            if (q==NULL) break;
            p=q+1;
            }

        tavuja=0; p=tavut;
        while (*p)
            {
            p=strchr(p,'-');
            if (p==NULL) break;
            ++tavuja; ++p;
            }
        if (tavuja<2 && strlen(sana)>5) return(-1);
        if ((double)strlen(sana)/(double)tavuja>5) return(-1);
        return(1);
        }


static int tavuta2(char *sana,char *tavut,char *tavut2)
        {
        int i,i1,len;
        char x[256];

        len=strlen(sana);
        for (i=0; i<len; ++i) if (vokaali[(int)(unsigned char)sana[i]])
                                   x[i]='O'; else x[i]='K';
        x[len]=EOS;

        i=0; i1=0; *tavut=EOS;
        while (i<len)
            {
            while (i<len && x[i]=='K') ++i;
            if (i==len) { strncat(tavut,sana+i1,i-i1+1); strcat(tavut,"-"); break; }
            while (i<len && x[i]=='O') ++i;
            if (i==len) { strncat(tavut,sana+i1,i-i1+1); strcat(tavut,"-"); break; }
            while (i<len && x[i]=='K') ++i;
            if (i<len) i-=2;
            strncat(tavut,sana+i1,i-i1+1); strcat(tavut,"-");
            i+=1;
            i1=i;
            }

        if (tavut2!=NULL)
            {
            len=strlen(tavut);
            for (i=0; i<len; ++i)
                {
                if (tavut[i]=='-') { tavut2[i]='-'; continue; }
                if (vokaali[(int)(unsigned char)tavut[i]])
                    tavut2[i]='O'; else tavut2[i]='K';
                }
            tavut2[len]=EOS;
            }
        return(1);
        }

static int sama_merkki_kolmasti(char *sana)
        {
        char *p;

        p=sana;
        while (*p)
            {
            if (*p==*(p+1) && *p==*(p+2)) return(1);
            ++p;
            }
        return(0);
        }

/* luvut.c 11.5.1989/SM (13.5.1989)
*/

static char *pot3yks[]={ "yksi ","tuhat ","miljoona ","miljardi ","biljoona " };
static char *pot3mon[]={ "","tuhatta ","miljoonaa ","miljardia ","biljoonaa " };
static char *pot[]={ "","kymment\204 ","sataa " };
static char *numero[]={"nolla ","yksi ","kaksi ","kolme ","nelj\204 ","viisi ","kuusi ","seitsem\204n ",
                "kahdeksan ","yhdeks\204n " };

static int luku_sanoina(char *luku,char *sanat)
        {
        int i,j,n,m,len;
        char *p;
        char *q;
        double arvo;
        char x[100],y[16];

        *sanat=EOS;
        if(!muste_isnumber(luku)) return(0);
        p=luku;
        while (*p==' ') ++p;
        if (*p=='+') { strcat(sanat,"plus "); ++p; }
        else if (*p=='-') { strcat(sanat,"miinus "); ++p; }
        if (*p==EOS) return(1);
        q=strchr(p,'.');
        if (q!=NULL) *q=EOS;
        arvo=atof(p);
        if (arvo>1e15) { strcpy(sanat,luku); return(0); }
        if (arvo==0.0)
            {
            strcat(sanat,"nolla ");
            if (q!=NULL) desimaaliosa(q+1,sanat);
            return(1);
            }
        x[0]=x[1]=' '; strcpy(x+2,p);
        p=x+2;
        len=strlen(p);
        n=(strlen(p)-1)/3;
        for (i=n; i>=0; --i)
            {
            m=len-3*i-1;

            if ( (p[m]=='1' || p[m]=='0') && (p[m-1]==' ' || p[m-1]=='0')
                          && (p[m-2]==' ' || p[m-2]=='0') )
                {
                if (p[m]=='1')
                    strcat(sanat,pot3yks[i]);
                else continue;
                }
            else
                {
                for (j=2; j>=0; --j)
                    {
                    if (p[m-j]==' ') continue;
                    if (p[m-j]=='0') continue;
                    if (p[m-j]=='1')
                        {
                        if (j==1)
                            {
                            if (p[m]=='0') { strcat(sanat,"kymmenen "); break; }
  /*       else { strcat(sanat,numero[p[m]-'0']); strcat(sanat,"toista "); break; }
  */                        else
                                {
                                strcpy(y,numero[p[m]-'0']);
                                y[strlen(y)-1]=EOS;
                                strcat(sanat,y); strcat(sanat,"toista ");
                                break;
                                }

                            }
                        else if (j==2)
                            strcat(sanat,"sata ");
                        else
                            strcat(sanat,"yksi");
                        }
                    else
                        {
                        strcat(sanat,numero[p[m-j]-'0']);
                        strcat(sanat,pot[j]);
                        }
                    }
                strcat(sanat,pot3mon[i]);
                }
            }
        if (q!=NULL)
            desimaaliosa(q+1,sanat);
        return(1);
        }

static int desimaaliosa(char *s,char *sanat)
        {
        char *p;

        strcat(sanat,"pilkku ");
        p=s;
        while (*p)
            {
            strcat(sanat,numero[*p-'0']);
            ++p;
            }
        return(1);
        }

