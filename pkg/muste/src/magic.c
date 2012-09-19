#include "muste.h"
#include <stdio.h>
#include <stdlib.h>
#include "survo.h"
#include "survoext.h"
#include "survolib.h"

static int *mamat;
static int dim,results_line;

static int varaa_tilat(int dim);
static void tyhjenna(int dim);
static int magic_staircase(int dim);
static int magic_pyramid(int dim);
static int magic_variation_staircase(int dim);
static int magic_knights_move(int dim,int offs,int move);
static void putnum(int x, int y, int count);
static int checknum(int x, int y);
static void print_line(void);
static void print_square(int dim);
static void print_square_info(int dim, int offs);
static int is_magic(int dim);
static int magic_random(int dim);

int muste_magic(char *argv)
	{
	int i,j,method,spec,offs,raja,magic;

	s_init(argv);

	if (g<3)
		{
		sur_print("\nUsage: MAGIC CREATE <dim>");
		WAIT; return(1);
		}

	results_line=r1+r;
	i=sp_init(r1+r-1); if (i<0) return(-1);
	dim=atoi(word[2]);
	i=varaa_tilat(dim); if (i<0) return(-1);
	
	if (dim%2==0)
		{
		sur_print("\nCurrently only odd dimensions allowed!");
		WAIT; return(-1);
		}
	method=1; // Default method for odd dimensions
	spec=1; 
	offs=dim>>1;
	magic=0;
	i=spfind("METHOD");
    if (i>=0) { method=atoi(spb[i]); }

	i=spfind("OFFSET");
	if (i>=0) { offs=atoi(spb[i])-1; }
	if (offs<0 || offs>=(dim*dim))
		{
		if (strcmp(spb[i],"ALL")==0) offs=-1;
		else
			{
			sprintf(sbuf,"\nERROR! OFFSET must be between 1 and %d!",dim*dim);
			sur_print(sbuf); WAIT;
			return(-1);
			}
		}

	raja=offs+1;
	if (offs<0)  // ALL
		{ 
		raja=dim*dim;
		offs=0;
		}
	for (j=offs; j<raja; j++)
		{
		tyhjenna(dim);
		
		switch(method)
			{
			case 0:
				i=magic_random(dim);
				break;
			case 1:
				i=magic_staircase(dim);
				break;
			case 2:
				i=magic_pyramid(dim);
				break;
			case 3:
				i=magic_variation_staircase(dim);
				break;
			case 4:
				i=spfind("MOVE");
				if (i>=0) { spec=atoi(spb[i]); }
				if (spec<1 || spec>8)
					{
					sur_print("\nERROR! Knight's move should be:");
					sur_print("\n1 - 2 up, 1 right     5 - 2 down, 1 left");
					sur_print("\n2 - 1 up, 2 right     6 - 1 down, 2 left");
					sur_print("\n3 - 2 right, 1 down   7 - 2 left, 1 up");
					sur_print("\n4 - 1 right, 2 down   8 - 1 left, 2 up");
					sur_print("\nUse specification MOVE to define the move.");  
					WAIT; return(-1);
					}		
				i=magic_knights_move(dim,j,spec);
				break;
			default:	
				break;
			}
		i=is_magic(dim);	
		if (i<0)
			{
			if (method==0) print_square(dim);
			
/*
			sprintf(sbuf,"Non magic! Offset=%d",j+1); print_line();
			print_square(dim);
			
			if (i<99) sprintf(sbuf,"Wrong sum(s) in diagonal(s)!"); print_line();
			if (i<9) sprintf(sbuf,"Wrong sum(s) in column(s)!"); print_line();
			sprintf(sbuf,"Wrong sum(s) in row(s)!"); print_line();
*/			
			}	
		else
			{
			magic++;
			print_square_info(dim,j+1);
			print_square(dim);
			sprintf(sbuf,"                                     ");
			print_line();
			}
		}
	if (magic<1)
		{
		sprintf(sbuf,"Non magic!                                         "); print_line();
		}
	s_end(argv);
	return(1);
	}
        
static int varaa_tilat(int dim)
	{
	int i;
	mamat=(int *)muste_malloc((dim+2)*(dim+2)*sizeof(int));
	if (mamat==NULL) { sur_print("\nNot enough memory!"); WAIT; return(-1); }
	tyhjenna(dim);
	return(1);
	}

static void tyhjenna(int dim)
	{
	int i;
	
	for (i=0; i<dim*dim; i++) mamat[i]=0;
	return;
	}
		
static int magic_staircase(int dim) // Moran's method A, the staircase method
	{
	int x,y,a,b,count;
	
	count=1; y=0; x=(dim>>1); // Rule one
	putnum(x,y,count++);

	while (count<=dim*dim)
		{	
		a=x; b=y;
		x++; y--; // Rule two, diagonally upwards
		if (y<0) y=dim-1; if (x>=dim) x=0;
		if (checknum(x,y)) putnum(x,y,count++);
		else
			{
			x=a; y=b+1;
			if (y>=dim) y=0; if (y<0) y=dim-1;
			putnum(x,y,count++);	
			}
		}
	}

static int magic_pyramid(int dim) // Moran's method B, the pyramid method
	{
	int i,j,x,y,a,b,count;
	
	count=1;
	y=(dim>>1); x=-y;	
	for (i=0; i<dim; i++)
		{
		for (j=0; j<dim; j++)
			{
			a=x+j; b=y-j;
			if (a<0) a=dim+a; if (a>=dim) a=a-dim;
			if (b<0) b=dim+b; if (b>=dim) b=b-dim;
			putnum(a,b,count++);
			}
		x++; y++;	
		}
	}

static int magic_variation_staircase(int dim) // Moran's method C, Variation of the staircase method
	{
	int x,y,a,b,count;
	
	count=1; x=y=(dim>>1); x++; // Rule one
	putnum(x,y,count++);

	while (count<=dim*dim)
		{	
		a=x; b=y;
		x++; y--; // Rule two, diagonally upwards
		if (y<0) y=dim-1; if (x>=dim) x=0;
		if (checknum(x,y)) putnum(x,y,count++);
		else
			{
			x=a+2; y=b;
			if (x>=dim) x=x-dim; if (y<0) y=dim-1;
			putnum(x,y,count++);	
			}
		}
	}

static int magic_knights_move(int dim,int offs,int move) // Moran's method D, The knight's move method
	{
	int x,y,a,b,count;
	
	count=1; 
	y=(int)offs/dim; x=offs%dim; // Starting place
	putnum(x,y,count++);

	while (count<=dim*dim)
		{	
		a=x; b=y;
		switch (move)
			{
			default:
			case 1:	y-=2; x+=1; break; // 2 up 1 right
			case 2:	y-=1; x+=2; break; // 1 up 2 right
			case 3:	y+=1; x+=2; break; // 1 down 2 right
			case 4:	y+=2; x+=1; break; // 2 down 1 right 
			case 5:	y+=2; x-=1; break; // 2 down 1 left
			case 6:	y+=1; x-=2; break; // 1 down 2 left
			case 7:	y-=1; x-=2; break; // 1 up 2 left
			case 8:	y-=2; x-=1; break; // 2 up 1 left
			}
		if (x<0) x=dim+x; if (x>=dim) x=x-dim;
		if (y<0) y=dim+y; if (y>=dim) y=y-dim;			
		if (checknum(x,y)) putnum(x,y,count++);
		else
			{
			x=a; y=b+1; // Blocked move one down
			if (x<0) x=dim+x; if (x>=dim) x=x-dim;
			if (y<0) y=dim+y; if (y>=dim) y=y-dim;			
			putnum(x,y,count++);	
			}
		}
	}


	
static void putnum(int x, int y, int count)
	{
// Rprintf("\nx: %d, y: %d, count: %d, offs: %d",x,y,count,y*dim+x);	
	mamat[y*dim+x]=count;
	}
	
static int checknum(int x, int y)
	{
// Rprintf("\nx: %d, y: %d, offs: %d",x,y,y*dim+x);	
	if (mamat[y*dim+x]) return(FALSE);
	return(TRUE);
	}

static void print_line(void)
	{
    edwrite(sbuf,results_line++,1);
	}
	
static void print_square(int dim)
	{
	int i,j;
	char luku[64];
	
	for (j=0; j<dim; j++)
		{
		*sbuf=EOS;
		for (i=0; i<dim; i++)
			{
			muste_itoa(mamat[j*dim+i],luku,10);
			strcat(sbuf,luku); strcat(sbuf," ");
			}
		print_line();
		}
	}
	
static void print_square_info(int dim, int offs)
	{
	int constant;
	constant=(1+dim*dim)*dim/2;
	sprintf(sbuf,"Square of order %d (with constant: %d), offset: %d",dim,constant,offs);
	print_line();
	}
	
static int is_magic(int dim)
	{
	int i,j,sumr,sumc,sumd1,sumd2,constant,nodia,norow,nocol,nomagic;
	constant=(1+dim*dim)*dim/2;
	nodia=0; norow=0; nocol=0; nomagic=0;
	
	sumd1=0; sumd2=0;
	for (j=0; j<dim; j++)
		{
		sumr=0; sumc=0;
		for (i=0; i<dim; i++)
			{
			sumr+=mamat[j*dim+i];
			sumc+=mamat[i*dim+j];
			}
		if (sumr!=constant) norow++;
		if (sumr!=constant) nocol++;
		sumd1+=mamat[j*dim+j];
		sumd2+=mamat[j*dim+dim-j-1];
		}
	if (sumd1!=constant) nodia++;
	if (sumd2!=constant) nodia++;
	if (norow>0) nomagic=-1;
	if (nocol>0) nomagic=-10;
	if (nodia>0) nomagic=-100; // Semi-magic (only cols and rows add up to magic sum)
// ADD pandiagonal (all broken diagonals add up to magic sum)
// ADD associative (all complementary pairs add up to same constant (n*n+1)
// ADD ultramagic = pandiagonal and associative
	return(nomagic);
	}

static int rnd(int min,int max)
	{
	return((unsigned int)(uniform_dev()*(max-min)+min));
	}

static void swap(int *src,int *dest)
	{
	int temp;
	temp=*dest;
	*dest=*src;
	*src=temp;
	return;
	}

static void shuffle(int dim, int *mat) 
	{ 
	int rand; 
	for(int i=dim*dim-1;i>=0;i--) 
		{ 
		rand=rnd(0,i); 
		swap(mat+i,mat+rand); 
		} 
	}



static int magic_random(int dim)
	{
	int *ormat;
	int i;

	ormat=(int *)muste_malloc((dim+2)*(dim+2)*sizeof(int));
	if (ormat==NULL) { sur_print("\nNot enough memory!"); WAIT; return(-1); }	
	for (i=0; i<dim*dim; i++) ormat[i]=i+1;

	spec_rnd_rndseed();
		
	shuffle(dim,ormat);

	for (i=0; i<dim*dim; i++) mamat[i]=ormat[i];
	return(1);
	}	

/*
käytettävissä olevat numerot: ormat = 1...n*n ensin järjestyksessä
+jarj lista sekä +enternode
-jarj lista sekä -enternode
sekoitetaan ormat, mutta säilytetään +-jarjestykset pointtereissa
kiinnitetaan puolet ensimmäisen rivin ja toisen sarakkeen luvuista
päivitetään rivi-, sarake- ja diagonaalisummat
tarkistetaan, onko ok jäljellä olevien numeroiden puolesta:
	* jäljellä olevien tilaan mahtuvien minimisumma+tarksumma <= taikavakio
	* jäljellä olevien tilaan mahtuvien maksimisumma+tarksumma >= taikavakio
täytetään ensimmäinen rivi n-1 asti, valitaan rivin viimeinen
vastaavasti täytetään sarake

*/	
	
	
	
/*	
#include <cstdio>
#include <cstring>
#include <algorithm>

using namespace std;

int N,NN,M[5][5],S;
int sumR[5],sumC[5],sumD1,sumD2;
int needR[5],needC[5],needD1,needD2;
bool found = false,used[26];

void solve(int done){
    if(found) return;

    if(done == NN){
        found = true;

        for(int i = 0;i < N;++i){
            printf("%d",M[i][0]);
            for(int j = 1;j < N;++j) printf(" %d",M[i][j]);
            printf("\n");
        }
    }else{
        int R = 0,C = 0,best = 11;

        for(int i = 0;i < N;++i) for(int j = 0;j < N;++j){
            if(M[i][j] == 0){
                int aux = min(needR[i],needC[j]);

                if(i == j) aux = min(aux,needD1);
                if(i+j == N-1) aux = min(aux,needD2);

                if(aux < best){
                    R = i;
                    C = j;
                    best = aux;
                }
            }
        }

        bool valid;

        for(int i = NN;i >= 1;--i){
            if(!used[i] && sumR[R]+i <= S && sumC[C]+i <= S){
                used[i] = true;
                M[R][C] = i;
                valid = true;

                sumR[R] += i; --needR[R];
                if(needR[R] == 0 && sumR[R] != S) valid = false;

                sumC[C] += i; --needC[C];
                if(needC[C] == 0 && sumC[C] != S) valid = false;

                if(R == C){
                    sumD1 += i;
                    --needD1;
                    if(sumD1 > S || (needD1 == 0 && sumD1 != S)) valid = false;
                }

                if(R+C == N-1){
                    sumD2 += i;
                    --needD2;
                    if(sumD2 > S || (needD2 == 0 && sumD2 != S)) valid = false;
                }

                if(valid) solve(done+1);
                if(found) return;

                used[i] = false;

                sumR[R] -= i; ++needR[R];
                sumC[C] -= i; ++needC[C];

                if(R == C){
                    sumD1 -= i;
                    ++needD1;
                }

                if(R+C == N-1){
                    sumD2 -= i;
                    ++needD2;
                }
            }
        }

        M[R][C] = 0;
    }

    return;
}

int main(){
    scanf("%d",&N);

    NN = N * N;
    S = N * (NN + 1) / 2;

    memset(used,false,sizeof(used));
    memset(sumR,0,sizeof(sumR));
    memset(sumC,0,sizeof(sumC));
    sumD1 = sumD2 = 0;

    for(int i = 0;i < N;++i) needR[i] = needC[i] = N;
    needD1 = needD2 = N;

    int cont = 0;

    for(int i = 0;i < N;++i) for(int j = 0;j < N;++j){
        scanf("%d",&M[i][j]);

        if(M[i][j] != 0){
            used[M[i][j]] = true;
            sumR[i] += M[i][j];
            sumC[j] += M[i][j];
            --needR[i];
            --needC[j];
            ++cont;

            if(i == j){
                sumD1 += M[i][j];
                --needD1;
            }

            if(i+j == N-1){
                sumD2 += M[i][j];
                --needD2;
            }
        }
    }

    solve(cont);

    if(!found) puts("-1");

    return 0;
}


*/	
