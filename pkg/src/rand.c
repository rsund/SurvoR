#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "survo.h"
#include "survoext.h"
#include "survolib.h"

static int rand_in_use=0;
static FILE *seedfile;
static unsigned long i1,i2;

extern char **spb;



void seedfile_err(char *s)
        {
        sprintf(sbuf,"\nSeed file error in %s!",s);
        sur_print(sbuf); WAIT;
        return; // RS CHA exit(1);
        }

int outseed()
        {
        int i;
        char x[LLENGTH];

        if (!rand_in_use) return(1);
        i=spfind("OUTSEED"); if (i<0) return(1);
        strcpy(x,spb[i]); if (strchr(x,':')==NULL) { strcpy(x,edisk); strcat(x,spb[i]); }
        seedfile=muste_fopen(x,"wb");
        if (seedfile==NULL) { seedfile_err(x); return(-1); } // RS ADD return
        fprintf(seedfile,"%lu %lu",i1,i2);  // RS FIXME 64bit
        fclose(seedfile);
        return(1);
        }
        
        
/* sur_rand.c 4.6.1993/SM (12.2.1994)

   Execution times: 1000000 numbers
   rand 147 s
   urand  92 s
*/




/* CombTaus */
/* Tezuka, L'Ecuyer, ACM Transactions on Modelling and Computer Simulation
 * April 1991
 *
 */

static short q1=13, q2=2, s1=12, s2=17, p1ms1=19, p2ms2=12, p1mp2=2;
static unsigned long i1,i2,b, mask1=2147483647, mask2=536870911;
static double norm=4.656612873e-10;

static int inseed()
        {
        int i;
        char x[LLENGTH];
        char *s[2];

        i=spfind("INSEED"); if (i<0) return(0);
        strcpy(x,spb[i]); if (strchr(x,':')==NULL) { strcpy(x,edisk); strcat(x,spb[i]); }
        seedfile=muste_fopen(x,"rb");
        if (seedfile==NULL) seedfile_err(x);

// 10.4.2001
        fread(x,100,1,seedfile);
        split(x,s,2); i1=atol(s[0]); i2=atol(s[1]);
//      fscanf(seedfile,"%lu %lu",&i1,&i2);   ei toiminut 10.4.2001
        fclose(seedfile);

        return(1);
        }


static unsigned long sur_randl()
        {
        b=((i1<<q1)^i1)&mask1;
        i1=((i1<<s1)^(b>>p1ms1))&mask1;
        b=((i2<<q2)^i2)&mask2;
        i2=((i2<<s2)^(b>>p2ms2))&mask2;
        return(i1^(i2<<p1mp2));
        }

static int sur_rand_seed(unsigned long n)
        {
        int i;
        unsigned long n2;
        unsigned long m1=357913941;
        unsigned long m2=178956970;
// RS REM        extern unsigned long sur_randl();
/*
10101010101010101010101010101(2:10)=357913941
01010101010101010101010101010(2:10)=178956970
*/
        rand_in_use=1;
        i=inseed();
        if (i==1) return(1);

        n2=237*n+6254269;
        i1=n2&mask1;
        n2=23*n+15783;
        i2=n2&mask2;

        sur_randl(); /* 12.2.1994 */
        return(1);
        }


static double sur_rand()
        {
//        extern unsigned long sur_randl();
        return((double)sur_randl()*norm);
        }

static int sur_srand_seed(unsigned long n)
        { sur_print("\nFunction srand not available!"); WAIT; return(-1); } // RS CHA exit(1) -> return

static double sur_srand()  { return(0.0); }

/* Lewis, Goodman, Miller 1969   a=7^5=16807 */
/* urand() */

static long ua=16807L;
static long um=2147483647L;
static long uq=127773L;
static long ur=2836L;
static long uhi,ulo,utest;

static long useed;

static long sur_urandl()
        {
        uhi=useed/uq;
        ulo=useed-uq*uhi;
        utest=ua*ulo-ur*uhi;
        if (utest>0L) useed=utest; else useed=utest+um;
        return(useed);
        }

static int sur_urand_seed(unsigned long n)
        {
// RS REM        extern long sur_urandl();

        useed=(long)(n);
        sur_urandl(); /* 12.2.1994 */
        return(1);
        }



static double sur_urand()
        {
        return((double)sur_urandl()/2147483647.0);
        }


/*
   A C-program for MT19937, with initialization improved 2002/1/26.
   Coded by Takuji Nishimura and Makoto Matsumoto.

   Before using, initialize the state by using init_genrand(seed)
   or init_by_array(init_key, key_length).

   Copyright (C) 1997 - 2002, Makoto Matsumoto and Takuji Nishimura,
   All rights reserved.

   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions
   are met:

     1. Redistributions of source code must retain the above copyright
        notice, this list of conditions and the following disclaimer.

     2. Redistributions in binary form must reproduce the above copyright
        notice, this list of conditions and the following disclaimer in the
        documentation and/or other materials provided with the distribution.

     3. The names of its contributors may not be used to endorse or promote
        products derived from this software without specific prior written
        permission.

   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
   A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT OWNER OR
   CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
   EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
   PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


   Any feedback is very welcome.
   http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/emt.html
   email: m-mat @ math.sci.hiroshima-u.ac.jp (remove space)
*/

/* Period parameters */
#define N 624
#define M 397
#define MATRIX_A 0x9908b0dfUL   /* constant vector a */
#define UPPER_MASK 0x80000000UL /* most significant w-r bits */
#define LOWER_MASK 0x7fffffffUL /* least significant r bits */

static unsigned long mt[N]; /* the array for the state vector  */
static int mti=N+1; /* mti==N+1 means mt[N] is not initialized */

/* initializes mt[N] with a seed */
static void init_genrand(unsigned long s)
{
    mt[0]= s & 0xffffffffUL;
    for (mti=1; mti<N; mti++) {
        mt[mti] =
            (1812433253UL * (mt[mti-1] ^ (mt[mti-1] >> 30)) + mti);
        /* See Knuth TAOCP Vol2. 3rd Ed. P.106 for multiplier. */
        /* In the previous versions, MSBs of the seed affect   */
        /* only MSBs of the array mt[].                        */
        /* 2002/01/09 modified by Makoto Matsumoto             */
        mt[mti] &= 0xffffffffUL;
        /* for >32 bit machines */
    }
}

/* initialize by an array with array-length */
/* init_key is the array for initializing keys */
/* key_length is its length */
/* slight change for C++, 2004/2/26 */
/**************************************************
void init_by_array(unsigned long init_key[], int key_length)
{
    int i, j, k;
    init_genrand(19650218UL);
    i=1; j=0;
    k = (N>key_length ? N : key_length);
    for (; k; k--) {
        mt[i] = (mt[i] ^ ((mt[i-1] ^ (mt[i-1] >> 30)) * 1664525UL))
          + init_key[j] + j; // non linear
        mt[i] &= 0xffffffffUL; // for WORDSIZE > 32 machines
        i++; j++;
        if (i>=N) { mt[0] = mt[N-1]; i=1; }
        if (j>=key_length) j=0;
    }
    for (k=N-1; k; k--) {
        mt[i] = (mt[i] ^ ((mt[i-1] ^ (mt[i-1] >> 30)) * 1566083941UL))
          - i; // non linear
        mt[i] &= 0xffffffffUL; // for WORDSIZE > 32 machines
        i++;
        if (i>=N) { mt[0] = mt[N-1]; i=1; }
    }

    mt[0] = 0x80000000UL; // MSB is 1; assuring non-zero initial array
}
*******************************************/
/* generates a random number on [0,0xffffffff]-interval */
static unsigned long genrand_int32(void)
{
    unsigned long y;
    static unsigned long mag01[2]={0x0UL, MATRIX_A};
    /* mag01[x] = x * MATRIX_A  for x=0,1 */

    if (mti >= N) { /* generate N words at one time */
        int kk;

        if (mti == N+1)   /* if init_genrand() has not been called, */
            init_genrand(5489UL); /* a default initial seed is used */

        for (kk=0;kk<N-M;kk++) {
            y = (mt[kk]&UPPER_MASK)|(mt[kk+1]&LOWER_MASK);
            mt[kk] = mt[kk+M] ^ (y >> 1) ^ mag01[y & 0x1UL];
        }
        for (;kk<N-1;kk++) {
            y = (mt[kk]&UPPER_MASK)|(mt[kk+1]&LOWER_MASK);
            mt[kk] = mt[kk+(M-N)] ^ (y >> 1) ^ mag01[y & 0x1UL];
        }
        y = (mt[N-1]&UPPER_MASK)|(mt[0]&LOWER_MASK);
        mt[N-1] = mt[M-1] ^ (y >> 1) ^ mag01[y & 0x1UL];

        mti = 0;
    }

    y = mt[mti++];

    /* Tempering */
    y ^= (y >> 11);
    y ^= (y << 7) & 0x9d2c5680UL;
    y ^= (y << 15) & 0xefc60000UL;
    y ^= (y >> 18);

    return y;
}

/* generates a random number on [0,0x7fffffff]-interval */
static long genrand_int31(void)
{
    return (long)(genrand_int32()>>1);
}

/* generates a random number on [0,1]-real-interval */
static double genrand_real1(void)
{
    return genrand_int32()*(1.0/4294967295.0);
    /* divided by 2^32-1 */
}

/* generates a random number on [0,1)-real-interval */
static double genrand_real2(void)
{
    return genrand_int32()*(1.0/4294967296.0);
    /* divided by 2^32 */
}

/* generates a random number on (0,1)-real-interval */
static double genrand_real3(void)
{
    return (((double)genrand_int32()) + 0.5)*(1.0/4294967296.0);
    /* divided by 2^32 */
}

/* generates a random number on [0,1) with 53-bit resolution*/
static double genrand_res53(void)
{
    unsigned long a=genrand_int32()>>5, b=genrand_int32()>>6;
    return(a*67108864.0+b)*(1.0/9007199254740992.0);
}
/* These real versions are due to Isaku Wada, 2002/01/09 added */




        

double sur_rand0(double x,int type)
        {
        static int next=0;

        if (x==0.0)
            {
            sur_print("\nArgument 0 not permitted in this rand function!");
            WAIT; return(0.0); // RS CHAR exit(1) -> return
            }

        switch (type)
            {
          case 1:
            if (!next)
               { sur_rand_seed((unsigned long)x); next=1; }
            return (sur_rand());
          case 2:
            if (!next)
               { sur_urand_seed((unsigned long)x); next=1; }
            return (sur_urand());
          case 3:
            if (!next)
               { if(sur_srand_seed((unsigned long)x)<0) return(0.0); next=1; } // RS ADD return
            return (sur_srand());          
          case 4:         
            if (!next)
               { init_genrand((unsigned long)x); next=1; }
            return (genrand_real2());

            }
        return(0.0);
        }



