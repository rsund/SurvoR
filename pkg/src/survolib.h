#include <stdio.h>
#include "survodat.h"

/* output.c */
extern int sur_print();
extern int output_open();
extern int output_line();
extern int output_close();

/* data.c */
extern int data_read_open();
extern int data_open();
extern int data_open2();
extern void data_close();
extern int data_load();
extern int mask();
extern int mask_sort();
extern void scales();
extern int scale_ok();
extern int activated(SURVO_DATA *,char);
extern int conditions();
extern int unsuitable();
extern int mat_load();
extern int matrix_load();

extern int matrix_save();

/* edit.c */
extern int fconv();
extern int fnconv();
extern char *muste_itoa();
extern char *strupr();
extern int split();
extern int splitp();
extern int sur_strcmpi();
extern int wfind();

extern int spfind();
extern int spec_init();
extern int sp_init();
extern int hae_apu();
extern int edline2();
extern void edread();
extern int edwrite();
extern int s_init();
extern int s_end();

/*
int filename(char *edfile, char *field);
int file_name_ext(char *name,char *ext);

int edit_file_not_found(char *edfile);
int edload32_err(char *s,char *edfile);
int ed_not_space();

int shadinit();
int creatshad(unsigned int j);

int ed_malloc(unsigned int ed1,unsigned int ed2,unsigned int edshad);
int edload32(char *edfile);
int edload(char *field,int shad);
int edsave32(char *edfile,int shad);
int edsave(char *field,int shad,int check);
void edt_talletus(char *s);

int xxd(int i);
int xxl(long li);
int xxs(char *x);
int xxe(double a);
int yys(char *x);
int yyl(long *pli);
int yyu(unsigned int *pi);
int yye(double *pa);
int yyd(int *pi);
int sur_dump(char *siirtop);
int restore_dump(char *siirtop);

extern int s_edt();
*/
