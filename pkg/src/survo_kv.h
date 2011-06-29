void s_err(char *, ...);
void s_disp(char *, ...);
void s_specu(void); // void since 8.6.2004
int kv_stru(char *);
int kv_strl(char *);
int kv_edline(char *, int, int);
int Leap_Year(int);
unsigned int kv_toupper(unsigned char);
void usage_info(void);
int space_split(char *, char **, int);
void trim(char *, char *);

// declarations for SURVO functions (avoid warnings from NMAKE)

int s_init(char *);
int s_end(char *);
int tut_init(void);
int tut_end(void);
int spec_init(int);
int spfind(char *);
int headline(char *);
int write_string(char *, int, int, int, int);
int sur_locate(int, int);
int sur_cls(char);
int sur_wait(long, void(void), int);
int sur_print(char *);
int sur_delete(char *); // jokeri k„y (DeleteFile)
int sur_delete1(char *); // ei jokeri (DeleteFile)
int sur_rename(char *, char *); // vanha,uusi (MoveFile)
int sur_copy_file(char *, char *); // source,target (CopyFile) (3. false, kirj.p„„lle)
int split(char *, char **, int);
int edread(char *, int);
int edwrite(char *, unsigned int, int);
int lastline2(void);
int edline2(char *, int, int);
int nextch(char *);
int getch(void);
int getcm(void);
int prompt(char *, char *, int);

#ifdef SURVO_DATA
int data_open(char *, SURVO_DATA *);
int data_open2(char *, SURVO_DATA *, int, int, int);
int fi_open(char *, SURVO_DATA_FILE *);
int varfind(SURVO_DATA *, char *);
int data_load(SURVO_DATA *, long, int, double *);
int data_alpha_load(SURVO_DATA *, long, int, char *);
int data_save(SURVO_DATA *, long, int, double);
int fi_save(SURVO_DATA_FILE *, long, int, double *);
int data_alpha_save(SURVO_DATA *, long, int, char *);
int fi_alpha_save(SURVO_DATA_FILE *, long, int, char *);
int data_close(SURVO_DATA *);
int fi_close(SURVO_DATA_FILE *);
int fi_increase_n(SURVO_DATA_FILE *, long);
int conditions(SURVO_DATA *);
int unsuitable(SURVO_DATA *, long);
int mask(SURVO_DATA *);
int activated(SURVO_DATA *, char);
int fi_puts(SURVO_DATA_FILE *, char *, int, long);
#endif

int fconv(double, char *, char *);
int fnconv(double, int, char *);
int init_remarks(void);
int rem_pr(char *);
int wait_remarks(int);
int hae_apu(char *, char *);
int subst_survo_path(char *);
int netd(char *s); // 1, jos s alkaa '\\':ll„ (verkkopolku)

extern int spn;
