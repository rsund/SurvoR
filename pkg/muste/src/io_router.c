extern int sur_locate_tcltk(int row,int col);
extern int sur_locate_html5(int row,int col);
extern int survo_webedit;

int sur_locate_router(int row,int col)
{
    sur_locate_tcltk(row,col);
    if (survo_webedit==1) sur_locate_html5(row,col);
    return(1);
}

