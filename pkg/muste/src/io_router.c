extern int sur_locate_tcltk(int row,int col);
extern int sur_locate_html5(int row,int col);

int sur_locate_router(int row,int col)
{
    sur_locate_tcltk(row,col);
    sur_locate_html5(row,col);
    return(1);
}

