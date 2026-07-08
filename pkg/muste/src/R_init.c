#include "muste.h"
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include "survolib.h"
#include <stdlib.h> // for NULL

/* .Call calls */
extern SEXP do_readSurvo(SEXP);
extern SEXP do_writeSurvo(SEXP, SEXP, SEXP);
extern SEXP Muste_Command(SEXP);
extern SEXP Muste_Editor(SEXP);
extern SEXP Muste_Edtdim(SEXP);
extern SEXP Muste_Edtgoto(SEXP);
extern SEXP Muste_Eventloop(SEXP);
extern SEXP Muste_ExpandPath(SEXP);
extern SEXP Muste_Selection(SEXP);
extern SEXP Survo_FindRegFunc(SEXP);

static const R_CallMethodDef CallEntries[] = {{"do_readSurvo", (DL_FUNC)&do_readSurvo, 1},
                                              {"do_writeSurvo", (DL_FUNC)&do_writeSurvo, 3},
                                              {"Muste_Command", (DL_FUNC)&Muste_Command, 1},
                                              {"Muste_Editor", (DL_FUNC)&Muste_Editor, 1},
                                              {"Muste_Edtdim", (DL_FUNC)&Muste_Edtdim, 1},
                                              {"Muste_Edtgoto", (DL_FUNC)&Muste_Edtgoto, 1},
                                              {"Muste_Eventloop", (DL_FUNC)&Muste_Eventloop, 1},
                                              {"Muste_ExpandPath", (DL_FUNC)&Muste_ExpandPath, 1},
                                              {"Muste_Selection", (DL_FUNC)&Muste_Selection, 1},
                                              {"Survo_FindRegFunc", (DL_FUNC)&Survo_FindRegFunc, 1},
                                              {NULL, NULL, 0}};

void R_init_muste(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);

    R_RegisterCCallable("muste", "data_open3", (DL_FUNC)data_open3);
    R_RegisterCCallable("muste", "data_close", (DL_FUNC)data_close);
    R_RegisterCCallable("muste", "muste_iconv", (DL_FUNC)muste_iconv);
    R_RegisterCCallable("muste", "fi_load", (DL_FUNC)fi_load);
    R_RegisterCCallable("muste", "fi_alpha_load", (DL_FUNC)fi_alpha_load);
    R_RegisterCCallable("muste", "muste_malloc", (DL_FUNC)muste_malloc);
    R_RegisterCCallable("muste", "muste_free", (DL_FUNC)muste_free);

    R_useDynamicSymbols(dll, FALSE);
}
