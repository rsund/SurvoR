#include "muste.h"
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include "survolib.h"

void R_init_muste(DllInfo *dll)
	{
	R_RegisterCCallable("muste", "data_open3", (DL_FUNC)data_open3);
	R_RegisterCCallable("muste", "data_close", (DL_FUNC)data_close);
	R_RegisterCCallable("muste", "muste_iconv", (DL_FUNC)muste_iconv);
	R_RegisterCCallable("muste", "fi_load", (DL_FUNC)fi_load);
	R_RegisterCCallable("muste", "fi_alpha_load", (DL_FUNC)fi_alpha_load);
	R_RegisterCCallable("muste", "muste_malloc", (DL_FUNC)muste_malloc);
	R_RegisterCCallable("muste", "muste_free", (DL_FUNC)muste_free);	
	}
