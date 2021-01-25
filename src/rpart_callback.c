/*
 * callback routines for "user" splitting functions in rpart
 */

#include <stddef.h>
#include <R.h>
#include <Rinternals.h>
/* don't include rpart.h: it conflicts */

#ifdef ENABLE_NLS
#include <libintl.h>
#define _(String) dgettext ("rpart", String)
#else
#define _(String) (String)
#endif

#include "node.h"


static int ysave;               /* number of columns of y  */
static int rsave;               /* the length of the returned "mean" from the
				 * user's eval routine */
static SEXP expr1;              /* the evaluation expression for splits */
static SEXP expr2;              /* the evaluation expression for values */
static SEXP rho;

static SEXP split_choice_function;
static SEXP split_choice_threshold;
static SEXP column_names;

static double *ydata;           /* pointer to the data portion of yback */
static double *xdata;           /* pointer to the data portion of xback */
static double *wdata;           /* pointer to the data portion of wback */
static int *ndata;              /* pointer to the data portion of nback */

/*
 * The first routine saves the parameters, the location
 *   of the evaluation frame and the 2 expressions to be computed within it,
 *   and away the memory location of the 4 "callback" objects.
 */

SEXP
init_rpcallback(SEXP rhox, SEXP ny, SEXP nr, SEXP expr1x, SEXP expr2x)
{
    SEXP stemp;

    rho = rhox;
    ysave = asInteger(ny);
    rsave = asInteger(nr);
    expr1 = expr1x;
    expr2 = expr2x;

    stemp = findVarInFrame(rho, install("yback"));
    if (!stemp)
	error(_("'yback' not found"));
    ydata = REAL(stemp);
    stemp = findVarInFrame(rho, install("wback"));
    if (!stemp)
	error(_("'wback' not found"));
    wdata = REAL(stemp);
    stemp = findVarInFrame(rho, install("xback"));
    if (!stemp)
	error(_("'xback' not found"));
    xdata = REAL(stemp);
    stemp = findVarInFrame(rho, install("nback"));
    if (!stemp)
	error(_("'nback' not found"));
    ndata = INTEGER(stemp);

    return R_NilValue;
}


SEXP init_split_choice_function(SEXP function, SEXP threshold, SEXP col_names)
{
	split_choice_function = function;
	split_choice_threshold = threshold;
	column_names = col_names;

	return R_NilValue;
}

Split* pick_split(Node* node)
{
	int protections = 3;

	double threshold = asReal(split_choice_threshold);
	int list_length = 0;
	for (Split *current = node->primary; current != NULL; current = current->nextsplit)
		if(current->improvment > threshold)
			list_length++;

	SEXP r_callback_arg_names = PROTECT(allocVector(STRSXP, list_length));
	SEXP r_callback_arg_improvments = PROTECT(allocVector(REALSXP, list_length));

	int index = 0;
	for(Split *current = node->primary; current != NULL; current = current->nextsplit)
		if(current->improvment > threshold)
		{
			SEXP name = PROTECT(STRING_ELT(column_names, current->var_num));

			SET_STRING_ELT(r_callback_arg_names, index, name);
			REAL(r_callback_arg_improvments)[index] = current->improvment;
			protections += 1;
			index++;
		}
	SEXP expression = PROTECT(lang3(split_choice_function,r_callback_arg_names, r_callback_arg_improvments));



	SEXP result = eval(expression, R_GlobalEnv);

	int splitIndex = asInteger(eval(match(r_callback_arg_names, result, 0), R_GlobalEnv));

	index = 0;
	for(Split *current = node->primary; current != NULL; current = current->nextsplit)
		if(current->improvment > threshold)
			if(index == splitIndex)
			{
				//UNPROTECT(protections);
				return current;
			}
			else
				index++;

}

/*
 * This is called by the usersplit init function
 *  For the "hardcoded" user routines, this is a constant written into
 *  their init routine, but here we need to grab it from outside.
 */
void
rpart_callback0(int *nr)
{
    *nr = rsave;
}

/*
 * This is called by the evaluation function
 */
void
rpart_callback1(int n, double *y[], double *wt, double *z)
{
    int i, j, k;
    SEXP value;
    double *dptr;

    /* Copy n and wt into the parent frame */
    for (i = 0, k = 0; i < ysave; i++)
	for (j = 0; j < n; j++)
	    ydata[k++] = y[j][i];


    for (i = 0; i < n; i++)
	wdata[i] = wt[i];
    ndata[0] = n;

    /*
     *  Evaluate the saved expression in the parent frame
     *   The result should be a vector of numerics containing the
     *   "deviance" followed by the "mean"
     */

    /* no need to protect as no memory allocation (or error) below */
    value = eval(expr2, rho);
    if (!isReal(value))
	error(_("return value not a vector"));
    if (LENGTH(value) != (1 + rsave))
	error(_("returned value is the wrong length"));
    dptr = REAL(value);
    for (i = 0; i <= rsave; i++)
	z[i] = dptr[i];
}

/*
 * This part is called by the rpart "split" function.
 * It is expected to return an n-1 length vector of "goodness of split"
 */
void
rpart_callback2(int n, int ncat, double *y[], double *wt,
		double *x, double *good)
{
    int i, j, k;
    SEXP goodness;
    double *dptr;

    for (i = 0, k = 0; i < ysave; i++)
	for (j = 0; j < n; j++)
	    ydata[k++] = y[j][i];

    for (i = 0; i < n; i++) {
	wdata[i] = wt[i];
	xdata[i] = x[i];
    }
    ndata[0] = (ncat > 0) ? -n : n;
	/* the negative serves as a marker for rpart.R */

    /* no need to protect as no memory allocation (or error) below */
    goodness = eval(expr1, rho);
    if (!isReal(goodness))
	error(_("the expression expr1 did not return a vector!"));
    j = LENGTH(goodness);
    dptr = REAL(goodness);

    /*
     * yes, the lengths have already been checked in the C code  --- call
     * this extra documenation then
     */
    if (ncat == 0) {
	if (j != 2 * (n - 1))
	    error("the expression expr1 returned a list of %d elements, %d required",
		  j, 2 * (n - 1));

	for (i = 0; i < j; i++)
	    good[i] = dptr[i];
    } else {
       /*
	* If not all categories were present in X, then the return list
	* will have 2(#categories present) - 1 elements
	* The first element of "good" contains the number of groups found
	*/
	good[0] = (j + 1) / 2;
	for (i = 0; i < j; i++)
	    good[i + 1] = dptr[i];
    }
}
