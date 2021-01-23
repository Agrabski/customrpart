/*
 * The main entry point for recursive partitioning routines.
 *
 * Input variables:
 *      ncat    = # categories for each var, 0 for continuous variables.
 *      method  = 1 - anova
 *                2 - exponential survival
 *		  3 - classification
 *	          4 - user defined callback
 *      opt     = vector of options.  Same order as rpart.control, as a vector
 *                   of doubles.
 *      parms   = extra parameters for the split function, e.g. poissoninit
 *      xvals    = number of cross-validations to do
 *      xgrp     = indices for the cross-validations
 *      ymat    = vector of response variables
 *      xmat    = matrix of continuous variables
 *      ny      = number of columns of the y matrix (it is passed in as a
 *                  vector)
 *      wt       = vector of case weights
 *
 * Returned: a list with elements
 *      which = vector of final node numbers for each input obs
 *      cptable = the complexity table
 *      dsplit = for each split, numeric variables (doubles)
 *      isplit = for each split, integer variables
 *      dnode =  for each node, numeric variables
 *      inode =  for each node, integer variables
 *
 * Naming convention: ncat = pointer to an integer vector, ncat2 = the
 *   input R object (SEXP) containing that vector, ncat3 = an output S object
 *   containing that vector.
 */
#define MAINRP
#include <math.h>
#include "rpart.h"
#include "node.h"
#include "func_table.h"
#include "rpartproto.h"

SEXP
rpart(SEXP variable_type, SEXP method2, SEXP opt2,
      SEXP parms2, SEXP xvals2, SEXP xgrp2,
      SEXP ymat2, SEXP xmat2, SEXP wt2, SEXP ny2, SEXP cost2)
{

    pNode tree;          /* top node of the tree */
    char *errmsg;
    int i, j, k, number_of_subjects;
    int maxcat;
    double temp;
    int *savesort = NULL /* -Wall */ ;
    double *dptr;               /* temp */
    int *iptr;
    /*
     * pointers to R objects
     */
    int *ncat, *xgrp;
    int xvals;
    double *wt, *parms;

    /*
     * Return objects for R -- end in "3" to avoid overlap with internal names
     */
    SEXP which3, cptable3, dsplit3, isplit3, csplit3 = R_NilValue, /* -Wall */
	dnode3, inode3;

    /* work arrays for the return process */
    int nodecount, catcount, splitcount;
    double **ddnode, *ddsplit[3];
    int *iinode[6], *iisplit[3];
    int **ccsplit;
    double scale;
    CpTable cp;

    ncat = INTEGER(variable_type);
    xgrp = INTEGER(xgrp2);
    xvals = asInteger(xvals2);
    wt = REAL(wt2);
    parms = REAL(parms2);
    /*
     * initialize the splitting functions from the function table
     */
    if (asInteger(method2) <= NUM_METHODS) {
	i = asInteger(method2) - 1;
	rp_init = func_table[i].init_split;
	rp_choose = func_table[i].choose_split;
	rp_eval = func_table[i].eval;
	rp_error = func_table[i].error;
	rp.num_y = asInteger(ny2);
    } else
	error(_("Invalid value for 'method'"));

    /*
     * set some other parameters
     */
    dptr = REAL(opt2);
    rp.min_node = (int) dptr[1];
    rp.min_split = (int) dptr[0];
    rp.complexity = dptr[2];
    rp.max_primary_splits = (int) dptr[3] + 1;      /* max primary splits =
					   max competitors + 1 */
    if (rp.max_primary_splits < 1)
	rp.max_primary_splits = 1;
    rp.maximum_surogate_splits = (int) dptr[4];
    rp.usesurrogate = (int) dptr[5];
    rp.sur_agree = (int) dptr[6];
    rp.maxnode = (int) pow((double) 2.0, (double) dptr[7]) - 1;
    rp.number_of_subjects = nrows(xmat2);
    number_of_subjects = rp.number_of_subjects;                   /* I get tired of typing "rp.n" 100 times
				 * below */
    rp.predictor_count = ncols(xmat2);
    rp.variable_types = INTEGER(variable_type);
    rp.wt = wt;
    rp.iscale = 0.0;
    rp.variable_cost = REAL(cost2);

    /*
     * create the "ragged array" pointers to the matrix
     *   x and missmat are in column major order
     *   y is in row major order
     */
    dptr = REAL(xmat2);
    rp.xdata = (double **) ALLOC(rp.predictor_count, sizeof(double *));
    for (i = 0; i < rp.predictor_count; i++) {
	rp.xdata[i] = dptr;
	dptr += number_of_subjects;
    }
    rp.ydata = (double **) ALLOC(number_of_subjects, sizeof(double *));

    dptr = REAL(ymat2);
    for (i = 0; i < number_of_subjects; i++) {
	rp.ydata[i] = dptr;
	dptr += rp.num_y;
    }
    /*
     * allocate some scratch
     */
    rp.tempvec = (int *) ALLOC(number_of_subjects, sizeof(int));
    rp.xtemp = (double *) ALLOC(number_of_subjects, sizeof(double));
    rp.ytemp = (double **) ALLOC(number_of_subjects, sizeof(double *));
    rp.wtemp = (double *) ALLOC(number_of_subjects, sizeof(double));

    /*
     * create a matrix of sort indices, one for each continuous variable
     *   This sort is "once and for all".
     * I don't have to sort the categoricals.
     */
    rp.sort_index_matrix = (int **) ALLOC(rp.predictor_count, sizeof(int *));
    rp.sort_index_matrix[0] = (int *) ALLOC(number_of_subjects * rp.predictor_count, sizeof(int));
    maxcat = 0;
    for (i = 0; i < rp.predictor_count; i++) {
	rp.sort_index_matrix[i] = rp.sort_index_matrix[0] + i * number_of_subjects;
	for (k = 0; k < number_of_subjects; k++) {
	    if (!R_FINITE(rp.xdata[i][k])) {
		rp.tempvec[k] = -(k + 1);       /* this variable is missing */
		rp.xtemp[k] = 0;        /* avoid weird numerics in S's NA */
	    } else {
		rp.tempvec[k] = k;
		rp.xtemp[k] = rp.xdata[i][k];
	    }
	}
	if (ncat[i] == 0)
	    mysort(0, number_of_subjects - 1, rp.xtemp, rp.tempvec);
	else if (ncat[i] > maxcat)
	    maxcat = ncat[i];
	for (k = 0; k < number_of_subjects; k++)
	    rp.sort_index_matrix[i][k] = rp.tempvec[k];
    }

    /*
     * save away a copy of the rp.sorts, if needed for xval
     */
    if (xvals > 1) {
	savesort = (int *) ALLOC(number_of_subjects * rp.predictor_count, sizeof(int));
	memcpy(savesort, rp.sort_index_matrix[0], number_of_subjects * rp.predictor_count * sizeof(int));
    }

    /*
     * And now the last of my scratch space
     */
    if (maxcat > 0) {
	rp.csplit = (int *) ALLOC(3 * maxcat, sizeof(int));
	rp.lwt = (double *) ALLOC(2 * maxcat, sizeof(double));
	rp.left = rp.csplit + maxcat;
	rp.right = rp.left + maxcat;
	rp.rwt = rp.lwt + maxcat;
    } else
	rp.csplit = (int *) ALLOC(1, sizeof(int));

    /*
     * initialize the top node of the tree
     */
    errmsg = _("unknown error");
    which3 = PROTECT(allocVector(INTSXP, number_of_subjects));
    rp.which = INTEGER(which3);
    temp = 0;
    for (i = 0; i < number_of_subjects; i++) {
	rp.which[i] = 1;
	temp += wt[i];
    }
    i = (*rp_init) (number_of_subjects, rp.ydata, maxcat, &errmsg, parms, &rp.num_resp, 1, wt);
    if (i > 0)
	error(errmsg);

    nodesize = sizeof(Node) + (rp.num_resp - 20) * sizeof(double);
    tree = (pNode) ALLOC(1, nodesize);
    memset(tree, 0, nodesize);
    tree->num_obs = number_of_subjects;
    tree->sum_wt = temp;

    (*rp_eval) (number_of_subjects, rp.ydata, tree->response_est, &(tree->risk), wt);
    tree->complexity = tree->risk;
    rp.alpha = rp.complexity * tree->risk;

    /*
     * Do the basic tree
     */
    partition(1, tree, &temp, 0, number_of_subjects);
    CpTable cptable = (CpTable) ALLOC(1, sizeof(cpTable));
    cptable->cp = tree->complexity;
    cptable->risk = tree->risk;
    cptable->nsplit = 0;
    cptable->forward = 0;
    cptable->xrisk = 0;
    cptable->xstd = 0;
    rp.num_unique_cp = 1;

    if (tree->rightson) {
	make_cp_list(tree, tree->complexity, cptable);
	make_cp_table(tree, tree->complexity, 0);
	if (xvals > 1) {
	    xval(xvals, cptable, xgrp, maxcat, &errmsg, parms, savesort);
	}
    }
    /*
     * all done, create the return list for R
     * first the cp table
     */
    scale = 1 / tree->risk;
    i = 0;
    cptable3 = PROTECT(allocMatrix(REALSXP, xvals > 1 ? 5 : 3,
				   rp.num_unique_cp));
    dptr = REAL(cptable3);
    for (cp = cptable; cp; cp = cp->forward) {
	dptr[i++] = cp->cp * scale;
	dptr[i++] = cp->nsplit;
	dptr[i++] = cp->risk * scale;
	if (xvals > 1) {
	    dptr[i++] = cp->xrisk * scale;
	    dptr[i++] = cp->xstd * scale;
	}
    }

    /*
     * Return the body of the tree
     *  For each component we first create a vector to hold the
     *  result, then a ragged array index into the vector.
     * The rpmatrix routine then fills everything in.
     */
    rpcountup(tree, &nodecount, &splitcount, &catcount);
    dnode3 = PROTECT(allocMatrix(REALSXP, nodecount, (3 + rp.num_resp)));
    ddnode = (double **) ALLOC(3 + rp.num_resp, sizeof(double *));
    dptr = REAL(dnode3);
    for (i = 0; i < 3 + rp.num_resp; i++) {
	ddnode[i] = dptr;
	dptr += nodecount;
    }

    dsplit3 = PROTECT(allocMatrix(REALSXP, splitcount, 3));
    dptr = REAL(dsplit3);
    for (i = 0; i < 3; i++) {
	ddsplit[i] = dptr;
	dptr += splitcount;
	for (j = 0; j < splitcount; j++)
	    ddsplit[i][j] = 0.0;
    }

    inode3 = PROTECT(allocMatrix(INTSXP, nodecount, 6));
    iptr = INTEGER(inode3);
    for (i = 0; i < 6; i++) {
	iinode[i] = iptr;
	iptr += nodecount;
    }

    isplit3 = PROTECT(allocMatrix(INTSXP, splitcount, 3));
    iptr = INTEGER(isplit3);
    for (i = 0; i < 3; i++) {
	iisplit[i] = iptr;
	iptr += splitcount;
    }

    if (catcount > 0) {
	csplit3 = PROTECT(allocMatrix(INTSXP, catcount, maxcat));
	ccsplit = (int **) ALLOC(maxcat, sizeof(int *));
	iptr = INTEGER(csplit3);
	for (i = 0; i < maxcat; i++) {
	    ccsplit[i] = iptr;
	    iptr += catcount;
	    for (j = 0; j < catcount; j++)
		ccsplit[i][j] = 0;      /* zero it out */
	}
    } else
	ccsplit = NULL;

    rpmatrix(tree, rp.variable_types, ddsplit, iisplit, ccsplit, ddnode, iinode, 1);
    free_tree(tree, 0);         /* let the memory go */

    /*
     * Fix up the 'which' array
     *  Nodes are sometimes trimmed during the
     *  tree building, and 'which' is not updated in that case
     */
    for (i = 0; i < number_of_subjects; i++) {
	k = rp.which[i];
	do {
	    for (j = 0; j < nodecount; j++)
		if (iinode[0][j] == k) {
		    rp.which[i] = j + 1;
		    break;
		}
	    k /= 2;
	} while (j >= nodecount);
    }

    /* Create the output list */
    int nout = catcount > 0 ? 7 : 6;
    SEXP rlist = PROTECT(allocVector(VECSXP, nout));
    SEXP rname = allocVector(STRSXP, nout);
    setAttrib(rlist, R_NamesSymbol, rname);
    SET_VECTOR_ELT(rlist, 0, which3);
    SET_STRING_ELT(rname, 0, mkChar("which"));
    SET_VECTOR_ELT(rlist, 1, cptable3);
    SET_STRING_ELT(rname, 1, mkChar("cptable"));
    SET_VECTOR_ELT(rlist, 2, dsplit3);
    SET_STRING_ELT(rname, 2, mkChar("dsplit"));
    SET_VECTOR_ELT(rlist, 3, isplit3);
    SET_STRING_ELT(rname, 3, mkChar("isplit"));
    SET_VECTOR_ELT(rlist, 4, dnode3);
    SET_STRING_ELT(rname, 4, mkChar("dnode"));
    SET_VECTOR_ELT(rlist, 5, inode3);
    SET_STRING_ELT(rname, 5, mkChar("inode"));
    if (catcount > 0) {
	SET_VECTOR_ELT(rlist, 6, csplit3);
	SET_STRING_ELT(rname, 6, mkChar("csplit"));
    }

    UNPROTECT(1 + nout);
    return rlist;
}
