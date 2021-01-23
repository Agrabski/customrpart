/*
 * Calculate the surrogate splits for a node and its primary
 *    (This routine is an awful lot like create_primary_split_list)
 *
 * Input :      node
 *              start and stop indices for the arrays (which obs apply)
 *
 * Output:      Fills in the node's
 *                      surrogate splits
 *                      lastsurrogate value
 *
 * Uses:        The global vector tempvec (integer) as a temporary, assumed
 *                to be of length n.
 */
#include "rpart.h"
#include "node.h"
#include "rpartproto.h"

void
surrogate(pNode me, int n1, int n2)
{
    int i, j, k;
    int primary_split_variable;                    /* the primary split variable */
    double split;
    double improve;
    double left_count, right_count;      /* weight sent left and right by
				 * primary */
    int extra;
    pSplit ss;
    int *index;
    int *tempy;
    double **xdata;
    int ncat;
    double adj_agree;

    tempy = rp.tempvec;
    xdata = rp.xdata;
    /*
     * First construct, in tempy, the "y" variable for this calculation.
     * It will be LEFT:goes left, 0:missing, RIGHT:goes right.
     *  Count up the number of obs the primary sends to the left, as my
     *  last surrogate (or to the right, if larger).
     */
    primary_split_variable = (me->primary)->var_num;
    if (rp.variable_types[primary_split_variable] == CONTINIOUS_VARIABLE)
	{
		split = (me->primary)->spoint;
		extra = (me->primary)->csplit[0];
		for (i = n1; i < n2; i++)
		{
			j = rp.sort_index_matrix[primary_split_variable][i];
			if (j < 0)
			tempy[-(j + 1)] = 0;
			else
			tempy[j] = (xdata[primary_split_variable][j] < split) ? extra : -extra;
		}
    }
	else
	{                    /* categorical variable */
		index = (me->primary)->csplit; //pointer decay
		for (i = n1; i < n2; i++)
		{
			j = rp.sort_index_matrix[primary_split_variable][i];
			if (j < 0)
				tempy[-(j + 1)] = 0;
			else
				tempy[j] = index[(int) xdata[primary_split_variable][j] - 1];
		}
    }

    /* count the total number sent left and right */
    left_count = 0;
    right_count = 0;
    for (i = n1; i < n2; i++) {
	j = rp.sort_index_matrix[primary_split_variable][i];
	if (j < 0)
	    j = -(1 + j);
	switch (tempy[j]) {
	case LEFT:
	    left_count += rp.wt[j];
	    break;
	case RIGHT:
	    right_count += rp.wt[j];
	    break;
	default:
	    break;
	}
    }

    if (left_count < right_count)
	me->lastsurrogate = RIGHT;
    else {
	if (left_count > right_count)
	    me->lastsurrogate = LEFT;
	else
	    me->lastsurrogate = 0;      /* no default */
    }

    /*
     * Now walk through the variables
     */
    me->surrogate = (pSplit) NULL;
    for (i = 0; i < rp.predictor_count; i++) {
	if (primary_split_variable == i)
	    continue;
	ncat = rp.variable_types[i];

	choose_surg(n1, n2, tempy, xdata[i], rp.sort_index_matrix[i], ncat,
		    &improve, &split, rp.csplit, left_count, right_count, &adj_agree);

	if (adj_agree <= 1e-10)    /* was 0 */
	    continue;           /* no better than default */

	/* sort it onto the list of surrogates */
	ss = insert_split(&(me->surrogate), ncat, improve, rp.maximum_surogate_splits);
	if (ss) {
	    ss->improvment = improve;
	    ss->var_num = i;
	    ss->count = 0;      /* corrected by nodesplit() */
	    ss->adj = adj_agree;
	    if (rp.variable_types[i] == 0) {
		ss->spoint = split;
		ss->csplit[0] = rp.csplit[0];
	    } else
		for (k = 0; k < rp.variable_types[i]; k++)
		    ss->csplit[k] = rp.csplit[k];
	}
    }
}
