/*
 * The routine which will find the best split for a node
 *
 * Input :      node
 *              node number
 *
 * Output:      Fills in the node's
 *                      primary splits
 *                      competitor splits
 */
#include "rpart.h"
#include "node.h"
#include "rpartproto.h"

void create_primary_split_list(Node* me, int n1, int n2)
{
    int i, j, k;
    int kk;
    int variable_type;
    double improvment;
    double split = 0.0;
    Split* tsplit;
    int *index;
    double *xtemp;
    double **ytemp;
    double *wtemp;

    xtemp = rp.xtemp;
    ytemp = rp.ytemp;
    wtemp = rp.wtemp;

    // test out the variables 1 at at time
    me->primary = NULL;
    for (i = 0; i < rp.predictor_count; i++)
	{
		index = rp.sort_index_matrix[i];
		variable_type = rp.variable_types[i];
		// extract x and y data
		k = 0;
		for (j = n1; j < n2; j++)
		{
			kk = index[j];
			if (kk >= 0 && rp.wt[kk] > 0) 
			{  /* x data not missing and wt > 0 */
				xtemp[k] = rp.xdata[i][kk];
				ytemp[k] = rp.ydata[kk];
				wtemp[k] = rp.wt[kk];
				k++;
			}
		}

		if (k == 0 || (variable_type == CONTINIOUS_VARIABLE && xtemp[0] == xtemp[k - 1]))
			continue; // no place to split

		(*rp_choose) (k, ytemp, xtemp, variable_type, rp.min_node, &improvment,
				&split, rp.csplit, me->risk, wtemp);

		/*
		* Originally, this just said "if (improve > 0)", but rounding
		* error will sometimes create a non zero that should be 0.  Yet we
		* want to retain invariance to the scale of "improve".
		*/
		if (improvment > rp.iscale)
			rp.iscale = improvment; //largest seen so far
		if (improvment > (rp.iscale * 1e-10))
		{
			improvment /= rp.variable_cost[i];     /* scale the improvement */
			tsplit = insert_split(&(me->primary), variable_type, improvment, rp.max_primary_splits);
			if (tsplit)
			{
				tsplit->improvment = improvment;
				tsplit->var_num = i;
				tsplit->spoint = split;
				tsplit->count = k;
				if (variable_type == CONTINIOUS_VARIABLE)
				{
					tsplit->spoint = split;
					tsplit->csplit[0] = rp.csplit[0];
				}
				else
					for (k = 0; k < variable_type; k++)
						tsplit->csplit[k] = rp.csplit[k];
			}
		}
    }
}
