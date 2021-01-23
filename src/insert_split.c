/*
 * sort a new split into a linked list, based on its "improvement"
 *
 *  allocates new memory as needed
 *   returns 0 if the new element isn't good enough,
 *   the address of the new element otherwise
 */
#include "rpart.h"
#include "node.h"
#include "rpartproto.h"

Split * insert_split(Split ** node_to_split, int variable_type, double improvment, int max_splits)
{
	int list_size;
	pSplit before_new, after_new, next_to_last = NULL, last_element;

// csplit[0] gets used even for continuous splits.
	if (variable_type == CONTINIOUS_VARIABLE)
		variable_type = 1; 
	int splitsize = sizeof(Split) + (variable_type - 20) * sizeof(int);

	// The Split structure is sized for 2 categpries.
	if (*node_to_split == NULL)
	{
		/* first call to a new list */
		next_to_last = (Split*) CALLOC(1, splitsize);
		next_to_last->nextsplit = NULL;
		*node_to_split = next_to_last;
		return next_to_last;
	}
	// replace or do noting
	if (max_splits < 2)
	{
		/* user asked for only 1 to be retained! */
		next_to_last = *node_to_split;
		if (improvment <= next_to_last->improvment)
			return NULL;
		if (variable_type > 1) {
			Free(next_to_last);
			next_to_last = (pSplit) CALLOC(1, splitsize);
			next_to_last->nextsplit = NULL;
			*node_to_split = next_to_last;
		}
		return next_to_last;
	}
	/* set up --- list_size = length of list, last_element=last element, next_to_last=next to last */
	list_size = 1;
	for (last_element = *node_to_split; last_element->nextsplit != NULL; last_element = last_element->nextsplit)
	{
		next_to_last = last_element;
		list_size++;
	}

	/* now set up so that the "to be added" is between before_new and after_new */
	before_new = *node_to_split;
	for (after_new = *node_to_split; after_new != NULL; after_new = after_new->nextsplit)
	{
		if (improvment > after_new->improvment)
			break;
		before_new = after_new;
	}

	if (list_size == max_splits)
	{
		if (after_new == NULL)
			return NULL; // not good enough
		if (variable_type > 1)
		{
		// FIXME: use Realloc
			Free(last_element); // get new memory -- this chunk may be too small 
			last_element = (pSplit) CALLOC(1, splitsize);
		}
		if (before_new == next_to_last)
			last_element->nextsplit = NULL;
		else
		{
			next_to_last->nextsplit = NULL;
			last_element->nextsplit = after_new;
		}
	}
	else
	{
		last_element = (pSplit) CALLOC(1, splitsize);
		last_element->nextsplit = after_new;
	}
	if (after_new == *node_to_split)
		*node_to_split = last_element;
	else
		before_new->nextsplit = last_element;
	return last_element;
}
