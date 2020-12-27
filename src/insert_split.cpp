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

pSplit
insert_split(Split ** node_to_split, int variable_type, double improvment, int max_splits)
{
	int list_size;
	pSplit s1, s2, s3 = NULL, s4;

// csplit[0] gets used even for continuous splits.
	if (variable_type == CONTINIOUS_VARIABLE)
		variable_type = 1; 
	int splitsize = sizeof(Split) + (variable_type - 20) * sizeof(int);

	// The Split structure is sized for 2 categpries.
	if (*node_to_split == NULL)
	{
		/* first call to a new list */
		s3 = (Split*) CALLOC(1, splitsize);
		s3->nextsplit = NULL;
		*node_to_split = s3;
		return s3;
	}
	if (max_splits < 2)
	{
	   /* user asked for only 1 to be retained! */
		s3 = *node_to_split;
		if (improvment <= s3->improvment)
			return NULL;
		if (variable_type > 1) {
			Free(s3);
			s3 = (pSplit) CALLOC(1, splitsize);
			s3->nextsplit = NULL;
			*node_to_split = s3;
		}
		return s3;
	}
	/* set up --- list_size = length of list, s4=last element, s3=next to last */
	list_size = 1;
	for (s4 = *node_to_split; s4->nextsplit; s4 = s4->nextsplit)
	{
		s3 = s4;
		list_size++;
	}

	/* now set up so that the "to be added" is between s1 and s2 */
	s1 = *node_to_split;
	for (s2 = *node_to_split; s2 != NULL; s2 = s2->nextsplit)
	{
		if (improvment > s2->improvment)
			break;
		s1 = s2;
	}

	if (list_size == max_splits)
	{
		if (s2 == NULL)
			return NULL;        /* not good enough */
		if (variable_type > 1) {
		// FIXME: use Realloc
			Free(s4);           /* get new memory -- this chunk may be too
					* small */
			s4 = (pSplit) CALLOC(1, splitsize);
		}
		if (s1 == s3)
			s4->nextsplit = NULL;
		else
		{
			s3->nextsplit = NULL;
			s4->nextsplit = s2;
		}
	}
	else
	{
		s4 = (pSplit) CALLOC(1, splitsize);
		s4->nextsplit = s2;
	}
	if (s2 == *node_to_split)
		*node_to_split = s4;
	else
		s1->nextsplit = s4;
	return s4;
}
