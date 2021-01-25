rpart(Kyphosis ~ Age + Number + Start, data = kyphosis, split_pick_function =  function(x, y){
	print(x);
	print(y)

	result = x[1];
	}, split_pick_threshold = 0)




