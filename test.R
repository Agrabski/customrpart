rpart(Kyphosis ~ Age + Number + Start, data = kyphosis, split_pick_function =  function(x, y){
	print(x);
	print(y)

	result = x;
	}, split_pick_threshold = 5)




