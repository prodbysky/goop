let upto: i32 = 10;

let sum: i32 = 0;

let counter: i32 = 0;

while counter < upto + 1 {
	if counter % 2 > 0 {
		sum = sum + 1;
	}
	counter = counter + 1;
}

return sum;


