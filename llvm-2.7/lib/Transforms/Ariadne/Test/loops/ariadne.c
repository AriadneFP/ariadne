int BOUND_MAX = 5;
int sum(){
    int i,sum;
    int temp;
    temp = 0;
    for (i = 0; i < 10; i++){
      temp++;
      if (temp > BOUND_MAX)
	break;
      sum += i;
    }
    return sum;
}