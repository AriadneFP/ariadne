struct foo_struct{
  double x;
};
typedef struct foo_struct foo;

int main(void) {
  //double x = 3.0;
  foo data = {2.0};
  //double y = sqrt(data.x);
  //printf("y=%f\n",data.x);
  return 0;
}