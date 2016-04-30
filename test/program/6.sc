int f(int a, int b, int c, int d, int e, int f)
{
  int x, y[4];
  x = a;
  y[0] = b + c;
  y[1] = d + e;
  y[2] = f;
  y[3] = 4;
  return x + y[0] + y[1] + y[2] + y[3];
}

void main()
{
  print(f(1, -2, 2, -3, 3, -4));
}
