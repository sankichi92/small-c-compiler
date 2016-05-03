void main()
{
  int *a[1], b;
  b = 1;
  a[0] = &b;
  print(*a[0]);
}
