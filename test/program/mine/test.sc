int f(int x) {
  while(x > 1) {
    x = x - 2;
  }
  return x;
}

void main() {
  int x;
  print(f(9));
}
