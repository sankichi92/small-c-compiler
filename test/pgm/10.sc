void main() {
  int v[2], *s[2];

  v[0] = 1;
  v[1] = 2;

  s[0] = &v[0];
  s[1] = &v[1];

  print(*s[1] - *s[0]);
}
