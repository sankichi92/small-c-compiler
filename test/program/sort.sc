int a[8];
void exchange(int i, int j);
int rearrange(int i, int j);
void sort(int i, int j);

void exchange(int i, int j)
{
  int temp;

  temp = a[i];
  a[i] = a[j];
  a[j] = temp;
}

int rearrange(int i, int j)
{
  int LEFT, RIGHT;
  int mover;

  LEFT = 0;
  RIGHT = 1;
  mover = LEFT;

  while (i < j) {
    if (mover == LEFT) {
      if (a[i] > a[j]) {
        exchange(a, i, j);
        mover = RIGHT;
      } else {
        i = i + 1;
      }
    } else {
      if (a[i] > a[j]) {
        exchange(a, i, j);
        mover = LEFT;
      } else {
        j = j - 1;
      }
    }
  }
  return i;
}

void sort(int i, int j)
{
  if (i < j) {
    int p;
    p = rearrange(a, i, j);
    sort(a, i, p - 1);
    sort(a, p + 1, j);
  }
}

void main()
{
  int i;

  a[0] = 6;
  a[1] = 3;
  a[2] = 1;
  a[3] = 7;
  a[4] = 2;
  a[5] = 4;
  a[6] = 8;
  a[7] = 5;

  sort(a, 0, 7);

  for(i = 0; i < 8; i = i + 1) {
    print(a[i]);
  }
}
