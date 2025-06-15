int read_int() {
  char ch;
  int num, sign;
  ch = ' ';
  num = 0;
  sign = 1;
  
  while (ch == ' ' || ch == '\t' || ch == '\n' || ch == '\r')
    read(0, &ch, 1);
  
  if (ch == '-') {
    sign = -1;
    read(0, &ch, 1);
  } else if (ch == '+') {
    read(0, &ch, 1);
  }
  
  while (ch >= '0' && ch <= '9') {
    num = num * 10 + (ch - '0');
    read(0, &ch, 1);
  }
  
  return num * sign;
}

void printint(int n) {
  char *buf;
  int i;
  int sign;
  i = 0; sign = 0;
  buf = (char *)malloc(20);
  if (n < 0) {
    sign = 1;
    n = -n;
  }
  
  if (n == 0) {
    putchar('0');
    return;
  }
  
  while (n > 0) {
    buf[i++] = '0' + (n % 10);
    n = n / 10;
  }
  
  if (sign) putchar('-');
  
  while (i > 0) {
    putchar(buf[--i]);
  }
}

int main() {
  int a,b;

  a = read_int();
  b = read_int();

  printint(a + b);
  putchar('\n');

  return 0;
}