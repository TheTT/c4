int read_int() {
  char ch;
  int num = 0;
  int sign = 1;
  
  while (1) {
    read(0, &ch, 1);
    if (ch != ' ' && ch != '\t' && ch != '\n' && ch != '\r') {
      break;
    }
  }
  
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

int main() {
  int a,b;

  a = read_int();
  b = read_int();

  printf("%d\n", a + b);

  return 0;
}