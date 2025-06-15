void printstr(char *str) {
  while (*str) {
    putchar(*str++);
  }
}

int main() {
  int ch;
  
  printstr("Enter a character: ");
  read(0, &ch, 1);
  printstr("You entered: ");
  putchar(ch);
  putchar('\n');
  
  return 0;
}
