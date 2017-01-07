void pChar(ch)
     char ch;
{
  char *uart = 0xffe0;

  while (*uart & 0x02) {
    ;
  }

  uart[1] = ch;
}

void say(message)
     char *message;
{
  while (*message) {
    pChar(*message++);
  }
}

int main()
{
  say("Hello, world\n");
}
