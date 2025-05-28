#include<stdio.h>
#include<signal.h>

void
handle_sigint()
{
    printf("RECV SIGINT\n");
    exit(0);
}

int main()
{
    signal(SIGINT, handle_sigint);
    while (1)
    {
        printf("hello world\n");
        sleep(1);
    }
    return 0;
}
