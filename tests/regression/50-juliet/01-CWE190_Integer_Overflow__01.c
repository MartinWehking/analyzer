#include<stdio.h>

void main()
{
    char data;
    fscanf(stdin, "%c", &data);
    {
        char result = data + 1;  // TODO WARN: potential overflow
        printf("%hhd\n", result);
    }
}
