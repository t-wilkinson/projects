#include <stdio.h>

#define SIZE 3

void sizes() {
  printf("char:%d int:%d double:%d\n", sizeof(char), sizeof(int),
         sizeof(double));
}

typedef struct Stack {
  int top;
  int size;
  void (*push)(struct Stack *, int);
  int (*pop)(struct Stack *);
  int elements[SIZE];
} Stack;

void Stack_push(Stack *this, int x) {
  int size = sizeof(this->elements) / sizeof(int);
  if (this->top == size) {
    printf("Error: cannot push full stack\n");
    return;
  }

  this->top++;
  this->elements[this->top] = x;
  printf("[stack push] %d\n", x);
}

int Stack_pop(Stack *this) {
  if (this->top < 0) {
    printf("Error: cannot pop empty stack\n");
    return -1;
  }

  int x = this->elements[this->top];
  printf("[stack pop] %d\n", x);
  this->top--;
  return x;
}

Stack Stack_create(int size) {
  Stack this = {.top = -1,
                .size = SIZE,
                .elements = {},
                .push = Stack_push,
                .pop = Stack_pop};
  return this;
}

typedef struct Queue {
  int head;
  int tail;
  int size;
  void (*enqueue)(struct Queue *, int);
  int (*dequeue)(struct Queue *);
  int elements[SIZE];
} Queue;

void Queue_enqueue(Queue *this, int elem) {
  // Increment tail and wrap if necessary
  int nextTail = this->tail + 1;
  if (nextTail == this->size) {
    nextTail = 0;
  }

  if (nextTail == this->head) {
    printf("[queue enqueue] queue is full at %d\n", nextTail);
    return;
  }

  this->elements[this->tail] = elem;
  printf("[queue enqueue] %d at %d\n", elem, this->tail);
  this->tail = nextTail;
}

int Queue_dequeue(Queue *this) {
  if (this->head == this->tail) {
    printf("[queue dequeue] empty queue\n");
    return -1;
  }

  int elem = this->elements[this->head];
  printf("[queue dequeue] %d at %d\n", elem, this->head);

  // Increment head to next position and wrap if necessary.
  this->head++;
  if (this->head == this->size) {
    this->head = 0;
  }
  return elem;
}

Queue Queue_create(int size) {
  Queue this = {
      .head = 0,
      .tail = 0,
      .size = SIZE,
      .enqueue = Queue_enqueue,
      .dequeue = Queue_dequeue,
      .elements = {},
  };
  return this;
}

int main() {
  // sizes();

  // Stack s = Stack_create(SIZE);
  // s.push(&s, 1);
  // s.push(&s, 2);
  // s.push(&s, 3);
  // printf("%d, %d, %d, %d\n", s.pop(&s), s.pop(&s), s.pop(&s), s.pop(&s));

  // Queue q = Queue_create(SIZE);
  // q.enqueue(&q, 1);
  // q.dequeue(&q);
  // q.enqueue(&q, 2);
  // q.dequeue(&q);
  // q.enqueue(&q, 3);
  // q.enqueue(&q, 4);
  // q.dequeue(&q);
  // q.dequeue(&q);
  // q.dequeue(&q);
}
