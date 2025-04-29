function* count() {
  yield "one";
  yield "two";
  return "three";
}

for (const value of count()) {
  console.log(value);
}

Promise.resolve().then(() => console.log(1));
queueMicrotask(() => console.log(2));
setTimeout(() => console.log(3), 0);
console.log(4);
new Promise(() => console.log(5));
(async () => console.log(6))();

class Dog {
  constructor(name) {
    this.name = name;
    this.wagTail = () => {
      return "Wag one";
    };
  }

  bark() {
    return "woof bruv";
  }
}

const dog1 = new Dog("matt");
const dog2 = new Dog("paul");
