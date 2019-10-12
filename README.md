## dtautom

_A decision theory automaton._

Alpha quality right now. See https://www.alignmentforum.org/posts/qdqYrcGZTh9Lp49Nj/creating-environments-to-design-and-test-embedded-agents

## Misc messy thoughts

There are no instruction pointers; there are "processors" which execute what's in front of them and then either travel or explode. Just a matter of terminology.

A common pattern will be

```
1. CONDITION INSTRUCTION
2. A
3. B
4. JUMP
5. LOCATION 1
6. 0
7. JUMP
8. LOCATION 2
9. 0
```

## Links

http://vyznev.net/corewar/guide.html