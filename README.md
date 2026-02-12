# Alien Alphabet Challenge (Stack Overflow)

This project reconstructs an unknown alphabet from a sorted dictionary.

## Approach
- Extract ordering constraints from adjacent words.
- Build a directed graph of symbol dependencies.
- Apply Kahn's algorithm to compute the alphabet.
- Validated the result by rechecking dictionary order.

Language: R
