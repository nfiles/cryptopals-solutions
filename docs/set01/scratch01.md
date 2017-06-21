# Challenges

## Challenge 1

### Base 64 Hash

```text
|0    |5    |10   |15   |20   |25   |30   |35   |40   |45   |50   |55   |60
ABCDE FGHIJ KLMNO PQRST UVWXY Zabcd efghi jklmn opqrs tuvwx yz012 34567 89+/
```

### Hex -> Base64

| Step | Data |
|-------|-------|
| hex | `01 AB CD` |
| bytes | `00000001 10101011 11001101` |
| sextets | `000000 011010 101111 001101` |
| base64 numbers | `0 26 47 13` |
| base64 encoded | `A a v N` |

## Challenge 3 Solve single-byte cipher

Calculate variance from expected frequencies

`Sum for each character in output => (expected frequency - actual frequency) ** 2 / length`
