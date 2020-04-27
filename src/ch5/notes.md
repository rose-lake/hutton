# List comprehensions

## Basic example
Squaring every element from 1 to 5
```
> [ x^2 | x <- [1..5] ]
[1,4,9,16,25]
```

## terminology
`|` - is read as 'such that'
`<-` - is read as 'is drawn from'
`x <- [1..5]` - is called a 'generator expression'

## more examples
All possible pairings of an element from the list [1, 2, 3] with an element from the list [4, 5]
```
> [ (x, y) | x <- [1, 2, 3], y <- [4, 5] ]
[(1,4),(1,5),(2,4),(2,5),(3,4),(3,5)]
```
notice the order of the output
x: 1, 1, 2, 2, 3, 3
y: 4, 5, 4, 5, 4, 5

### later generators change their values more frequently than earlier generators

notice the difference if you change the order of the generator expressions
```
> [ (x, y) | y <- [4, 5], x <- [1, 2, 3] ]
[(1,4),(2,4),(3,4),(1,5),(2,5),(3,5)]
```
x: 1, 2, 3, 1, 2, 3
y: 4, 4, 4, 5, 5, 5

### later generators can also depend on earlier generators

a list of all possible pairings of the elements 1, 2, 3
```
> [(x,y) | x <- [1..3], y <- [x..3]]
[(1,1),(1,2),(1,3),(2,2),(2,3),(3,3)]
```

## Guards
Guards are logical expressions that come after a generator, also separated by a comma. Just like the nesting / dependence of later generators on earlier ones, a guard is a logical expression that 'filters' the earlier generator.
```
> [x | x <- [1..10], even x]
[2,4,6,8,10]
```
