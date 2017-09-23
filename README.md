# Schemers and Provers

Code exercises from the *Little Schemer* series of books. 


## Books series:

 * [Little Schemer](https://mitpress.mit.edu/books/little-schemer)
   by Daniel P. Friedmann and Matthias Felleisen

 * [Seasoned Schemer](https://mitpress.mit.edu/books/seasoned-schemer)
   by Daniel P. Friedman and Matthias Felleisen

 * [Reasoned Schemer](https://mitpress.mit.edu/books/reasoned-schemer)
   by Daniel P. Friedman, William E. Byrd and Oleg Kiselyov

 * [Little Prover](https://mitpress.mit.edu/books/little-prover)
   by Daniel P. Friedman and Carl Eastlund


## Implementation

The code is implemented in [Racket](https://racket-lang.org/) with 
a collection of unit tests implemented as submodules in
[RackUnit](https://docs.racket-lang.org/rackunit/) and
[raco](https://docs.racket-lang.org/raco) as a test runner. 


## Running the code

```{bash}
$ git clone https://github.com/trzewiczek/schemers.git
$ yarn
$ yarn test
```

There is a script for running tests in watch mode that reruns tests for
every change in `**/*.rkt` files. 

```{bash}
$ yarn test:watch
```

## License

All original code is licensed under MIT license. 

