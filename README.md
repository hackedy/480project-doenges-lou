# Building

To build the code, OCaml and OPAM must be installed. OPAM is a package manager
that OCaml programmers use to track and install dependencies. If OPAM and OCaml
aren't installed, run the following commands and respond **Y** to any prompts.
```
apt-get install ocaml opam
opam init
eval $(opam config env)
```
Replace `apt-get install` with your local system's package manager if necessary.

Once OPAM is all set up, install dependencies.
```
opam install zarith ounit batteries
opam depext
```

Now running
```
make
```
should build the demos, tests, and documentation.

To see tests run, you can execute `make test`. To get an idea of what tests are
actually being run, try `./test.native -list-test` or look at `tests/test.ml`.

To read the documentation, open `ntru.docdir/index.html` after running `make` or
`make doc`.

# The demos
## Parameter estimation
The program `./ntruparams.native`, given various other NTRU parameters and a
positive integer *m*, will compute the least prime *q* such that the resulting
cryptosystem is leveled homomorphic of level *m*. It's a good way to generate
very large primes: *q* is exponential in the prime *p* and the dimension *N*.

The source code for `ntruparams.native` can be found in `src/ntruparams.ml`.

```
$ ./ntruparams.native -help
NTRU parameter selection demo
  -rank degree N for the modulus phi = x^N - 1
  -ntru-p lower bound on the prime modulus p
  -alpha additive level
  -mu multiplicative level
  -help  Display this list of options
  --help  Display this list of options
```

## Homomorphic multiplication
The program `./ntrudemo.native` demonstrates that `q` is a conservative estimate
by performing random multiplications of ciphertexts until they fail to decrypt.
Typically failures appear after around 2*m* to 4*m* multiplications.
The demo starts with an array of random *p*-bounded polynomials and at each
iteration randomly chooses to multiply some of them together.

Its source code is in `src/ntrudemo.ml`.

```
$ ./ntrudemo.native -help
NTRU demo
  -rank degree N for the modulus phi = x^N - 1 [default: 11]
  -ntru-p lower bound on the prime modulus p [default: 13]
  -alpha desired additive level [default: 1]
  -mu desired multiplicative level [default: 10]
  -polys number of polynomials to seed with [default: 10]
  -mix probability in 0.0 to 1.0 of a multiplication happening [default: 0.1]
  -help  Display this list of options
  --help  Display this list of options
```
