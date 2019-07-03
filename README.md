# FunSeqSet

Welcome to the description of FunSeqSet, the proposal for a purely Functional Sequential-Set data structure. The following are the (hopefully) useful steps towards the compilation and running for some experiments.

Since we want to prove that a semi-strict (or semi-lazy) structure is faster ,in our case, than its strict counterpart, we have uploaded a corresponding version for each case, that is, a version for LazyPairingHeap and a version for Set. So, in total, we are providing eight files, four for each evaluation-ish version.

The main module is "EF**x**.hs", where **x** is either _sets_ or _heap_. Next, we have the module for random-generation of forests (and trees), called "RndDynTs**y**.hs", where **y** is for _Set_ or _Heap_. Finally we have the running modules. Here we have one for the lookups or _connectivity-only_ operation and one module for the _update_ operations. The former called "readConn**y**.hs" and the latter called "readDynTs**y**.hs" 

Prior to build up our first executable file, we need to verify we have installed the corresponding libraries for randomness and the semi-lazy structure LazyPairingHeap. We can do this through **cabal**. For the first library simply run 
> cabal update

> cabal install random

then

> cabal install edisoncore 

both accesible from the [Hackage library repository] at (http://hackage.haskell.org/package/random-1.1) and (https://hackage.haskell.org/package/EdisonCore-1.3.2.1) respectively.

## Compilation process
Now, we are ready to compile our first module. We will show the *heap* version but the correspoding *set* is easily mirrored

> ghc -O2 -o dt   readDynTsHeap.hs

> ghc -O2 -o conn readConnHeap.hs


## Performing the 1st experiment
In order to obtain the results from Figure ![Fig1](https://github.com/jcsaenzcarrasco/FunSeqSet/blob/master/figs/Fig1.pdf), we run the following (time varies as it depends on the host architecture)

> time ./dt 5000 txts/link--19992-82888-20000-0-19.txt 

The above executes 5,000 *link* operations over a forest of 20,000 node-size. The file-argument can be found in the present repository

## Performing the 2nd experiment
In order to obtain the results from Figure ![Fig2] (https://github.com/jcsaenzcarrasco/FunSeqSet/blob/master/figs/Fig2a.pdf), we run the following. Since the figure is plotting four functions we need to perform **dt** altogether with four input files

> time ./dt 2000 txts/link--9999-49000-10000-0-19.txt 

The above executes 2,000 *link* operations over a forest of 10,000 node-size.

> time ./dt 12000 txts/cut--19998-49000-10000-9999-19.txt

Since it is not possible to perform *cuts* on singleton-tree, we first *link* the desired amount and then *cut*. That is, the above execution requests 12,000 operations from which the first 10000 are just for *link* and the remaining 2,000 are finally ready for the *cut* operations

> time ./dt 2000 txts/both--10K-10016-100K-0-1.txt 

