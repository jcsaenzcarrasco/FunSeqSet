# FunSeqSet

Welcome to the description of FunSeqSet, the proposal for a purely Functional Sequential-Set data structure. The following are the (hopefully) useful steps towards the compilation and running for some experiments.

Since we want to prove that a semi-strict (or semi-lazy) structure is faster ,in our case, than its strict counterpart, we have uploaded a corresponding version for each case, that is, a version for LazyPairingHeap and a version for Set. So, in total, we are providing eight files, four for each evaluation-ish version.

The main module is "EF**x**.hs", where **x** is either _sets_ or _heap_. Next, we have the module for random-generation of forests (and trees), called "RndDynTs**y**.hs", where **y** is for _Set_ or _Heap_. Finally we have the running modules. Here we have one for the lookups or _connectivity-only_ operation and one module for the _update_ operations. The former called "readConn**y**.hs" and the latter called "readDynTs**y**.hs" 

Prior to build up our first executable file, we need to verify we have installed the corresponding libraries for randomness and the semi-lazy structure LazyPairingHeap. We can do this through **cabal**. For the first library simply run 
> cabal update

> cabal install random

then

> cabal install edisoncore 

both accesible from the library repository Hackage.

Now, we are ready to compile our first module. We will show the *heap* version but the correspoding *set* is easily mirrored

> ghc -O2 -o dt   readDynTsHeap.hs

> ghc -O2 -o conn readConnHeap.hs


