# FunSeqSet

Welcome to the description of FunSeqSet, the proposal for a purely Functional Sequential-Set data structure. The following are the (hopefully) useful steps towards the compilation and running for some experiments.

Since we want to prove that a semi-strict (or semilazy) structure is faster ,in our case, than its strict counterpart, we have uploaded a corresponding version for each case, that is, a version for PairingLazyHeap and a version for Set. So, in total, we are providing eight files, four for each evaluation-ish version.

The main module is "EF**x**.hs", where **x** is either _sets_ or _heap_. Next, we have the module for random-generation of forests (and trees), called "RndDynTs**y**.hs", where **y** is for _Set_ or _Heap_. Finally we have the running modules. Here we have one for the lookups or _connectivity-only_ operation and one module for the _update_ operations. The former called "readConn**y**.hs" and the latter called "readDynTs**y**.hs" 
