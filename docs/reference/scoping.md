# Scoping

Moss distinguishes between _lexical_ scoping (determining which declaration a particular symbol refers to) and _semantic_ scoping (determining which definition to use for a particular usage of a declaration). This document describes the former, which can be mostly resolved using only syntactic information before typechecking, although there is one exception.
